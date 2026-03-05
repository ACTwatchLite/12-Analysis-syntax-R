################################################################################
# ACTwatch LITE 
# Step 2.1 APPEND PRODUCT DATASETS AND APPEND OUTLET DATA TO CREATE A SINGLE ANALYTIC DATASET
# REORGANIZED VERSION - HTML logging at end
################################################################################
#
# This section appends the cleaned antimalarial and RDT product audit datasets 
# into a single long-format dataset and merges them with the outlet dataset to 
# create one unified analytic file. A variable is generated to classify product 
# types (e.g., TSG, non-TSG, diagnostics, stockouts), and checks are included 
# to confirm that all product records are correctly linked to an outlet. After 
# verifying merge results and recoding missing values, the final cleaned dataset 
# is saved for use in downstream analysis.

# The following flags are used throughout the syntax.
# $$$ = Breaks the script to remind analyst to modify syntax.
# /* EXAMPLE: */ = Sample syntax from pilot studies for reference
# 
# Please initial all comments/responses and make note of changes.

# NOTE 
# *EACH* STEP FLAGGED WITH "$$$" HAS SECTIONS WHERE MANUAL INPUTS OR EDITS ARE REQUIRED. 	
# REVIEW LINES OF SYNTAX MARKED WITH "$$$". MANAGE/CLEAN DATA ACCORDINGLY. 

################################################################################
# SECTION 1: DATA FILE VALIDATION
################################################################################

# Define required files with their descriptions
required_files <- list(
  list(
    path = here("Data", "Cleaned data", paste0("AwL_", country, "_", year, "_amaudit_clean.csv")),
    name = "Cleaned antimalarial data",
    script = "Antimalarial Cleaning"
  ),
  list(
    path = here("Data", "Cleaned data", paste0("AwL_", country, "_", year, "_rdtaudit_clean.csv")),
    name = "Cleaned RDT data",
    script = "RDT Cleaning"
  ),
  list(
    path = here("Data", "Cleaned data", paste0("AwL_", country, "_", year, "_outlet_clean.csv")),
    name = "Cleaned outlet data", 
    script = "Outlet Cleaning"
  )
)

# Check which files are missing
missing_files <- required_files[!sapply(required_files, function(x) file.exists(x$path))]

# Stop if any files are missing
if (length(missing_files) > 0) {
  error_messages <- sapply(missing_files, function(x) {
    paste0("\n", x$name, " not found. Please run '", x$script, "' first.")
  })
  stop(paste(error_messages, collapse = ""))
}

################################################################################
# SECTION 2: LOAD AND PREPARE ANTIMALARIAL DATA
################################################################################

am_data <- fread(here("Data", "Cleaned data", paste0("AwL_", country, "_", year, "_amaudit_clean.csv")))

# SAVE FOR LOGGING
am_count <- nrow(am_data)

# Create a variable to identify product types
am_data <- am_data %>%
  mutate(
    producttype = case_when(
      a3 %in% c(1, 2, 3) ~ 1L,  # TSG (tablet, suppository, granule)
      a3 %in% c(4, 5, 6, 7, 8) ~ 2L,  # non-TSG (other antimalarial types)
      TRUE ~ NA_integer_
    )
  )

# Add value labels (stored as attributes for Stata compatibility)
am_data$producttype <- labelled(am_data$producttype,
                                labels = c("TSG" = 1, "NT" = 2, "Diagnostics" = 3, "Stock-out" = 4))

# SAVE FOR LOGGING
producttype_am <- create_summary_table(am_data, producttype)

################################################################################
# SECTION 3: LOAD AND APPEND RDT DATA
################################################################################

rdt_data <- fread(here("Data", "Cleaned data", paste0("AwL_", country, "_", year, "_rdtaudit_clean.csv"))) 

# SAVE FOR LOGGING
rdt_count <- nrow(rdt_data)

# Combine datasets
combined_data <- bind_rows(am_data, rdt_data)

# Update producttype for RDT records
combined_data <- combined_data %>%
  mutate(
    producttype = case_when(
      is.na(producttype) & !is.na(setofrdtaudit) & setofrdtaudit != "" ~ 3L,
      TRUE ~ producttype
    )
  ) %>% 
  mutate(merge_key = coalesce(amauditkey, rdtauditkey))

# Re-apply labels
combined_data$producttype <- labelled(combined_data$producttype,
                                      labels = c("TSG" = 1, "NT" = 2, "Diagnostics" = 3, "Stock-out" = 4))

# SAVE FOR LOGGING
producttype_combined <- create_summary_table(combined_data, producttype)
combined_count <- nrow(combined_data)

# Remove _merge column if it exists
combined_data <- combined_data %>%
  select(-any_of("_merge"))

# Save temporary file
fwrite(combined_data, here("Data", "Management data", paste0(country, "_", year, "_amrdt_long_cleaned_temp.csv")),
       row.names=F, na="")

################################################################################
# SECTION 4: MERGE OUTLET-LEVEL DATA
################################################################################

# Load outlet data
outlet_data <- fread(here("Data", "Cleaned data", paste0("AwL_", country, "_", year, "_outlet_clean.csv")))

# SAVE FOR LOGGING
outlet_count <- nrow(outlet_data)

# Remove _merge column if it exists
outlet_data <- outlet_data %>%
  select(-any_of("_merge"))

# Perform merge (equivalent to Stata's merge 1:m)
merged_data <- outlet_data %>%
  left_join(combined_data,  by="key", 
            relationship = "one-to-many",
            suffix = c("", ".y")) %>% 
  select(-contains(".y")) %>% 
  mutate(formdef_version  = as.character(formdef_version))

# Create merge indicator (mimicking Stata's _merge variable)
merged_data <- merged_data %>%
  mutate(
    `_merge` = case_when(
      !is.na(setofamaudit) | !is.na(setofrdtaudit) ~ 3L,  # matched
      is.na(setofamaudit) & is.na(setofrdtaudit) ~ 1L,    # master only (outlet only)
      TRUE ~ 2L  # using only (product only)
    )
  )

################################################################################
# SECTION 5: MERGE VALIDATION
################################################################################

# SAVE FOR LOGGING - Check merge results by producttype
merge_results <- merged_data %>%
  count(`_merge`, producttype, .drop = FALSE) %>%
  pivot_wider(names_from = producttype, values_from = n, values_fill = 0)

# SAVE FOR LOGGING - Check outlet records with no antimalarials/diagnostics (_merge==1)
outlets_only <- merged_data %>%
  filter(`_merge` == 1) %>%
  select(key, outletid)

# SAVE FOR LOGGING - Check setofamaudit for these outlets
outlets_amaudit <- merged_data %>%
  filter(`_merge` == 1) %>%
  count(setofamaudit, .drop = FALSE)

# SAVE FOR LOGGING - Check antimalarial audit records that don't match to an outlet (_merge==2)
products_only <- merged_data %>%
  filter(`_merge` == 2)

# SAVE FOR LOGGING
products_producttype <- products_only %>%
  count(producttype, .drop = FALSE)

products_sample <- products_only %>%
  select(key, setofamaudit, setofrdtaudit) %>%
  head(10)

# Once merge is checked, drop _merge
merged_data <- merged_data %>%
  select(-`_merge`)

################################################################################
# SECTION 6: RECODE SYSTEM MISSING VALUES
################################################################################

merged_data <- merged_data %>%
  mutate(
    formdef_version = as.character(formdef_version),
    across(where(is.numeric), ~case_when(
      .x %in% c(-998, -988, -977, -99, -97, -98, -9998, -9777, -9888) ~ NA_real_,
      TRUE ~ .x
    ))
  )

################################################################################
# SECTION 7: VALIDATE AND COMPLETE PRODUCT TYPE CLASSIFICATION
################################################################################

# SAVE FOR LOGGING
producttype_before <- create_summary_table(merged_data, producttype)

# Recode producttype for diagnostics and stockouts
merged_data <- merged_data %>%
  mutate(
    producttype = case_when(
      is.na(producttype) & (d3 == 1 | d7 == 1) ~ 3L,  # diagnostics
      is.na(producttype) & (a16 == 1 | d16 == 1) ~ 4L,  # stocked out
      TRUE ~ producttype
    )
  )

# Re-apply labels
merged_data$producttype <- labelled(merged_data$producttype,
                                    labels = c("TSG" = 1, "NT" = 2, "Diagnostics" = 3, "Stock-out" = 4))

# SAVE FOR LOGGING
producttype_after <- create_summary_table(merged_data, producttype)

################################################################################
# SECTION 8: GENERATE nOut VARIABLE
################################################################################

merged_data <- merged_data %>%
  select(-any_of("nOut")) %>%
  arrange(outletid) %>% 
  group_by(outletid, .drop=F) %>%
  mutate(nOut = ifelse(row_number() == 1, 1L, 0L)) %>%
  ungroup()

# Add variable label
attr(merged_data$nOut, "label") <- "Flags one entry for each outlet"

# SAVE FOR LOGGING
unique_outlets <- sum(merged_data$nOut == 1, na.rm = TRUE)

################################################################################
# SECTION 9: SAVE FINAL DATASET
################################################################################

output_file <- here("Data", "Management data", 
                    paste0(country, "_", year, "_am_rdt_os_cleaned_long.csv"))

fwrite(merged_data, output_file, row.names=F, na="")

# SAVE FOR LOGGING
missing_producttype <- sum(is.na(merged_data$producttype), na.rm = TRUE)

################################################################################
# GENERATE HTML LOG
################################################################################

# Setup log file
log_dir <- here("Data", "Management data", "Data cleaning notes")
if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE)

log_file <- file.path(log_dir, paste0("AwL_", country, "_", year, "_long_dataset_notes.html"))

# Initialize HTML
sink(log_file, type = "output")

cat("<html><head><title>Long Dataset Creation Log</title>")
cat("<style>
body { font-family: Arial, sans-serif; margin: 20px; }
h1, h2, h3 { color: #2E86AB; }
table { border-collapse: collapse; width: 100%; margin: 10px 0; }
th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }
th { background-color: #f2f2f2; }
.section { margin: 20px 0; padding: 15px; border-left: 4px solid #2E86AB; }
.warning { background-color: #fff3cd; border-color: #ffc107; }
.error { background-color: #f8d7da; border-color: #dc3545; }
.critical { background-color: #f8d7da; border: 3px solid #dc3545; font-weight: bold; }
pre { background-color: #f8f9fa; padding: 10px; border-radius: 5px; }
</style></head><body>")

cat("<h1>ACTwatch Lite Long Dataset Creation Log</h1>")
cat(glue("<div class='section'>
<h2>Processing Session Information</h2>
<p><strong>Date/Time:</strong> {Sys.time()}</p>
<p><strong>Country:</strong> {country}</p>
<p><strong>Year:</strong> {year}</p>
<p><strong>Script:</strong> Long Dataset Creation</p>
</div>\n"))

# =============================================================================
# HTML CONTENT GENERATION
# =============================================================================

cat("<h2>1.1.0 Data File Validation</h2>\n")
cat("<div class='section'>\n<p>All required files found and validated</p>\n</div>\n")

cat("<h2>2.1 Create Single Dataset</h2>\n")

cat("<h3>2.1.1 Load and Prepare Antimalarial Data</h3>\n")
cat(glue("<div class='section'>
<p><strong>Antimalarial records loaded:</strong> {am_count}</p>
</div>\n"))

cat("<h4>Product Type Distribution (Antimalarials Only)</h4>\n")
html_table(producttype_am, caption = "Product Type - Antimalarial Data")

cat("<h3>2.1.2 Load and Append RDT Data</h3>\n")
cat(glue("<div class='section'>
<p><strong>RDT records loaded:</strong> {rdt_count}</p>
</div>\n"))

cat("<h4>Product Type Distribution (After Append)</h4>\n")
html_table(producttype_combined, caption = "Product Type - Combined Data")

cat(glue("<div class='section'>
<p><strong>Total product records after append:</strong> {combined_count}</p>
</div>\n"))

cat("<h3>2.1.3 Merge Outlet-Level Data</h3>\n")
cat("<div class='section'>\n<p>Note: This step merges all outlet level variables to the appended product dataset</p>\n</div>\n")

cat(glue("<div class='section'>
<p><strong>Outlet records loaded:</strong> {outlet_count}</p>
</div>\n"))

cat("<h3>2.1.4 $$$ Merge Results Validation</h3>\n")

html_table(merge_results, caption = "Merge Results by Product Type - INSERT RESULTS")

cat("<div class='section warning'>\n")
cat("<p><strong>$$$ CRITICAL VALIDATION:</strong></p>\n")
cat("<ul>\n")
cat("<li>ALL PRODUCTS (AM/RDT) SHOULD MATCH TO AN OUTLET (_merge==3)</li>\n")
cat("<li>NOT ALL OUTLETS MUST MATCH TO PRODUCTS (may be screened out or stocked out)</li>\n")
cat("</ul>\n")
cat("</div>\n")

if (nrow(outlets_only) > 0) {
  cat("<h4>Outlets with No Products (_merge==1)</h4>\n")
  cat(glue("<div class='section'>
  <p><strong>Count:</strong> {nrow(outlets_only)} outlet records with no antimalarial/diagnostic products</p>
  <p>This may be expected for outlets that were screened out or had stockouts</p>
  </div>\n"))
  
  html_table(outlets_amaudit, caption = "setofamaudit values for outlets with no products")
} else {
  cat("<div class='section'>\n<p><strong>All outlet records have matching antimalarial/diagnostic products</strong></p>\n</div>\n")
}

if (nrow(products_only) > 0) {
  cat("<h4>$$$ CRITICAL: Products Not Matched to Outlets (_merge==2)</h4>\n")
  cat("<div class='section critical'>\n")
  cat(glue("<p><strong>Count:</strong> {nrow(products_only)} product records not matched to outlets</p>\n"))
  cat("<p><strong>ACTION REQUIRED:</strong> INVESTIGATE - All products must match to an outlet</p>\n")
  
  html_table(products_producttype, caption = "Product Type for Unmatched Records - REQUIRES INVESTIGATION")
  html_table(products_sample, caption = "Sample of Unmatched Product Records")
  cat("</div>\n")
} else {
  cat("<div class='section'>\n<p><strong>All product records have matching outlet records</strong></p>\n</div>\n")
}

cat("<h3>2.1.5 Recode System Missing Values</h3>\n")
cat("<div class='section'>\n<p>System missing values (-998, -988, -977, -99, -97, -98, -9998, -9777, -9888) recoded to NA</p>\n</div>\n")

cat("<h3>2.1.6 $$$ Validate and Complete Product Type Classification</h3>\n")

cat("<h4>Product Type Before Stockout Recoding</h4>\n")
html_table(producttype_before, caption = "Product Type Distribution - INSERT RESULTS")

cat("<div class='section warning'>\n")
cat("<p><strong>$$$ ACTION REQUIRED:</strong> Review and address any missing product type values</p>\n")
cat("</div>\n")

cat("<h4>Product Type After Stockout Recoding</h4>\n")
html_table(producttype_after, caption = "Final Product Type Distribution")

cat("<h3>2.1.7 Generate nOut Variable</h3>\n")
cat(glue("<div class='section'>
<p><strong>nOut variable created:</strong> Flags first occurrence of each outlet</p>
<p><strong>Unique outlets in dataset:</strong> {unique_outlets}</p>
</div>\n"))

cat("<h2>Final Dataset Summary</h2>\n")

final_summary <- data.frame(
  Metric = c("Total Records", "Total Variables", "Unique Outlets", 
             "TSG Products", "Non-TSG Products", "Diagnostics", "Stock-outs"),
  Count = c(
    nrow(merged_data),
    ncol(merged_data),
    unique_outlets,
    sum(merged_data$producttype == 1, na.rm = TRUE),
    sum(merged_data$producttype == 2, na.rm = TRUE),
    sum(merged_data$producttype == 3, na.rm = TRUE),
    sum(merged_data$producttype == 4, na.rm = TRUE)
  ),
  Percentage = c(
    100,
    NA,
    NA,
    round(sum(merged_data$producttype == 1, na.rm = TRUE)/nrow(merged_data)*100, 1),
    round(sum(merged_data$producttype == 2, na.rm = TRUE)/nrow(merged_data)*100, 1),
    round(sum(merged_data$producttype == 3, na.rm = TRUE)/nrow(merged_data)*100, 1),
    round(sum(merged_data$producttype == 4, na.rm = TRUE)/nrow(merged_data)*100, 1)
  )
)

html_table(final_summary, caption = "Final Dataset Summary Statistics")

# Critical issues summary
cat("<h3>$$$ CRITICAL ISSUES REQUIRING MANUAL REVIEW</h3>\n")

critical_issues <- list()

if (nrow(products_only) > 0) {
  critical_issues <- c(critical_issues, 
                       glue("Products not matched to outlets: {nrow(products_only)}"))
}

if (missing_producttype > 0) {
  critical_issues <- c(critical_issues, 
                       glue("Records with missing product type: {missing_producttype}"))
}

if (length(critical_issues) > 0) {
  cat("<div class='section critical'>\n<ul>\n")
  for (issue in critical_issues) {
    cat(glue("<li><strong>{issue}</strong></li>\n"))
  }
  cat("</ul>\n<p><strong>ALL $$$ FLAGGED ITEMS MUST BE ADDRESSED BEFORE PROCEEDING</strong></p>\n</div>\n")
} else {
  cat("<div class='section'>\n<p><strong>NO CRITICAL ISSUES DETECTED - READY FOR FINAL SAVE</strong></p>\n</div>\n")
}

cat(glue("<div class='section'>
<h2>Data Processing Complete</h2>
<p><strong>Clean dataset saved to:</strong> {output_file}</p>
<p><strong>Final dataset dimensions:</strong> {nrow(merged_data)} rows × {ncol(merged_data)} columns</p>
</div>\n"))

sink()
cat("</body></html>", file = log_file, append = TRUE)

################################################################################
# CONSOLE OUTPUT
################################################################################

cat("\n=== DATA PROCESSING COMPLETE ===\n")
cat("Total records:", nrow(merged_data), "\n")
cat("Unique outlets:", unique_outlets, "\n")

# Product type summary
cat("\nProduct type distribution:\n")
cat(sprintf("  TSG: %d (%.1f%%)\n", 
            sum(merged_data$producttype == 1, na.rm = TRUE),
            sum(merged_data$producttype == 1, na.rm = TRUE)/nrow(merged_data)*100))
cat(sprintf("  Non-TSG: %d (%.1f%%)\n", 
            sum(merged_data$producttype == 2, na.rm = TRUE),
            sum(merged_data$producttype == 2, na.rm = TRUE)/nrow(merged_data)*100))
cat(sprintf("  Diagnostics: %d (%.1f%%)\n", 
            sum(merged_data$producttype == 3, na.rm = TRUE),
            sum(merged_data$producttype == 3, na.rm = TRUE)/nrow(merged_data)*100))
cat(sprintf("  Stock-outs: %d (%.1f%%)\n", 
            sum(merged_data$producttype == 4, na.rm = TRUE),
            sum(merged_data$producttype == 4, na.rm = TRUE)/nrow(merged_data)*100))

# Merge results summary
cat("\n$$$ MERGE VALIDATION:\n")
if (nrow(outlets_only) > 0) {
  cat("  Outlets with no products:", nrow(outlets_only), "\n")
} else {
  cat("  All outlets matched to products\n")
}

if (nrow(products_only) > 0) {
  cat("  $$$ CRITICAL: Products not matched to outlets:", nrow(products_only), "\n")
} else {
  cat("  All products matched to outlets\n")
}

cat("\nClean dataset saved to:", output_file, "\n")
cat("HTML report saved to:", log_file, "\n")

# Check for critical issues
if (nrow(products_only) > 0 || missing_producttype > 0) {
  cat("\n$$$ CRITICAL ISSUES REQUIRING REVIEW:\n")
  if (nrow(products_only) > 0) {
    cat("  • ", nrow(products_only), " product records not matched to outlets\n", sep = "")
  }
  if (missing_producttype > 0) {
    cat("  • ", missing_producttype, " records with missing product type\n", sep = "")
  }
  cat("$$$ ALL FLAGGED ITEMS MUST BE ADDRESSED\n")
  cat("$$$ REVIEW THE HTML LOG FOR DETAILED ANALYSIS\n")
}

cat("\n$$$ Total observations in final dataset:", nrow(merged_data), "\n")

#*************************
#*************************
#******	END 		*****
#*************************