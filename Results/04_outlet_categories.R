################################################################################
# ACTwatch LITE 
# Step 2.4. OUTLET CATEGORIES - SIMPLIFIED VERSION
# REORGANIZED VERSION - HTML logging at end
################################################################################

# PURPOSE:
# Creates outlet category variables for analysis by classifying outlets into
# standardized types (pharmacy, private facility, etc.) and creating binary
# indicator variables for each type.

# INPUTS:  {country}_{year}_am_rdt_os_cleaned_long_wt_denom.csv
# OUTPUTS: {country}_{year}_am_rdt_os_cleaned_long_wt_outletcat.csv

################################################################################
# CONFIGURATION - EDIT THIS SECTION TO MODIFY CATEGORIES
################################################################################

# This is the only section you need to edit to change outlet categories.
# Each outlet gets classified based on its c7 (outlet type) and c7_profit values.

create_outlet_categories <- function(data) {
  
  # Step 1: Create the main category variable (outcat2)
  # This assigns each outlet to one primary category
  data <- data %>%
    mutate(
      outcat2 = case_when(
        c7 == 1 & c7_profit == 1                  ~ 3,   # Private not-for-profit facility
        c7 == 1 & c7_profit %in% c(0, 98)         ~ 4,   # Private for-profit facility  
        c7 == 11                                  ~ 5,   # Pharmacy
        c7 == 20                                  ~ 6,   # Drug store
        c7 %in% c(22, 25, 26)                    ~ 7,   # General retailer (informal)
        c7 %in% c(30, 31, 32)                    ~ 11,  # Wholesaler/importer/distributor
        c7 == 3                                  ~ 20,  # Laboratory
        TRUE                                     ~ NA_real_
      ),
      
      # Step 2: Create individual outlet type dummy variables
      # Each variable = 1 if outlet is that type, NA otherwise
      private_nonprofit = if_else(outcat2 == 3, 1, NA_real_),
      private_forprofit = if_else(outcat2 == 4, 1, NA_real_),
      pharmacy          = if_else(outcat2 == 5, 1, NA_real_),
      drug_store        = if_else(outcat2 == 6, 1, NA_real_),
      gen_retail        = if_else(outcat2 == 7, 1, NA_real_),
      wholesale         = if_else(outcat2 == 11, 1, NA_real_),
      laboratory        = if_else(outcat2 == 20, 1, NA_real_),
      
      # Step 3: Create aggregate/total variables
      # These combine multiple outlet types into broader categories
      total_private = if_else(outcat2 %in% c(4, 5, 6, 7, 8), 1, NA_real_),
      total_informal = if_else(gen_retail==1, 1, NA_real_),             
      total_retail = if_else(outcat2 %in% c(3, 4, 5, 6, 7, 8, 9, 20), 1, NA_real_))
  
  return(data)
}

################################################################################
# SECTION 1: LOAD DATA
################################################################################

options(stringsAsFactors = FALSE, warn = -1)

long_data <- fread(here("Data", "Management data", paste0(country, "_", year, "_am_rdt_os_cleaned_long_wt_denom.csv"))) %>% 
  mutate(across(where(is.character), ~ str_trim(str_to_upper(.))))

# SAVE FOR LOGGING (must save before transformation)
initial_records <- nrow(long_data)
initial_vars <- ncol(long_data)

################################################################################
# SECTION 2: APPLY OUTLET CATEGORIZATION
################################################################################

long_data <- create_outlet_categories(long_data)

################################################################################
# SECTION 3: SAVE ENHANCED DATASET
################################################################################

output_file <- here("Data", "Management data", paste0(country, "_", year, "_am_rdt_os_cleaned_long_wt_outletcat.csv"))
fwrite(long_data, output_file, row.names = FALSE, na = "")

################################################################################
# GENERATE HTML LOG
################################################################################

# Setup log file
log_dir <- here("Data", "Management data", "Data cleaning notes")
if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE)

log_file <- file.path(log_dir, paste0("AwL_", country, "_", year, "_outlet_categories_notes.html"))

# Initialize HTML
cat('<!DOCTYPE html>
<html><head>
<title>Outlet Categories Processing Log</title>
<style>
body {font-family: Arial, sans-serif; margin: 20px;}
h1, h2, h3 {color: #2E86AB;}
table {border-collapse: collapse; width: 100%; margin: 10px 0;}
th, td {border: 1px solid #ddd; padding: 8px; text-align: left;}
th {background-color: #f2f2f2;}
.section {margin: 20px 0; padding: 15px; border-left: 4px solid #2E86AB;}
.warning {background-color: #fff3cd; border-color: #ffc107;}
.success {background-color: #d4edda; border-color: #28a745;}
</style>
</head><body>', file = log_file)

sink(log_file, append = TRUE)

cat("<h1>ACTwatch Lite Outlet Categories Processing</h1>\n")
cat(glue("<p><strong>Country:</strong> {country} | <strong>Year:</strong> {year} | <strong>Date:</strong> {Sys.Date()}</p>\n"))

# =============================================================================
# HTML CONTENT GENERATION - All calculated here from final long_data
# =============================================================================

cat("<h2>1. Loading Data</h2>\n")
cat(glue("<p>Loaded {initial_records} records with {initial_vars} variables</p>\n"))

cat("<h2>2. Creating Outlet Categories</h2>\n")

# Primary categories summary
cat("<h3>2.1 Primary Categories (outcat2)</h3>\n")
outcat2_summary <- long_data %>% 
  count(outcat2, sort = TRUE) %>%
  mutate(
    percent = round(n/sum(n)*100, 1),
    category_label = case_when(
      outcat2 == 3 ~ "Private not-for-profit facility",
      outcat2 == 4 ~ "Private for-profit facility", 
      outcat2 == 5 ~ "Pharmacy",
      outcat2 == 6 ~ "Drug store",
      outcat2 == 7 ~ "General retailer (informal)",
      outcat2 == 11 ~ "Wholesaler/importer/distributor",
      outcat2 == 20 ~ "Laboratory",
      TRUE ~ "Unmapped"
    )
  ) %>%
  select(outcat2, category_label, n, percent)

html_table(outcat2_summary, caption = "Distribution of Primary Outlet Categories")

# Cross-tab of c7 vs outcat2 
cat("<h3>2.2 Mapping Verification (C7 vs OUTCAT2)</h3>\n")
crosstab_summary <- long_data %>%
  filter(nOut == 1) %>%
  count(c7, outcat2) %>%
  arrange(c7, outcat2)

html_table(crosstab_summary, caption = "Cross-tabulation: Raw C7 codes vs Generated OUTCAT2")

# Summary of all dummy variables created
cat("<h3>2.3 Dummy Variables Created</h3>\n")
dummy_vars <- c("private_nonprofit", "private_forprofit", "pharmacy", "drug_store", 
                "gen_retail", "wholesale", "laboratory", "total_private", 
                "total_informal", "total_retail")

variable_summary <- map_dfr(dummy_vars, function(var) {
  data.frame(
    Variable = var,
    Count = sum(!is.na(long_data[[var]])),
    Percent = round(sum(!is.na(long_data[[var]]))/nrow(long_data)*100, 1),
    stringsAsFactors = FALSE
  )
})

html_table(variable_summary, caption = "Summary of Generated Dummy Variables")

# Check for unmapped outlets
unmapped_count <- sum(is.na(long_data$outcat2))
if (unmapped_count > 0) {
  unmapped_outlets <- long_data %>% 
    filter(is.na(outcat2)) %>%
    count(c7, c7_profit, sort = TRUE)
  
  cat("<div class='section warning'>\n")
  cat(glue("<h3>Warning: {unmapped_count} Unmapped Outlets</h3>\n"))
  cat("<p>These outlet/profit combinations were not assigned to any category:</p>\n")
  html_table(unmapped_outlets, caption = "Unmapped C7/C7_Profit Combinations")
  cat("</div>\n")
}

# Compare with existing informal variable if present
if ("informal" %in% names(long_data)) {
  cat("<h3>2.4 Comparison with Existing Variables</h3>\n")
  informal_comparison <- long_data %>%
    count(total_informal, informal, useNA = "always") %>%
    arrange(total_informal, informal)
  
  html_table(informal_comparison, caption = "New total_informal vs Existing informal Variable")
}

cat("<div class='section success'>\n")
cat("<h2>3. Processing Complete</h2>\n")
cat(glue("<p>Dataset saved with {length(dummy_vars)} new outlet category variables</p>\n"))
cat(glue("<p>Output file: {basename(output_file)}</p>\n"))
cat("</div>\n")

sink()
cat("</body></html>", file = log_file, append = TRUE)
options(warn = 0)

################################################################################
# CONSOLE OUTPUT
################################################################################

cat("\n=== OUTLET CATEGORIES PROCESSING COMPLETE ===\n")
cat("Records processed:", nrow(long_data), "\n")
cat("Variables created:", length(dummy_vars), "\n")
cat("Unmapped outlets:", unmapped_count, "\n")

if (unmapped_count > 0) {
  cat("\nWARNING:", unmapped_count, "outlets could not be categorized\n")
  cat("Review the HTML log for details\n")
}

cat("\nOutput files:\n")
cat("  Dataset:", output_file, "\n")
cat("  HTML log:", log_file, "\n")

cat("\n=== TO MODIFY CATEGORIES ===\n")
cat("Edit the 'create_outlet_categories' function at the top of this script\n")

# =============================================================================
# ####               END                   ####
# =============================================================================