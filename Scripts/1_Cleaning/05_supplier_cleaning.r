################################################################################
# SUPPLIER CLEANING (05_supplier_cleaning.r)
################################################################################

################################################################################
# ACTwatch LITE 
# Step 1.5 Supplier Data Cleaning
# REORGANIZED VERSION - HTML logging at end
################################################################################
#
# This script cleans and standardizes supplier data reported by outlets during
# the ACTwatch LITE survey. It processes supplier names, types (manufacturer,
# distributor, importer, etc.), locations (state/region, city/town), and contact
# information. The script merges supplier records with outlet data to enable
# supply chain analysis by outlet characteristics. Duplicate suppliers are
# identified and removed based on exact matches and name-location combinations.
# After cleaning and deduplication, two output files are created: a comprehensive
# cleaned dataset for analysis and a formatted Excel file for field team reference.
# Comprehensive HTML logs document all cleaning decisions and data quality metrics.
#
# The following flags are used throughout the syntax:
# $$$ = Breaks the script to remind analyst to modify syntax.
# /* EXAMPLE: */ = Sample syntax from pilot studies for reference
# 
# Please initial all comments/responses and make note of changes.
#
# NOTE 
# *EACH* STEP FLAGGED WITH "$$$" HAS SECTIONS WHERE MANUAL INPUTS OR EDITS ARE REQUIRED. 	
# REVIEW LINES OF SYNTAX MARKED WITH "$$$". MANAGE/CLEAN DATA ACCORDINGLY.



# Set global options
options(stringsAsFactors = FALSE, warn = -1)

################################################################################
# SECTION 1: DATA DEPENDENCY CHECKS
################################################################################

if (!file.exists(here("Data", "Cleaned data", paste0("AwL_", country, "_", year, "_outlet_clean.csv")))) {
  stop("\nCleaned outlet data not found. Please run 'Outlet Cleaning' first")
}

################################################################################
# SECTION 2: DATA IMPORT AND PREPARATION
################################################################################

supp_data <- fread(here("Data", "Raw data", paste0("AwL-", country, "-", year, "-supplier.csv"))) %>% 
  rename_with(tolower) %>%
  mutate_if(is.character, ~ str_trim(str_to_upper(.)))

# SAVE FOR LOGGING
initial_count <- nrow(supp_data)

# Rename and prepare
supp_data <- supp_data %>%
  rename(
    supp1 = rdtsupp1,
    supp2 = rdtsupp2,
    supp3 = rdtsupp3,
    supp4 = rdtsupp4,
    supp5 = rdtsupp5,
    supp6 = rdtsupp6,
    supp7 = rdtsupp7
  ) %>%
  mutate(source = 0)

################################################################################
# SECTION 3: VARIABLE LABELING AND FACTOR CREATION
################################################################################

# Define labels
supptype_labels <- c(
  "1" = "International manufacturer",
  "2" = "Local manufacturer", 
  "3" = "Importer",
  "4" = "Distributor",
  "5" = "Pharmacy wholesale",
  "7" = "PPMV / chemist",
  "11" = "Public sector supply chain",
  "12" = "Other informal outlet",
  "96" = "Other private outlet/seller",
  "97" = "Refuse to answer",
  "98" = "Don't know"
)

supploc_labels <- c(
  "1" = "STATE 1",
  "2" = "STATE 2", 
  "3" = "STATE 3",
  "96" = "Other",
  "-9998" = "Don't know",
  "-9777" = "Refused"
)

source_labels <- c(
  "0" = "Reported RDT supplier",
  "1" = "Reported antimalarial supplier"
)

# Apply factors
supp_data <- supp_data %>%
  mutate(
    supp2 = factor(supp2, levels = names(supptype_labels), labels = supptype_labels),
    supp4 = factor(supp4, levels = names(supploc_labels), labels = supploc_labels),
    source = factor(source, levels = names(source_labels), labels = source_labels)
  )

################################################################################
# SECTION 4: TEXT FIELD STANDARDIZATION
################################################################################

text_fields <- c("supp1", "supp3", "supp5", "supp6", "supp7", "supp8")

supp_data <- supp_data %>%
  mutate(across(any_of(text_fields), ~ str_trim(str_to_upper(as.character(.)), side = "both")))

################################################################################
# SECTION 5: DUPLICATE DETECTION
################################################################################

# SAVE FOR LOGGING
duplicates_check <- supp_data %>%
  count(key) %>%
  filter(n > 1) %>%
  arrange(desc(n))

# Remove duplicates
if(nrow(duplicates_check) > 0) {
  supp_data <- supp_data %>% distinct(key, .keep_all = TRUE)
}

################################################################################
# SECTION 6: OUTLET DATA MERGE
################################################################################

outlet_data <- fread(here("Data", "Cleaned data", paste0("AwL_", country, "_", year, "_outlet_clean.csv")))

suppliers_prepared <- supp_data %>%
  rename(supplierkey = key, key = parent_key)

# SAVE FOR LOGGING
pre_merge_count <- nrow(suppliers_prepared)

suppliers_prepared <- suppliers_prepared %>%
  left_join(
    outlet_data %>% 
      select(key, date, formtype, auditlevel, c1a, c2, c3, c4, c7, outletid, sa1, sa1a, st1, st1a),
    by = "key"
  )

# SAVE FOR LOGGING
merge_check <- suppliers_prepared %>% filter(is.na(date))
matched_count <- nrow(suppliers_prepared) - nrow(merge_check)

# Save prepared data
fwrite(suppliers_prepared, 
       here("Data", "Cleaned data", paste0("AwL_", country, "_", year, "_suppliers_prepared.csv")),
       row.names = FALSE, na = "")

################################################################################
# SECTION 7: SUPPLIER NAME CLEANING
################################################################################

suppliers_prepared <- suppliers_prepared %>%
  mutate(supp1_orig = supp1) %>%
  relocate(supp1_orig, .before = supp2) %>%
  mutate(
    supp1 = str_to_upper(supp1),
    supp1 = str_replace_all(supp1, c(
      "(C\\)|C\t)" = "E",
      "\t" = " ",
      "  +" = " ",
      "SARL" = "",
      "COMPAGNIE" = " CO",
      "COMPANY" = " CO",
      "PHARMACEUTIQUE" = " PHARMA",
      "PHARMACEUTICAL" = " PHARMA",
      "PHARMAS" = " PHARMA"
    )),
    supp1 = case_when(
      supp1 == "-9777" ~ "REFUSED",
      supp1 == "-9888" ~ "DONT KNOW",
      TRUE ~ supp1
    ),
    supp1 = str_trim(supp1)
  )

# SAVE FOR LOGGING
potential_duplicates <- suppliers_prepared %>%
  count(supp1) %>%
  filter(n > 1) %>%
  arrange(desc(n)) %>%
  slice_head(n = 15)

# SAVE FOR LOGGING
other_suppliers <- suppliers_prepared %>%
  filter(supp2 == "Other private outlet/seller" & !is.na(supp3) & supp3 != "") %>%
  count(supp3, sort = TRUE)

other_states <- suppliers_prepared %>%
  filter(supp4 == "Other" & !is.na(supp5) & supp5 != "") %>%
  count(supp5, sort = TRUE)

# Clean contact fields
suppliers_prepared <- suppliers_prepared %>%
  mutate(
    supp6 = case_when(
      supp6 %in% c("-9888", "-9777", "9888") ~ NA_character_,
      TRUE ~ supp6
    ),
    supp7 = case_when(
      supp7 %in% c("-9888", "-9777", "9888") ~ NA_character_,
      TRUE ~ supp7
    )
  )

################################################################################
# SECTION 8: DUPLICATE SUPPLIER ANALYSIS
################################################################################

# Exact duplicates
suppliers_prepared <- suppliers_prepared %>%
  arrange(supp1, supp2, supp4, supp5, supp6, supp7) %>%
  group_by(supp1, supp2, supp4, supp5, supp6, supp7) %>%
  mutate(exact_dup = n()) %>%
  ungroup()

# Name-location duplicates
suppliers_prepared <- suppliers_prepared %>%
  arrange(supp1, supp2, supp4, supp5) %>%
  group_by(supp1, supp2, supp4, supp5) %>%
  mutate(name_location_dup = n()) %>%
  ungroup()

################################################################################
# SECTION 9: FINAL DATA PROCESSING
################################################################################

# Remove duplicates - exact original process
suppliers_clean <- suppliers_prepared %>%
  arrange(supp1, supp2, supp4, supp5, supp6, supp7) %>%
  group_by(supp1, supp2, supp4, supp5, supp6, supp7) %>%
  mutate(dup1 = ifelse(n() == 1, 0, row_number())) %>%
  ungroup() %>%
  filter(dup1 <= 1) %>%
  select(-dup1)

suppliers_clean <- suppliers_clean %>%
  arrange(supp1, supp2, supp4, supp5) %>%
  group_by(supp1, supp2, supp4, supp5) %>%
  mutate(dup2 = ifelse(n() == 1, 0, row_number())) %>%
  ungroup() %>%
  filter(dup2 <= 1) %>%
  select(-dup2, -exact_dup, -name_location_dup)

# Final standardization
suppliers_clean <- suppliers_clean %>% 
  rename_with(tolower) %>%
  rename_with(~ str_replace_all(.x, "-", "_")) %>% 
  rename(setofrdt_supp1 = any_of("set_of_rdt_supp1"))

# Save files
output_file <- here("Data", "Cleaned data", paste0("AwL_", country, "_", year, "_suppliers_clean.csv"))
fwrite(suppliers_clean, output_file, row.names = FALSE, na = "")

suppliers_export <- suppliers_clean %>%
  select(
    supplier_name = supp1,
    supplier_type = supp2,
    supplier_state = supp4,
    supplier_town = supp5,
    supplier_address = supp6,
    supplier_contact = supp7,
    source
  ) %>%
  filter(!is.na(supplier_name) & supplier_name != "")

export_file <- here("Data", "Cleaned data", paste0("AwL_", country, "_", year, "_supplier_list.xlsx"))
write_xlsx(suppliers_export, path = export_file)

################################################################################
# GENERATE HTML LOG
################################################################################

# Setup log file
log_dir <- here("Data", "Cleaned data", "Data cleaning notes")
if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE)

log_file <- file.path(log_dir, paste0("AwL_", country, "_", year, "_suppliers_clean_notes.html"))

# Initialize HTML
cat('<!DOCTYPE html>
<html><head>
<title>Supplier Data Cleaning Log</title>
<style>
body {font-family: Arial, sans-serif; margin: 20px;}
h1, h2, h3 {color: #2E86AB;}
table {border-collapse: collapse; width: 100%; margin: 10px 0;}
th, td {border: 1px solid #ddd; padding: 8px; text-align: left;}
th {background-color: #f2f2f2;}
.section {margin: 20px 0; padding: 15px; border-left: 4px solid #2E86AB;}
.warning {background-color: #fff3cd; border-color: #ffc107;}
.error {background-color: #f8d7da; border-color: #dc3545;}
pre {background-color: #f8f9fa; padding: 10px; border-radius: 5px;}
</style>
</head><body>', file = log_file)

sink(log_file, append = TRUE)

cat("<h1>ACTwatch Lite Supplier Data Cleaning Log</h1>\n")
cat(glue("<div class='section'>
<h2>Cleaning Session Information</h2>
<p><strong>Date/Time:</strong> {Sys.time()}</p>
<p><strong>Country:</strong> {country}</p>
<p><strong>Year:</strong> {year}</p>
<p><strong>Script:</strong> Supplier Data Cleaning</p>
</div>\n"))

# =============================================================================
# HTML CONTENT GENERATION
# =============================================================================

cat("<h2>1. Data Dependency Verification</h2>\n")
cat("<div class='section'>\n<p>Required outlet data file found and verified</p>\n</div>\n")

cat("<h2>2. Data Import and Preparation</h2>\n")
cat(glue("<div class='section'>
<h3>2.1 Data Import Summary</h3>
<p><strong>Total supplier records imported:</strong> {initial_count}</p>
<p><strong>Total variables:</strong> {ncol(suppliers_clean)}</p>
</div>\n"))

cat("<h2>3. Variable Standardization and Labeling</h2>\n")
cat("<h3>3.1 Variable Distribution Summary</h3>\n")
html_table(create_summary_table(suppliers_prepared, supp2), 
           caption = "Supplier Type Distribution")
html_table(create_summary_table(suppliers_prepared, supp4), 
           caption = "Supplier Location Distribution")
html_table(create_summary_table(suppliers_prepared, source), 
           caption = "Source Type Distribution")

cat("<h2>4. Text Field Standardization</h2>\n")
cat(glue("<div class='section'>
<h3>4.1 Text Field Processing</h3>
<p>Text fields converted to uppercase and whitespace trimmed</p>
<p><strong>Fields processed:</strong> {paste(intersect(text_fields, names(suppliers_prepared)), collapse = ', ')}</p>
</div>\n"))

cat("<h2>5. Duplicate Detection and Key Management</h2>\n")
if(nrow(duplicates_check) > 0) {
  cat("<div class='section warning'>\n")
  cat(glue("<h4>Duplicate Keys Found: {nrow(duplicates_check)} keys with duplicates</h4>\n"))
  html_table(head(duplicates_check, 10), caption = "Top 10 Duplicate Keys")
  cat("</div>\n")
  cat("<p>Duplicates removed - keeping first occurrence of each key</p>\n")
} else {
  cat("<div class='section'>\n<p>No duplicate keys found</p>\n</div>\n")
}

cat("<h2>6. Outlet Data Merge</h2>\n")
cat(glue("<div class='section'>
<h3>6.1 Merge Results</h3>
<p><strong>Supplier records before merge:</strong> {pre_merge_count}</p>
<p><strong>Successfully matched to outlets:</strong> {matched_count}</p>
<p><strong>Unmatched records:</strong> {nrow(merge_check)}</p>
</div>\n"))

if(nrow(merge_check) > 0) {
  cat("<div class='section warning'>\n")
  cat("<h4>Suppliers Without Outlet Records</h4>\n")
  html_table(head(merge_check %>% select(supplierkey, supp1, supp2, supp4), 10),
             caption = "Sample of Unmatched Supplier Records")
  cat("</div>\n")
}

cat(glue("<div class='section'>
<h3>6.2 Data Preparation Complete</h3>
<p><strong>Final prepared dataset:</strong> {nrow(suppliers_prepared)} records</p>
<p>Prepared dataset saved for cleaning process</p>
</div>\n"))

cat("<p><em>End of Section 1: Data Import and Preparation</em></p>\n")

cat("<h2>7. Supplier Name Standardization</h2>\n")
cat("<h3>7.1 Supplier Name Distribution</h3>\n")
html_table(
  suppliers_prepared %>%
    count(supp1, sort = TRUE) %>%
    mutate(percent = round(n/sum(n)*100, 1)) %>%
    slice_head(n = 20),
  caption = "Top 20 Supplier Names (After Cleaning)"
)

if(nrow(potential_duplicates) > 0) {
  cat("<h4>7.1.1 Potential Name Duplicates</h4>\n")
  html_table(potential_duplicates, caption = "Supplier Names Reported Multiple Times")
}

cat("<h2>8. Supplier Type Analysis</h2>\n")
cat("<h3>8.1 Overall Supplier Type Distribution</h3>\n")
html_table(create_summary_table(suppliers_prepared, supp2), 
           caption = "Supplier Type Distribution")

cat("<h3>8.2 Supplier Type by Source</h3>\n")
html_table(
  suppliers_prepared %>%
    count(source, supp2) %>%
    group_by(source) %>%
    mutate(percent = round(n/sum(n)*100, 1)) %>%
    ungroup() %>%
    arrange(source, desc(n)),
  caption = "Supplier Type Distribution by Source Type"
)

if(nrow(other_suppliers) > 0) {
  cat("<h4>8.2.1 'Other' Supplier Type Specifications</h4>\n")
  html_table(other_suppliers, caption = "Other Supplier Type Responses")
}

cat("<h2>9. Supplier Location Analysis</h2>\n")
cat("<h3>9.1 Supplier State Distribution</h3>\n")
html_table(create_summary_table(suppliers_prepared, supp4), 
           caption = "Supplier Location by State")

cat("<h3>9.2 Top Cities/Towns by State</h3>\n")
html_table(
  suppliers_prepared %>%
    count(supp4, supp5) %>%
    group_by(supp4) %>%
    slice_max(n, n = 3) %>%
    ungroup() %>%
    arrange(supp4, desc(n)),
  caption = "Top 3 Cities/Towns per State"
)

if(nrow(other_states) > 0) {
  cat("<h4>9.2.1 'Other' State Specifications</h4>\n")
  html_table(other_states, caption = "Other State Responses")
}

cat("<h3>9.3 Contact Information Completeness</h3>\n")
html_table(
  suppliers_prepared %>%
    summarise(
      total_records = n(),
      has_address = sum(!is.na(supp6) & supp6 != "", na.rm = TRUE),
      has_contact = sum(!is.na(supp7) & supp7 != "", na.rm = TRUE),
      address_percent = round(has_address/total_records*100, 1),
      contact_percent = round(has_contact/total_records*100, 1)
    ),
  caption = "Address and Contact Information Availability"
)

cat("<h2>10. Supplier-Outlet Relationship Analysis</h2>\n")
cat("<h3>10.1 Supplier Types by Outlet Type</h3>\n")
html_table(
  suppliers_prepared %>%
    filter(!is.na(c7)) %>%
    count(c7, supp2) %>%
    group_by(c7) %>%
    mutate(percent = round(n/sum(n)*100, 1)) %>%
    ungroup() %>%
    arrange(c7, desc(n)),
  caption = "Supplier Types Reported by Different Outlet Types"
)

cat("<h3>10.2 Supplier Locations by Outlet Region</h3>\n")
html_table(
  suppliers_prepared %>%
    filter(!is.na(c2)) %>%
    count(c2, supp4) %>%
    group_by(c2) %>%
    slice_max(n, n = 3) %>%
    ungroup() %>%
    arrange(c2, desc(n)),
  caption = "Top 3 Supplier States per Outlet Region"
)

cat("<h2>11. Duplicate Supplier Analysis and Cleaning</h2>\n")
cat("<h3>11.1 Exact Duplicate Analysis</h3>\n")
html_table(create_summary_table(suppliers_prepared, exact_dup), 
           caption = "Exact Duplicate Frequency Distribution")

cat("<h3>11.2 Name-Location Duplicate Analysis</h3>\n")
html_table(create_summary_table(suppliers_prepared, name_location_dup), 
           caption = "Name-Location Duplicate Frequency Distribution")

if(any(suppliers_prepared$exact_dup > 1)) {
  cat("<h4>11.2.1 Example Exact Duplicates</h4>\n")
  html_table(
    suppliers_prepared %>%
      filter(exact_dup > 1) %>%
      select(supp1, supp2, supp4, supp5, exact_dup) %>%
      distinct() %>%
      slice_head(n = 10),
    caption = "Sample of Exact Duplicate Suppliers"
  )
}

cat("<h2>12. Final Data Processing and Export Preparation</h2>\n")

cat("<h2>13. Final Data Summary and Validation</h2>\n")

final_summary <- data.frame(
  Metric = c("Total Supplier Records", "Unique Suppliers (After Deduplication)", 
             "Suppliers with Complete Location", "Suppliers with Contact Info", 
             "Records Matched to Outlets"),
  Count = c(
    nrow(suppliers_prepared),
    nrow(suppliers_clean),
    sum(!is.na(suppliers_clean$supp4) & !is.na(suppliers_clean$supp5)),
    sum(!is.na(suppliers_clean$supp7) & suppliers_clean$supp7 != ""),
    sum(!is.na(suppliers_clean$date))
  ),
  Percentage = c(
    100,
    round(nrow(suppliers_clean)/nrow(suppliers_prepared)*100, 1),
    round(sum(!is.na(suppliers_clean$supp4) & !is.na(suppliers_clean$supp5))/nrow(suppliers_clean)*100, 1),
    round(sum(!is.na(suppliers_clean$supp7) & suppliers_clean$supp7 != "")/nrow(suppliers_clean)*100, 1),
    round(sum(!is.na(suppliers_clean$date))/nrow(suppliers_clean)*100, 1)
  )
)

html_table(final_summary, caption = "Final Dataset Summary Statistics")

export_summary <- data.frame(
  Export_File = c("Clean Dataset", "Field Team List"),
  Records = c(nrow(suppliers_clean), nrow(suppliers_export)),
  Location = c(output_file, export_file),
  Purpose = c("Analysis and reporting", "Field team supplier tracking")
)

cat("<h3>13.1 Export Files Created</h3>\n")
html_table(export_summary, caption = "Output Files Summary")

quality_assessment <- data.frame(
  Quality_Indicator = c("Missing Supplier Names", "Missing Supplier Types", 
                        "Missing Locations", "Unmatched to Outlets"),
  Count = c(
    sum(is.na(suppliers_clean$supp1) | suppliers_clean$supp1 == ""),
    sum(is.na(suppliers_clean$supp2)),
    sum(is.na(suppliers_clean$supp4) | is.na(suppliers_clean$supp5)),
    sum(is.na(suppliers_clean$date))
  ),
  Status = c(
    if_else(sum(is.na(suppliers_clean$supp1) | suppliers_clean$supp1 == "") > 0, "Review", "OK"),
    if_else(sum(is.na(suppliers_clean$supp2)) > 0, "Review", "OK"),
    if_else(sum(is.na(suppliers_clean$supp4) | is.na(suppliers_clean$supp5)) > 0, "Review", "OK"),
    if_else(sum(is.na(suppliers_clean$date)) > 0, "Review", "OK")
  )
)

cat("<h3>13.2 Data Quality Assessment</h3>\n")
html_table(quality_assessment, caption = "Data Quality Indicators")

cat(glue("<div class='section'>
<h2>14. Cleaning Process Complete</h2>
<p><strong>Clean dataset saved to:</strong> {output_file}</p>
<p><strong>Field team list saved to:</strong> {export_file}</p>
<p><strong>Final dataset dimensions:</strong> {nrow(suppliers_clean)} rows × {ncol(suppliers_clean)} columns</p>
<p><strong>Processing completed:</strong> {Sys.time()}</p>
</div>\n"))

sink()
cat("</body></html>", file = log_file, append = TRUE)
options(warn = 0)

################################################################################
# CONSOLE OUTPUT
################################################################################

cat("\n=== SUPPLIER DATA CLEANING COMPLETE ===\n")
cat("Total supplier entries processed:", nrow(suppliers_prepared), "\n")
cat("Unique suppliers after deduplication:", nrow(suppliers_clean), "\n")
cat("Suppliers exported for field teams:", nrow(suppliers_export), "\n")
cat("Clean dataset saved to:", output_file, "\n")
cat("Field team list saved to:", export_file, "\n")
cat("HTML log saved to:", log_file, "\n")

console_issues <- character()

missing_names <- sum(is.na(suppliers_clean$supp1) | suppliers_clean$supp1 == "")
if (missing_names > 0) {
  console_issues <- c(console_issues, glue("{missing_names} suppliers missing names"))
}

missing_types <- sum(is.na(suppliers_clean$supp2))
if (missing_types > 0) {
  console_issues <- c(console_issues, glue("{missing_types} suppliers missing type information"))
}

unmatched_outlets <- sum(is.na(suppliers_clean$date))
if (unmatched_outlets > 0) {
  console_issues <- c(console_issues, glue("{unmatched_outlets} suppliers not matched to outlets"))
}

if (length(console_issues) > 0) {
  cat("\nDATA ISSUES DETECTED:\n")
  for (issue in console_issues) cat("• ", issue, "\n")
  cat("\nPlease review the HTML log for detailed analysis.\n")
} else {
  cat("\nNo major data quality issues detected.\n")
}

cat("\n=== PLEASE REVIEW DATA CLEANING LOG PRIOR TO RUNNING ADDITIONAL SCRIPTS ===\n")

# =============================================================================
# ####               END                   ####
# =============================================================================