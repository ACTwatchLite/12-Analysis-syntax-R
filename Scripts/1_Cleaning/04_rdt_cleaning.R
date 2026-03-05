################################################################################
# RDT CLEANING (04_rdt_cleaning.R)
################################################################################

################################################################################
# ACTwatch LITE 
# Step 1.4 RDT Audit Data Cleaning
# REORGANIZED VERSION - HTML logging at end
################################################################################
#
# This script cleans and validates malaria rapid diagnostic test (RDT) audit data.
# It processes RDT product characteristics including brand names, manufacturers,
# parasite detection types (P.f., P.v., Pan, etc.), antigen types (HRP-2, pLDH,
# Aldolase), and country of manufacture. The script validates pricing data for
# both in-outlet testing and take-away sales, processes wholesale pricing, and
# matches products to the RDT masterlist for WHO PQ quality classification (QARDT).
# Extensive consistency checks validate brand-manufacturer-parasite-antigen
# combinations. Manual entries are flagged for matching to database products.
# After cleaning, the validated dataset is saved with comprehensive HTML
# documentation of all quality checks and flagged items requiring review.
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
# SECTION 1: DATA IMPORT AND PREPARATION
################################################################################

# Import data
rdt_data <- fread(here("Data", "Raw data", paste0("AwL-", country, "-", year, "-rdtAudit.csv"))) %>% 
  rename_with(tolower) %>% 
  rename_with(~ "setofrdtaudit", .cols = intersect("set-of-rdtaudit", names(.)))

# SAVE FOR LOGGING
initial_count <- nrow(rdt_data)

# Text field preparation
text_fields <- c("rdtbrand_search", "parasite_search", "anti_search",
                 "dtmanu_search", "r1", "r3_lbl", "r2_lbl", "r4",
                 "rdtbrand", "r5code_other", "r19")

# Clean text fields
rdt_data <- rdt_data %>%
  mutate(across(any_of(text_fields), ~ replace_na(as.character(.), "")))

# Remove accents and standardize text
rdt_data <- rdt_data %>%
  mutate(across(where(is.character), ~ {
    .x %>%
      str_replace_all(c(
        "[\u00e9\u00c8\u00e8\u00c9]" = "E",
        "[\u00e0\u00c0]" = "A",
        "[\u00e7\u00c7]" = "C",
        "[\u00d1\u00f1]" = "N"
      )) %>%
      str_trim() %>%
      str_to_upper()
  }))

# Combine searched and manual product information
rdt_data <- rdt_data %>%
  mutate(
    parasite = if_else(fillmethod2 == 2, r3_lbl, parasite_search),
    antigen = if_else(fillmethod2 == 2, r2_lbl, anti_search),
    rdtmanu = if_else(fillmethod2 == 2, r4, rdtmanu_search)
  ) %>%
  select(-r3, -r3_lbl, -parasite_search, -r2, -r2_lbl, -anti_search, -r4, -rdtmanu_search)

# SAVE FOR LOGGING
fillmethod_summary <- create_summary_table(rdt_data, fillmethod2)

# Check for duplicates
duplicates <- rdt_data %>%
  count(key) %>%
  filter(n > 1) %>%
  arrange(desc(n))

# Remove duplicates if found
if(nrow(duplicates) > 0) {
  rdt_data <- rdt_data %>% distinct(key, .keep_all = TRUE)
}

# Merge outlet data
rdt_data <- rdt_data %>%
  rename(rdtauditkey = key, key = parent_key)

outlet_data <- fread(here("Data", "Cleaned data", paste0("AwL_", country, "_", year, "_outlet_clean.csv")))

# SAVE FOR LOGGING
pre_merge_count <- nrow(rdt_data)

rdt_data <- rdt_data %>%
  left_join(
    outlet_data %>%
      select(key, formtype, outletid, consented, s5b, s6, rdt_stock, d7,
             status_other, setofrdtaudit, rdtaudit_complete),
    by = c("key", "setofrdtaudit")
  )

# SAVE FOR LOGGING
unmatched <- rdt_data %>% filter(is.na(formtype))
matched_count <- nrow(rdt_data) - nrow(unmatched)

# Filter inappropriate records
rdt_data <- rdt_data %>%
  filter(!(is.na(rdtauditkey) & (consented != 1 | rdt_stock != 1 | d7 != 1)))

################################################################################
# SECTION 2: RDT DATA CLEANING
################################################################################

# Clean comments
standard_empty <- c("NONE", "NO", "NOTHING.", "NO COMMENT(S)", 
                    "NO COMMENT", "NOTHING", "", "NA", "N/A")

rdt_data <- rdt_data %>%
  mutate(r19 = if_else(str_to_upper(r19) %in% standard_empty, "", r19))

# SAVE FOR LOGGING
meaningful_comments <- rdt_data %>% 
  filter(r19 != "" & !is.na(r19)) %>%
  count(r19, sort = TRUE) %>%
  slice_head(n = 10)

# Save missing codes check for logging
missing_codes <- rdt_data %>% 
  filter((rdtcode == "" | is.na(rdtcode)) & fillmethod2 == 1)

# Manufacturer & Country
rdt_data <- rdt_data %>%
  rename(rdtCountry_other = r5code_other, rdtCountry = r5code)

# SAVE FOR LOGGING
other_countries <- rdt_data %>% 
  filter(rdtCountry %in% c(996, 998)) %>% 
  count(rdtCountry_other, sort = TRUE)

# Clean manufacturer names
rdt_data <- rdt_data %>%
  rename(manu_original = rdtmanu) %>%
  mutate(rdtmanu = clean_text_field(manu_original))

# Clean brand names
rdt_data <- rdt_data %>%
  rename(brand_original = rdtbrand) %>%
  mutate(rdtbrand = clean_text_field(brand_original))

# Recode amount sold
rdt_data <- rdt_data %>%
  mutate(r13 = case_when(
    r13 == 9998 ~ -9888,
    r13 == 9997 ~ -9777,
    TRUE ~ r13
  ))

# SAVE FOR LOGGING
amount_sold_summary <- rdt_data %>% 
  filter(r13 > 0) %>%
  summarise(
    n = n(),
    mean_sold = round(mean(r13, na.rm = TRUE), 1),
    median_sold = median(r13, na.rm = TRUE),
    q25 = quantile(r13, 0.25, na.rm = TRUE),
    q75 = quantile(r13, 0.75, na.rm = TRUE),
    min_sold = min(r13, na.rm = TRUE),
    max_sold = max(r13, na.rm = TRUE)
  )

missing_amount_comments <- rdt_data %>% 
  filter(is.na(r13) | r13 < 0, r19 != "") %>%
  count(r19, sort = TRUE) %>%
  slice_head(n = 10)

missing_stockout_comments <- rdt_data %>% 
  filter(is.na(r14), r19 != "") %>%
  count(r19, sort = TRUE) %>%
  slice_head(n = 5)

# Calculate wholesale price per test
rdt_data <- rdt_data %>%
  mutate(r17p_test = case_when(
    !r17p %in% c(-9777, -9888) & !r17n %in% c(99995, 99997, 99998) ~ r17p / r17n,
    TRUE ~ NA_real_
  ))

# SAVE FOR LOGGING
wholesale_summary <- rdt_data %>%
  filter(!is.na(r17p_test)) %>%
  summarise(
    n = n(),
    mean_wholesale_total = round(mean(r17p, na.rm = TRUE), 2),
    mean_wholesale_per_test = round(mean(r17p_test, na.rm = TRUE), 2),
    median_wholesale_per_test = round(median(r17p_test, na.rm = TRUE), 2),
    mean_quantity = round(mean(r17n, na.rm = TRUE), 1)
  )

price_outlier_summary <- rdt_data %>%
  summarise(
    adult_inoutlet_n = sum(!r15b %in% c(-9777, -9888) & !is.na(r15b)),
    adult_inoutlet_mean = round(mean(r15b[!r15b %in% c(-9777, -9888)], na.rm = TRUE), 2),
    adult_inoutlet_max = max(r15b[!r15b %in% c(-9777, -9888)], na.rm = TRUE),
    child_inoutlet_n = sum(!r15c %in% c(-9777, -9888) & !is.na(r15c)),
    child_inoutlet_mean = round(mean(r15c[!r15c %in% c(-9777, -9888)], na.rm = TRUE), 2),
    adult_takeaway_n = sum(!r16b %in% c(-9777, -9888) & !is.na(r16b)),
    adult_takeaway_mean = round(mean(r16b[!r16b %in% c(-9777, -9888)], na.rm = TRUE), 2),
    child_takeaway_n = sum(!r16c %in% c(-9777, -9888) & !is.na(r16c)),
    child_takeaway_mean = round(mean(r16c[!r16c %in% c(-9777, -9888)], na.rm = TRUE), 2),
    wholesale_per_test_n = sum(!is.na(r17p_test)),
    wholesale_per_test_mean = round(mean(r17p_test, na.rm = TRUE), 2)
  )

################################################################################
# SECTION 3: GENERATE QARDT VARIABLE
################################################################################

# Initialize qardt
rdt_data <- rdt_data %>%
  mutate(qardt = 0, rdtcode = as.character(rdtcode))

# Join RDT masterlist
rdt_masterlist <- fread(here("Data", "Product lists", "rdt_masterlist_clean.csv"))

rdt_data <- rdt_data %>%
  left_join(
    rdt_masterlist %>% select(rdtcode, natapp_master, qardt_master),
    by = "rdtcode"
  ) %>%
  mutate(qardt = if_else(!is.na(qardt_master), qardt_master, 0))

# Generate rdt_true
rdt_data <- rdt_data %>%
  mutate(
    rdt_true = if_else(rdtbrand != "94" & rdtbrand != "" & !is.na(rdtbrand), 1, 0),
    producttype = if_else(rdt_true == 1, 3, NA_real_)
  )

################################################################################
# SECTION 4: FINAL STANDARDIZATION AND SAVE
################################################################################

# Final standardization
rdt_data <- rdt_data %>% 
  rename_with(tolower) %>% 
  mutate(across(where(is.character), ~ if_else(.x == ".", "", .x)))

# Save clean dataset
output_file <- here("Data", "Cleaned data", paste0("AwL_", country, "_", year, "_rdtaudit_clean.csv"))
fwrite(rdt_data, output_file, row.names = FALSE, na = "")

################################################################################
# GENERATE HTML LOG
################################################################################

# Setup log file
log_dir <- here("Data", "Cleaned data", "Data cleaning notes")
if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE)

log_file <- file.path(log_dir, paste0("AwL_", country, "_", year, "_rdtaudit_clean_notes.html"))

# Initialize HTML
cat('<!DOCTYPE html>
<html><head>
<title>RDT Audit Cleaning Log</title>
<style>
body {font-family: Arial, sans-serif; margin: 20px;}
h1, h2, h3 {color: #2E86AB;}
table {border-collapse: collapse; width: 100%; margin: 10px 0;}
th, td {border: 1px solid #ddd; padding: 8px; text-align: left;}
th {background-color: #f2f2f2;}
.section {margin: 20px 0; padding: 15px; border-left: 4px solid #2E86AB;}
.warning {background-color: #fff3cd; border-color: #ffc107;}
.error {background-color: #f8d7da; border-color: #dc3545;}
.critical {background-color: #f8d7da; border: 3px solid #dc3545; font-weight: bold;}
pre {background-color: #f8f9fa; padding: 10px; border-radius: 5px;}
</style>
</head><body>', file = log_file)

sink(log_file, append = TRUE)

cat("<h1>ACTwatch Lite RDT Audit Data Cleaning Log</h1>\n")
cat(glue("<div class='section'>
<h2>Cleaning Session Information</h2>
<p><strong>Date/Time:</strong> {Sys.time()}</p>
<p><strong>Country:</strong> {country}</p>
<p><strong>Year:</strong> {year}</p>
<p><strong>Script:</strong> RDT Audit Data Cleaning</p>
</div>\n"))

# =============================================================================
# HTML CONTENT GENERATION
# =============================================================================

cat("<h2>1.3.1 Data Preparation</h2>\n")

cat("<h3>1.3.1.1: Import Datasets</h3>\n")
cat(glue("<div class='section'>
<p><strong>$$$ RECORD COUNT:</strong> {initial_count} RDT records imported</p>
<p><strong>Total variables:</strong> {ncol(rdt_data)}</p>
</div>\n"))

cat("<h3>1.3.1.2-1.3.1.3: Text Field Preparation</h3>\n")
cat("<div class='section'>
<p>Text fields converted to uppercase, accents removed, and whitespace trimmed</p>
</div>\n")

cat("<h3>1.3.1.4: Combine Searched and Manual Product Information</h3>\n")
cat("<h4>Fill Method Distribution (Critical for Data Quality Assessment)</h4>\n")
html_table(fillmethod_summary, caption = "Data Entry Method (fillmethod2)")

cat("<h3>1.3.1.5: $$$ Check and Correct Duplicates</h3>\n")
if(nrow(duplicates) > 0) {
  cat("<div class='section critical'>\n")
  cat(glue("<h4>$$$ CRITICAL: Duplicate Keys Found - {nrow(duplicates)} keys with duplicates</h4>\n"))
  cat("<p><strong>ACTION REQUIRED:</strong> Review and manually address duplicates</p>\n")
  html_table(head(duplicates, 10), caption = "Top 10 Duplicate Keys - REQUIRES MANUAL REVIEW")
  cat("</div>\n")
  cat("<p>Duplicates removed - keeping first occurrence of each key</p>\n")
} else {
  cat("<div class='section'>
  <p>$$$ NO DUPLICATE KEYS FOUND - DATA QUALITY CHECK PASSED</p>
  </div>\n")
}

cat("<h3>1.3.1.6: $$$ Correct Known Errors</h3>\n")
cat("<div class='section warning'>
<p><strong>$$$ MANUAL REVIEW REQUIRED:</strong> Address any errors noted during data collection</p>
<p>Review comments field (r19) and apply corrections as needed</p>
</div>\n")

cat("<h3>1.3.1.7: Merge Outlet-Level Data</h3>\n")
cat(glue("<div class='section'>
<h4>Merge Results</h4>
<p><strong>RDT records before merge:</strong> {pre_merge_count}</p>
<p><strong>Successfully matched to outlets:</strong> {matched_count}</p>
<p><strong>$$$ UNMATCHED RECORDS:</strong> {nrow(unmatched)}</p>
</div>\n"))

if(nrow(unmatched) > 0) {
  cat("<div class='section warning'>\n")
  cat("<h4>$$$ PRODUCTS THAT DON'T MATCH TO OUTLET RECORD - REQUIRES INVESTIGATION</h4>\n")
  html_table(head(unmatched %>% select(rdtauditkey, setofrdtaudit, rdtbrand, rdtmanu), 10),
             caption = "Sample of Unmatched Records - INVESTIGATE")
  cat("</div>\n")
}

cat("<h2>1.3.2 Malaria Blood Testing Data Cleaning</h2>\n")
cat(glue("<div class='section'>
<p><strong>Records in prepared dataset:</strong> {nrow(rdt_data)}</p>
</div>\n"))

cat("<h3>1.3.2.1: $$$ Check Reported Completeness</h3>\n")
cat("<div class='section'>\n")
html_table(create_summary_table(rdt_data, rdtaudit_complete), 
           caption = "RDT Audit Completeness - INSERT RESULTS")
cat("<p><strong>$$$ ACTION REQUIRED:</strong> Check incomplete audits and edit accordingly</p>\n")
cat("</div>\n")

cat("<h3>1.3.2.2: $$$ Check Comments</h3>\n")
cat("<div class='section'>\n")
if (nrow(meaningful_comments) > 0) {
  html_table(meaningful_comments, caption = "Meaningful Comments - INSERT RESULTS")
  cat("<p><strong>$$$ ACTION REQUIRED:</strong> Review comments and make edits accordingly</p>\n")
} else {
  cat("<p>No meaningful comments found after cleaning</p>\n")
}
cat("</div>\n")

cat("<h3>1.3.2.3: $$$ Fill Method Check</h3>\n")
cat("<div class='section'>\n")
html_table(create_summary_table(rdt_data, fillmethod2), 
           caption = "Fill Method Distribution - INSERT RESULTS")
cat("</div>\n")

cat("<h3>1.3.2.4: $$$ RDT Code Validation</h3>\n")
cat("<h4>RDT Codes for Searched Products</h4>\n")
html_table(
  rdt_data %>% filter(fillmethod2 == 1) %>% count(rdtcode, sort = TRUE) %>% slice_head(n = 15),
  caption = "RDT Codes (fillmethod==1) - INSERT RESULTS"
)

if(nrow(missing_codes) > 0) {
  cat("<div class='section critical'>\n")
  cat(glue("<h4>$$$ CRITICAL: Searched Products Missing RDT Codes: {nrow(missing_codes)} records</h4>\n"))
  cat("<p><strong>ACTION REQUIRED:</strong> Edit accordingly - these should have codes</p>\n")
  html_table(head(missing_codes %>% select(rdtauditkey, rdtbrand, rdtmanu, parasite, antigen), 10),
             caption = "Missing Code Records - REQUIRES ATTENTION")
  cat("</div>\n")
} else {
  cat("<div class='section'>
  <p>$$$ VALIDATION PASSED: All searched products have RDT codes assigned</p>
  </div>\n")
}

cat("<h3>1.3.2.5: $$$ Manufacturer Name & Country</h3>\n")
cat("<h4>Country of Manufacture - Other Specify</h4>\n")
if(nrow(other_countries) > 0) {
  cat("<div class='section warning'>\n")
  html_table(other_countries, caption = "Other Country Specifications - INSERT RESULTS")
  cat("<p><strong>$$$ ACTION REQUIRED:</strong> Recode all other countries to valid types</p>\n")
  cat("</div>\n")
} else {
  cat("<div class='section'>\n<p>No other-specify countries found</p>\n</div>\n")
}

cat("<h4>Manufacturer Distribution</h4>\n")
html_table(
  create_summary_table(rdt_data, rdtmanu) %>% slice_head(n = 15),
  caption = "Manufacturer Summary - INSERT RESULTS"
)

cat("<h4>$$$ Manufacturer-Country Consistency Check</h4>\n")
cat("<div class='section'>\n")
html_table(
  rdt_data %>% count(rdtmanu, rdtcountry, sort = TRUE) %>% slice_head(n = 20),
  caption = "Manufacturer-Country Combinations - INSERT RESULTS"
)
cat("<p><strong>$$$ ACTION REQUIRED:</strong> Sense check and edit for consistency</p>\n")
cat("</div>\n")

cat("<h3>1.3.2.6: $$$ Brand Name Cleaning</h3>\n")
cat("<div class='section'>\n")
html_table(
  create_summary_table(rdt_data, rdtbrand) %>% slice_head(n = 15),
  caption = "Brand Distribution - INSERT RESULTS"
)
cat("<p><strong>$$$ ACTION REQUIRED:</strong> Continue cleaning brand names as needed</p>\n")
cat("</div>\n")

cat("<h4>$$$ Brand-Manufacturer Consistency (Manual Entry)</h4>\n")
cat("<div class='section'>\n")
html_table(
  rdt_data %>% filter(fillmethod2 == 2) %>% 
    count(rdtmanu, rdtbrand, sort = TRUE) %>% slice_head(n = 15),
  caption = "Brand-Manufacturer Combinations (fillmethod==2) - INSERT RESULTS"
)
cat("<p><strong>$$$ ACTION REQUIRED:</strong> Sense check and edit for consistency</p>\n")
cat("</div>\n")

cat("<h3>1.3.2.7: $$$ Parasite & Antigen</h3>\n")
cat("<div class='section'>\n")
html_table(
  rdt_data %>% count(parasite, antigen, sort = TRUE) %>% mutate(percent = round(n/sum(n)*100, 1)),
  caption = "Parasite-Antigen Combinations - INSERT RESULTS"
)
cat("<p><strong>$$$ ACTION REQUIRED:</strong> Fix missing info and check consistency within brand</p>\n")
cat("</div>\n")

cat("<h4>$$$ Brand-Parasite-Antigen Consistency (Manual Entry)</h4>\n")
cat("<div class='section'>\n")
html_table(
  rdt_data %>% filter(fillmethod2 == 2) %>% 
    count(rdtbrand, parasite, antigen, sort = TRUE) %>% slice_head(n = 15),
  caption = "Brand-Parasite-Antigen (fillmethod==2) - INSERT RESULTS"
)
cat("<p><strong>$$$ ACTION REQUIRED:</strong> Sense check using product photos where available</p>\n")
cat("</div>\n")

cat("<h3>1.3.2.8: $$$ Self-Administer Proportion</h3>\n")
cat("<div class='section'>\n")
html_table(create_summary_table(rdt_data, self), 
           caption = "Self-Administration Distribution - INSERT RESULTS")
cat("</div>\n")

cat("<h4>$$$ Self-Administration by Brand</h4>\n")
cat("<div class='section'>\n")
html_table(
  rdt_data %>% filter(rdtbrand != "") %>% 
    count(rdtbrand, self, sort = TRUE) %>% slice_head(n = 15),
  caption = "Self-Admin by Brand - INSERT RESULTS"
)
cat("<p><strong>$$$ ACTION REQUIRED:</strong> Sense check based on product photos</p>\n")
cat("</div>\n")

cat("<h3>1.3.2.9: $$$ Amount Sold/Distributed Range</h3>\n")
cat("<div class='section'>\n")
html_table(amount_sold_summary, caption = "Amount Sold Distribution - INSERT RESULTS")
cat("</div>\n")

if(nrow(missing_amount_comments) > 0) {
  cat("<h4>Comments for Missing Amount Sold</h4>\n")
  html_table(missing_amount_comments, caption = "Comments for Missing r13 Values")
}

cat("<h3>1.3.2.10: $$$ Stock Out Proportion</h3>\n")
cat("<div class='section'>\n")
html_table(create_summary_table(rdt_data, r14), 
           caption = "Stock Out Distribution - INSERT RESULTS")
cat("</div>\n")

if(nrow(missing_stockout_comments) > 0) {
  cat("<h4>Comments for Missing Stock Out Data</h4>\n")
  html_table(missing_stockout_comments, caption = "Comments for Missing r14 Values")
}

cat("<h3>1.3.2.11: $$$ Retail Price Analysis</h3>\n")
cat("<h4>In-Outlet Testing Services</h4>\n")
cat("<div class='section'>\n")
html_table(create_summary_table(rdt_data, r15a), 
           caption = "Do You Test in This Outlet? - INSERT RESULTS")
cat("</div>\n")

# Price statistics for in-outlet testing
for(price_type in list(
  list(filter_val = 1, price_var = "r15b", title = "Adult In-Outlet Testing Price"),
  list(filter_val = 1, price_var = "r15c", title = "Child In-Outlet Testing Price")
)) {
  stats <- calc_price_stats(rdt_data %>% filter(r15a == price_type$filter_val), price_type$price_var)
  if(nrow(stats) > 0 && stats$n > 0) {
    cat(glue("<h4>{price_type$title}</h4>\n"))
    html_table(stats, caption = glue("{price_type$title} Statistics - INSERT RESULTS"))
  }
}

cat("<h4>Take-Away Sales</h4>\n")
cat("<div class='section'>\n")
html_table(create_summary_table(rdt_data, r16a), 
           caption = "Do You Sell RDTs for Take Away? - INSERT RESULTS")
cat("</div>\n")

# Price statistics for take-away
for(price_type in list(
  list(filter_val = 1, price_var = "r16b", title = "Adult Take-Away Price"),
  list(filter_val = 1, price_var = "r16c", title = "Child Take-Away Price")
)) {
  stats <- calc_price_stats(rdt_data %>% filter(r16a == price_type$filter_val), price_type$price_var)
  if(nrow(stats) > 0 && stats$n > 0) {
    cat(glue("<h4>{price_type$title}</h4>\n"))
    html_table(stats, caption = glue("{price_type$title} Statistics - INSERT RESULTS"))
  }
}

cat("<h3>1.3.2.12: $$$ Wholesale Price Analysis</h3>\n")
cat("<h4>Wholesale Number</h4>\n")
cat("<div class='section'>\n")
html_table(
  rdt_data %>% filter(!r17n %in% c(-9777, -9888)) %>% 
    count(r17n, sort = TRUE) %>% slice_head(n = 10),
  caption = "Wholesale Number Distribution - INSERT RESULTS"
)
cat("</div>\n")

cat("<h4>Wholesale Price</h4>\n")
cat("<div class='section'>\n")
html_table(wholesale_summary, caption = "Wholesale Price Statistics - INSERT RESULTS")
cat("<p><strong>$$ ACTION REQUIRED:</strong> Sense check and review comments, clean as needed</p>\n")
cat("</div>\n")

cat("<h3>1.3.2.13: $$ Retail and Wholesale Price Outliers</h3>\n")
cat("<div class='section'>\n")
html_table(price_outlier_summary, caption = "Price Outlier Summary - INSERT RESULTS")
cat("<p><strong>$$ ACTION REQUIRED:</strong> Document suspicious values including possible miscodes and negative values</p>\n")
cat("</div>\n")

cat("<h2>1.3.3 Generate QARDT Variable</h2>\n")
cat("<h3>1.3.3.1: $$ Generate the QARDT Variable</h3>\n")
cat("<h3>1.3.3.2: $$ Recode Searched Products Using QA Column in RDT Database</h3>\n")

cat("<div class='section'>\n")
html_table(create_summary_table(rdt_data, qardt), 
           caption = "WHO PQ RDT Distribution - INSERT RESULTS")
cat("</div>\n")

cat("<h4>$$ Manual Entry vs Database Product Check</h4>\n")
cat("<div class='section critical'>\n")
html_table(
  rdt_data %>% filter(fillmethod2 == 2) %>% 
    count(rdtmanu, rdtbrand, parasite, rdtcode, fillmethod2, sort = TRUE) %>% 
    slice_head(n = 15),
  caption = "Manual Entries by Manufacturer/Brand/Parasite - INSERT RESULTS"
)
cat("<p><strong>$ CRITICAL ACTION REQUIRED:</strong> Locate specific products that require an RDT code</p>\n")
cat("<p>Manually entered RDTs matching DB products should be assigned corresponding rdtcode</p>\n")
cat("<p><strong>EXAMPLE SYNTAX:</strong></p>\n")
cat('<pre>replace rdtcode="A130_NGA" if rdtcode=="" & rdtmanu=="DR MEYER" & rdtbrand=="SUPERTEST" & parasite=="2"</pre>\n')
cat("</div>\n")

cat("<h3>1.3.3.3: $ RDT True - Generate</h3>\n")
cat("<div class='section'>\n")
html_table(create_summary_table(rdt_data, rdt_true), 
           caption = "RDT Audited is True RDT? - INSERT RESULTS")
cat("</div>\n")

cat("<h2>Final Data Validation Summary</h2>\n")

final_summary <- data.frame(
  Metric = c("Total RDT Records", "Total Variables", "Valid RDTs", 
             "Quality RDTs (QARDT)", "Records with Pricing Data"),
  Count = c(
    nrow(rdt_data),
    ncol(rdt_data),
    sum(rdt_data$rdt_true == 1, na.rm = TRUE),
    sum(rdt_data$qardt == 1, na.rm = TRUE),
    sum(!is.na(rdt_data$r15b) | !is.na(rdt_data$r16b), na.rm = TRUE)
  ),
  Percentage = c(
    100,
    NA,
    round(sum(rdt_data$rdt_true == 1, na.rm = TRUE)/nrow(rdt_data)*100, 1),
    round(sum(rdt_data$qardt == 1, na.rm = TRUE)/nrow(rdt_data)*100, 1),
    round(sum(!is.na(rdt_data$r15b) | !is.na(rdt_data$r16b), na.rm = TRUE)/nrow(rdt_data)*100, 1)
  )
)

html_table(final_summary, caption = "Final Dataset Summary Statistics")

# Critical issues summary
cat("<h3>$ CRITICAL ISSUES REQUIRING MANUAL REVIEW</h3>\n")

critical_issues <- list()

missing_codes_searched <- sum((rdt_data$rdtcode == "" | is.na(rdt_data$rdtcode)) & 
                                rdt_data$fillmethod2 == 1, na.rm = TRUE)
if (missing_codes_searched > 0) {
  critical_issues <- c(critical_issues, 
                       glue("Searched products missing RDT codes: {missing_codes_searched}"))
}

duplicates_found <- nrow(duplicates)
if (duplicates_found > 0) {
  critical_issues <- c(critical_issues, glue("Duplicate keys found: {duplicates_found}"))
}

unmatched_outlets <- sum(is.na(rdt_data$formtype), na.rm = TRUE)
if (unmatched_outlets > 0) {
  critical_issues <- c(critical_issues, 
                       glue("RDT records not matched to outlets: {unmatched_outlets}"))
}

other_countries_count <- nrow(other_countries)
if (other_countries_count > 0) {
  critical_issues <- c(critical_issues, 
                       glue("Other-specify countries requiring recoding: {other_countries_count}"))
}

if (length(critical_issues) > 0) {
  cat("<div class='section critical'>\n<ul>\n")
  for (issue in critical_issues) {
    cat(glue("<li><strong>{issue}</strong></li>\n"))
  }
  cat("</ul>\n<p><strong>ALL $ FLAGGED ITEMS MUST BE ADDRESSED BEFORE PROCEEDING</strong></p>\n</div>\n")
} else {
  cat("<div class='section'>\n<p><strong>NO CRITICAL ISSUES DETECTED - READY FOR FINAL SAVE</strong></p>\n</div>\n")
}

cat(glue("<div class='section'>
<h2>Cleaning Process Complete</h2>
<p><strong>Clean dataset saved to:</strong> {output_file}</p>
<p><strong>Final dataset dimensions:</strong> {nrow(rdt_data)} rows × {ncol(rdt_data)} columns</p>
</div>\n"))

sink()
cat("</body></html>", file = log_file, append = TRUE)
options(warn = 0)

################################################################################
# CONSOLE OUTPUT
################################################################################

cat("\n=== RDT AUDIT DATA CLEANING COMPLETE ===\n")
cat("$ RECORD COUNT:", nrow(rdt_data), "\n")
cat("Valid RDTs:", sum(rdt_data$rdt_true == 1, na.rm = TRUE), "\n")
cat("Quality RDTs (QARDT):", sum(rdt_data$qardt == 1, na.rm = TRUE), "\n")
cat("Clean dataset saved to:", output_file, "\n")
cat("Data cleaning log saved to:", log_file, "\n")

# Console critical issues
console_critical_issues <- character()

if (missing_codes_searched > 0) {
  console_critical_issues <- c(console_critical_issues, 
                               glue("$ CRITICAL: {missing_codes_searched} searched products missing RDT codes"))
}

if (duplicates_found > 0) {
  console_critical_issues <- c(console_critical_issues, 
                               glue("$ CRITICAL: {duplicates_found} duplicate keys found"))
}

if (unmatched_outlets > 0) {
  console_critical_issues <- c(console_critical_issues, 
                               glue("$ CRITICAL: {unmatched_outlets} RDT records not matched to outlets"))
}

if (other_countries_count > 0) {
  console_critical_issues <- c(console_critical_issues, 
                               glue("$ ACTION REQUIRED: {other_countries_count} other-specify countries need recoding"))
}

missing_brands <- sum(rdt_data$rdtbrand == "" | is.na(rdt_data$rdtbrand), na.rm = TRUE)
if (missing_brands > 0) {
  console_critical_issues <- c(console_critical_issues, 
                               glue("$ REVIEW: {missing_brands} records missing brand names"))
}

incomplete_audits <- sum(rdt_data$rdtaudit_complete != 1, na.rm = TRUE)
if (incomplete_audits > 0) {
  console_critical_issues <- c(console_critical_issues, 
                               glue("$ REVIEW: {incomplete_audits} incomplete audits"))
}

meaningful_comments_count <- sum(rdt_data$r19 != "" & !is.na(rdt_data$r19), na.rm = TRUE)
if (meaningful_comments_count > 0) {
  console_critical_issues <- c(console_critical_issues, 
                               glue("$ REVIEW: {meaningful_comments_count} records with meaningful comments"))
}

if (length(console_critical_issues) > 0) {
  cat("\n$ CRITICAL ISSUES REQUIRING MANUAL REVIEW:\n")
  for (issue in console_critical_issues) {
    cat("• ", issue, "\n")
  }
  cat("\n$ ALL FLAGGED ITEMS (marked with $) MUST BE ADDRESSED\n")
  cat("$ REVIEW THE HTML LOG FOR DETAILED ANALYSIS AND INSERT RESULTS\n")
} else {
  cat("\n$ NO CRITICAL ISSUES DETECTED - DATA READY FOR NEXT STEPS\n")
}

cat("\n=== PLEASE REVIEW DATA CLEANING LOG PRIOR TO RUNNING ADDITIONAL SCRIPTS ===\n")

cat("\n$ MANUAL ACTIONS REQUIRED:\n")
cat("1. Review and address all duplicate keys (if found)\n")
cat("2. Correct known errors from data collection\n")
cat("3. Check and edit incomplete audits\n")
cat("4. Review meaningful comments and apply corrections\n")
cat("5. Recode all other-specify countries\n")
cat("6. Clean manufacturer names for consistency\n")
cat("7. Clean brand names and check consistency\n")
cat("8. Fix missing parasite/antigen info and check brand consistency\n")
cat("9. Sense check self-administration by brand using product photos\n")
cat("10. Review price outliers and document suspicious values\n")
cat("11. Assign RDT codes to manually entered products matching database\n")

cat("\n$ INSERT RESULTS sections must be completed in HTML log\n")
cat("$ Total observations in final dataset:", nrow(rdt_data), "\n")

# =============================================================================
# ####               END                   ####
# =============================================================================