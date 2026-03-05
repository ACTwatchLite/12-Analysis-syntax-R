################################################################################
# ACTwatch LITE 
# Step 2.10 Generate volume and market share variables 
################################################################################

# SCRIPT PURPOSE:
# This script generates volume and market share variables for antimalarials and 
# diagnostics. It creates product-level volume measures standardized by Adult 
# Equivalent Treatment Dose (AETD) and generates market share indicator variables
# for major manufacturers, brands, and manufacturer-brand combinations.
#
# CALCULATION STEPS:
# 1. DIAGNOSTIC VOLUMES:
#    - Generate microscopy volume (vd_micro) and flag (vf_micro) from d4
#    - Aggregate RDT volumes (vd_rdt) by outlet and create flag (vf_rdt) from r13
#    - Handles outlier management with user-specified adjustments
#
# 2. ANTIMALARIAL VOLUMES (AETD STANDARDIZATION):
#    - Calculate volume = packages sold × AETD per package (primary method)
#    - Calculate volume = (units sold / package size) × AETD (secondary method)
#    - Create anyAMsales flag for products with volume > 0
#    - Create st_anyAMsales outlet-level flag for any antimalarial sales
#
# 3. MANUFACTURER ANALYSIS:
#    - Calculate total volume by manufacturer (manu_vol_tot)
#    - Identify manufacturers exceeding volume threshold (default: 1000 AETDs)
#    - Create manufacturer frequency tables and volume rankings
#
# 4. BRAND ANALYSIS:
#    - Calculate total volume by brand (brand_vol_tot)
#    - Identify brands exceeding volume threshold
#    - Create brand frequency tables and volume rankings
#    - Generate quality ACT (qaact) manufacturer summaries if available
#
# 5. MANUFACTURER-BRAND CONCATENATION:
#    - Create manu_brand variable (format: "Manufacturer; Brand")
#    - Calculate manbra_tot (total volume by manufacturer-brand combination)
#    - Export manufacturer-brand table to Data/Tables/manu_brand_tab.csv
#
# 6. MARKET SHARE INDICATOR VARIABLES:
#    - man_1, man_2, ... man_N: Binary indicators for top N manufacturers
#    - bra_1, bra_2, ... bra_N: Binary indicators for top N brands
#    - mb_1, mb_2, ... mb_N: Binary indicators for top N manufacturer-brand combos
#    - man_other, bra_other, mb_other: Indicators for below-threshold products
#    - man_tot, mb_tot: Indicators for any manufacturer/manufacturer-brand with volume
#    - Number of indicators created dynamically based on threshold
#
# USER INPUTS REQUIRED:
# $$$ Diagnostic outlier management: Adjust d4 and r13 after sensitivity analysis
# $$$ Volume calculation restrictions: Modify invalid_codes exclusion criteria
# $$$ Volume threshold: Set volume_threshold (default 1000) for market share cutoffs
# $$$ Record results: Document manufacturer and brand lists meeting threshold
#
# VARIABLES CREATED:
# - vd_micro, vf_micro: Microscopy volume distributed and flag
# - vd_rdt, vf_rdt: RDT volume distributed and flag
# - volume: Total AETDs sold (standardized antimalarial volume)
# - anyAMsales, st_anyAMsales: Product and outlet-level AM sales flags
# - manu_vol_tot, brand_vol_tot, manbra_tot: Volume totals by manufacturer/brand
# - man_*, bra_*, mb_*: Dynamic market share indicator variables
#
# FLAG LEGEND:
# $$$ = User input required
# → SAVE = Variable saved for use in HTML section
# # EXAMPLE: = Example of expected behavior

################################################################################
# SETUP
################################################################################

# Set global options
options(stringsAsFactors = FALSE, warn = -1)

# Create logs directory 
log_dir <- here("Data", "Management data", "Data cleaning notes")
if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE)

log_file <- file.path(log_dir, paste0("AwL_", country, "_", year, "_volume_notes.html"))

################################################################################
# SECTION 1: LOAD DATA
################################################################################

# Load the cleaned long weighted data
long_data <- fread(here("Data", "Management data", paste0(country, "_", year, "_am_rdt_os_cleaned_long_wt_prices.csv"))) %>%
  as.data.frame()

# → SAVE: Initial record count for HTML summary
initial_records <- nrow(long_data)


################################################################################
# SECTION 2: DIAGNOSTIC VOLUMES
################################################################################

# $$$ USER INPUT REQUIRED - Manage diagnostic outliers
# After the diagnostic sensitivity analysis is completed, you will need to return 
# to this section to set volumes to missing or to recode volumes according to the 
# outcome of the sensitivity analysis. Make all edits to the microscopy volume (d4) 
# and rdt volume (r13) variables.

# Generate diagnostic volume variables
# Volume flag variables (vf_*) = 1 for outlets with volumes distributed 
# Volume distributed variables (vd_*) = actual volume distributed

# Microscopy volumes
long_data <- long_data %>%
  mutate(
    vd_micro = ifelse(!d4 %in% c(-9555, -9777, -9888), d4, NA),
    vf_micro = ifelse(!is.na(vd_micro) & vd_micro >= 0, 1, NA)
  )

# RDT volumes (aggregate by outlet)
rdt_temp <- long_data %>%
  filter(producttype == 3, !r13 %in% c(-9555, -9777, -9888)) %>%
  group_by(outletid) %>%
  summarise(vd_rdt = sum(r13, na.rm = TRUE), .groups = "drop")

# Join RDT data and create flag variable
long_data <- long_data %>%
  left_join(rdt_temp, by = "outletid") %>%
  mutate(vf_rdt = ifelse(!is.na(vd_rdt) & vd_rdt >= 0, 1, NA))

################################################################################
# SECTION 3: ANTIMALARIAL VOLUMES (AETD STANDARDIZATION)
################################################################################

# $$$ USER INPUT REQUIRED - Volume calculation restrictions
# Modify the exclusion criteria as appropriate so that calculations correctly 
# restrict to valid sales volume and omit inconsistent, refused, don't know 
# and user-missing values.

# Define invalid codes
invalid_codes <- c(-9555, -9777, -9888, 988, 998, 9888, 9988, 9997, 9998)

# Generate volume variable (matches Stata logic exactly)
long_data$volume <- NA_real_

# Package sales first
package_condition <- !is.na(long_data$amsold_pack) & 
  !is.na(long_data$packageaetd) & 
  !long_data$amsold_pack %in% invalid_codes

long_data$volume[package_condition] <- long_data$amsold_pack[package_condition] * 
  long_data$packageaetd[package_condition]

# Unit sales second (only where volume is still missing)
unit_condition <- is.na(long_data$volume) &
  !is.na(long_data$amsold_unit) & 
  !is.na(long_data$size) & 
  !is.na(long_data$packageaetd) & 
  !long_data$amsold_unit %in% invalid_codes

long_data$volume[unit_condition] <- (long_data$amsold_unit[unit_condition] / 
                                       long_data$size[unit_condition]) * 
  long_data$packageaetd[unit_condition]

# → SAVE: High volume count for HTML summary
high_volume_count <- sum(long_data$volume > 500, na.rm = TRUE)

# Create outlet-level flags for any AM sales
long_data <- long_data %>%
  mutate(anyAMsales = ifelse(!is.na(volume) & volume > 0, 1, NA))

long_data <- long_data %>%
  group_by(outletid) %>%
  mutate(st_anyAMsales = as.numeric(any(anyAMsales == 1, na.rm = TRUE))) %>%
  ungroup()

################################################################################
# SECTION 4: BRAND/MANUFACTURER ANALYSIS
################################################################################

# $$$ USER INPUT REQUIRED - Volume threshold
# This step identifies AM brands and manufacturers with sales volumes exceeding 
# a threshold for market share analysis. In ACTwatch Lite Nigeria 2024, threshold 
# was set to 1000 AETDs, but this should be revised according to local conditions.

# → SAVE: Volume threshold for HTML section
volume_threshold <- 1000

# MANUFACTURER ANALYSIS

# Create manufacturer frequency table
manu_freq <- long_data %>%
  filter(!is.na(manu)) %>%
  count(manu, sort = TRUE)

## Calculate manufacturer totals
long_data <- long_data %>%
  mutate(manu_num = as.numeric(as.factor(manu))) %>%
  group_by(manu_num) %>%
  mutate(manu_vol_tot = sum(volume, na.rm = TRUE)) %>%
  ungroup()



# → SAVE: Manufacturer totals and high-volume manufacturers
manu_totals <- long_data %>%
  filter(!is.na(volume), !is.na(manu)) %>%
  group_by(manu_num, manu) %>%
  summarise(manu_vol_tot = sum(volume), .groups = "drop") %>%
  arrange(desc(manu_vol_tot))

high_vol_manu <- manu_totals %>%
  filter(manu_vol_tot >= volume_threshold)

# BRAND ANALYSIS  

# Create brand frequency table
brand_freq <- long_data %>%
  filter(!is.na(brand)) %>%
  count(brand, sort = TRUE)

# Encode brand names and calculate totals
#long_data <- long_data %>%
#  mutate(brand_num_DELETETHISSUFFIXONCEDATAISFIXED = as.numeric(as.factor(brand))) %>%
#  group_by(brand_num) %>%
#  mutate(brand_vol_tot = ifelse(is.na(volume), 
#                                NA_real_, 
#                                sum(volume, na.rm = TRUE))) %>%
#  ungroup()


# Encode brand names and calculate totals
long_data <- long_data %>%
  mutate(brand_num = as.numeric(as.factor(brand))) %>%
  group_by(brand_num) %>%
  mutate(
    brand_vol_tot = ifelse(is.na(volume),
                           NA_real_,
                           sum(volume, na.rm = TRUE))
  ) %>%
  ungroup()

# → SAVE: Brand totals and high-volume brands
brand_totals <- long_data %>%
  filter(!is.na(volume), !is.na(brand_num)) %>%
  group_by(brand_num, brand) %>%
  summarise(brand_vol_tot = sum(volume), .groups = "drop") %>%
  arrange(desc(brand_vol_tot))

high_vol_brand <- brand_totals %>%
  filter(brand_vol_tot >= volume_threshold)

# Cross-tabulation of brands by manufacturer
brand_by_manu <- long_data %>%
  filter(!is.na(brand_num) & !is.na(manu_num)) %>%
  count(manu_num, manu, brand_num, brand) %>%
  arrange(manu_num, brand_num)

# Quality check by qaact if available
if ("qaact" %in% names(long_data)) {
  qaact_summary <- long_data %>%
    filter(!is.na(manu), qaact == 1) %>%
    count(manu, sort = TRUE)
}

################################################################################
# SECTION 5: CREATE MANUFACTURER-BRAND CONCATENATED VARIABLE
################################################################################

# Create concatenated manufacturer-brand variable (matches Stata: space semicolon space)
long_data <- long_data %>%
  mutate(
    manu_brand = ifelse(!is.na(manu) & !is.na(brand),
                        paste(manu, brand, sep = "; "),
                        NA)
  )

# Calculate manufacturer-brand totals
long_data <- long_data %>%
  group_by(manu_brand) %>%
  mutate(manbra_tot = ifelse(is.na(volume), 
                             NA_real_, 
                             sum(volume, na.rm = TRUE))) %>%
  ungroup()

# → SAVE: Manufacturer-brand totals
manbra_totals <- long_data %>%
  filter(!is.na(manbra_tot)) %>%
  distinct(manu_brand, manbra_tot) %>%
  arrange(desc(manbra_tot))

# Save manufacturer-brand table for review
fwrite(manbra_totals, here("Data", "Tables", "manu_brand_tab.csv"))

################################################################################
# SECTION 6: CREATE MARKET SHARE INDICATOR VARIABLES
################################################################################

# Create manufacturer indicator variables (deterministic ordering)
# → SAVE: High-volume manufacturer numbers
high_manu_nums <- high_vol_manu %>%
  arrange(desc(manu_vol_tot), manu) %>%
  pull(manu_num)

# Create manufacturer variables
for (i in seq_along(high_manu_nums)) {
  var_name <- paste0("man_", i)
  long_data[[var_name]] <- ifelse(long_data$manu_num == high_manu_nums[i], 1, 0)
}

# Manufacturer "other" and "total" variables
long_data <- long_data %>%
  mutate(
    man_other = ifelse(!is.na(manu_vol_tot) & manu_vol_tot < volume_threshold, 1, 0),
    man_tot = ifelse(!is.na(manu_vol_tot), 1, NA)
  )

# Create brand indicator variables
# → SAVE: High-volume brand numbers
high_brand_nums <- high_vol_brand %>%
  arrange(desc(brand_vol_tot), brand) %>%
  pull(brand_num)

for (i in seq_along(high_brand_nums)) {
  var_name <- paste0("bra_", i)
  long_data[[var_name]] <- ifelse(long_data$brand_num == high_brand_nums[i], 1, 0)
}

# Brand "other" variable
long_data <- long_data %>%
  mutate(bra_other = ifelse(!is.na(brand_vol_tot) & brand_vol_tot < volume_threshold, 1, 0))

# Create manufacturer-brand indicator variables
# → SAVE: High-volume manufacturer-brand combinations
high_manbra_list <- manbra_totals %>%
  filter(manbra_tot >= volume_threshold) %>%
  arrange(desc(manbra_tot), manu_brand) %>%
  pull(manu_brand)

for (i in seq_along(high_manbra_list)) {
  var_name <- paste0("mb_", i)
  long_data[[var_name]] <- ifelse(long_data$manu_brand == high_manbra_list[i], 1, 0)
}

# Manufacturer-brand "other" and "total" variables
long_data <- long_data %>%
  mutate(
    mb_other = ifelse(!is.na(manbra_tot) & manbra_tot < volume_threshold, 1, 0),
    mb_tot = ifelse(!is.na(manbra_tot), 1, NA)
  )

################################################################################
# SECTION 7: SAVE PROCESSED DATA
################################################################################

# Save the processed data
fwrite(long_data, 
       here("Data", "Management data", paste0(country, "_", year, "_am_rdt_os_cleaned_long_wt_volume.csv")),
       na = "")

################################################################################
# HTML LOGGING SECTION
################################################################################

# Initialize HTML log
cat('<!DOCTYPE html>
<html><head>
<title>Volume Variables Analysis</title>
<style>
body {font-family: Arial, sans-serif; margin: 20px;}
h1, h2, h3 {color: #2E86AB;}
table {border-collapse: collapse; width: 100%; margin: 10px 0;}
th, td {border: 1px solid #ddd; padding: 8px; text-align: left;}
th {background-color: #f2f2f2;}
.section {margin: 20px 0; padding: 15px; border-left: 4px solid #2E86AB;}
.warning {background-color: #fff3cd; border-color: #ffc107;}
.success {background-color: #d4edda; border-color: #28a745;}
.user_input {background-color: #ffebee; padding: 10px; margin: 10px 0; border-left: 4px solid #f44336;}
</style>
</head><body>', file = log_file)

# Start capturing output
sink(log_file, append = TRUE)

cat("<h1>ACTwatch LITE - Volume and Market Share Variables</h1>\n")
cat("<p><strong>Country:</strong> ", country, " | <strong>Year:</strong> ", year, " | <strong>Date:</strong> ", as.character(Sys.Date()), "</p>\n")

################################################################################
# HTML: LOADING DATA
################################################################################

cat("<h2>LOADING DATA</h2>\n")
cat("<p>Data loaded successfully with ", initial_records, " rows</p>\n")

################################################################################
# HTML: DIAGNOSTIC VOLUMES
################################################################################

cat("<h2>DIAGNOSTIC VOLUMES</h2>\n")

# USER INPUT WARNING
cat("<div class='user_input'>\n")
cat("<h4>$$$ USER INPUT REQUIRED: DIAGNOSTIC OUTLIER MANAGEMENT $$$</h4>\n")
cat("<p>After the diagnostic sensitivity analysis is completed, you will need to return to this section to set volumes to missing or to recode volumes according to the outcome of the sensitivity analysis. Make all edits to the microscopy volume (d4) and rdt volume (r13) variables.</p>\n")
cat("</div>\n")

# Recalculate diagnostic summary from final data
diagnostic_summary <- data.frame(
  Variable = c("Microscopy outlets with volume", "RDT outlets with volume", "Total microscopy volume", "Total RDT volume"),
  Count = c(
    sum(long_data$vf_micro == 1, na.rm = TRUE),
    sum(long_data$vf_rdt == 1, na.rm = TRUE),
    sum(long_data$vd_micro, na.rm = TRUE),
    sum(long_data$vd_rdt, na.rm = TRUE)
  )
)

html_table(diagnostic_summary, "Diagnostic Volume Summary")

################################################################################
# HTML: ANTIMALARIAL VOLUMES
################################################################################

cat("<h2>ANTIMALARIAL VOLUMES</h2>\n")

# USER INPUT WARNING
cat("<div class='user_input'>\n")
cat("<h4>$$$ USER INPUT REQUIRED: VOLUME CALCULATION RESTRICTIONS $$$</h4>\n")
cat("<p>Modify the exclusion criteria as appropriate so that calculations correctly restrict to valid sales volume and omit inconsistent, refused, don't know and user-missing values.</p>\n")
cat("</div>\n")

cat("<p>Volume variable created: AETDs sold in the past week</p>\n")

# Display high volume outliers
if (high_volume_count > 0) {
  cat("<h3>High Volume Products (>500 AETDs)</h3>\n")
  cat("<p>Found ", high_volume_count, " products with volume > 500 AETDs</p>\n")
}

################################################################################
# HTML: BRAND/MANUFACTURER MARKET SHARE ANALYSIS
################################################################################

cat("<h2>BRAND/MANUFACTURER MARKET SHARE ANALYSIS</h2>\n")

# USER INPUT WARNING - Volume threshold
cat("<div class='user_input'>\n")
cat("<h4>$$$ USER INPUT REQUIRED: VOLUME THRESHOLD SETTING $$$</h4>\n")
cat("<p>This step identifies AM brands and manufacturers with sales volumes exceeding a threshold for market share analysis. In ACTwatch Lite Nigeria 2024, threshold was set to 1000 AETDs, but this should be revised according to local conditions.</p>\n")
cat("<p><strong>Current threshold: ", volume_threshold, " AETDs</strong></p>\n")
cat("</div>\n")

# MANUFACTURER ANALYSIS
cat("<h3>Manufacturer Analysis</h3>\n")

html_table(manu_freq, "Manufacturer Frequencies")
html_table(manu_totals, "Manufacturer Volume Totals")

cat("<h4>Manufacturers with ≥ ", volume_threshold, " AETDs sold:</h4>\n")
html_table(high_vol_manu, paste("Manufacturers Above", volume_threshold, "AETD Threshold"))

cat("<div class='user_input'>\n")
cat("<h4>$$$ RECORD RESULTS: Insert manufacturer results here $$$</h4>\n")
cat("</div>\n")

# BRAND ANALYSIS  
cat("<h3>Brand Analysis</h3>\n")

html_table(brand_freq, "Brand Frequencies")
html_table(brand_totals, "Brand Volume Totals")

cat("<h4>Brands with ≥ ", volume_threshold, " AETDs sold:</h4>\n")
html_table(high_vol_brand, paste("Brands Above", volume_threshold, "AETD Threshold"))

cat("<div class='user_input'>\n")
cat("<h4>$$$ RECORD RESULTS: Insert brand results here $$$</h4>\n")
cat("</div>\n")

# Cross-tabulation
html_table(brand_by_manu, "Brand by Manufacturer Cross-tabulation")

# Quality check by qaact if available
if ("qaact" %in% names(long_data)) {
  html_table(qaact_summary, "Manufacturers with Quality ACT (qaact=1)")
  
  cat("<div class='user_input'>\n")
  cat("<h4>$$$ RECORD RESULTS: Insert qaact manufacturer results here $$$</h4>\n")
  cat("</div>\n")
}

################################################################################
# HTML: MANUFACTURER-BRAND CONCATENATED VARIABLE
################################################################################

cat("<h3>Creating Manufacturer-Brand Concatenated Variable</h3>\n")

html_table(manbra_totals, "Manufacturer-Brand Combination Totals")

cat("<p>Manufacturer-brand table saved to: Data/Tables/manu_brand_tab.csv</p>\n")

################################################################################
# HTML: MARKET SHARE INDICATOR VARIABLES
################################################################################

cat("<h3>Creating Market Share Indicator Variables</h3>\n")

# Summary of indicator variables created
indicator_vars <- long_data %>%
  select(starts_with("man_"), starts_with("bra_"), starts_with("mb_")) %>%
  summarise(across(everything(), ~ sum(.x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Count") %>%
  arrange(desc(Count))

html_table(indicator_vars, "Market Share Indicator Variables Created")

################################################################################
# HTML: VARIABLE SUMMARY
################################################################################

cat("<h2>VARIABLE SUMMARY</h2>\n")

cat("<p><strong>Diagnostic variables:</strong> vd_micro, vf_micro, vd_rdt, vf_rdt</p>\n")
cat("<p><strong>Volume variables:</strong> volume, anyAMsales, st_anyAMsales</p>\n")
cat("<p><strong>Totals variables:</strong> manu_vol_tot, brand_vol_tot, manbra_tot</p>\n")
cat("<p><strong>Market share indicators:</strong> man_*, bra_*, mb_* (dynamically created)</p>\n")

################################################################################
# HTML: COMPLETION SUMMARY
################################################################################

cat("<h2>SAVING PROCESSED DATA</h2>\n")
cat("<p>Data saved successfully</p>\n")

# Final summary
final_summary <- data.frame(
  Metric = c(
    "Total observations",
    "Outlets with volume",
    "Total volume (AETDs)",
    "Manufacturers above threshold",
    "Brands above threshold", 
    "Manufacturer-brand combos above threshold"
  ),
  Value = c(
    nrow(long_data),
    sum(!is.na(long_data$volume) & long_data$volume > 0, na.rm = TRUE),
    round(sum(long_data$volume, na.rm = TRUE), 0),
    length(high_manu_nums),
    length(high_brand_nums),
    length(high_manbra_list)
  )
)

html_table(final_summary, "Final Processing Summary")

cat("<div class='section success'>\n")
cat("<h2>Processing Complete</h2>\n")
cat("<p>Volume and market share variables have been successfully generated.</p>\n")
cat("</div>\n")

# Close HTML log
sink()
cat("</body></html>", file = log_file, append = TRUE)

################################################################################
# CONSOLE OUTPUT
################################################################################

# Reset warning options
options(warn = 0)

cat("\n=== VOLUME AND MARKET SHARE PROCESSING COMPLETE ===\n")
cat("Records processed:", nrow(long_data), "\n")

if (exists("final_summary")) {
  total_volume <- final_summary$Value[final_summary$Metric == "Total volume (AETDs)"]
  outlets_with_volume <- final_summary$Value[final_summary$Metric == "Outlets with volume"]
  manu_count <- final_summary$Value[final_summary$Metric == "Manufacturers above threshold"]
  brand_count <- final_summary$Value[final_summary$Metric == "Brands above threshold"]
  
  cat("Total volume (AETDs):", total_volume, "\n")
  cat("Outlets with volume:", outlets_with_volume, "\n")
  cat("Manufacturers above threshold:", manu_count, "\n")
  cat("Brands above threshold:", brand_count, "\n")
}

cat("\nVolume threshold used:", volume_threshold, "AETDs\n")
cat("Variables generated: volume, market share indicators\n")
cat("\nHTML report saved to:", log_file, "\n")

################################################################################
# END
################################################################################