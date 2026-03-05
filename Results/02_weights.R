################################################################################
# ACTwatch LITE 
# Step 2.2 Apply Sampling Weights
# REORGANIZED VERSION - HTML logging at end
################################################################################
#
# This script applies sampling weights to the cleaned long-format dataset to
# enable representative analysis of antimalarial and RDT markets. It reads
# main and booster sample weights from the weighting tool, merges them with
# the outlet dataset, and creates two weight variables: wt_marketShare (for
# market share estimates, excluding booster samples) and wt_allOutlet (for
# all-outlet estimates, including booster samples). The script calculates
# finite population corrections (FPC) for variance estimation and performs
# comprehensive validation checks to ensure all outlets are matched to
# appropriate weights. After processing, the weighted dataset is saved for
# use in denominator variable creation and subsequent analysis.
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

### NOTE!!! SET FPC VALUES BEFORE PROCEEDING ###

# Set options
options(stringsAsFactors = FALSE, warn = -1)

################################################################################
# SECTION 1: LOAD MAIN DATASET
################################################################################

long_data <- fread(here("Data", "Management data", 
                        paste0(country, "_", year, "_am_rdt_os_cleaned_long.csv"))) %>% 
  mutate(across(where(is.character), ~ str_trim(str_to_upper(.))))

# SAVE FOR LOGGING
initial_records <- nrow(long_data)
initial_outlets <- sum(long_data$nOut == 1, na.rm = TRUE)

################################################################################
# SECTION 2: READ AND COMBINE WEIGHT FILES
################################################################################

# Construct weighting tool filename dynamically
weight_file <- paste0("AwL Weighting Tool-", country, "-", year, ".xlsx")

# Read and process main weights
main_weights <- read_excel(here("Data", "Weights", weight_file),
                           sheet = "3. Main weights") %>%
  filter(!is.na(cluster_id), cluster_id != "0", cluster_id != 0) %>%
  mutate(across(where(is.character), ~ str_trim(str_to_upper(.))),
         booster = 0)

# → SAVE: Main cluster count for logging
main_clusters <- nrow(main_weights)

# Read and process booster weights
booster_weights <- read_excel(here("Data", "Weights", weight_file),
                              sheet = "4. Booster weights") %>%
  filter(!is.na(cluster_id), cluster_id != "0", cluster_id != 0) %>%
  mutate(across(where(is.character), ~ str_trim(str_to_upper(.))),
         booster = 1)

# SAVE FOR LOGGING
booster_clusters <- nrow(booster_weights)

# Combine weights
all_weights <- if(booster_clusters > 0) {
  bind_rows(main_weights, booster_weights)
} else {
  main_weights
}

# SAVE FOR LOGGING
total_clusters <- nrow(all_weights)

################################################################################
# SECTION 3: MERGE WEIGHTS WITH MAIN DATASET
################################################################################

long_data <- long_data %>%
  mutate(cluster_id = c4) %>% 
  left_join(all_weights, by = "cluster_id")

# SAVE FOR LOGGING - Merge summary
merge_summary <- long_data %>%
  summarise(
    Total_Records = n(),
    Matched_Records = sum(!is.na(weight)),
    Match_Rate_Pct = round(sum(!is.na(weight))/n()*100, 1),
    Total_Outlets = sum(nOut == 1, na.rm = TRUE),
    Matched_Outlets = sum(nOut == 1 & !is.na(weight), na.rm = TRUE),
    Outlet_Match_Rate_Pct = round(sum(nOut == 1 & !is.na(weight))/sum(nOut == 1)*100, 1)
  )

# SAVE FOR LOGGING - Check for unmatched outlets
unmatched_outlets <- long_data %>%
  filter(is.na(weight) & nOut == 1)

unmatched_sample <- unmatched_outlets %>%
  select(outletid, cluster_id, strata1, any_of("strata2")) %>%
  head(10)

################################################################################
# SECTION 4: CREATE WEIGHT VARIABLES
################################################################################

long_data <- long_data %>%
  mutate(
    # Market share weight (zero for booster samples)
    wt_marketShare = if_else(booster == 1, 0, weight),
    
    # All-outlet weight (not set to zero for booster samples)
    wt_allOutlet = weight
  )

# SAVE FOR LOGGING - Weight summary statistics
weight_summary <- long_data %>%
  filter(nOut == 1) %>%
  summarise(
    Total_Outlets = n(),
    Main_Sample = sum(booster == 0, na.rm = TRUE),
    Booster_Sample = sum(booster == 1, na.rm = TRUE),
    Market_Weight_Min = round(min(wt_marketShare, na.rm = TRUE), 4),
    Market_Weight_Max = round(max(wt_marketShare, na.rm = TRUE), 4),
    Market_Weight_Mean = round(mean(wt_marketShare, na.rm = TRUE), 4),
    All_Weight_Min = round(min(wt_allOutlet, na.rm = TRUE), 4),
    All_Weight_Max = round(max(wt_allOutlet, na.rm = TRUE), 4),
    All_Weight_Mean = round(mean(wt_allOutlet, na.rm = TRUE), 4)
  )

# SAVE FOR LOGGING - Weight distribution by booster status
weight_by_booster <- long_data %>%
  filter(nOut == 1) %>%
  group_by(Booster_Status = if_else(booster == 1, "Booster", "Main")) %>%
  summarise(
    Outlets = n(),
    Market_Weight_Mean = round(mean(wt_marketShare, na.rm = TRUE), 4),
    All_Weight_Mean = round(mean(wt_allOutlet, na.rm = TRUE), 4),
    .groups = "drop"
  )

################################################################################
# SECTION 5: FINITE POPULATION CORRECTION (FPC)
################################################################################

# $$$ USER INPUT: Update FPC values for each stratum
# Format: (clusters_sampled) / (clusters_total_in_frame)
long_data <- long_data %>%
  mutate(fpc = case_when(
    strata1 == "ABIA" ~ (44)/(124),   # $$$ UPDATE with actual values
    strata1 == "KANO" ~ (51)/(299),   # $$$ UPDATE with actual values
    strata1 == "LAGOS" ~ (34)/(610),  # $$$ UPDATE with actual values
    TRUE ~ NA_real_
  ))

# SAVE FOR LOGGING
missing_fpc <- sum(is.na(long_data$fpc))

################################################################################
# SECTION 6: FINAL DATA PROCESSING
################################################################################

# Regenerate nOut
long_data <- long_data %>%
  select(-any_of("nOut")) %>%
  arrange(outletid) %>% 
  group_by(outletid) %>%
  mutate(nOut = as.numeric(row_number() == 1)) %>%
  ungroup()

# SAVE FOR LOGGING
final_outlets <- sum(long_data$nOut == 1, na.rm = TRUE)

################################################################################
# SECTION 7: SAVE DATASET
################################################################################

output_file <- here("Data", "Management data", 
                    paste0(country, "_", year, "_am_rdt_os_cleaned_long_wt.csv"))

fwrite(long_data, output_file, row.names = FALSE, na = "")

################################################################################
# SECTION 8: FINAL SUMMARY CALCULATIONS
################################################################################

# SAVE FOR LOGGING
final_summary <- long_data %>%
  filter(nOut == 1) %>%
  summarise(
    Total_Outlets = n(),
    Main_Sample = sum(booster == 0, na.rm = TRUE),
    Booster_Sample = sum(booster == 1, na.rm = TRUE),
    Outlets_With_Weights = sum(!is.na(wt_allOutlet)),
    Outlets_Missing_Weights = sum(is.na(wt_allOutlet))
  ) %>%
  mutate(
    Weight_Coverage_Pct = round(Outlets_With_Weights / Total_Outlets * 100, 1)
  )

# SAVE FOR LOGGING - Strata breakdown
if ("strata2" %in% names(long_data) && 
    sum(!is.na(long_data$strata2) & long_data$strata2 != "") > 0 &&
    length(unique(long_data$strata2[!is.na(long_data$strata2) & long_data$strata2 != ""])) >= 2) {
  strata_breakdown <- long_data %>% 
    filter(nOut == 1) %>% 
    count(strata1, strata2, booster, name = "Outlets") %>%
    arrange(strata1, strata2, booster)
  has_strata2 <- TRUE
} else {
  strata_breakdown <- long_data %>% 
    filter(nOut == 1) %>% 
    count(strata1, booster, name = "Outlets") %>%
    arrange(strata1, booster)
  has_strata2 <- FALSE
}

################################################################################
# GENERATE HTML LOG
################################################################################

# Setup log file
log_dir <- here("Data", "Management data", "Data cleaning notes")
if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE)
log_file <- file.path(log_dir, paste0("AwL_", country, "_", year, "_weights_notes.html"))

# Initialize HTML
cat('<!DOCTYPE html>
<html><head>
<title>Sampling Weights Processing</title>
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
.success {background-color: #d4edda; border-color: #28a745;}
pre {background-color: #f8f9fa; padding: 10px; border-radius: 5px;}
</style>
</head><body>', file = log_file)

sink(log_file, append = TRUE)

cat("<h1>ACTwatch Lite Sampling Weights Processing</h1>\n")
cat(glue("<div class='section'>
<h2>Processing Session Information</h2>
<p><strong>Date/Time:</strong> {Sys.time()}</p>
<p><strong>Country:</strong> {country}</p>
<p><strong>Year:</strong> {year}</p>
<p><strong>Script:</strong> Sampling Weights Application</p>
</div>\n"))

# =============================================================================
# HTML CONTENT GENERATION
# =============================================================================

cat("<h2>1. Loading Data</h2>\n")
cat(glue("<div class='section'>
<p><strong>Main dataset loaded successfully</strong></p>
<p><strong>Total records:</strong> {initial_records}</p>
<p><strong>Unique outlets:</strong> {initial_outlets}</p>
</div>\n"))

cat("<h2>2. Reading Weight Files</h2>\n")
cat(glue("<div class='section'>
<p><strong>Main weights loaded:</strong> {main_clusters} clusters</p>
</div>\n"))

cat(glue("<div class='section'>
<p><strong>Booster weights loaded:</strong> {booster_clusters} clusters</p>
</div>\n"))

cat(glue("<div class='section'>
<p><strong>Total weight clusters:</strong> {total_clusters}</p>
<p><strong>Main sample clusters:</strong> {main_clusters}</p>
<p><strong>Booster sample clusters:</strong> {booster_clusters}</p>
</div>\n"))

cat("<h2>3. Merging Weights with Main Dataset</h2>\n")
html_table(merge_summary, "Merge Summary - INSERT RESULTS")

if (nrow(unmatched_outlets) > 0) {
  cat("<div class='section warning'>\n")
  cat(glue("<h3>$$$ WARNING: {nrow(unmatched_outlets)} outlets not matched to weights</h3>\n"))
  cat("<p><strong>ACTION REQUIRED:</strong> Investigate unmatched outlets</p>\n")
  html_table(unmatched_sample, "Sample of Unmatched Outlets (first 10)")
  cat("</div>\n")
} else {
  cat("<div class='section success'>\n<p>All outlets successfully matched to weights</p>\n</div>\n")
}

cat("<h2>4. Creating Weight Variables</h2>\n")
cat("<div class='section'>\n")
cat("<p><strong>Weight variable definitions:</strong></p>\n")
cat("<ul>\n")
cat("<li><strong>wt_marketShare:</strong> Market share weight (zero for booster samples)</li>\n")
cat("<li><strong>wt_allOutlet:</strong> All-outlet weight (includes booster samples)</li>\n")
cat("</ul>\n")
cat("</div>\n")

html_table(weight_summary, "Weight Summary Statistics - INSERT RESULTS")
html_table(weight_by_booster, "Weight Distribution by Sample Type")

cat("<h2>5. Finite Population Correction (FPC)</h2>\n")
cat("<p>FPC values calculated for each stratum</p>\n")

if (missing_fpc > 0) {
  cat(glue("<p class='warning'>$$$ WARNING: {missing_fpc} records with missing FPC values</p>\n"))
}

cat("<h2>6. Final Data Processing</h2>\n")
cat(glue("<div class='section'>
<p><strong>nOut variable regenerated</strong></p>
<p><strong>Unique outlets in final dataset:</strong> {final_outlets}</p>
</div>\n"))

cat(glue("<div class='section success'>
<p><strong>Dataset saved successfully to:</strong></p>
<p>{output_file}</p>
</div>\n"))

cat("<h2>7. Final Dataset Summary</h2>\n")
html_table(final_summary, "Final Dataset Summary")

cat("<h3>Outlet Distribution by Strata</h3>\n")
if (has_strata2) {
  html_table(strata_breakdown, "Strata Breakdown (strata1 × strata2 × booster)")
} else {
  html_table(strata_breakdown, "Strata Breakdown (strata1 × booster)")
}

cat("<h2>$$$ Critical Issues Summary</h2>\n")

critical_issues <- list()

if (nrow(unmatched_outlets) > 0) {
  critical_issues <- c(critical_issues, 
                       glue("Outlets not matched to weights: {nrow(unmatched_outlets)}"))
}

missing_fpc_check <- sum(is.na(long_data$fpc), na.rm = TRUE)
if (missing_fpc_check == nrow(long_data)) {
  critical_issues <- c(critical_issues, 
                       "FPC values not yet specified - requires manual input")
}

if (length(critical_issues) > 0) {
  cat("<div class='section critical'>\n<ul>\n")
  for (issue in critical_issues) {
    cat(glue("<li><strong>{issue}</strong></li>\n"))
  }
  cat("</ul>\n<p><strong>ALL $$$ FLAGGED ITEMS MUST BE ADDRESSED BEFORE ANALYSIS</strong></p>\n</div>\n")
} else {
  cat("<div class='section success'>\n<p><strong>NO CRITICAL ISSUES DETECTED</strong></p>\n</div>\n")
}

cat("<div class='section success'>\n")
cat("<h2>Processing Complete</h2>\n")
cat("<p>Sampling weights have been successfully applied to the dataset.</p>\n")
cat("<p><strong>Next steps:</strong></p>\n")
cat("<ul>\n")
cat("<li>Review unmatched outlets (if any) and investigate discrepancies</li>\n")
cat("<li>Update FPC values with actual sampling design parameters</li>\n")
cat("<li>Verify weight distributions are reasonable for your survey design</li>\n")
cat("<li>Proceed to denominator variable creation (Step 2.3)</li>\n")
cat("</ul>\n")
cat("</div>\n")

sink()
cat("</body></html>", file = log_file, append = TRUE)
options(warn = 0)

################################################################################
# CONSOLE OUTPUT
################################################################################

cat("\n")
cat("================================================================================\n")
cat("=== SAMPLING WEIGHTS PROCESSING COMPLETE ===\n")
cat("================================================================================\n\n")

# Basic dataset info
cat("DATASET SUMMARY:\n")
cat("  Total records:", nrow(long_data), "\n")
cat("  Unique outlets:", final_outlets, "\n")

# Weight files loaded
cat("\nWEIGHT FILES LOADED:\n")
cat("  Main sample clusters:", main_clusters, "\n")
cat("  Booster sample clusters:", booster_clusters, "\n")
cat("  Total weight clusters:", total_clusters, "\n")

# Sample composition
cat("\nSAMPLE COMPOSITION:\n")
main_outlets <- sum(long_data$booster == 0 & long_data$nOut == 1, na.rm = TRUE)
booster_outlets <- sum(long_data$booster == 1 & long_data$nOut == 1, na.rm = TRUE)
cat(sprintf("  Main sample: %d outlets (%.1f%%)\n", 
            main_outlets, main_outlets/final_outlets*100))
cat(sprintf("  Booster sample: %d outlets (%.1f%%)\n", 
            booster_outlets, booster_outlets/final_outlets*100))

# Weight coverage
outlets_with_weights <- sum(long_data$nOut == 1 & !is.na(long_data$wt_allOutlet), na.rm = TRUE)
outlets_missing_weights <- final_outlets - outlets_with_weights
cat("\nWEIGHT COVERAGE:\n")
cat(sprintf("  Outlets with weights: %d/%d (%.1f%%)\n",
            outlets_with_weights, final_outlets,
            outlets_with_weights/final_outlets*100))
if (outlets_missing_weights > 0) {
  cat(sprintf("  Outlets missing weights: %d\n", outlets_missing_weights))
}

# Weight variable summary
cat("\nWEIGHT VARIABLES CREATED:\n")
cat("  wt_marketShare: Market share weight (zero for booster samples)\n")
cat("  wt_allOutlet: All-outlet weight (includes booster samples)\n")
cat("  fpc: Finite population correction (requires manual input)\n")

# Critical issues section
cat("\n")
if (length(critical_issues) > 0) {
  cat("*** CRITICAL ISSUES DETECTED ***\n")
  cat(strrep("=", 80), "\n")
  for (issue in critical_issues) {
    cat("$$$ ", issue, "\n", sep = "")
  }
  cat(strrep("=", 80), "\n")
  
  # Specific warnings
  if (nrow(unmatched_outlets) > 0) {
    cat("\n$$$ WARNING:", nrow(unmatched_outlets), "outlets not matched to weights\n")
    cat("    ACTION: Review unmatched outlets in HTML log\n")
  }
  
  if (missing_fpc_check == nrow(long_data)) {
    cat("\n$$$ ACTION REQUIRED: FPC values need to be specified\n")
    cat("    ACTION: Update FPC calculation in script with actual sampling design parameters\n")
  }
  
  cat("\n$$$ REVIEW HTML LOG FOR FULL DETAILS ON REQUIRED ACTIONS\n\n")
} else {
  cat("*** NO CRITICAL ISSUES DETECTED ***\n\n")
}

# Output files
cat("OUTPUT FILES:\n")
cat("  Dataset:", basename(output_file), "\n")
cat("  HTML log:", basename(log_file), "\n")

cat("\nFull file paths:\n")
cat("  ", output_file, "\n", sep = "")
cat("  ", log_file, "\n", sep = "")

# Next steps
cat("\nNEXT STEPS:\n")
cat("  1. Review HTML log for detailed processing information\n")
if (nrow(unmatched_outlets) > 0) {
  cat("  2. Investigate and resolve unmatched outlets\n")
}
if (missing_fpc_check == nrow(long_data)) {
  cat("  3. Update FPC values with actual sampling design parameters\n")
}
cat("  4. Verify weight distributions are appropriate for your survey\n")
cat("  5. Proceed to Step 2.3: Denominator variable creation\n")

cat("\n")
cat("================================================================================\n\n")

################################################################################
# END
################################################################################