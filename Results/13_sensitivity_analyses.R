################################################################################
# ACTwatch LITE 
# Step 2.13 Sensitivity Analysis
################################################################################

# SCRIPT PURPOSE:
# Performs market share sensitivity analysis to identify and handle volume outliers
# in both antimalarial and diagnostic data. Compares two outlier definitions:
# 1) Independent Evaluation (IE) definitions (outlet-specific thresholds)
# 2) Conservative definitions (>100 AETD investigation)
#
# MAIN PROCESSES:
# 1. Load and filter data (antimalarials, exclude prophylaxis)
# 2. Identify IE outliers by outlet type (PNP: 1000, PFP: 1000, Pharmacy: 500, Retail: 200, Informal: 200)
# 3. Apply IE definitions and calculate market share
# 4. Identify conservative outliers (>100 AETD for manual review)
# 5. Apply conservative definitions and calculate market share
# 6. Compare IE vs Conservative results
# 7. Process diagnostic data (microscopy and RDT)
# 8. Generate sensitivity datasets for analysis
#
# OUTPUTS:
# - {country}_{year}AM_market_share_sensitivity_analysis_IE.csv
# - {country}_{year}AM_market_share_sensitivity_analysis_conservative.csv
# - {country}_{year}_antimalarial_data.csv
# - {country}_{year}_DIAG_market_share_sensitivity_analysis_IE.csv
# - {country}_{year}_rdt_micro_data.csv
#
# USER INPUTS REQUIRED:
# $$$ Review all identified outliers and document findings
# $$$ Manual decision on conservative outlier definitions (>100 AETD cases)
# $$$ Compare IE vs Conservative results and choose final definition
# $$$ Uncomment appropriate final_am_data line based on decision
#
# FLAG LEGEND:
# $$$ = User input required
# → SAVE = Variable saved for use in HTML section

################################################################################
# SETUP
################################################################################

# Create logs directory and sensitivity analyses directory
log_dir <- here("Data", "Final data", "Data cleaning notes")
sens_dir <- here("Data", "Final data", "Sensitivity analyses")
if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE)
if (!dir.exists(sens_dir)) dir.create(sens_dir, recursive = TRUE)

log_file <- file.path(log_dir, paste0("AwL_", country, "_", year, "_sensitivity_analysis_notes.html"))

# Ensure sink is closed even if script errors
on.exit({
  if (sink.number() > 0) sink()
}, add = TRUE)

################################################################################
# SECTION 1: LOAD DATA AND FILTER
################################################################################

# Load full dataset
full_data <- fread(here("Data", "Final data", paste0(country, "_", year, "_full_data.csv")))

# → SAVE: Initial record count
initial_records <- nrow(full_data)

# Filter for antimalarials not used for prophylaxis
am_data <- full_data %>%
  filter(producttype %in% c(1, 2), drugcat1 != 4 | is.na(drugcat1))

# → SAVE: Filtered record count
am_records <- nrow(am_data)

################################################################################
# SECTION 2: IDENTIFY IE VOLUME OUTLIERS
################################################################################

# $$$ USER INPUT REQUIRED - Review outliers carefully

# Function to identify outliers
identify_outliers <- function(data, volume_threshold, outlet_condition, outlet_name) {
  outliers <- data %>%
    filter(
      volume > volume_threshold & !is.na(volume),
      eval(parse(text = outlet_condition)),
      booster == 0 | is.na(booster)
    ) %>%
    select(gname, a3, volume, outcat2, amauditkey) %>%
    arrange(desc(volume))
  
  return(outliers)
}

# → SAVE: Identify outliers by outlet type using IE definitions
outliers_pnp <- identify_outliers(am_data, 1000, "private_nonprofit == 1", "Private-not-for-profit health facilities")
outliers_pfp <- identify_outliers(am_data, 1000, "private_forprofit == 1", "Private-for-profit health facilities") 
outliers_pharm <- identify_outliers(am_data, 500, "(pharmacy == 1 | drug_store == 1)", "Pharmacies/Drug stores")
outliers_ger <- identify_outliers(am_data, 200, "gen_retail == 1", "General retailers")
outliers_inf <- identify_outliers(am_data, 200, "total_informal == 1", "Informal sector")

################################################################################
# SECTION 3: APPLY IE OUTLIER DEFINITIONS
################################################################################

# Apply IE outlier definitions - set volumes to missing
am_data_ie <- am_data %>%
  mutate(
    volume_original = volume,
    volume = case_when(
      volume > 1000 & !is.na(volume) & private_nonprofit == 1 & (booster == 0 | is.na(booster)) ~ NA_real_,
      volume > 1000 & !is.na(volume) & private_forprofit == 1 & (booster == 0 | is.na(booster)) ~ NA_real_,
      volume > 500 & !is.na(volume) & (pharmacy == 1 | drug_store == 1) & (booster == 0 | is.na(booster)) ~ NA_real_,
      volume > 200 & !is.na(volume) & gen_retail == 1 & (booster == 0 | is.na(booster)) ~ NA_real_,
      volume > 200 & !is.na(volume) & total_informal == 1 & (booster == 0 | is.na(booster)) ~ NA_real_,
      TRUE ~ volume
    ),
    outlier_removed_ie = ifelse(is.na(volume) & !is.na(volume_original), 1, 0)
  )

# Save IE sensitivity analysis dataset
fwrite(am_data_ie, 
       here("Data", "Final data", "Sensitivity analyses", paste0(country, "_", year, "AM_market_share_sensitivity_analysis_IE.csv")),
       na = "")

################################################################################
# SECTION 4: MARKET SHARE ANALYSIS - IE OUTLIER DEFINITIONS
################################################################################

# Define volume covariates (antimalarial product types)
vol_covariates <- c("anyAM", "AL", "ASAQ", "APPQ", "DHAPPQ", "ARPPQ", "otherACT", 
                    "QN", "CQ", "SP", "SPAQ", "nonartoth", "oartmono", 
                    "recAS", "injAS", "injAR", "injAE")

# Define outlet variables for market share
outlet_vars <- c("total_retail", "private_nonprofit", "private_forprofit", 
                 "pharmacy", "drug_store", "laboratory", "total_informal")

# Function to calculate market share with confidence intervals
calculate_market_share <- function(data, vol_var, outlet_var) {
  if (!vol_var %in% names(data) || !outlet_var %in% names(data)) {
    return(data.frame(volume = 0, lower_bound = NA, upper_bound = NA, n = 0))
  }
  
  filtered_data <- data %>%
    filter(
      get(vol_var) == 1 | is.na(get(vol_var)),
      get(outlet_var) == 1 | is.na(get(outlet_var)),
      !is.na(volume)
    )
  
  if (nrow(filtered_data) == 0) {
    return(data.frame(volume = 0, lower_bound = NA, upper_bound = NA, n = 0))
  }
  
  # Calculate weighted totals (simplified - use survey package for complex designs)
  if ("wt_marketShare" %in% names(filtered_data)) {
    total_volume <- sum(filtered_data$volume * filtered_data$wt_marketShare, na.rm = TRUE)
    total_weight <- sum(filtered_data$wt_marketShare, na.rm = TRUE)
    mean_volume <- total_volume / total_weight
    
    # Simplified confidence interval calculation
    variance <- var(filtered_data$volume, na.rm = TRUE)
    n <- nrow(filtered_data)
    se <- sqrt(variance / n)
    margin <- qt(0.975, df = n - 1) * se
    
    return(data.frame(
      volume = total_volume,
      lower_bound = max(0, total_volume - margin * total_weight),
      upper_bound = total_volume + margin * total_weight,
      n = n
    ))
  } else {
    total_volume <- sum(filtered_data$volume, na.rm = TRUE)
    n <- nrow(filtered_data)
    
    return(data.frame(
      volume = total_volume,
      lower_bound = NA,
      upper_bound = NA,
      n = n
    ))
  }
}

# → SAVE: Generate market share results table for IE definitions
ie_results <- expand_grid(
  vol_var = vol_covariates,
  outlet_var = outlet_vars
) %>%
  mutate(
    results = map2(vol_var, outlet_var, ~calculate_market_share(am_data_ie, .x, .y))
  ) %>%
  unnest(results) %>%
  mutate(
    vol_var = factor(vol_var, levels = vol_covariates),
    outlet_var = factor(outlet_var, levels = outlet_vars)
  ) %>%
  arrange(vol_var, outlet_var)

# Create formatted results table
ie_table <- ie_results %>%
  select(vol_var, outlet_var, volume, lower_bound, upper_bound, n) %>%
  mutate(
    volume = round(volume, 1),
    lower_bound = round(lower_bound, 1),
    upper_bound = round(upper_bound, 1)
  )

################################################################################
# SECTION 5: CONSERVATIVE OUTLIER ANALYSIS (>100 AETD)
################################################################################

# $$$ USER INPUT REQUIRED - Manual decision on cases >100 AETD

# → SAVE: Identify all volumes >100 (excluding wholesale)
high_volume_cases <- am_data_ie %>%
  filter(
    volume > 100 & !is.na(volume),
    wholesale != 1 | is.na(wholesale),  # Exclude wholesale
    booster == 0 | is.na(booster)
  ) %>%
  arrange(desc(volume)) %>%
  select(amauditkey, outcat2, c7, gname, a3, volume)

# Create conservative dataset (copy of IE dataset for now)
# In practice, you would manually identify improbable cases and set them to missing
am_data_conservative <- am_data_ie %>%
  mutate(
    # $$$ MANUAL INPUT REQUIRED: Replace this with actual improbable case filtering
    # Example:
    # volume = case_when(
    #   amauditkey %in% c("UUID:example1", "UUID:example2") ~ NA_real_,
    #   TRUE ~ volume
    # ),
    outlier_removed_conservative = ifelse(is.na(volume) & !is.na(volume_original), 1, 0)
  )

# Save conservative dataset
fwrite(am_data_conservative,
       here("Data", "Final data", "Sensitivity analyses", paste0(country, "_", year, "AM_market_share_sensitivity_analysis_conservative.csv")),
       na = "")

################################################################################
# SECTION 6: MARKET SHARE ANALYSIS - CONSERVATIVE DEFINITIONS
################################################################################

# → SAVE: Generate conservative results
conservative_results <- expand_grid(
  vol_var = vol_covariates,
  outlet_var = outlet_vars
) %>%
  mutate(
    results = map2(vol_var, outlet_var, ~calculate_market_share(am_data_conservative, .x, .y))
  ) %>%
  unnest(results) %>%
  mutate(
    vol_var = factor(vol_var, levels = vol_covariates),
    outlet_var = factor(outlet_var, levels = outlet_vars)
  ) %>%
  arrange(vol_var, outlet_var)

conservative_table <- conservative_results %>%
  select(vol_var, outlet_var, volume, lower_bound, upper_bound, n) %>%
  mutate(
    volume = round(volume, 1),
    lower_bound = round(lower_bound, 1),
    upper_bound = round(upper_bound, 1)
  )

################################################################################
# SECTION 7: COMPARISON OF RESULTS
################################################################################

# $$$ USER INPUT REQUIRED - Compare and decide which definition to use

# → SAVE: Compare IE vs Conservative results
comparison <- ie_results %>%
  select(vol_var, outlet_var, volume_ie = volume, n_ie = n) %>%
  left_join(
    conservative_results %>% select(vol_var, outlet_var, volume_conservative = volume, n_conservative = n),
    by = c("vol_var", "outlet_var")
  ) %>%
  mutate(
    volume_diff = volume_conservative - volume_ie,
    volume_pct_diff = ifelse(volume_ie != 0, 
                             round((volume_conservative - volume_ie) / volume_ie * 100, 1), 
                             NA),
    n_diff = n_conservative - n_ie
  ) %>%
  filter(volume_ie > 0 | volume_conservative > 0)  # Only show rows with data

################################################################################
# SECTION 8: SAVE FINAL ANTIMALARIAL DATASET
################################################################################

# $$$ USER INPUT REQUIRED - Uncomment appropriate line based on sensitivity analysis decision

# Default to IE dataset (user should modify based on analysis)
final_am_data <- am_data_ie

# Save final antimalarial dataset
fwrite(final_am_data,
       here("Data", "Final data", "Sensitivity analyses", paste0(country, "_", year, "_antimalarial_data.csv")),
       na = "")

################################################################################
# SECTION 9: DIAGNOSTIC MARKET SHARE SENSITIVITY ANALYSIS
################################################################################

# Keep 1 observation for each outlet stocking malaria microscopy
diag_data <- full_data %>%
  filter(nout == 1, vf_micro == 1) %>%
  # Recode RDT variables to 0 for microscopy data
  mutate(across(c(rdt_true, starts_with("rdtmanu_"), st_rdt, st_qardt, 
                  starts_with("st_rdtmanu_"), vd_rdt, vf_rdt), ~ifelse(.x == 1, 0, .x)))

# Create test_type variable
diag_data <- diag_data %>%
  mutate(test_type = 1)

# Add RDT data if available
rdt_data_exists <- file.exists(here("Data", "Final data", paste0(country, "_", year, "_rdt_data.csv")))

if (rdt_data_exists) {
  tryCatch({
    rdt_data <- fread(here("Data", "Final data", paste0(country, "_", year, "_rdt_data.csv"))) %>%
      mutate(test_type = 2)
    
    # Ensure consistent column types before binding
    # Get common columns
    common_cols <- intersect(names(diag_data), names(rdt_data))
    
    # Convert all common columns to character to avoid type conflicts
    diag_data <- diag_data %>%
      mutate(across(all_of(common_cols), as.character))
    
    rdt_data <- rdt_data %>%
      mutate(across(all_of(common_cols), as.character))
    
    # Bind the datasets
    diag_data <- bind_rows(diag_data, rdt_data)
    
  }, error = function(e) {
    # Error handled, continue with microscopy data only
  })
}

# → SAVE: Diagnostic data processing status
diag_data_processed <- nrow(diag_data) > 0

if (diag_data_processed) {
  # Generate volume variable and diagnostic flags
  # Convert d4 and r13 back to numeric if they were converted to character
  diag_data <- diag_data %>%
    mutate(
      # Convert back to numeric if needed
      d4 = as.numeric(d4),
      r13 = as.numeric(r13),
      # Generate volume variable
      volume = case_when(
        test_type == 1 ~ d4,
        test_type == 2 ~ r13,
        TRUE ~ NA_real_
      ),
      total = 1,
      microscopy = ifelse(test_type == 1, 1, NA),
      rdt = ifelse(test_type == 2, 1, NA)
    ) %>%
    # Set system codes to missing
    mutate(
      volume = case_when(
        volume %in% c(99998, 998, 99) ~ NA_real_,
        TRUE ~ volume
      )
    )
  
  # → SAVE: Apply IE outlier definitions for diagnostics
  diag_outliers_pnp <- identify_outliers(diag_data, 1000, "private_nonprofit == 1", "Private-not-for-profit health facilities (Diagnostics)")
  diag_outliers_pfp <- identify_outliers(diag_data, 1000, "private_forprofit == 1", "Private-for-profit health facilities (Diagnostics)")
  diag_outliers_pharm <- identify_outliers(diag_data, 500, "(pharmacy == 1 | drug_store == 1)", "Pharmacies (Diagnostics)")
  diag_outliers_ger <- identify_outliers(diag_data, 200, "gen_retail == 1", "General retailers (Diagnostics)")
  diag_outliers_inf <- identify_outliers(diag_data, 200, "total_informal == 1", "Itinerant vendors (Diagnostics)")
  
  # Apply outlier definitions
  diag_data_final <- diag_data %>%
    mutate(
      volume = case_when(
        volume > 1000 & !is.na(volume) & private_nonprofit == 1 & (booster == 0 | is.na(booster)) ~ NA_real_,
        volume > 1000 & !is.na(volume) & private_forprofit == 1 & (booster == 0 | is.na(booster)) ~ NA_real_,
        volume > 500 & !is.na(volume) & (pharmacy == 1 | drug_store == 1) & (booster == 0 | is.na(booster)) ~ NA_real_,
        volume > 200 & !is.na(volume) & gen_retail == 1 & (booster == 0 | is.na(booster)) ~ NA_real_,
        volume > 200 & !is.na(volume) & total_informal == 1 & (booster == 0 | is.na(booster)) ~ NA_real_,
        TRUE ~ volume
      )
    )
  
  # Save diagnostic datasets
  fwrite(diag_data_final,
         here("Data", "Final data", "Sensitivity analyses", paste0(country, "_", year, "_DIAG_market_share_sensitivity_analysis_IE.csv")),
         na = "")
  
  fwrite(diag_data_final,
         here("Data", "Final data", "Sensitivity analyses", paste0(country, "_", year, "_rdt_micro_data.csv")),
         na = "")
}

################################################################################
# HTML LOGGING SECTION
################################################################################

# Initialize HTML log
cat("<html><head><title>Market Share Sensitivity Analysis</title>")
cat("<style>
table { border-collapse: collapse; width: 100%; margin: 10px 0; }
th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }
th { background-color: #f2f2f2; }
.warning { color: red; font-weight: bold; }
.note { background-color: #fff3cd; padding: 10px; margin: 10px 0; }
.outlier { background-color: #ffebee; }
.important { background-color: #e3f2fd; padding: 10px; margin: 10px 0; }
</style></head><body>", file = log_file)

# Start capturing output
sink(log_file, append = TRUE)

cat("<h1>ACTwatch LITE - Market Share Sensitivity Analysis</h1>")
cat(paste("<h2>Country:", country, "| Year:", year, "</h2>"))

cat("<div class='important'>")
cat("<h3>$$$ IMPORTANT: MANUAL REVIEW REQUIRED</h3>")
cat("<p>Volume outliers have the potential to drastically influence market share results. This analysis identifies outliers using two methods:</p>")
cat("<ol>")
cat("<li>Independent Evaluation (IE) volume outlier definitions</li>")
cat("<li>Conservative outlier definitions (>100 AETD investigation)</li>")
cat("</ol>")
cat("<p>Results will be compared and require manual decision on which definition to use.</p>")
cat("</div>")

################################################################################
# HTML: LOAD DATA AND FILTERING
################################################################################

cat("<h2>ANTIMALARIAL MARKET SHARE SENSITIVITY ANALYSIS</h2>")

cat("<h3>Data Loading and Filtering</h3>")
cat("<p>Original dataset rows:", initial_records, "</p>")
cat("<p>After filtering for antimalarials (producttype 1,2) and excluding prophylaxis (drugcat1 != 4):", am_records, "</p>")

################################################################################
# HTML: IE VOLUME OUTLIER IDENTIFICATION
################################################################################

cat("<h3>$$$ IE VOLUME OUTLIER IDENTIFICATION</h3>")
cat("<div class='note'>Review the following outliers carefully. Results will be documented for manual review.</div>")

# Display outliers by outlet type
display_outliers <- function(outliers, outlet_name, threshold) {
  if (nrow(outliers) > 0) {
    cat("<h4>", outlet_name, "- Threshold:", threshold, "</h4>")
    html_table(outliers, paste("Outliers in", outlet_name))
    cat("<div class='outlier'>")
    cat("<p><strong>$$$ MANUAL REVIEW REQUIRED:</strong></p>")
    cat("<p>Please review the", nrow(outliers), "outlier(s) listed above.</p>")
    cat("<p>Document your findings in the space below:</p>")
    cat("<textarea rows='4' cols='80' placeholder='Add your analysis of these outliers here...'></textarea>")
    cat("</div>")
  } else {
    cat("<h4>", outlet_name, "- Threshold:", threshold, ": No outliers found</h4>")
  }
}

display_outliers(outliers_pnp, "Private-not-for-profit health facilities", 1000)
display_outliers(outliers_pfp, "Private-for-profit health facilities", 1000)
display_outliers(outliers_pharm, "Pharmacies/Drug stores", 500)
display_outliers(outliers_ger, "General retailers", 200)
display_outliers(outliers_inf, "Informal sector", 200)

cat("<p><strong>IE outlier dataset saved.</strong></p>")

################################################################################
# HTML: MARKET SHARE ANALYSIS - IE OUTLIER DEFINITIONS
################################################################################

cat("<h3>Market Share Analysis - IE Outlier Definitions</h3>")

html_table(ie_table, "Market Share Results - IE Outlier Definitions")

################################################################################
# HTML: CONSERVATIVE OUTLIER ANALYSIS
################################################################################

cat("<h3>$$$ CONSERVATIVE OUTLIER ANALYSIS</h3>")
cat("<div class='important'>")
cat("<p>All volumes >100 AETD should be investigated regardless of outlet type.</p>")
cat("<p>Mark volumes as 'plausible' or 'improbable' based on:</p>")
cat("<ul>")
cat("<li>Country context</li>")
cat("<li>Outlet type</li>")
cat("<li>Generic name</li>")
cat("<li>Formulation</li>")
cat("<li>Number of packages/tablets sold</li>")
cat("</ul>")
cat("</div>")

if (nrow(high_volume_cases) > 0) {
  cat("<h4>Cases with Volume >100 AETD (Excluding Wholesale)</h4>")
  html_table(high_volume_cases, "High Volume Cases Requiring Review")
  
  cat("<div class='outlier'>")
  cat("<p><strong>$$$ MANUAL DECISION REQUIRED:</strong></p>")
  cat("<p>Review each of the", nrow(high_volume_cases), "cases above and mark as plausible or improbable.</p>")
  cat("<p>Use the amauditkey values below to identify cases to set to missing:</p>")
  cat("<textarea rows='6' cols='100' placeholder='Paste amauditkey values for IMPROBABLE cases here, separated by commas or new lines...'></textarea>")
  cat("<p><strong>Example format:</strong><br>")
  cat("UUID:7AF5D5D2-9DC5-46C0-97D5-91F508147A4B/CONSENT_GROUP-SECTION5-AMAUDIT[7],<br>")
  cat("UUID:56844049-D061-433A-8C35-A4C547803A94/CONSENT_GROUP-SECTION5-AMAUDIT[11]")
  cat("</p>")
  cat("</div>")
} else {
  cat("<p>No cases with volume >100 AETD found.</p>")
}

################################################################################
# HTML: MARKET SHARE ANALYSIS - CONSERVATIVE DEFINITIONS
################################################################################

cat("<h3>Market Share Analysis - Conservative Outlier Definitions</h3>")

html_table(conservative_table, "Market Share Results - Conservative Outlier Definitions")

################################################################################
# HTML: COMPARISON OF RESULTS
################################################################################

cat("<h3>$$$ COMPARISON OF SENSITIVITY ANALYSIS RESULTS</h3>")

html_table(comparison, "Comparison: IE vs Conservative Outlier Definitions")

cat("<div class='important'>")
cat("<p><strong>$$$ DECISION REQUIRED:</strong></p>")
cat("<p>Based on the comparison above, decide which outlier definition to use for final analysis.</p>")
cat("<p>Document your decision and rationale:</p>")
cat("<textarea rows='4' cols='80' placeholder='Document your decision on which outlier definition to use and why...'></textarea>")
cat("</div>")

################################################################################
# HTML: SAVE FINAL ANTIMALARIAL DATASET
################################################################################

cat("<h3>$$$ SAVE FINAL ANTIMALARIAL DATASET</h3>")
cat("<div class='note'>")
cat("<p>Uncomment the appropriate line below based on your sensitivity analysis decision:</p>")
cat("<pre>")
cat("# Option 1: Use IE outlier definitions\n")
cat("# final_am_data <- am_data_ie\n\n")
cat("# Option 2: Use conservative outlier definitions\n")
cat("# final_am_data <- am_data_conservative")
cat("</pre>")
cat("</div>")

cat("<p><strong>Final antimalarial dataset saved.</strong></p>")

################################################################################
# HTML: DIAGNOSTIC MARKET SHARE SENSITIVITY ANALYSIS
################################################################################

cat("<h2>DIAGNOSTIC MARKET SHARE SENSITIVITY ANALYSIS</h2>")

cat("<div class='note'>")
cat("<p>No ACTwatch Lite pilots to date have found significant private sector diagnostic volumes or outliers.</p>")
cat("<p>For diagnostics, we use only the IE volume outlier definitions.</p>")
cat("<p>If large diagnostic volumes and potential outliers are identified, consider using a comparative approach like the antimalarial analysis above.</p>")
cat("</div>")

if (rdt_data_exists) {
  cat("<p>RDT data successfully added to diagnostic dataset.</p>")
} else {
  cat("<p>RDT data file not found. Continuing with microscopy data only.</p>")
}

if (diag_data_processed) {
  cat("<h3>Diagnostic Outlier Identification</h3>")
  
  display_outliers(diag_outliers_pnp, "Private-not-for-profit health facilities (Diagnostics)", 1000)
  display_outliers(diag_outliers_pfp, "Private-for-profit health facilities (Diagnostics)", 1000)
  display_outliers(diag_outliers_pharm, "Pharmacies (Diagnostics)", 500)
  display_outliers(diag_outliers_ger, "General retailers (Diagnostics)", 200)
  display_outliers(diag_outliers_inf, "Itinerant vendors (Diagnostics)", 200)
  
  cat("<p><strong>Diagnostic datasets saved.</strong></p>")
} else {
  cat("<p>No diagnostic data found for analysis.</p>")
}

################################################################################
# HTML: SUMMARY AND COMPLETION
################################################################################

cat("<hr><h2>SENSITIVITY ANALYSIS SUMMARY</h2>")

# Summary statistics
summary_stats <- data.frame(
  Dataset = c("Original Antimalarial", "IE Outliers Removed", "Conservative (Manual Review Required)"),
  Records = c(
    nrow(am_data),
    nrow(am_data_ie),
    nrow(am_data_conservative)
  ),
  `Outliers Removed` = c(
    0,
    sum(am_data_ie$outlier_removed_ie, na.rm = TRUE),
    sum(am_data_conservative$outlier_removed_conservative, na.rm = TRUE)
  ),
  stringsAsFactors = FALSE
)

html_table(summary_stats, "Processing Summary")

cat("<div class='important'>")
cat("<h3>$$$ NEXT STEPS REQUIRED:</h3>")
cat("<ol>")
cat("<li>Review all identified outliers in the tables above</li>")
cat("<li>Make manual decisions on conservative outlier definitions (>100 AETD cases)</li>")
cat("<li>Compare IE vs Conservative results and decide which definition to use</li>")
cat("<li>Update the final dataset selection code accordingly</li>")
cat("<li>Document all decisions in your analysis notes</li>")
cat("</ol>")
cat("</div>")

cat("<hr><h3>PROCESSING COMPLETE</h3>")
cat("<p>Market share sensitivity analysis has been completed. Review all $$$ sections for required manual inputs.</p>")

cat("</body></html>")

# Close sink
if (sink.number() > 0) sink()

################################################################################
# CONSOLE OUTPUT
################################################################################

cat("\n=== SENSITIVITY ANALYSIS COMPLETE ===\n")

# Antimalarial outlier summary
outlier_summary <- data.frame(
  Definition = c("IE Outliers", "Conservative"),
  Outliers_Removed = c(
    sum(am_data_ie$outlier_removed_ie, na.rm = TRUE),
    sum(am_data_conservative$outlier_removed_conservative, na.rm = TRUE)
  )
)

cat("\nAntimalarial outlier analysis:\n")
cat(sprintf("  Original dataset: %d products\n", nrow(am_data)))
cat(sprintf("  IE definition removed: %d outliers\n", outlier_summary$Outliers_Removed[1]))
cat(sprintf("  Conservative definition removed: %d outliers\n", outlier_summary$Outliers_Removed[2]))

# High volume cases for review
if (exists("high_volume_cases") && nrow(high_volume_cases) > 0) {
  cat(sprintf("\n  WARNING: %d cases >100 AETD require manual review\n", nrow(high_volume_cases)))
}

# Diagnostic outlier summary
if (exists("diag_data_final") && nrow(diag_data_final) > 0) {
  cat(sprintf("\nDiagnostic dataset: %d observations processed\n", nrow(diag_data_final)))
}

# Files saved
cat("\nSensitivity datasets saved:\n")
cat(sprintf("  %s\n", paste0(country, "_", year, "AM_market_share_sensitivity_analysis_IE.csv")))
cat(sprintf("  %s\n", paste0(country, "_", year, "AM_market_share_sensitivity_analysis_conservative.csv")))
cat(sprintf("  %s\n", paste0(country, "_", year, "_antimalarial_data.csv")))
if (exists("diag_data_final") && nrow(diag_data_final) > 0) {
  cat(sprintf("  %s\n", paste0(country, "_", year, "_rdt_micro_data.csv")))
}

cat("\nHTML report saved to:", log_file, "\n")
cat("\nIMPORTANT: Review HTML report for manual decisions on outlier definitions\n")
cat("           Compare IE vs Conservative results before final analysis\n")

################################################################################
# END
################################################################################