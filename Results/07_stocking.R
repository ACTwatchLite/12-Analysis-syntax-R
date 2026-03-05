################################################################################
# ACTwatch LITE 
# Step 2.7 Generate Availability (Stocking) Variables
# REORGANIZED VERSION - HTML logging at end
################################################################################
#
# This script generates outlet-level availability indicators by aggregating
# product-level data to determine whether outlets stock specific antimalarial
# and diagnostic categories. Binary stocking variables are created for each
# major product type (e.g., st_anyACT, st_qaact, st_flact for antimalarials;
# st_test, st_rdt, st_micro for diagnostics) indicating whether at least one
# product of that type was available at the outlet during the survey. The
# script also generates composite indicators combining antimalarials with
# diagnostics (st_flactTest), stockout variables tracking products usually
# stocked but unavailable on survey day, and MFT readiness indicators assessing
# whether outlets stock multiple ACT types. These outlet-level stocking variables
# serve as key denominators and outcomes in availability analyses throughout
# the results tables, enabling assessment of market readiness and access to
# essential malaria commodities.
#
# The following flags are used throughout the syntax:
# $$$ = Breaks the script to remind analyst to modify syntax
# # EXAMPLE: = Sample syntax from pilot studies for reference

################################################################################
# SECTION 1: LOAD DATA
################################################################################

long_data <- fread(here("Data", "Management data", paste0(country, "_", year, "_am_rdt_os_cleaned_long_wt_amcat.csv"))) %>%
  mutate(across(where(is.character), ~ str_trim(str_to_upper(.))))

# SAVE FOR LOGGING (initial count before processing)
initial_records <- nrow(long_data)

# Create gname1 for reference (matching Stata)
long_data <- long_data %>%
  mutate(gname1 = gname)

################################################################################
# SECTION 2: DIAGNOSTIC STOCKING VARIABLES
################################################################################

long_data_processed <- long_data %>%
  mutate(
    # Any malaria test availability (individual level)
    st_test_temp = case_when(
      d3 == 1 | rdt_true == 1 ~ 1,
      d3 == 0 & rdt_true == 0 ~ 0,
      TRUE ~ NA_real_
    ))

long_data_processed <- long_data_processed %>%
  mutate(
    st_s1_test1 = case_when(
      # Explicitly handle missing values like Stata does
      (is.na(d3) | d3 != 1) & (is.na(rdt_true) | rdt_true != 1) & screened == 1 ~ 0,
      d3 == 1 | rdt_true == 1 ~ 1,
      TRUE ~ NA_real_
    )
  )

long_data_processed = long_data_processed %>% 
  # Generate outlet-level stocking variables using group_by and summarise
  group_by(outletid) %>%
  mutate(
    # Diagnostic stocking variables (outlet level)
    st_test = as.numeric(any(st_test_temp == 1, na.rm = TRUE)),
    st_micro = as.numeric(any(d3 == 1, na.rm = TRUE)),
    st_rdt = as.numeric(any(rdt_true == 1, na.rm = TRUE)),
    st_qardt = as.numeric(any(qardt == 1, na.rm = TRUE)),
    st_s1_test = as.numeric(any(st_s1_test1 == 1, na.rm = TRUE))
  ) %>%
  ungroup() %>%
  # Clean up temporary variables and handle missing values
  mutate(
    st_qardt = ifelse(is.na(d7), NA_real_, st_qardt)
  ) %>%
  select(-st_test_temp)

# Apply value labels
diagnostic_vars <- c("st_test", "st_micro", "st_rdt", "st_qardt", "st_s1_test")
for(var in diagnostic_vars) {
  val_labels(long_data_processed[[var]]) <- c("No" = 0, "Yes" = 1)
}

################################################################################
# SECTION 3: ANTIMALARIAL STOCKING VARIABLES
################################################################################

# Define all binary antimalarial variables to process
# $$$ USER INPUT: Modify this list to match your specific implementation
antimalarial_vars_all <- c(
  "anyAM", "anyACT", "notnatapp", "flact", "qaact", "natapp", "QA_all", "QA_WHO", "QA_NAT", "QA_none", 
  "faact", "naact", "pqaact", "aqaact", "aact_fl", "nonqaact", "nonart", "CQ", "SP", "SPAQ", "QN", 
  "oralQN", "injQN", "nonartoth", "artmono", "oartmono", "noartmono", "AS", "injAS", "recAS", "AR", 
  "injAR", "AE", "injAE", "severe", "sev_fl", "AL", "ASAQ", "APPQ", "DHAPPQ", "ARPPQ", "ALqaact", 
  "ASAQqaact", "DHAPPQqaact", "ALnonqaact", "DHAPPQnonqaact", "ASAQnonqaact", "otherACT", "otherNonart",
  "rdtmanu_1", "rdtmanu_2", "rdtmanu_3", "rdtmanu_other", "rdtmanu_dk"
)

# Filter to only include variables that actually exist in the dataset
antimalarial_vars <- antimalarial_vars_all[antimalarial_vars_all %in% names(long_data_processed)]

# SAVE FOR LOGGING (need to report which vars were found/missing)
antimalarial_vars_found <- length(antimalarial_vars)
antimalarial_vars_missing <- setdiff(antimalarial_vars_all, antimalarial_vars)

# Generate stocking variables for all antimalarial categories
long_data_processed <- long_data_processed %>%
  group_by(outletid) %>%
  mutate(
    across(
      all_of(antimalarial_vars),
      ~ as.numeric(any(.x == 1, na.rm = TRUE)),
      .names = "st_{.col}"
    )
  ) %>%
  ungroup()

################################################################################
# SECTION 4: COMPOSITE INDICATORS
################################################################################

long_data_processed <- long_data_processed %>%
  mutate(
    # Stocks first-line ACT and testing
    st_flactTest = case_when(
      screened == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    st_flactTest = case_when(
      faact == 1 & st_test == 1 ~ 1,
      TRUE ~ st_flactTest
    ),
    
    # Stocks first-line ACT and NOT testing
    st_flactNoTest = case_when(
      screened == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    st_flactNoTest = case_when(
      faact == 1 & st_test != 1 ~ 1,
      TRUE ~ st_flactNoTest
    )
  )

################################################################################
# SECTION 5: STOCKOUT VARIABLES
################################################################################

long_data_processed <- long_data_processed %>%
  mutate(
    # All ACTs stockout
    so_ACT = case_when(
      screened == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    so_ACT = case_when(
      a17_5 == 1 & st_anyACT == 0 ~ 1,
      TRUE ~ so_ACT
    ),
    
    # AL stockout
    so_AL = case_when(
      screened == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    so_AL = case_when(
      a17_1 == 1 & st_AL == 0 ~ 1,
      TRUE ~ so_AL
    ),
    
    # ASAQ stockout
    so_ASAQ = case_when(
      screened == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    so_ASAQ = case_when(
      a17_2 == 1 & st_ASAQ == 0 ~ 1,
      TRUE ~ so_ASAQ
    ),
    
    # DHAPPQ stockout
    so_DHAPPQ = case_when(
      screened == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    so_DHAPPQ = case_when(
      a17_3 == 1 & st_DHAPPQ == 0 ~ 1,
      TRUE ~ so_DHAPPQ
    ),
    
    # Artemether stockout
    so_AR = case_when(
      screened == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    so_AR = case_when(
      a17_6 == 1 & st_AR == 0 ~ 1,
      TRUE ~ so_AR
    ),
    
    # Artesunate stockout
    so_AS = case_when(
      screened == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    so_AS = case_when(
      a17_8 == 1 & st_AS == 0 ~ 1,
      TRUE ~ so_AS
    ),
    
    # Chloroquine stockout
    so_CQ = case_when(
      screened == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    so_CQ = case_when(
      a17_10 == 1 & st_CQ == 0 ~ 1,
      TRUE ~ so_CQ
    ),
    
    # Quinine stockout
    so_QN = case_when(
      screened == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    so_QN = case_when(
      a17_11 == 1 & st_QN == 0 ~ 1,
      TRUE ~ so_QN
    ),
    
    # SP stockout
    so_SP = case_when(
      screened == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    so_SP = case_when(
      a17_12 == 1 & st_SP == 0 ~ 1,
      TRUE ~ so_SP
    ),
    
    # RDT stockout
    so_rdt = case_when(
      (rdt_stock == 1 | micro_stockcurrent == 1 | s6 == 1) ~ 0,
      TRUE ~ NA_real_
    ),
    so_rdt = case_when(
      d16 == 1 ~ 1,
      TRUE ~ so_rdt
    )
  )

################################################################################
# SECTION 6: COMPOSITE AVAILABILITY VARIABLES
################################################################################

long_data_processed <- long_data_processed %>%
  mutate(
    # Any CQ (includes CQ packaged alone and co-packaged with primaquine)
    anyCQ = case_when(
      drugcat1 < 4 ~ 0,
      TRUE ~ NA_real_
    ),
    anyCQ = case_when(
      CQ == 1 ~ 1,  # | CQPRI==1 (commented out in Stata)
      TRUE ~ anyCQ
    ),
    
    # Any AL (includes AL packaged alone and co-packaged with primaquine)
    anyAL = case_when(
      drugcat1 < 4 ~ 0,
      TRUE ~ NA_real_
    ),
    anyAL = case_when(
      AL == 1 ~ 1,  # | ALPRI==1 (commented out in Stata)
      TRUE ~ anyAL
    )
  ) %>%
  group_by(outletid) %>%
  mutate(
    # Outlet-level stocking variables for composite categories
    st_anyCQ = as.numeric(any(anyCQ == 1, na.rm = TRUE)),
    st_anyAL = as.numeric(any(anyAL == 1, na.rm = TRUE)),
    
    # Oral artemisinin monotherapy stockout (past 3 months but not currently)
    st_oartmonoso = as.numeric(any(oartmono == 1, na.rm = TRUE))
  ) %>%
  ungroup() %>%
  mutate(
    # Adjust st_oartmonoso logic
    st_oartmonoso = case_when(
      st_oartmono == 1 ~ 0,
      is.na(st_oartmonoso) ~ 0,
      TRUE ~ st_oartmonoso
    )
  )

################################################################################
# SECTION 7: MFT READINESS INDICATORS
################################################################################

# Count of ACT types stocked at facility for MFT readiness indicator
long_data_processed <- long_data_processed %>%
  mutate(
    # Count ACT types stocked
    mft_act_count = rowSums(select(., st_AL, st_ASAQ, st_APPQ, st_DHAPPQ, st_ARPPQ, st_otherACT), na.rm = TRUE),
    
    # Stocks 2 or more ACTs (MFT readiness)
    st_2acts = case_when(
      mft_act_count > 1 & !is.na(mft_act_count) ~ 1,
      TRUE ~ 0
    )
  )

################################################################################
# SECTION 8: SAVE PROCESSED DATA
################################################################################

output_file <- here("Data", "Management data", paste0(country, "_", year, "_am_rdt_os_cleaned_long_wt_stocking.csv"))
fwrite(long_data_processed, output_file, row.names = FALSE, na = "")

################################################################################
# GENERATE HTML LOG
################################################################################

# Setup log file
log_dir <- here("Data", "Management data", "Data cleaning notes")
if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE)
log_file <- file.path(log_dir, paste0("AwL_", country, "_", year, "_stocking_notes.html"))

# Initialize sink to write HTML output to file
sink(log_file, type = "output")

cat("<html><head><title>Stocking Variables Analysis</title>")
cat("<style>
table { border-collapse: collapse; width: 100%; margin: 10px 0; }
th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }
th { background-color: #f2f2f2; }
.warning { color: red; font-weight: bold; }
.note { background-color: #fff3cd; padding: 10px; margin: 10px 0; }
</style></head><body>")

cat("<h1>ACTwatch LITE - Stocking Variables</h1>")
cat(paste("<h2>Country:", country, "| Year:", year, "</h2>"))

# =============================================================================
# HTML CONTENT GENERATION - All calculated from final data
# =============================================================================

cat("<h3>Data loaded successfully</h3>")
cat(paste("<p>Total observations:", initial_records, "</p>"))

cat("<h3>GENERATING DIAGNOSTIC STOCKING VARIABLES</h3>")

cat("<h3>GENERATING ANTIMALARIAL STOCKING VARIABLES</h3>")

cat("<div class='note'>USER INPUT REQUIRED ($$$ in Stata): The antimalarial types should be edited to match the context as needed. Any changes made to the types must be reflected in the analysis syntax files.</div>")

cat("Variables found:", antimalarial_vars_found, "out of", length(antimalarial_vars_all), "\n")
cat("Missing variables:", paste(antimalarial_vars_missing, collapse = ", "), "\n")

cat("<h3>GENERATING COMPOSITE INDICATORS</h3>")

cat("<h3>GENERATING STOCKOUT VARIABLES</h3>")

cat("<div class='note'>USER INPUT REQUIRED ($$$ in Stata): Ensure a17_ variables match the definitions here, or modify the syntax.</div>")

cat("<h3>GENERATING COMPOSITE AVAILABILITY VARIABLES</h3>")

cat("<h3>GENERATING MFT READINESS INDICATORS</h3>")

cat("<h3>SAVING PROCESSED DATA</h3>")

cat("<hr><h3>PROCESSING COMPLETE</h3>")
cat("<p>Stocking variables have been successfully generated.</p>")
cat("<p>Key areas flagged for review ($$$ in original Stata):</p>")
cat("<ul>")
cat("<li>Antimalarial types list - should be edited to match context</li>")
cat("<li>a17_ variable definitions - ensure they match your survey</li>")
cat("</ul>")

cat("</body></html>")
if (sink.number() > 0) sink()

################################################################################
# CONSOLE OUTPUT
################################################################################

cat("\n=== STOCKING VARIABLES PROCESSING COMPLETE ===\n")
cat("Records processed:", nrow(long_data_processed), "\n")

# Count unique outlets
outlet_count <- n_distinct(long_data_processed$outletid)
cat("Unique outlets:", outlet_count, "\n")

# Key stocking indicators
stocking_summary <- long_data_processed %>%
  filter(nOut == 1) %>%  # One record per outlet
  summarise(
    outlets = n(),
    any_am = sum(st_anyAM == 1, na.rm = TRUE),
    any_act = sum(st_anyACT == 1, na.rm = TRUE),
    qaact = sum(st_qaact == 1, na.rm = TRUE),
    first_line = sum(st_flact == 1, na.rm = TRUE),
    any_test = sum(st_test == 1, na.rm = TRUE),
    rdt = sum(st_rdt == 1, na.rm = TRUE)
  )

cat("\nStocking indicators (outlet-level):\n")
cat(sprintf("  Any antimalarial: %d (%.1f%%)\n", 
            stocking_summary$any_am, 
            stocking_summary$any_am/stocking_summary$outlets*100))
cat(sprintf("  Any ACT: %d (%.1f%%)\n", 
            stocking_summary$any_act, 
            stocking_summary$any_act/stocking_summary$outlets*100))
cat(sprintf("  QAACT: %d (%.1f%%)\n", 
            stocking_summary$qaact, 
            stocking_summary$qaact/stocking_summary$outlets*100))
cat(sprintf("  First-line ACT: %d (%.1f%%)\n", 
            stocking_summary$first_line, 
            stocking_summary$first_line/stocking_summary$outlets*100))
cat(sprintf("  Any blood test: %d (%.1f%%)\n", 
            stocking_summary$any_test, 
            stocking_summary$any_test/stocking_summary$outlets*100))
cat(sprintf("  RDT: %d (%.1f%%)\n", 
            stocking_summary$rdt, 
            stocking_summary$rdt/stocking_summary$outlets*100))

# Count variables generated
var_count <- length(grep("^st_", names(long_data_processed)))
cat("\nVariables generated:", var_count, "stocking indicator variables\n")

cat("\nOutput files:\n")
cat("  Dataset:", output_file, "\n")
cat("  HTML log:", log_file, "\n")

cat("\nNOTE: Review $$$ flagged areas in HTML report for context-specific adjustments\n")

################################################################################
# END
################################################################################