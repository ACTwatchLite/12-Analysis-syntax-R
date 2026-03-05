################################################################################
# ACTwatch LITE 
# Step 2.11 Generate provider interview variables
################################################################################

# SCRIPT PURPOSE:
# Generates provider-level indicators from provider interviews covering knowledge,
# practices, training, infrastructure, and regulatory compliance.
#
# MAIN VARIABLE GROUPS:
# 1. Clinical knowledge: effectiveAM (identifies ACT as most effective)
# 2. Testing practices: treatNegTest variables (would treat negative test)
# 3. Product storage: Storage condition indicators
# 4. Health qualifications: Hq, healthqual (qualified staff presence)
# 5. Training: ppm_dx, ppm_rx, ppm_sx, ppm_mx, ppm_any (malaria training)
# 6. RDT knowledge: p21_1, p22_1 (RDT awareness and usage)
# 7. Price stability: sa13_*, st13_* (price change patterns)
# 8. Surveillance: dat_mr, dat_gov, dat_dhis, dat_ngo, dat_sv, dat_sl, dat_st
# 9. Licensing: reg_ab, reg_mh, reg_gi (registration status)
#
# USER INPUTS REQUIRED:
# $$$ ACT effectiveness codes: Update p16_name values that count as correct answers
# $$$ Top 3 reasons for treating negative tests: Update treatNegTest_1/2/3 based on p24 results
# $$$ Health qualification categories: Modify Hq definition for local qualifications
# $$$ Training categories: Adjust char9 variable numbers if needed
# $$$ Registration variables: Adapt to local licensing requirements
#
# FLAG LEGEND:
# $$$ = User input required
# → SAVE = Variable saved for use in HTML section

################################################################################
# SETUP
################################################################################

# Create logs directory 
log_dir <- here("Data", "Management data", "Data cleaning notes")
if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE)

log_file <- file.path(log_dir, paste0("AwL_", country, "_", year, "_provider_notes.html"))

################################################################################
# SECTION 1: LOAD DATA
################################################################################

# Load the cleaned data
data_file <- here("Data", "Management data", paste0(country, "_", year, "_am_rdt_os_cleaned_long_wt_volume.csv"))
long_data <- fread(data_file)

# → SAVE: Initial record count for HTML summary
initial_records <- nrow(long_data)

################################################################################
# SECTION 2: MOST EFFECTIVE REPORTED ANTIMALARIAL
################################################################################

# $$$ USER INPUT REQUIRED - ACT effectiveness codes
# Correct responses for ACTs (update if needed):
# 1 - Artemether-lumefantrine
# 2 - Artesunate-amodiaquine
# 3 - Dihydroartemisinine-piperaquine
# 4 - Artesunate-SP
# 5 - ACT (artemisinin combination therapies)

# → SAVE: Cross-tabulation for HTML review
p16_crosstab <- long_data %>%
  filter(nOut == 1) %>%
  count(p16_name, p16_form) %>%
  arrange(p16_name, p16_form)

# Generate effectiveAM variable
long_data <- long_data %>%
  mutate(
    effectiveAM = case_when(
      !is.na(p16_name) & p16_name %in% c(1, 2, 3, 4, 5) & p16_form == 1 ~ 1,
      !is.na(p16_name) ~ 0,
      TRUE ~ NA_real_
    )
  )

################################################################################
# SECTION 3: PROVIDER WOULD RECOMMEND ANTIMALARIAL AFTER NEGATIVE TEST
################################################################################

# → SAVE: P23 tabulation for HTML
p23_tab <- long_data %>%
  count(p23) %>%
  mutate(percentage = round(n / sum(n) * 100, 1))

# Generate treatment variables
long_data <- long_data %>%
  mutate(
    treatNegTest = case_when(
      p23 == 98 ~ NA_real_,
      TRUE ~ p23
    ),
    # Generate dummy variables
    treatNegTestSome = case_when(
      treatNegTest == 1 ~ 1,
      treatNegTest %in% c(2, 3) ~ 0,
      TRUE ~ NA_real_
    ),
    treatNegTestAll = case_when(
      treatNegTest == 2 ~ 1,
      treatNegTest %in% c(1, 3) ~ 0,
      TRUE ~ NA_real_
    ),
    treatNegTestNev = case_when(
      treatNegTest == 3 ~ 1,
      treatNegTest %in% c(1, 2) ~ 0,
      TRUE ~ NA_real_
    ),
    # Generate denominator variable
    denomNegTest = case_when(
      p23 %in% c(1, 2) ~ 1,
      TRUE ~ NA_real_
    )
  )

################################################################################
# SECTION 4: REASONS FOR TREATING NEGATIVE TEST
################################################################################

# $$$ USER INPUT REQUIRED - Top 3 reasons
# Based on the results, update the variables below for the top 3 most common reasons

# → SAVE: P24 tabulations for HTML review
p24_tab <- long_data %>%
  filter(!is.na(p24) & p24 != "") %>%
  count(p24, sort = TRUE) %>%
  mutate(percentage = round(n / sum(n) * 100, 1))

p24_var_summary <- long_data %>%
  filter(nOut == 1) %>%
  summarise(
    across(starts_with("p24_") & where(is.numeric), ~ sum(.x == 1, na.rm = TRUE))
  ) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Count") %>%
  arrange(desc(Count))

# Generate reason variables (UPDATE BASED ON YOUR RESULTS)
long_data <- long_data %>%
  mutate(
    # Most common reason
    treatNegTest_1 = case_when(
      !is.na(p24) & p24 != "" & p24_1 == 1 ~ 1,
      !is.na(p24) & p24 != "" ~ 0,
      TRUE ~ NA_real_
    ),
    # 2nd most common reason
    treatNegTest_2 = case_when(
      !is.na(p24) & p24 != "" & p24_2 == 1 ~ 1,
      !is.na(p24) & p24 != "" ~ 0,
      TRUE ~ NA_real_
    ),
    # 3rd most common reason (UPDATE p24_6 based on your results)
    treatNegTest_3 = case_when(
      !is.na(p24) & p24 != "" & p24_6 == 1 ~ 1,
      !is.na(p24) & p24 != "" ~ 0,
      TRUE ~ NA_real_
    ),
    # Other reasons (UPDATE based on which variables to exclude)
    treatNegTest_O = case_when(
      !is.na(p24) & p24 != "" & (p24_3 == 1 | p24_4 == 1 | p24_5 == 1 | p24_7 == 1 | 
                                   (!is.na(p24_other) & p24_other != "")) ~ 1,
      !is.na(p24) & p24 != "" ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  # Apply denominator filter
  mutate(
    across(c(treatNegTest_1, treatNegTest_2, treatNegTest_3, treatNegTest_O),
           ~ case_when(denomNegTest != 1 ~ NA_real_, TRUE ~ .x))
  )

################################################################################
# SECTION 5: PRODUCT STORAGE
################################################################################

# Recode storage variables (98 = Don't know → missing)
long_data <- long_data %>%
  mutate(
    across(starts_with(c("am_storage", "rdt_storage")), ~ case_when(.x == 98 ~ NA_real_, TRUE ~ .x))
  )

################################################################################
# SECTION 6: HEALTH QUALIFICATIONS
################################################################################

# $$$ USER INPUT REQUIRED - Health qualification categories
# Update the qualification numbers below based on your questionnaire:
# 1-Pharmacist, 2-Doctor, 3-Nurse, 4-Midwife, 5-Lab tech, 6-Pharm tech, etc.

# → SAVE: P8 variables summary for HTML
p8_summary <- long_data %>%
  filter(nOut == 1) %>%
  summarise(
    across(starts_with("p8_") & where(is.numeric), ~ sum(.x == 1, na.rm = TRUE))
  ) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Count")

# Generate health qualification variables
long_data <- long_data %>%
  mutate(
    # Update these numbers based on your questionnaire
    Hq = case_when(
      !is.na(p8) & (p8_1 == 1 | p8_2 == 1 | p8_3 == 1 | p8_4 == 1 | p8_6 == 1 | p8_7 == 1 | p8_9 == 1 | p8_10 == 1) ~ 1,
      !is.na(p8) ~ 0,
      TRUE ~ NA_real_
    ),
    healthqual = case_when(
      p8_1 == 1 | p8_2 == 1 | p8_3 == 1 | p8_4 == 1 | p8_5 == 1 | p8_6 == 1 ~ 1,
      TRUE ~ 0
    )
  )

################################################################################
# SECTION 7: OUTLET CHARACTERISTICS
################################################################################

# 1 person works at outlet
long_data <- long_data %>%
  mutate(
    char4_1 = case_when(
      char4 == 1 ~ 1,
      TRUE ~ 0
    )
  )

################################################################################
# SECTION 8: CASE MANAGEMENT TRAINING
################################################################################

# $$$ USER INPUT REQUIRED - Training variable names may need modification
# Training categories: 1-Diagnosis, 2-Treatment, 3-Surveillance, 96-Other, 99-None, 98-Don't know

# → SAVE: char9_other responses for HTML review
char9_other <- long_data %>%
  filter(nOut == 1, !is.na(char9_other) & char9_other != "") %>%
  count(char9_other)

# Generate training variables
long_data <- long_data %>%
  mutate(
    # Malaria diagnosis training
    ppm_dx = case_when(
      !is.na(char9) & char9 != "" & char9_1 == 1 ~ 1,
      !is.na(char9) & char9 != "" ~ 0,
      TRUE ~ NA_real_
    ),
    # Malaria treatment training  
    ppm_rx = case_when(
      !is.na(char9) & char9 != "" & char9_2 == 1 ~ 1,
      !is.na(char9) & char9 != "" ~ 0,
      TRUE ~ NA_real_
    ),
    # Malaria surveillance training
    ppm_sx = case_when(
      !is.na(char9) & char9 != "" & char9_3 == 1 ~ 1,
      !is.na(char9) & char9 != "" ~ 0,
      TRUE ~ NA_real_
    ),
    # All three types of training
    ppm_mx = case_when(
      !is.na(char9) & char9 != "" & ppm_dx == 1 & ppm_rx == 1 & ppm_sx == 1 ~ 1,
      !is.na(char9) & char9 != "" ~ 0,
      TRUE ~ NA_real_
    ),
    # Any malaria training
    ppm_any = case_when(
      ppm_dx == 1 | ppm_rx == 1 | ppm_sx == 1 | ppm_mx == 1 ~ 1,
      !is.na(char9) & char9 != "" ~ 0,
      TRUE ~ NA_real_
    )
  )

################################################################################
# SECTION 9: RDT KNOWLEDGE AND DIGITAL/PRICE VARIABLES
################################################################################

# RDT knowledge and use
long_data <- long_data %>%
  mutate(
    p21_1 = case_when(
      p21 == 98 ~ 0,
      p21 == 99 ~ NA_real_,
      TRUE ~ p21
    ),
    p22_1 = case_when(
      p22 == 98 ~ 0,
      p22 == 99 ~ NA_real_,
      TRUE ~ p22
    ),
    # Online retail
    retonline = case_when(
      retonline == 98 ~ 0,
      retonline %in% c(99, 97) ~ NA_real_,
      TRUE ~ retonline
    )
  )

# Price stability - Antimalarials
long_data <- long_data %>%
  mutate(
    across(starts_with("sa2_"), ~ case_when(.x == 98 ~ NA_real_, TRUE ~ .x)),
    sa6 = case_when(
      sa6 == 98 ~ 0,
      sa6 %in% c(99, 97) ~ NA_real_,
      TRUE ~ sa6
    ),
    sa13_1 = case_when(sa13 == 1 ~ 1, sa13 %in% 2:98 ~ 0, TRUE ~ NA_real_),
    sa13_2 = case_when(sa13 %in% c(4, 5, 6, 7) ~ 1, sa13 %in% c(1, 2, 3, 98) ~ 0, TRUE ~ NA_real_),
    sa13_3 = case_when(sa13 %in% c(2, 3) ~ 1, sa13 %in% c(4, 5, 6, 7, 1, 98) ~ 0, TRUE ~ NA_real_)
  )

# Price stability - RDTs
long_data <- long_data %>%
  mutate(
    d1 = case_when(d1 == 98 ~ 0, d1 == 99 ~ NA_real_, TRUE ~ d1),
    d2 = case_when(d2 == 98 ~ 0, d2 == 99 ~ NA_real_, TRUE ~ d2),
    across(starts_with("st2_"), ~ case_when(.x == 98 ~ NA_real_, TRUE ~ .x)),
    st6 = case_when(
      st6 == 98 ~ 0,
      st6 %in% c(99, 97) ~ NA_real_,
      TRUE ~ st6
    ),
    st13_1 = case_when(st13 == 1 ~ 1, st13 %in% 2:98 ~ 0, TRUE ~ NA_real_),
    st13_2 = case_when(st13 %in% c(4, 5, 6, 7) ~ 1, st13 %in% c(1, 2, 3, 98) ~ 0, TRUE ~ NA_real_),
    st13_3 = case_when(st13 %in% c(2, 3) ~ 1, st13 %in% c(4, 5, 6, 7, 1, 98) ~ 0, TRUE ~ NA_real_)
  )

################################################################################
# SECTION 10: CASE REPORTING/SURVEILLANCE
################################################################################

# Digital and data variables
long_data <- long_data %>%
  mutate(
    # Digital variables
    across(c(dig0, dig1, dig2, dig3, dig5), ~ case_when(.x %in% c(98, 99, 2) ~ 0, TRUE ~ .x)),
    # Data reporting
    data4 = case_when(data4 == 98 ~ 0, data4 == 99 ~ NA_real_, TRUE ~ data4),
    # Monthly reporting
    dat_mr = case_when(
      !is.na(eligible) & data1 == 1 ~ 1,
      !is.na(eligible) ~ 0,
      TRUE ~ NA_real_
    ),
    # Reporting channels
    dat_gov = case_when(
      dat_mr == 1 & data2_1 == 1 ~ 1,
      dat_mr == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    dat_dhis = case_when(
      dat_mr == 1 & data2_2 == 1 ~ 1,
      dat_mr == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    dat_ngo = case_when(
      dat_mr == 1 & data2_3 == 1 ~ 1,
      dat_mr == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    # Supervisory visits
    dat_sv = case_when(
      !is.na(eligible) & !data3 %in% c(0, 98, 5) ~ 1,
      !is.na(eligible) ~ 0,
      TRUE ~ NA_real_
    ),
    # Supervision feedback
    dat_sl = case_when(
      dat_sv == 1 & data3c == 1 ~ 1,
      dat_sv == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    # Surveillance training and checklist
    dat_st = case_when(
      !is.na(eligible) & data4 == 1 ~ 1,
      !is.na(eligible) ~ 0,
      TRUE ~ NA_real_
    )
  )

################################################################################
# SECTION 11: LICENSE AND REGISTRATION
################################################################################

# $$$ USER INPUT REQUIRED - Adapt to local context
# Update registration variables based on your local licensing requirements

# → SAVE: Registration summary for HTML
reg_summary <- long_data %>%
  summarise(
    across(starts_with("reg") & where(is.numeric), ~ sum(.x == 1, na.rm = TRUE))
  ) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Count")

# Generate registration variables (EXAMPLE - ADAPT TO LOCAL CONTEXT)
long_data <- long_data %>%
  mutate(
    # Active license (reported or observed)
    reg_ab = case_when(
      !is.na(eligible) & reg1 %in% c(1, 2) ~ 1,
      !is.na(eligible) ~ 0,
      TRUE ~ NA_real_
    ),
    # Ministry of Health license
    reg_mh = case_when(
      !is.na(eligible) & reg2 %in% c(1, 2) ~ 1,
      !is.na(eligible) ~ 0,
      TRUE ~ NA_real_
    ),
    # Government inspection in past year
    reg_gi = case_when(
      !is.na(eligible) & reg6 == 1 ~ 1,
      !is.na(eligible) ~ 0,
      TRUE ~ NA_real_
    )
  )

################################################################################
# SECTION 12: SAVE PROCESSED DATA
################################################################################

# Save the processed data
output_file <- here("Data", "Management data", paste0(country, "_", year, "_am_rdt_os_cleaned_long_wt_provider.csv"))
fwrite(long_data, output_file, row.names = FALSE, na = "")

################################################################################
# HTML LOGGING SECTION
################################################################################

# Initialize HTML log
cat("<html><head><title>Provider Interview Variables</title>")
cat("<style>
table { border-collapse: collapse; width: 100%; margin: 10px 0; }
th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }
th { background-color: #f2f2f2; }
.warning { color: red; font-weight: bold; }
.note { background-color: #fff3cd; padding: 10px; margin: 10px 0; }
.important { background-color: #f8d7da; padding: 10px; margin: 10px 0; border-left: 5px solid #dc3545; }
</style></head><body>", file = log_file)

# Start capturing output
sink(log_file, append = TRUE)

cat("<h1>ACTwatch LITE - Provider Interview Variables</h1>")
cat(paste("<h2>Country:", country, "| Year:", year, "</h2>"))

################################################################################
# HTML: LOAD DATA
################################################################################

cat("<h3>2.11 PROVIDER INTERVIEW INDICATORS</h3>")
cat("<p>Data loaded successfully. Number of observations:", initial_records, "</p>")

################################################################################
# HTML: MOST EFFECTIVE REPORTED ANTIMALARIAL
################################################################################

cat("<h4>MOST EFFECTIVE REPORTED ANTIMALARIAL</h4>")

cat("<div class='note'>")
cat("<p>Provider reports ACT as most effective treatment for uncomplicated malaria.</p>")
cat("<p><strong>Correct responses include:</strong> any specific ACT, or reference to 'ACT'</p>")
cat("</div>")

html_table(p16_crosstab, "Cross-tabulation of p16_name and p16_form (nOut=1)")

cat("<div class='important'>")
cat("<h4>$$$ MANUAL REVIEW REQUIRED</h4>")
cat("<p>Correct responses for ACTs (update if needed):</p>")
cat("<ul>")
cat("<li>1 - Artemether-lumefantrine</li>")
cat("<li>2 - Artesunate-amodiaquine</li>")
cat("<li>3 - Dihydroartemisinine-piperaquine</li>")
cat("<li>4 - Artesunate-SP</li>")
cat("<li>5 - ACT (artemisinin combination therapies)</li>")
cat("</ul>")
cat("</div>")

# Recalculate summary from final data
effective_summary <- long_data %>%
  filter(nOut == 1) %>%
  count(effectiveAM) %>%
  mutate(percentage = round(n / sum(n) * 100, 1))

html_table(effective_summary, "Names tablet-ACT as most effective AM (nOut=1)")

################################################################################
# HTML: PROVIDER WOULD RECOMMEND ANTIMALARIAL AFTER NEGATIVE TEST
################################################################################

cat("<h4>PROVIDER WOULD RECOMMEND ANTIMALARIAL AFTER NEGATIVE TEST</h4>")

html_table(p23_tab, "p23 - Would recommend antimalarial after negative test")

################################################################################
# HTML: REASONS FOR TREATING NEGATIVE TEST
################################################################################

cat("<h4>REASONS FOR TREATING NEGATIVE TEST</h4>")

html_table(p24_tab, "Reasons for treating negative test (p24)")
html_table(p24_var_summary, "P24 Reason Variables (nOut=1)")

cat("<div class='important'>")
cat("<h4>$$$ UPDATE TOP 3 REASONS</h4>")
cat("<p>Based on the results above, update the variables below for the top 3 most common reasons.</p>")
cat("</div>")

################################################################################
# HTML: PRODUCT STORAGE
################################################################################

cat("<h4>PRODUCT STORAGE</h4>")
cat("<p>Storage variables recoded: 98 (Don't know) set to missing</p>")

################################################################################
# HTML: HEALTH QUALIFICATIONS
################################################################################

cat("<h4>OUTLETS WITH HEALTH QUALIFIED STAFF</h4>")

cat("<div class='important'>")
cat("<h4>$$$ MODIFY HEALTH QUALIFICATION CATEGORIES</h4>")
cat("<p>Update the qualification numbers below based on your questionnaire:</p>")
cat("<ul>")
cat("<li>1-Pharmacist, 2-Doctor, 3-Nurse, 4-Midwife, 5-Lab tech, 6-Pharm tech, etc.</li>")
cat("</ul>")
cat("</div>")

html_table(p8_summary, "Health Qualification Variables (nOut=1)")

################################################################################
# HTML: OUTLET CHARACTERISTICS
################################################################################

cat("<h4>OUTLET CHARACTERISTICS</h4>")
cat("<p>char4_1 variable created: 1 person works at outlet</p>")

################################################################################
# HTML: CASE MANAGEMENT TRAINING
################################################################################

cat("<h4>CASE MANAGEMENT TRAINING</h4>")

cat("<div class='important'>")
cat("<h4>$$$ VARIABLE NAMES MAY NEED MODIFICATION</h4>")
cat("<p>Training categories: 1-Diagnosis, 2-Treatment, 3-Surveillance, 96-Other, 99-None, 98-Don't know</p>")
cat("</div>")

html_table(char9_other, "char9_other responses (nOut=1)")

################################################################################
# HTML: RDT KNOWLEDGE AND DIGITAL/PRICE VARIABLES
################################################################################

cat("<h4>RDT KNOWLEDGE, DIGITAL SERVICES, AND PRICE STABILITY</h4>")
cat("<p>Variables created: p21_1, p22_1 (RDT knowledge), sa13_1/2/3, st13_1/2/3 (price stability)</p>")

################################################################################
# HTML: CASE REPORTING/SURVEILLANCE
################################################################################

cat("<h4>CASE REPORTING/PARTICIPATION IN SURVEILLANCE</h4>")
cat("<p>Variables created: dat_mr, dat_gov, dat_dhis, dat_ngo, dat_sv, dat_sl, dat_st</p>")

################################################################################
# HTML: LICENSE AND REGISTRATION
################################################################################

cat("<h4>LICENSE AND REGISTRATION</h4>")

cat("<div class='important'>")
cat("<h4>$$$ ADAPT TO LOCAL CONTEXT</h4>")
cat("<p>Update registration variables based on your local licensing requirements.</p>")
cat("</div>")

html_table(reg_summary, "Registration Variables Summary")

################################################################################
# HTML: SAVE AND COMPLETION
################################################################################

cat("<h3>SAVING PROCESSED DATA</h3>")
cat("<p>Data saved successfully to:", basename(output_file), "</p>")
cat("<p>Total observations:", nrow(long_data), "</p>")

# Final summary of variables created
variables_created <- c("effectiveAM", "treatNegTest", "treatNegTestSome", "treatNegTestAll", "treatNegTestNev", 
                       "denomNegTest", "treatNegTest_1", "treatNegTest_2", "treatNegTest_3", "treatNegTest_O",
                       "Hq", "healthqual", "char4_1", "ppm_dx", "ppm_rx", "ppm_sx", "ppm_mx", "ppm_any",
                       "p21_1", "p22_1", "sa13_1", "sa13_2", "sa13_3", "st13_1", "st13_2", "st13_3",
                       "dat_mr", "dat_gov", "dat_dhis", "dat_ngo", "dat_sv", "dat_sl", "dat_st",
                       "reg_ab", "reg_mh", "reg_gi")

final_summary <- data.frame(
  Variables_Created = length(variables_created),
  Total_Observations = nrow(long_data),
  stringsAsFactors = FALSE
)

html_table(final_summary, "Processing Summary")

################################################################################
# HTML: FINAL REVIEW
################################################################################

cat("<hr><h3>PROCESSING COMPLETE</h3>")
cat("<p>Provider interview variables have been successfully generated.</p>")

cat("<div class='important'>")
cat("<h4>$$$ FINAL REVIEW REQUIRED:</h4>")
cat("<ol>")
cat("<li>Update ACT effectiveness codes for p16_name</li>")
cat("<li>Identify and update top 3 reasons for treating negative tests</li>")
cat("<li>Modify health qualification categories for your context</li>")
cat("<li>Update training variable categories for char9</li>")
cat("<li>Adapt registration variables to local licensing requirements</li>")
cat("</ol>")
cat("</div>")

cat("</body></html>")

# Close sink
if (sink.number() > 0) sink()

################################################################################
# CONSOLE OUTPUT
################################################################################

cat("Check the HTML log for detailed analysis and review points.\n")

cat("\n=== PROVIDER VARIABLES PROCESSING COMPLETE ===\n")
cat("Records processed:", nrow(long_data), "\n")

# Count outlets (unique)
outlet_count <- long_data %>% filter(nOut == 1) %>% nrow()
cat("Unique outlets:", outlet_count, "\n")

# Key provider indicators
provider_summary <- long_data %>%
  filter(nOut == 1) %>%
  summarise(
    correct_treatment = sum(effectiveAM == 1, na.rm = TRUE),
    health_qualified = sum(Hq == 1, na.rm = TRUE),
    malaria_training = sum(ppm_any == 1, na.rm = TRUE),
    would_treat_neg = sum(treatNegTestSome == 1 | treatNegTestAll == 1, na.rm = TRUE),
    monthly_reporting = sum(dat_mr == 1, na.rm = TRUE),
    active_license = sum(reg_ab == 1, na.rm = TRUE)
  )

cat("\nProvider indicators (outlet-level):\n")
cat(sprintf("  Identifies ACT as most effective: %d (%.1f%%)\n",
            provider_summary$correct_treatment,
            provider_summary$correct_treatment/outlet_count*100))
cat(sprintf("  Health qualified staff: %d (%.1f%%)\n",
            provider_summary$health_qualified,
            provider_summary$health_qualified/outlet_count*100))
cat(sprintf("  Received malaria training: %d (%.1f%%)\n",
            provider_summary$malaria_training,
            provider_summary$malaria_training/outlet_count*100))
cat(sprintf("  Would treat negative test: %d (%.1f%%)\n",
            provider_summary$would_treat_neg,
            provider_summary$would_treat_neg/outlet_count*100))
cat(sprintf("  Monthly case reporting: %d (%.1f%%)\n",
            provider_summary$monthly_reporting,
            provider_summary$monthly_reporting/outlet_count*100))
cat(sprintf("  Active license/registration: %d (%.1f%%)\n",
            provider_summary$active_license,
            provider_summary$active_license/outlet_count*100))

# Count variables generated
cat("\nVariables generated: ~36 provider indicator variables\n")

cat("\nHTML report saved to:", log_file, "\n")
cat("\nIMPORTANT: Review $$$ flagged sections in HTML for context-specific updates\n")

################################################################################
# END
################################################################################