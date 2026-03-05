################################################################################
# ACTwatch LITE - Step 2.3 APPLY DENOMINATORS
# REORGANIZED VERSION - HTML logging at end
################################################################################

# This R file generates key binary variables that define the analysis denominators 
# used throughout the results tables. These variables categorize outlets based on 
# eligibility, screening status, and interview completion, allowing analysts to 
# consistently calculate proportions and disaggregate results by relevant groups.

# The following flags are used throughout the syntax:
# $$$ = Breaks the script to remind analyst to modify syntax
# # EXAMPLE: = Sample syntax from pilot studies for reference

################################################################################
# SECTION 1: LOAD DATA
################################################################################

long_data <- fread(here("Data", "Management data", paste0(country, "_", year, "_am_rdt_os_cleaned_long_wt.csv"))) %>%
  mutate(across(where(is.character), ~ str_trim(str_to_upper(.)))) %>% 
  rename(finalIntStat = finalintstat)

# SAVE FOR LOGGING
initial_records <- nrow(long_data)

################################################################################
# SECTION 2: CHECK FINAL INTERVIEW STATUS
################################################################################

# SAVE FOR LOGGING - Check finalIntStat distribution
finalint_formtype <- long_data %>%
  count(finalIntStat, formtype, .drop = FALSE) %>%
  pivot_wider(names_from = formtype, values_from = n, values_fill = 0)

finalint_first <- long_data %>%
  filter(nOut == 1) %>%
  count(finalIntStat, .drop = FALSE) %>%
  mutate(percentage = round(n / sum(n) * 100, 1))

################################################################################
# SECTION 3: GENERATE ELIGIBILITY VARIABLE
################################################################################

long_data <- long_data %>%
  mutate(
    eligible = case_when(
      am_stockcurrent == 1 ~ 1,  # has AM in stock
      am_stockcurrent == 0 & am_stockpast == 1 ~ 2,  # had AM in stock in previous 3 months but not today
      am_stockcurrent == 0 & (rdt_stock == 1 | micro_stockcurrent == 1) ~ 3,  # has or had in prev 3m RDT or microscopy in stock, no AM in stock
      TRUE ~ NA_real_
    )
  ) %>%
  relocate(eligible, .after = consented)

# SAVE FOR LOGGING - Check eligibility distribution
eligibility_dist <- long_data %>%
  count(eligible, .drop = FALSE) %>%
  mutate(
    Label = case_when(
      eligible == 1 ~ "Has AM in stock",
      eligible == 2 ~ "Had AM in stock in previous 3 months but not today",
      eligible == 3 ~ "Has or had in prev 3m RDT or microscopy in stock, no AM in stock",
      is.na(eligible) ~ "Missing/Ineligible"
    ),
    percentage = round(n / sum(n) * 100, 1)
  ) %>%
  select(eligible, Label, n, percentage)

# SAVE FOR LOGGING - Cross-tabulation of finalIntStat by eligible (first outlets only)
eligible_finalint <- long_data %>%
  filter(nOut == 1) %>%
  count(finalIntStat, eligible, .drop = FALSE) %>%
  pivot_wider(names_from = eligible, values_from = n, values_fill = 0)

################################################################################
# SECTION 4: GENERATE BINARY DENOMINATOR VARIABLES
################################################################################

long_data <- long_data %>%
  mutate(
    # screened - outlets that were screened (not refused/not found/etc.)
    screened = case_when(
      !finalIntStat %in% c(103, 104, 105, 106, 107, 108) & !is.na(finalIntStat) ~ 1,
      TRUE ~ NA_real_
    ),
    
    # interviewed - outlets that started a full interview
    interviewed = case_when(
      finalIntStat %in% c(1, 303, 304, 305, 307, 308, 309, 310, 400) ~ 1,
      is.na(screened) ~ NA_real_,
      TRUE ~ NA_real_
    ),
    
    # enum - outlets that were enumerated
    enum = case_when(
      !is.na(finalIntStat) ~ 1,
      TRUE ~ NA_real_
    ),
    
    # denomEligible - outlets that were eligible and interviewed
    denomEligible = case_when(
      !is.na(eligible) & interviewed == 1 ~ 1,
      TRUE ~ NA_real_
    ),
    
    # denomAMcurrent - outlets that stocked antimalarials at the time of interview
    denomAMcurrent = case_when(
      eligible == 1 & interviewed == 1 ~ 1,
      TRUE ~ NA_real_
    ),
    
    # denomAMrecent - outlets that stocked antimalarials within last 3 months
    denomAMrecent = case_when(
      eligible == 2 & interviewed == 1 ~ 1,
      TRUE ~ NA_real_
    ),
    
    # denomDX - outlets that provide testing only
    denomDX = case_when(
      eligible == 3 & interviewed == 1 ~ 1,
      TRUE ~ NA_real_
    ),
    
    # Additional denominator dummy variables for detailed sample description table
    Sc = screened,
    El = case_when(
      eligible %in% c(1, 2, 3) & interviewed == 1 ~ 1,
      TRUE ~ NA_real_
    ),
    ElNIn = case_when(
      eligible %in% c(1, 2, 3) & is.na(interviewed) ~ 1,
      TRUE ~ NA_real_
    ),
    InAmStock = case_when(
      interviewed == 1 & eligible == 1 ~ 1,
      TRUE ~ NA_real_
    ),
    InAmRec = case_when(
      interviewed == 1 & eligible %in% c(1, 2) ~ 1,
      TRUE ~ NA_real_
    ),
    InDx = case_when(
      interviewed == 1 & eligible == 3 ~ 1,
      TRUE ~ NA_real_
    )
  )

# Apply variable labels
var_label(long_data$eligible) <- "Eligibility status"
var_label(long_data$screened) <- "Outlets screened"
var_label(long_data$interviewed) <- "Outlets interviewed"
var_label(long_data$enum) <- "Outlets enumerated"
var_label(long_data$denomEligible) <- "Eligible and interviewed outlets"
var_label(long_data$denomAMcurrent) <- "Current AM stockists"
var_label(long_data$denomAMrecent) <- "Recent AM stockists"
var_label(long_data$denomDX) <- "Diagnostics-only outlets"

# Apply value labels
denom_vars <- c("screened", "interviewed", "enum", "denomEligible", "denomAMcurrent", "denomAMrecent", "denomDX", "Sc", "El", "ElNIn", "InAmStock", "InAmRec", "InDx")
for(var in denom_vars) {
  val_labels(long_data[[var]]) <- c("No" = 0, "Yes" = 1)
}

val_labels(long_data$eligible) <- c("Has AM in stock" = 1, "Had AM in stock in previous 3 months but not today" = 2, "Has or had in prev 3m RDT or microscopy in stock, no AM in stock" = 3)

################################################################################
# SECTION 5: CREATE VERIFICATION TABLES
################################################################################

# SAVE FOR LOGGING - Final Interview Status by Screened (first outlets only)
finalint_screened <- long_data %>%
  filter(nOut == 1) %>%
  count(finalIntStat, screened, .drop = FALSE) %>%
  pivot_wider(names_from = screened, values_from = n, values_fill = 0)

# SAVE FOR LOGGING - Final Interview Status by Interviewed (first outlets only)
finalint_interviewed <- long_data %>%
  filter(nOut == 1) %>%
  count(finalIntStat, interviewed, .drop = FALSE) %>%
  pivot_wider(names_from = interviewed, values_from = n, values_fill = 0)

# SAVE FOR LOGGING - Summary of all denominator variables (first outlets only)
denom_summary <- long_data %>%
  filter(nOut == 1) %>%
  summarise(
    across(all_of(c("interviewed", "enum", "denomEligible", "denomAMcurrent", "denomAMrecent", "denomDX")),
           ~ sum(.x == 1, na.rm = TRUE), .names = "{.col}")
  ) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Count") %>%
  mutate(
    Label = case_when(
      Variable == "interviewed" ~ "Interviewed outlets",
      Variable == "enum" ~ "Enumerated outlets",
      Variable == "denomEligible" ~ "Eligible outlets",
      Variable == "denomAMcurrent" ~ "Current AM stockists",
      Variable == "denomAMrecent" ~ "Recent AM stockists", 
      Variable == "denomDX" ~ "Diagnostics only"
    )
  ) %>%
  select(Variable, Label, Count)

################################################################################
# SECTION 6: SAVE PROCESSED DATA
################################################################################

output_file <- here("Data", "Management data", paste0(country, "_", year, "_am_rdt_os_cleaned_long_wt_denom.csv"))

fwrite(long_data, output_file, row.names = FALSE, na = "")

################################################################################
# GENERATE HTML LOG
################################################################################

# Setup log file
log_dir <- here("Data", "Management data", "Data cleaning notes")
if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE)

log_file <- file.path(log_dir, paste0("AwL_", country, "_", year, "_denominators_notes.html"))

# Initialize HTML
cat("<html><head><title>Denominators Analysis</title>", file = log_file)
cat("<style>
table { border-collapse: collapse; width: 100%; margin: 10px 0; }
th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }
th { background-color: #f2f2f2; }
.warning { color: red; font-weight: bold; }
.note { background-color: #fff3cd; padding: 10px; margin: 10px 0; }
</style></head><body>", file = log_file, append = TRUE)

sink(log_file, append = TRUE)

cat("<h1>ACTwatch LITE - Denominator Variables</h1>")
cat(paste("<h2>Country:", country, "| Year:", year, "</h2>"))

# =============================================================================
# HTML CONTENT GENERATION
# =============================================================================

cat("<h3>LOADING DATA</h3>")
cat("<p>Data loaded successfully</p>")
cat(paste("<p>Total observations:", initial_records, "</p>"))

cat("<h3>FINAL INTERVIEW STATUS ANALYSIS</h3>")
html_table(finalint_formtype, "Final Interview Status by Form Type")
html_table(finalint_first, "Final Interview Status (First Outlets Only)")

cat("<h3>GENERATING ELIGIBILITY VARIABLE</h3>")
cat("<div class='note'>USER INPUT REQUIRED ($$$ in Stata): Review eligibility criteria and modify if needed for your implementation.</div>")
html_table(eligibility_dist, "Eligibility Status Distribution")
html_table(eligible_finalint, "Final Interview Status by Eligibility (First Outlets Only)")

cat("<h3>GENERATING BINARY DENOMINATOR VARIABLES</h3>")
cat("<p>The following denominator variables have been created:</p>")
cat("<ul>")
cat("<li><strong>screened</strong> - Outlets that were screened (not refused/not found)</li>")
cat("<li><strong>interviewed</strong> - Outlets that started a full interview</li>")
cat("<li><strong>enum</strong> - Outlets that were enumerated</li>")
cat("<li><strong>denomEligible</strong> - Outlets that were eligible and interviewed</li>")
cat("<li><strong>denomAMcurrent</strong> - Current AM stockists</li>")
cat("<li><strong>denomAMrecent</strong> - Recent AM stockists (last 3 months)</li>")
cat("<li><strong>denomDX</strong> - Diagnostics-only outlets</li>")
cat("</ul>")

cat("<h3>VERIFICATION TABLES</h3>")
html_table(finalint_screened, "Final Interview Status by Screened (First Outlets Only)")
html_table(finalint_interviewed, "Final Interview Status by Interviewed (First Outlets Only)")
html_table(denom_summary, "Summary of Denominator Variables (First Outlets Only)")

cat("<h3>SAVING PROCESSED DATA</h3>")
cat("<p>Data saved successfully</p>")

cat("<hr><h3>PROCESSING COMPLETE</h3>")
cat("<p>Denominator variables have been successfully generated.</p>")
cat("<p>Key areas flagged for review ($$$ in original Stata):</p>")
cat("<ul>")
cat("<li>Eligibility criteria - review and modify if needed for your implementation</li>")
cat("<li>Final interview status codes - ensure they match your survey codes</li>")
cat("</ul>")

cat("</body></html>")

sink()

################################################################################
# CONSOLE OUTPUT
################################################################################

cat("\n=== DENOMINATORS PROCESSING COMPLETE ===\n")
cat("Records processed:", nrow(long_data), "\n")

# Count unique outlets
outlet_count <- long_data %>% filter(nOut == 1) %>% nrow()
cat("Unique outlets:", outlet_count, "\n")

# Key denominator summary for console
denom_summary_console <- long_data %>%
  filter(nOut == 1) %>%
  summarise(
    screened = sum(screened == 1, na.rm = TRUE),
    interviewed = sum(interviewed == 1, na.rm = TRUE),
    eligible = sum(denomEligible == 1, na.rm = TRUE),
    am_current = sum(denomAMcurrent == 1, na.rm = TRUE),
    am_recent = sum(denomAMrecent == 1, na.rm = TRUE),
    dx_only = sum(denomDX == 1, na.rm = TRUE)
  )

cat("\nOutlet denominators:\n")
cat(sprintf("  Screened: %d (%.1f%%)\n", 
            denom_summary_console$screened, denom_summary_console$screened/outlet_count*100))
cat(sprintf("  Interviewed: %d (%.1f%%)\n", 
            denom_summary_console$interviewed, denom_summary_console$interviewed/outlet_count*100))
cat(sprintf("  Eligible: %d (%.1f%%)\n", 
            denom_summary_console$eligible, denom_summary_console$eligible/outlet_count*100))
cat(sprintf("  Current AM stockists: %d\n", denom_summary_console$am_current))
cat(sprintf("  Recent AM stockists: %d\n", denom_summary_console$am_recent))
cat(sprintf("  Diagnostics only: %d\n", denom_summary_console$dx_only))

# Show interview completion rate
if (denom_summary_console$screened > 0) {
  completion_rate <- round(denom_summary_console$interviewed/denom_summary_console$screened*100, 1)
  cat(sprintf("\nInterview completion rate: %.1f%%\n", completion_rate))
}

cat("\nVariables created: eligible, screened, interviewed, denomEligible, denomAMcurrent, denomAMrecent, denomDX\n")

cat("\nOutput files:\n")
cat("  Dataset:", output_file, "\n")
cat("  HTML report:", log_file, "\n")

cat("\nNOTE: Review eligibility criteria in HTML report for context-specific adjustments\n")

################################################################################
# END
################################################################################