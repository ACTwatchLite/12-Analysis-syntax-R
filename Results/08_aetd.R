################################################################################
# ACTwatch LITE 
# Step 2.8 Define and Assign Adult Equivalent Treatment Doses (AETDs)
# REORGANIZED VERSION - HTML logging at end
################################################################################
#
# This script calculates Adult Equivalent Treatment Doses (AETDs) for all
# antimalarial products to enable standardized volume and market share
# comparisons across different formulations, dosage forms, and package sizes.
# The calculation proceeds through five steps: (1) defining full-course treatment
# dose in mg for a 60kg adult for each generic antimalarial type, (2) adjusting
# active ingredient strengths for salt content where applicable, (3) calculating
# the number of units (tablets, injections, etc.) required for one full course
# treatment, (4) standardizing package size accounting for fixed-dose combinations
# versus loose combinations and formulation-specific adjustments, and (5)
# computing final packageaetd by dividing standardized package size by units
# required per course. The resulting packageaetd variable represents the number
# of complete adult treatment courses contained in each product package and
# serves as the foundation for AETD-adjusted volume analyses throughout the
# results tables.
#
# The following flags are used throughout the syntax:
# $$$ = Breaks the script to remind analyst to modify syntax
# # EXAMPLE: = Sample syntax from pilot studies for reference

################################################################################
# SECTION 1: LOAD DATA
################################################################################

long_data <- fread(here("Data", "Management data", 
                        paste0(country, "_", year, "_am_rdt_os_cleaned_long_wt_stocking.csv"))) %>%
  mutate(across(where(is.character), ~ str_trim(str_to_upper(.))))

# SAVE FOR LOGGING
initial_records <- nrow(long_data)

################################################################################
# SECTION 2: DEFINE FULL-COURSE TREATMENT DOSE
################################################################################

# Create lookup table for fullcourse1 values (mg required for 60kg adult)
fullcourse1_lookup <- c(
  `1` = 1800, `2` = 1800, `3` = 3000, `4` = 1500, `6` = 1500, `7` = 360, 
  `9` = 1398, `10` = 1550, `11` = 1000, `12` = 1000, `13` = 45, `18` = 10500,
  `19` = 10408, `20` = 10408, `21` = 1500, `22` = 2212, `30` = 1050, `31` = 480,
  `32` = 600, `33` = 480, `34` = 1500, `40` = 480, `41` = 2400, `42` = 504,
  `43` = 504, `44` = 600, `45` = 600, `46` = 600, `47` = 600, `48` = 600,
  `49` = 600, `50` = 600, `51` = 360, `52` = 360, `53` = 360, `54` = 360,
  `55` = 360, `56` = 256, `57` = 360, `58` = 360, `59` = 480, `61` = 450
)

long_data_processed <- long_data %>%
  mutate(fullcourse1 = fullcourse1_lookup[as.character(gname)])

var_label(long_data_processed$fullcourse1) <- "mg required for one AETD"

################################################################################
# SECTION 3: ADJUST FOR SALT CONTENT
################################################################################

# Salt adjustment lookup
salt_adjustments <- list(
  list(ai_ing = 60, salt = 2, salton = 60, factor = 0.765),   # Amodiaquine hydrochloride
  list(ai_ing = 68, salt = 2, salton = 68, factor = 0.8),     # Chloroquine dihydrochloride
  list(ai_ing = 68, salt = 5, salton = 68, factor = 0.6),     # Chloroquine phosphate
  list(ai_ing = 73, salt = 6, salton = 73, factor = 0.62),    # Hydroxychloroquine sulphate
  list(ai_ing = 79, salt = 2, salton = 79, factor = 0.87),    # Proguanil hydrochloride
  list(ai_ing = 83, salt = 1, salton = 83, factor = 0.592),   # Quinine bisulphate
  list(ai_ing = 83, salt = 3, salton = 83, factor = 0.82),    # Quinine dihydrochloride
  list(ai_ing = 83, salt = 2, salton = 83, factor = 0.82),    # Quinine hydrochloride
  list(ai_ing = 83, salt = 6, salton = 83, factor = 0.826)    # Quinine sulphate
)

# Apply salt adjustments
for (adj in salt_adjustments) {
  long_data_processed <- long_data_processed %>%
    mutate(
      aiStrength_a = if_else(ai1_ing == adj$ai_ing & salt == adj$salt & 
                               hassalt == 1 & salton == adj$salton,
                             aiStrength_a * adj$factor, aiStrength_a),
      aiStrength_b = if_else(ai2_ing == adj$ai_ing & salt == adj$salt & 
                               hassalt == 1 & salton == adj$salton,
                             aiStrength_b * adj$factor, aiStrength_b),
      aiStrength_c = if_else(ai3_ing == adj$ai_ing & salt == adj$salt & 
                               hassalt == 1 & salton == adj$salton,
                             aiStrength_c * adj$factor, aiStrength_c)
    )
}

################################################################################
# SECTION 4: CALCULATE NUMBER OF UNITS REQUIRED FOR FULL COURSE
################################################################################

# Define monotherapy gnames
monotherapy_gnames <- c(1, 4, 10, 11, 14, 15, 19, 22, 30, 31, 32)

# Define combination therapy active ingredients
combo_ai_mapping <- list(
  list(gname = 2, ai_ing = 60),   # SPAQ - Amodiaquine
  list(gname = 3, ai_ing = 66),   # Atovaquone proguanil
  list(gname = 6, ai_ing = 68),   # Chloroquine SP
  list(gname = 12, ai_ing = 75),  # Mefloquine SP
  list(gname = 20, ai_ing = 83),  # Quinine SP
  list(gname = 21, ai_ing = 81),  # SP
  list(gname = 40, ai_ing = 61),  # AL - artemether
  list(gname = 41, ai_ing = 62),  # Artemisinin naphthoquine
  list(gname = 42, ai_ing = 62),  # Artemisinin piperaquine
  list(gname = 43, ai_ing = 62),  # Artemisinin piperaquine primaquine
  list(gname = 44, ai_ing = 65),  # ASAQ - artesunate
  list(gname = 45, ai_ing = 65),  # Artesunate halofantrine
  list(gname = 46, ai_ing = 65),  # Artesunate lumefantrine
  list(gname = 47, ai_ing = 65),  # Artesunate mefloquine
  list(gname = 48, ai_ing = 65),  # Artesunate piperaquine
  list(gname = 49, ai_ing = 65),  # Artesunate pyronaridine
  list(gname = 50, ai_ing = 65),  # Artesunate SP
  list(gname = 51, ai_ing = 71),  # DHA amodiaquine
  list(gname = 52, ai_ing = 71),  # DHA halofantrine
  list(gname = 53, ai_ing = 71),  # DHA lumefantrine
  list(gname = 54, ai_ing = 71),  # DHA mefloquine
  list(gname = 55, ai_ing = 71),  # DHA piperaquine
  list(gname = 56, ai_ing = 71),  # DHA piperaquine trimethoprim
  list(gname = 57, ai_ing = 71),  # DHA pyronaridine
  list(gname = 58, ai_ing = 71),  # DHA SP
  list(gname = 59, ai_ing = 68),  # Chloroquine primaquine
  list(gname = 60, ai_ing = 61),  # AL primaquine
  list(gname = 61, ai_ing = 69)   # Arterolane-PPQ
)

long_data_processed <- long_data_processed %>%
  mutate(fullcourse2 = NA_real_)

# Monotherapies
long_data_processed <- long_data_processed %>%
  mutate(
    fullcourse2 = case_when(
      !is.na(ai1_ing) & ai1_ing != 94 & is.na(ai2_ing) & is.na(ai3_ing) & 
        gname %in% monotherapy_gnames ~ fullcourse1/aiStrength_a,
      is.na(ai1_ing) & !is.na(ai2_ing) & ai2_ing != 94 & is.na(ai3_ing) & 
        gname %in% monotherapy_gnames ~ fullcourse1/aiStrength_b,
      is.na(ai1_ing) & is.na(ai2_ing) & !is.na(ai3_ing) & ai3_ing != 94 & 
        gname %in% monotherapy_gnames ~ fullcourse1/aiStrength_c,
      TRUE ~ fullcourse2
    )
  )

# Combination therapies
for (combo in combo_ai_mapping) {
  long_data_processed <- long_data_processed %>%
    mutate(
      fullcourse2 = case_when(
        is.na(fullcourse2) & gname == combo$gname & ai1_ing == combo$ai_ing ~ fullcourse1/aiStrength_a,
        is.na(fullcourse2) & gname == combo$gname & ai2_ing == combo$ai_ing ~ fullcourse1/aiStrength_b,
        is.na(fullcourse2) & gname == combo$gname & ai3_ing == combo$ai_ing ~ fullcourse1/aiStrength_c,
        TRUE ~ fullcourse2
      )
    )
}

# Powder injections
long_data_processed <- long_data_processed %>%
  mutate(fullcourse2 = if_else(a3 == 7, fullcourse1, fullcourse2))

var_label(long_data_processed$fullcourse2) <- "Number of units required for fullcourse1"

################################################################################
# SECTION 5: STANDARDIZE PRODUCT SIZE (NEWSIZE)
################################################################################

long_data_processed <- long_data_processed %>%
  mutate(
    newsize = case_when(
      # Special cases FIRST (these override everything)
      a3 == 8 ~ 6,
      brand == "CAMOQUINE PLUS PEDIATRIQUE" & a3 != 1 ~ 6,
      producttype == 2 & brand == "ARTEDIAM CHILD AND INFANT" ~ 30,
      
      # Tablet monotherapy
      gname %in% c(1,4,8,9,10,11,13,14,15,17,18,19,22,30,31,32,33) & producttype == 1 ~ size,
      
      # Tablet FDC
      fdc == 1 & producttype == 1 ~ size,
      
      # Tablet non-FDC (divide by 2 except for specific generics)
      a3 == 1 & fdc == 0 & !gname %in% c(2,6,12,20,44,47,50,51,52,54,58) ~ size * 0.5,
      
      # SPAQ exceptions
      gname == 2 & a3 == 1 & fdc == 0 & 
        ((ai1_ing == 60 & aiStrength_a == 600) | 
           (ai2_ing == 60 & aiStrength_b == 600) | 
           (ai3_ing == 60 & aiStrength_c == 600)) ~ size * 0.5,
      gname == 2 & a3 == 1 & fdc == 0 & 
        ((ai1_ing == 60 & aiStrength_a == 200) | 
           (ai2_ing == 60 & aiStrength_b == 200) | 
           (ai3_ing == 60 & aiStrength_c == 200)) ~ size * 0.75,
      
      # ASAQ exceptions
      gname == 44 & a3 == 1 & fdc == 0 & 
        !brand %in% c("AMONATE ADULT", "AMONATE JUNIOR") ~ size * 0.5,
      gname == 44 & a3 == 1 & fdc == 0 & brand == "AMONATE ADULT" ~ size * 0.5,
      
      # AS-SP exceptions
      gname == 50 & a3 == 1 & fdc == 0 & 
        ai1_ing == 65 & ai1_mg %in% c(100, 200) ~ size * 0.5,
      
      # Non-tablets
      producttype == 2 ~ size,
      
      # Default
      TRUE ~ size
    )
  )

var_label(long_data_processed$newsize) <- "Number of AETD defining units in package TSG/NT"

################################################################################
# SECTION 6: CALCULATE PACKAGEAETD
################################################################################

long_data_processed <- long_data_processed %>%
  mutate(packageaetd = if_else(!is.na(fullcourse2), newsize / fullcourse2, NA_real_))

var_label(long_data_processed$packageaetd) <- "Number of AETDs Per Package"

################################################################################
# SECTION 7: SAVE PROCESSED DATA
################################################################################

output_file <- here("Data", "Management data", paste0(country, "_", year, "_am_rdt_os_cleaned_long_wt_aetd.csv"))
fwrite(long_data_processed, output_file, row.names = FALSE, na = "")

################################################################################
# GENERATE HTML LOG
################################################################################

# Setup log file
log_dir <- here("Data", "Management data", "Data cleaning notes")
if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE)
log_file <- file.path(log_dir, paste0("AwL_", country, "_", year, "_aetd_notes.html"))

sink(log_file, type = "output")

cat("<html><head><title>AETD Calculations Analysis</title>")
cat("<style>
table { border-collapse: collapse; width: 100%; margin: 10px 0; }
th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }
th { background-color: #f2f2f2; }
.warning { color: red; font-weight: bold; }
.note { background-color: #fff3cd; padding: 10px; margin: 10px 0; }
</style></head><body>")

cat("<h1>ACTwatch LITE - AETD Calculations</h1>")
cat(paste("<h2>Country:", country, "| Year:", year, "</h2>"))

# =============================================================================
# HTML CONTENT GENERATION - All calculated from final data
# =============================================================================

cat("<h3>LOADING DATA</h3>")

cat("<p>Data loaded successfully</p>")
cat(paste("<p>Total observations:", initial_records, "</p>"))
cat("<div class='note'>USER INPUT REQUIRED: Review and make any needed changes to the AETD syntax before proceeding (specifically if new antimalarials have come to market since Nov 2024).</div>")

cat("<h3>STEP 1: DEFINING FULL-COURSE TREATMENT DOSES</h3>")

gname_dist <- long_data_processed %>%
  filter(producttype %in% c(1, 2)) %>%
  count(gname, .drop = FALSE) %>%
  filter(n > 0) %>%
  arrange(gname)

html_table(gname_dist, "Generic Name Distribution for Antimalarials (producttype 1 & 2)")

fullcourse_summary <- long_data_processed %>%
  filter(!is.na(fullcourse1)) %>%
  distinct(gname, fullcourse1) %>%
  arrange(gname)

html_table(fullcourse_summary, "Full Course Treatment Doses Assigned (mg for 60kg adult)")

missing_fullcourse <- long_data_processed %>%
  filter(producttype %in% c(1, 2), is.na(fullcourse1)) %>%
  count(gname) %>%
  filter(n > 0)

if (nrow(missing_fullcourse) > 0) {
  html_table(missing_fullcourse, "WARNING: Generic Names Missing fullcourse1 Values")
}

cat("<h3>STEP 2: ADJUSTING FOR SALT CONTENT</h3>")
cat("<div class='note'>USER INPUT REQUIRED: Ensure this section is updated and run to adjust strengths for salts.</div>")

salt_summary <- long_data_processed %>%
  filter(!is.na(hassalt)) %>%
  count(hassalt, salt, salton, .drop = FALSE) %>%
  filter(n > 0)

html_table(salt_summary, "Salt Distribution in Dataset")

cat("<h3>STEP 3: CALCULATING UNITS REQUIRED FOR FULL COURSE</h3>")

missing_fullcourse2 <- long_data_processed %>%
  filter(producttype %in% c(1, 2), is.na(fullcourse2), !is.na(fullcourse1)) %>%
  count(gname, .drop = FALSE) %>%
  filter(n > 0)

if (nrow(missing_fullcourse2) > 0) {
  html_table(missing_fullcourse2, "WARNING: Products Missing fullcourse2 Values")
}

cat("<h3>STEP 4: STANDARDIZING PRODUCT SIZE</h3>")

missing_newsize <- long_data_processed %>%
  filter(producttype %in% c(1, 2), is.na(newsize), !is.na(aiStrength_a)) %>%
  count(gname, brand, a3, .drop = FALSE) %>%
  filter(n > 0)

if (nrow(missing_newsize) > 0) {
  html_table(missing_newsize, "WARNING: Products Missing newsize Values")
}

cat("<h3>STEP 5: CALCULATING PACKAGE AETD</h3>")

packageaetd_summary <- long_data_processed %>%
  filter(!is.na(packageaetd)) %>%
  summarise(
    count = n(),
    min_aetd = round(min(packageaetd, na.rm = TRUE), 3),
    q25_aetd = round(quantile(packageaetd, 0.25, na.rm = TRUE), 3),
    median_aetd = round(median(packageaetd, na.rm = TRUE), 3),
    q75_aetd = round(quantile(packageaetd, 0.75, na.rm = TRUE), 3),
    max_aetd = round(max(packageaetd, na.rm = TRUE), 3),
    mean_aetd = round(mean(packageaetd, na.rm = TRUE), 3)
  )

html_table(packageaetd_summary, "Package AETD Summary Statistics")

cat("<h3>QUALITY CHECKS</h3>")

missing_packageaetd <- long_data_processed %>%
  filter(producttype %in% c(1, 2), is.na(packageaetd)) %>%
  count(gname, brand, a3, .drop = FALSE) %>%
  filter(n > 0) %>%
  slice_head(n = 20)

if (nrow(missing_packageaetd) > 0) {
  html_table(missing_packageaetd, "Products Missing packageaetd (Top 20)")
}

small_packageaetd <- long_data_processed %>%
  filter(packageaetd < 0.25, producttype != 4) %>%
  count(gname, a3, .drop = FALSE) %>%
  filter(n > 0)

if (nrow(small_packageaetd) > 0) {
  html_table(small_packageaetd, "Products with packageaetd < 0.25 (by gname and a3)")
}

very_small_packageaetd <- long_data_processed %>%
  filter(packageaetd < 0.1, producttype != 4, gname > 39) %>%
  count(a3, size, .drop = FALSE) %>%
  filter(n > 0)

if (nrow(very_small_packageaetd) > 0) {
  html_table(very_small_packageaetd, "ACT Products with packageaetd < 0.1 (by a3 and size)")
}

large_packageaetd <- long_data_processed %>%
  filter(packageaetd > 2, !is.na(packageaetd)) %>%
  count(gname, brand, a3, .drop = FALSE) %>%
  filter(n > 0) %>%
  slice_head(n = 20)

if (nrow(large_packageaetd) > 0) {
  html_table(large_packageaetd, "Products with packageaetd > 2 (Top 20)")
}

dosage_summary <- long_data_processed %>%
  filter(!is.na(packageaetd)) %>%
  count(a3, .drop = FALSE) %>%
  filter(n > 0) %>%
  arrange(a3)

html_table(dosage_summary, "Package AETD Distribution by Dosage Form (a3)")

cat("<h3>FINAL REVIEW AND SAVE</h3>")

cat("<div class='note'>USER INPUT REQUIRED: Review cases where packageaetd < 0.1 AETDs to ensure cleaning was complete and correct. Check the observations carefully to determine if there are errors in the strength, generic name or package size variables. If so correct the data in your cleaning syntax file. Note that injectables frequently have 0.1 AETD per package.</div>")

cat("<div class='note'>USER INPUT REQUIRED: Review cases where packageaetd > 2 AETDs to ensure cleaning was complete and correct. Check the observations carefully to determine if there are errors in the strength, generic name or package size variables. If so correct the data in your cleaning syntax file.</div>")

final_summary <- long_data_processed %>%
  filter(producttype %in% c(1, 2)) %>%
  summarise(
    total_antimalarials = n(),
    with_fullcourse1 = sum(!is.na(fullcourse1)),
    with_fullcourse2 = sum(!is.na(fullcourse2)),
    with_newsize = sum(!is.na(newsize)),
    with_packageaetd = sum(!is.na(packageaetd)),
    missing_packageaetd = sum(is.na(packageaetd))
  ) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Count")

html_table(final_summary, "Final AETD Processing Summary")

cat("<h3>SAVING PROCESSED DATA</h3>")

cat("<p>Data saved successfully</p>")

cat("<hr><h3>PROCESSING COMPLETE</h3>")
cat("<p>AETD calculations have been successfully completed.</p>")
cat("<p>Key areas flagged for review:</p>")
cat("<ul>")
cat("<li>Review and make any needed changes to the AETD syntax if new antimalarials have come to market since Nov 2024</li>")
cat("<li>Ensure salt adjustment section is updated and run to adjust strengths for salts</li>")
cat("<li>Review cases where packageaetd < 0.1 or > 2 to ensure data cleaning was complete</li>")
cat("</ul>")

cat("<p>Variables created:</p>")
cat("<ul>")
cat("<li><strong>fullcourse1</strong>: mg required for one AETD (60kg adult)</li>")
cat("<li><strong>fullcourse2</strong>: Number of units required for fullcourse1</li>")
cat("<li><strong>newsize</strong>: Number of AETD defining units in package</li>")
cat("<li><strong>packageaetd</strong>: Number of AETDs per package</li>")
cat("</ul>")

cat("</body></html>")

if (sink.number() > 0) sink()

################################################################################
# CONSOLE OUTPUT
################################################################################

cat("\n=== AETD CALCULATIONS PROCESSING COMPLETE ===\n")
cat("Records processed:", nrow(long_data_processed), "\n")

# Count antimalarial products
am_products <- long_data_processed %>%
  filter(producttype %in% c(1, 2))

cat("Antimalarial products:", nrow(am_products), "\n")

# AETD calculation summary
aetd_summary <- am_products %>%
  summarise(
    with_fullcourse1 = sum(!is.na(fullcourse1)),
    with_fullcourse2 = sum(!is.na(fullcourse2)),
    with_packageaetd = sum(!is.na(packageaetd)),
    missing_aetd = sum(is.na(packageaetd))
  )

cat("\nAETD calculation success:\n")
cat(sprintf("  Full course dose assigned: %d (%.1f%%)\n",
            aetd_summary$with_fullcourse1,
            aetd_summary$with_fullcourse1/nrow(am_products)*100))
cat(sprintf("  Units per course calculated: %d (%.1f%%)\n",
            aetd_summary$with_fullcourse2,
            aetd_summary$with_fullcourse2/nrow(am_products)*100))
cat(sprintf("  Package AETD calculated: %d (%.1f%%)\n",
            aetd_summary$with_packageaetd,
            aetd_summary$with_packageaetd/nrow(am_products)*100))

if (aetd_summary$missing_aetd > 0) {
  cat(sprintf("  WARNING: %d products missing AETD\n", aetd_summary$missing_aetd))
}

# Quality flags
quality_flags <- am_products %>%
  filter(!is.na(packageaetd)) %>%
  summarise(
    very_small = sum(packageaetd < 0.1),
    large = sum(packageaetd > 2)
  )

if (quality_flags$very_small > 0 || quality_flags$large > 0) {
  cat("\nQuality check alerts:\n")
  if (quality_flags$very_small > 0) {
    cat(sprintf("  Products with AETD < 0.1: %d (review required)\n", quality_flags$very_small))
  }
  if (quality_flags$large > 0) {
    cat(sprintf("  Products with AETD > 2: %d (review required)\n", quality_flags$large))
  }
}

# AETD statistics if available
if (sum(!is.na(am_products$packageaetd)) > 0) {
  aetd_stats <- am_products %>%
    filter(!is.na(packageaetd)) %>%
    summarise(
      median = round(median(packageaetd), 2),
      mean = round(mean(packageaetd), 2)
    )
  
  cat(sprintf("\nPackage AETD statistics: Median = %.2f, Mean = %.2f\n",
              aetd_stats$median, aetd_stats$mean))
}

cat("\nVariables created: fullcourse1, fullcourse2, newsize, packageaetd\n")

cat("\nOutput files:\n")
cat("  Dataset:", output_file, "\n")
cat("  HTML log:", log_file, "\n")

cat("\nNOTE: Review flagged products in HTML report for data quality issues\n")

################################################################################
# END
################################################################################