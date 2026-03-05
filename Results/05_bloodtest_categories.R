################################################################################
# ACTwatch LITE 
# Step 2.5 Generate Blood Testing Category Variables
# REORGANIZED VERSION - HTML logging at end
################################################################################
#
# This script automatically identifies the top three RDT manufacturers by
# frequency in the dataset and generates binary indicator variables for each.
# Additional variables are created for "other" manufacturers (those not in the
# top 3) and "unknown" manufacturer (missing data). These variables enable
# market composition analysis and facilitate comparisons of diagnostic product
# availability across different manufacturer groups. The manufacturer selection
# is data-driven and deterministic, ensuring consistency across repeated runs
# while adapting to country-specific market structures.
#
# The following flags are used throughout the syntax:
# $$$ = Breaks the script to remind analyst to modify syntax
# # EXAMPLE: = Sample syntax from pilot studies for reference

################################################################################
# SECTION 1: LOAD AND PREPARE DATA
################################################################################

options(stringsAsFactors = FALSE, warn = -1)

# Load and clean manufacturer data
long_data <- fread(here("Data", "Management data", paste0(country, "_", year, "_am_rdt_os_cleaned_long_wt_outletcat.csv"))) %>% 
  mutate(across(where(is.character), ~ str_trim(str_to_upper(.)))) %>% 
  mutate(
    rdtmanu_orig = rdtmanu,
    rdtmanu = rdtmanu %>%
      str_trim() %>%
      str_to_upper() %>%
      str_replace_all("PVTLTD", "PVT LTD") %>%  # Standardize this pattern
      str_squish()  # Remove extra spaces
  )

# SAVE FOR LOGGING (original count before processing)
initial_records <- nrow(long_data)

################################################################################
# SECTION 2: IDENTIFY TOP 3 MANUFACTURERS
################################################################################

# Filter to RDT products only
rdt_data <- long_data %>% 
  filter(producttype == 3, !is.na(rdtmanu), rdtmanu != "")

# SAVE FOR LOGGING AND VARIABLE CREATION (needed in mutate below)
if (nrow(rdt_data) > 0) {
  # Automatically select top 3 manufacturers (deterministic order)
  manufacturer_freq <- rdt_data %>% 
    count(rdtmanu, sort = TRUE) %>%
    mutate(percentage = round(n / sum(n) * 100, 1))
  
  top_manufacturers <- manufacturer_freq %>%
    arrange(desc(n), rdtmanu) %>%  # Frequency first, then alphabetical for ties
    slice_head(n = 3) %>%
    pull(rdtmanu)
  
  # Extract patterns for partial matching (like Stata's regexm)
  manufacturer_patterns <- top_manufacturers %>%
    str_extract("^[A-Z]+ [A-Z]+") %>%  # Extract first two words as search pattern
    str_trim()
  
} else {
  top_manufacturers <- character(0)
  manufacturer_patterns <- character(0)
}

################################################################################
# SECTION 3: GENERATE MANUFACTURER CATEGORY VARIABLES
################################################################################

# Generate manufacturer category variables using partial matching (like Stata's regexm)
long_data_processed <- long_data %>%
  mutate(
    rdtmanu_1 = case_when(
      producttype == 3 & length(manufacturer_patterns) >= 1 & str_detect(rdtmanu, manufacturer_patterns[1]) ~ 1,
      TRUE ~ NA_real_  # Everything else stays missing
    ),
    rdtmanu_2 = case_when(
      producttype == 3 & length(manufacturer_patterns) >= 2 & str_detect(rdtmanu, manufacturer_patterns[2]) ~ 1,
      TRUE ~ NA_real_  # Everything else stays missing
    ),
    rdtmanu_3 = case_when(
      producttype == 3 & length(manufacturer_patterns) >= 3 & str_detect(rdtmanu, manufacturer_patterns[3]) ~ 1,
      TRUE ~ NA_real_  # Everything else stays missing
    ),
    rdtmanu_other = case_when(
      producttype == 3 & !is.na(rdtmanu) & rdtmanu != "" & 
        (is.na(rdtmanu_1) | rdtmanu_1 != 1) &
        (is.na(rdtmanu_2) | rdtmanu_2 != 1) &
        (is.na(rdtmanu_3) | rdtmanu_3 != 1) ~ 1,
      TRUE ~ NA_real_
    ),
    rdtmanu_dk = case_when(
      producttype == 3 & (is.na(rdtmanu) | rdtmanu == "") ~ 1,
      TRUE ~ NA_real_
    )
  )

################################################################################
# SECTION 4: SAVE PROCESSED DATA
################################################################################

output_file <- here("Data", "Management data", paste0(country, "_", year, "_am_rdt_os_cleaned_long_wt_bloodtest.csv"))
fwrite(long_data_processed, output_file, row.names = FALSE, na = "")

################################################################################
# GENERATE HTML LOG
################################################################################

# Setup log file
log_dir <- here("Data", "Management data", "Data cleaning notes")
if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE)

log_file <- file.path(log_dir, paste0("AwL_", country, "_", year, "_bloodtest_categories_notes.html"))

# Initialize HTML
cat('<!DOCTYPE html>
<html><head>
<title>Blood Test Categories Analysis</title>
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

cat("<h1>ACTwatch Lite Blood Testing Category Variables</h1>\n")
cat("<p><strong>Country:</strong> ", country, " | <strong>Year:</strong> ", year, " | <strong>Date:</strong> ", as.character(Sys.Date()), "</p>\n")

# =============================================================================
# HTML CONTENT GENERATION - All calculated here from final data
# =============================================================================

cat("<h2>MANUFACTURER FREQUENCY ANALYSIS</h2>\n")

if (length(top_manufacturers) > 0) {
  # Recalculate manufacturer frequency for display
  manufacturer_freq_display <- long_data_processed %>%
    filter(producttype == 3, !is.na(rdtmanu), rdtmanu != "") %>%
    count(rdtmanu, sort = TRUE) %>%
    mutate(percentage = round(n / sum(n) * 100, 1))
  
  html_table(manufacturer_freq_display %>% 
               rename(Manufacturer = rdtmanu, Count = n, Percentage = percentage), 
             "RDT Manufacturer Frequencies (producttype == 3)")
  
  cat("<h3>Top 3 Manufacturers Selected:</h3>\n")
  cat("<ol>\n")
  for (i in seq_along(top_manufacturers)) {
    cat(paste0("<li>", top_manufacturers[i], "</li>\n"))
  }
  cat("</ol>\n")
  
} else {
  cat("<p class='warning'>No RDT data found (producttype == 3)</p>\n")
}

cat("<h2>GENERATING MANUFACTURER VARIABLES</h2>\n")

cat("<h2>MANUFACTURER CATEGORY SUMMARY</h2>\n")

# Create summary of generated variables
if (length(top_manufacturers) > 0) {
  variable_summary <- long_data_processed %>%
    filter(producttype == 3) %>%
    summarise(
      total = n(),
      manu_1 = sum(rdtmanu_1 == 1, na.rm = TRUE),
      manu_2 = sum(rdtmanu_2 == 1, na.rm = TRUE),
      manu_3 = sum(rdtmanu_3 == 1, na.rm = TRUE),
      other = sum(rdtmanu_other == 1, na.rm = TRUE),
      unknown = sum(rdtmanu_dk == 1, na.rm = TRUE)
    ) %>%
    pivot_longer(everything(), names_to = "Category", values_to = "Count") %>%
    mutate(
      Category = case_when(
        Category == "total" ~ "Total RDT Products",
        Category == "manu_1" ~ paste("Manufacturer 1:", top_manufacturers[1]),
        Category == "manu_2" ~ paste("Manufacturer 2:", if(length(top_manufacturers) >= 2) top_manufacturers[2] else "None"),
        Category == "manu_3" ~ paste("Manufacturer 3:", if(length(top_manufacturers) >= 3) top_manufacturers[3] else "None"),
        Category == "other" ~ "Other Manufacturers",
        Category == "unknown" ~ "Unknown Manufacturer"
      ),
      Percentage = round(Count / first(Count) * 100, 1)
    )
  
  html_table(variable_summary, "Manufacturer Category Summary")
}

cat("<h2>QUALITY CHECKS</h2>\n")

# Verify all RDT products are categorized
total_rdt <- long_data_processed %>% filter(producttype == 3) %>% nrow()

if (total_rdt > 0) {
  categorized <- long_data_processed %>%
    filter(producttype == 3) %>%
    summarise(
      cat_count = sum(
        sum(rdtmanu_1 == 1, na.rm = TRUE),
        sum(rdtmanu_2 == 1, na.rm = TRUE), 
        sum(rdtmanu_3 == 1, na.rm = TRUE),
        sum(rdtmanu_other == 1, na.rm = TRUE),
        sum(rdtmanu_dk == 1, na.rm = TRUE)
      )
    ) %>%
    pull(cat_count)
  
  cat("<p>Total RDT products:", total_rdt, "</p>\n")
  cat("<p>Categorized products:", categorized, "</p>\n")
  
  if (total_rdt == categorized) {
    cat("<p class='success'>All RDT products successfully categorized</p>\n")
  } else {
    cat("<p class='warning'>WARNING: Categorization mismatch detected</p>\n")
  }
} else {
  cat("<p>No RDT products found for categorization</p>\n")
}

cat("<h2>SAVING PROCESSED DATA</h2>\n")
cat("<p>Data saved successfully</p>\n")

# Show variable labels created
if (length(top_manufacturers) > 0) {
  labels_df <- data.frame(
    Variable = c("rdtmanu_1", "rdtmanu_2", "rdtmanu_3", "rdtmanu_other", "rdtmanu_dk"),
    Label = c(
      paste("RDT manufacturer:", if(length(top_manufacturers) >= 1) top_manufacturers[1] else "None"),
      paste("RDT manufacturer:", if(length(top_manufacturers) >= 2) top_manufacturers[2] else "None"),
      paste("RDT manufacturer:", if(length(top_manufacturers) >= 3) top_manufacturers[3] else "None"),
      "RDT manufacturer: other",
      "RDT manufacturer: don't know"
    ),
    stringsAsFactors = FALSE
  )
  
  html_table(labels_df, "Variable Labels Created")
}

cat("<div class='section success'>\n")
cat("<h2>Processing Complete</h2>\n")
cat("<p>Blood testing category variables have been successfully generated.</p>\n")
cat("</div>\n")

sink()
cat("</body></html>", file = log_file, append = TRUE)
options(warn = 0)

################################################################################
# CONSOLE OUTPUT
################################################################################

cat("\n=== BLOOD TESTING CATEGORIES PROCESSING COMPLETE ===\n")
cat("Records processed:", nrow(long_data_processed), "\n")

if (length(top_manufacturers) > 0) {
  cat("Variables generated: 5 manufacturer category variables\n")
  cat("Top 3 manufacturers:\n")
  for (i in seq_along(top_manufacturers)) {
    cat("  ", i, ".", top_manufacturers[i], "\n")
  }
} else {
  cat("Variables generated: 0 (no RDT data found)\n")
}

if (total_rdt > 0) {
  if (total_rdt == categorized) {
    cat("All RDT products successfully categorized\n")
  } else {
    cat("WARNING: Categorization mismatch detected\n")
  }
}

cat("\nOutput files:\n")
cat("  Dataset:", output_file, "\n")
cat("  HTML log:", log_file, "\n")

################################################################################
# END
################################################################################