################################################################################
# OUTLET CLEANING (01_outlet_cleaning.R)
################################################################################

################################################################################
# ACTwatch LITE 
# Step 1.1 Outlet-Level Data Cleaning
# REORGANIZED VERSION - HTML logging at end
################################################################################
#
# This script processes and cleans outlet-level survey data from the ACTwatch
# LITE outlet audit. It validates screening criteria, standardizes outlet 
# classification and location variables, processes outlet characteristics and
# stocking patterns, and performs comprehensive quality checks. The script
# generates detailed HTML logs documenting all cleaning decisions and flagging
# items requiring manual review. After validation, the cleaned outlet dataset
# is saved and serves as the foundation for merging with product audit data.
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

required_files <- list(
  list(
    path = here("Data", "Product lists", "antimalarial_masterlist_clean.csv"),
    name = "Cleaned antimalarial product list data",
    script = "Product List Cleaning"
  )
)

missing_files <- required_files[!sapply(required_files, function(x) file.exists(x$path))]

if (length(missing_files) > 0) {
  error_messages <- sapply(missing_files, function(x) {
    paste0("\n", x$name, " not found. Please run '", x$script, "' first.")
  })
  stop(paste(error_messages, collapse = ""))
}

################################################################################
# SECTION 2: DATA IMPORT AND PREPARATION
################################################################################

outlet_data <- fread(here("Data", "Raw data", paste0("AwL-", country, "-", year, "-final.csv"))) %>% 
  rename_with(tolower) %>% 
  rename_with(~ gsub("-", "", .)) 

# for test data where checkpoint1 doesn't exist (can remove in final script)
if(!"checkpoint1" %in% names(outlet_data)) {
  outlet_data <- outlet_data %>% mutate(checkpoint1 = 1)
}

# Save prepared dataset
fwrite(outlet_data, here("Data", "Cleaned data", paste0("AwL_", country, "_", year, "_outlet_prepared.csv")))

# Auto-convert date and character types
outlet_data <- outlet_data %>%
  mutate(
    # Convert date columns
    across(any_of(c("date", "reg2a", "starttime", "endtime", "submissiondate")), 
           ~ coalesce(mdy_hms(.x, quiet = TRUE), mdy(.x, quiet = TRUE))),
    
    # Clean character columns
    across(where(is.character), ~ if_else(is.na(.x), "", str_trim(str_to_upper(.x))))
  )

################################################################################
# SECTION 3: DUPLICATE DETECTION AND KEY VARIABLE MANAGEMENT
################################################################################

# Consolidate unique ID
outlet_data <- outlet_data %>%
  mutate(key = if_else(is.na(key) | key == "", instanceid, key)) %>%
  select(-instanceid)

# Check duplicates (saved for logging)
duplicate_keys <- outlet_data %>%
  count(key) %>%
  filter(n > 1) %>%
  arrange(desc(n))

duplicate_outletids <- outlet_data %>%
  count(outletid) %>%
  filter(n > 1) %>%
  arrange(desc(n))

################################################################################
# SECTION 4: DATASET LINK VERIFICATION
################################################################################

# Generate hasamaudit variable
outlet_data <- outlet_data %>%
  mutate(
    hasamaudit = case_when(
      (is.na(setofamaudit) | setofamaudit == "") & 
        ((is.na(eligible) | eligible == 0) | (is.na(consented) | consented != 1)) ~ 9888,
      (is.na(setofamaudit) | setofamaudit == "") & 
        (is.na(am_stockcurrent) | am_stockcurrent == 0) ~ 9888,
      (is.na(setofamaudit) | setofamaudit == "") & 
        (is.na(n1) | n1 == 0) ~ 9888,
      !is.na(setofamaudit) & setofamaudit != "" ~ 1,
      TRUE ~ 0
    )
  )

# Generate hasrdtaudit variable
outlet_data <- outlet_data %>%
  mutate(
    hasrdtaudit = case_when(
      (is.na(setofrdtaudit) | setofrdtaudit == "") & 
        ((is.na(eligible) | eligible == 0) | (is.na(consented) | consented != 1)) ~ 9888,
      (is.na(setofrdtaudit) | setofrdtaudit == "") & 
        (is.na(rdt_stock) | rdt_stock == 0) ~ 9888,
      (is.na(setofrdtaudit) | setofrdtaudit == "") & 
        (is.na(d7) | d7 == 0) ~ 9888,
      !is.na(setofrdtaudit) & setofrdtaudit != "" ~ 1,
      TRUE ~ 0
    )
  )

# Supplier links
outlet_data <- outlet_data %>%
  mutate(
    hasamsuppliers = case_when(
      (is.na(consented) | consented == 0) | (is.na(sa1a) | sa1a != 1) ~ 9888,
      !is.na(setofam_suppliers) & setofam_suppliers != "" ~ 1,
      TRUE ~ 0
    )
  )

# Drop unnecessary variables
vars_to_drop <- c("deviceid", "subscriberid", "simid", "phonenumber", 
                  "device_info", "instancename", "randomnum", "outletnum")
outlet_data <- outlet_data %>% select(-any_of(vars_to_drop))

################################################################################
# SECTION 5: INTERVIEWER COMMENTS CLEANING
################################################################################

standard_empty <- c("", "NONE", "NO COMMENT", "NO COMMENTS", "NO", 
                    "9998", "NA", "N/A", "N.A", "NONE.", "NOTHING", 
                    "NOTHING.", "SUCCESSFUL...", "SUCCESSFUL..", "SUCESSFUL",
                    "SUCESSFUL INTERVIEW", "NOT APPLICABLE", "SUCCESSFUL",
                    "SUCCESSFUL INTERVIEW", "SATISFACTORY")

outlet_data <- outlet_data %>%
  mutate(
    end3 = str_to_upper(end3),
    end3 = if_else(end3 %in% standard_empty, ".", end3),
    p_cmts = str_to_upper(p_cmts),
    p_cmts = if_else(p_cmts %in% c(standard_empty, "GOOD", "NO COMMENT(S)"), ".", p_cmts)
  )

################################################################################
# SECTION 6: CENSUS AND SCREENING MODULE
################################################################################

# GPS duplicate analysis
outlet_data <- outlet_data %>%
  group_by(gpslatitude, gpslongitude) %>%
  mutate(dup_gps = if_else(
    is.na(gpslatitude) | is.na(gpslongitude), 
    NA_real_, 
    n() - 1
  )) %>%
  ungroup()

# Remove temporary GPS variable and other unnecessary vars
outlet_data <- outlet_data %>% select(-dup_gps, -c7_r, -c7_ws)

# Outlet type labels
outlet_data <- outlet_data %>%
  mutate(
    c7_labels = case_when(
      c7 == 1  ~ "Clinic/hospital",
      c7 == 3  ~ "Laboratory",
      c7 == 11 ~ "CP",
      c7 == 20 ~ "Chemist/PPMV",
      c7 == 22 ~ "Retail/other shop",
      c7 == 25 ~ "Street vendor",
      c7 == 26 ~ "At home",
      c7 == 30 ~ "Importer",
      c7 == 31 ~ "Manufacturer",
      c7 == 32 ~ "Distributor",
      c7 == 96 ~ "Other",
      TRUE ~ as.character(c7)
    )
  )

################################################################################
# SECTION 7: FINAL INTERVIEW STATUS CREATION
################################################################################

# Create new status variable
outlet_data <- outlet_data %>%
  mutate(c9_new = c9) %>%
  relocate(c9_new, .after = consented)

# Define status labels
final_status_labels <- c(
  "1" = "Completed form",
  "2" = "Outlet does not meet screening criteria", 
  "104" = "Not screened: Respondent not available/Time not convenient",
  "106" = "Not screened: Outlet closed permanently",
  "107" = "Not screened: Other",
  "108" = "Not screened: Refused",
  "204" = "Not interviewed: Respondent not available/Time not convenient",
  "208" = "Not interviewed: Refused/did not consent",
  "303" = "Partial interview: Interview interrupted",
  "304" = "Partial interview: Respondent not available/time not convenient", 
  "307" = "Partial interview: Other",
  "308" = "Partial interview: Refused to continue",
  "309" = "Partial interview: AM audit incomplete",
  "310" = "Partial interview: RDT audit incomplete"
)

# STATUS RECODING
# Step 1: Not screened cases
outlet_data <- outlet_data %>%
  mutate(c9_new = if_else(is.na(c9_new) & canscreen == 0, 100, c9_new))

# Step 2: Recode based on reason for not screening  
outlet_data <- outlet_data %>%
  mutate(
    c9_new = case_when(
      c9_new == 100 & cantscreen == 1 ~ 106,  # Outlet closed permanently
      c9_new == 100 & cantscreen == 2 ~ 108,  # Refused
      c9_new == 100 & cantscreen %in% c(3, 4, 5, 6) ~ 104,  # Not available
      c9_new == 100 & cantscreen == 96 ~ 107,  # Other
      TRUE ~ c9_new
    )
  )

# Step 3: Not eligible
outlet_data <- outlet_data %>%
  mutate(c9_new = if_else(is.na(c9_new) & eligible == 0, 2, c9_new))

# Step 4: Not consenting
outlet_data <- outlet_data %>%
  mutate(c9_new = if_else(is.na(c9_new) & (is.na(consented) | consented != 1), 208, c9_new))

# Step 5: Respondent not available
outlet_data <- outlet_data %>%
  mutate(c9_new = if_else(c9_new == 99, 304, c9_new))

# Step 6: Refused to continue
outlet_data <- outlet_data %>%
  mutate(c9_new = if_else(c9_new == 97, 308, c9_new))

# Interview started
outlet_data <- outlet_data %>%
  mutate(outletint = case_when(
    prov_int == 1 & consented == 1 ~ 1,
    TRUE ~ 0
  )) %>%
  relocate(outletint, .after = c9)

# INTERVIEW INTERRUPTED - CHECKPOINTS
checkpoint_vars <- names(outlet_data)[str_starts(names(outlet_data), "checkpoint")]

outlet_data <- outlet_data %>%
  mutate(outletint_interrupted = 0)

if(length(checkpoint_vars) > 0) {
  outlet_data <- outlet_data %>%
    mutate(
      outletint_interrupted = ifelse(if_any(all_of(checkpoint_vars), is.na), 1, 0),
      outletint_interrupted = ifelse(outletint == 0, 0, outletint_interrupted)
    )
  
  # More nuanced handling of interrupted interviews
  outlet_data <- outlet_data %>%
    mutate(
      c9_new = case_when(
        outletint_interrupted == 1 & (is.na(c9_new) | c9_new == 1) ~ 303,
        TRUE ~ c9_new
      )
    )
}

# Final status
outlet_data <- outlet_data %>%
  mutate(
    finalIntStat = c9_new,
    finalintstat_labels = final_status_labels[as.character(finalIntStat)],
    # Add lowercase version for backward compatibility
    finalintstat = as.character(finalIntStat)
  ) %>%
  select(-finalIntStat)  # Remove camelCase version, keep only lowercase

################################################################################
# SECTION 8: PROVIDER CHARACTERISTICS
################################################################################

# Malaria training - any type
outlet_data <- outlet_data %>%
  mutate(
    char9_any = case_when(
      char9_2 == 1 | char9_3 == 1 ~ 1,
      TRUE ~ char9_1
    ),
    .after = char9_98
  )

# Storage conditions
outlet_data <- outlet_data %>%
  mutate(
    am_storage_OK = case_when(
      am_storage_1 == 1 & am_storage_2 == 1 & am_storage_3 == 0 ~ 1,
      TRUE ~ 0
    ),
    rdt_storage_OK = case_when(
      rdt_storage_1 == 1 & rdt_storage_2 == 1 & rdt_storage_3 == 0 ~ 1,
      TRUE ~ 0
    )
  )

################################################################################
# SECTION 9: INTERVIEW DURATION ANALYSIS
################################################################################


outlet_data <- outlet_data %>%
  mutate(
    duration = as.numeric(duration),
    duration_min = duration / 60
  )

# Calculate duration statistics (for logging only)
duration_stats <- list(
  negative = sum(outlet_data$duration_min < 0, na.rm = TRUE),
  very_short = sum(outlet_data$duration_min < 2 & outlet_data$duration_min >= 0, na.rm = TRUE),
  very_long = sum(outlet_data$duration_min > 120, na.rm = TRUE),
  missing = sum(is.na(outlet_data$duration_min))
)

################################################################################
# SECTION 10: GPS COORDINATE VALIDATION
################################################################################

outlet_data <- outlet_data %>%
  mutate(
    # Flag invalid GPS coordinates
    gps_invalid = case_when(
      is.na(gpslatitude) | is.na(gpslongitude) ~ NA,
      gpslatitude < -90 | gpslatitude > 90 ~ TRUE,
      gpslongitude < -180 | gpslongitude > 180 ~ TRUE,
      TRUE ~ FALSE
    ),
    # Flag missing GPS
    gps_missing = is.na(gpslatitude) | is.na(gpslongitude)
  )

################################################################################
# SECTION 12: SA4 DUMMY VARIABLES
################################################################################

# Create SA4 dummy variables
sa4_values <- unique(outlet_data$sa4[!is.na(outlet_data$sa4)])

if (length(sa4_values) > 0) {
  dummy_cols <- map(sa4_values, ~ {
    if_else(outlet_data$sa4 == .x, 1, 0, missing = NA_real_)
  }) %>%
    set_names(paste0("sa4_", sa4_values))
  
  outlet_data <- outlet_data %>% bind_cols(dummy_cols)
}

################################################################################
# SECTION 13: SPECIAL VALUE RECODING
################################################################################

outlet_data <- outlet_data %>%
  mutate(across(c(d4, d5, d6), ~ if_else(.x == 9998, -9888, .x)))

################################################################################
# SECTION 14: FINAL DATA CLEANING
################################################################################

# Standardize missing values
outlet_data <- outlet_data %>%
  mutate(
    across(where(is.numeric), ~ {
      x <- .
      x[x %in% c(-77, -88, -99)] <- NA
      x
    }),
    across(where(is.character), ~ ifelse(. %in% c("NA", "-77", "-88", "-99", ""), NA_character_, .))
  )

# Remove temporary variables that were only for logging
outlet_data <- outlet_data %>%
  select(-any_of(c("duration_sec", "gps_invalid", "gps_missing", 
                   "wholesale", "screened", "completed_interview", "consented_numeric")))

################################################################################
# SECTION 15: VARIABLE NAME STANDARDIZATION
################################################################################

# Final variable name cleaning
outlet_data <- outlet_data %>%
  rename_with(tolower) %>%
  rename(
    consent_group1checkpoint1 = `consent_group[1]/checkpoint1`,
    meta1instancename = `meta[1]/instancename`
  )

################################################################################
# SAVE CLEANED DATASET
################################################################################

output_file <- here("Data", "Cleaned data", paste0("AwL_", country, "_", year, "_outlet_clean.csv"))
fwrite(outlet_data, output_file, row.names = FALSE, na = "")

################################################################################
# GENERATE HTML LOG
################################################################################

# Helper function for creating summary tables
create_summary_table <- function(data, var) {
  var_enquo <- enquo(var)
  data %>%
    count(!!var_enquo) %>%
    mutate(percent = round(n/sum(n)*100, 1)) %>%
    arrange(desc(n))
}

# Helper function for HTML tables
html_table <- function(data, caption = "") {
  if (nrow(data) == 0) {
    cat("<p>No data available</p>\n")
    return()
  }
  
  cat("<table>\n")
  if (caption != "") {
    cat(paste0("<caption>", caption, "</caption>\n"))
  }
  
  # Header
  cat("<tr>")
  for (col in names(data)) {
    cat(paste0("<th>", col, "</th>"))
  }
  cat("</tr>\n")
  
  # Rows (limit to first 100 for performance)
  for (i in 1:min(nrow(data), 100)) {
    cat("<tr>")
    for (j in 1:ncol(data)) {
      cell_value <- data[[j]][i]
      if (is.na(cell_value)) cell_value <- "NA"
      cat(paste0("<td>", cell_value, "</td>"))
    }
    cat("</tr>\n")
  }
  
  if (nrow(data) > 100) {
    cat(glue("<tr><td colspan='{ncol(data)}'><em>... {nrow(data) - 100} more rows not shown</em></td></tr>\n"))
  }
  
  cat("</table>\n<br>\n")
}

# Setup log file
log_dir <- here("Data", "Cleaned data", "Data cleaning notes")
if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE)

log_file <- file.path(log_dir, paste0("AwL_", country, "_", year, "_outlet_clean_notes.html"))

# Initialize HTML file
cat('<!DOCTYPE html>
<html><head>
<title>Outlet Cleaning Log</title>
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

# =============================================================================
# HTML CONTENT GENERATION
# =============================================================================

cat("<h1>ACTwatch Lite Outlet Data Cleaning Log</h1>\n")
cat(glue("<div class='section'>
<h2>Cleaning Session Information</h2>
<p><strong>Date/Time:</strong> {Sys.time()}</p>
<p><strong>Country:</strong> {country}</p>
<p><strong>Year:</strong> {year}</p>
<p><strong>Script:</strong> Outlet Level Data Cleaning</p>
</div>\n"))

# 1. Data Dependency Verification
cat("<h2>1. Data Dependency Verification</h2>\n")
cat("<div class='section'>\n<p>All required dependency files found</p>\n</div>\n")

# 2. Data Import
cat("<h2>2. Data Import and Preparation</h2>\n")
cat(glue("<div class='section'>
<h3>2.1 Data Import Summary</h3>
<p><strong>Total observations imported:</strong> {nrow(outlet_data)}</p>
<p><strong>Total variables:</strong> {ncol(outlet_data)}</p>
</div>\n"))

cat("<div class='section'>
<h3>2.2 Field Type Conversion</h3>
<p>Automatic date/datetime/text field conversion completed</p>
<p>Text fields converted to uppercase and trimmed</p>
</div>\n")

# 3. Duplicate Detection
cat("<h2>3. Duplicate Detection and Key Variable Management</h2>\n")
cat("<h3>3.1 Duplicate Analysis</h3>\n")

if (nrow(duplicate_keys) > 0) {
  cat("<div class='section warning'>
  <h4>Duplicate Keys Found</h4>\n")
  html_table(head(duplicate_keys, 10), caption = "Top 10 Duplicate Keys")
  cat("</div>\n")
} else {
  cat("<div class='section'>\n<p>No duplicate keys found</p>\n</div>\n")
}

if (nrow(duplicate_outletids) > 0) {
  cat("<div class='section warning'>
  <h4>Duplicate Outlet IDs Found</h4>\n")
  html_table(head(duplicate_outletids, 10), caption = "Top 10 Duplicate Outlet IDs")
  cat("</div>\n")
} else {
  cat("<div class='section'>\n<p>No duplicate outlet IDs found</p>\n</div>\n")
}

# 4. Dataset Link Verification
cat("<h2>4. Dataset Link Verification</h2>\n")

cat("<h3>4.1 Antimalarial Audit Completeness</h3>\n")
html_table(create_summary_table(outlet_data, amaudit_complete), 
           caption = "Antimalarial Audit Completion Status")

html_table(create_summary_table(outlet_data, hasamaudit), 
           caption = "Has AM Audit Distribution")

cat("<h3>4.2 RDT Audit Completeness</h3>\n")
html_table(create_summary_table(outlet_data, rdtaudit_complete), 
           caption = "RDT Audit Completion Status")

html_table(create_summary_table(outlet_data, hasrdtaudit), 
           caption = "Has RDT Audit Distribution")

cat("<h3>4.3 AM Suppliers Completeness</h3>\n")
html_table(create_summary_table(outlet_data, hasamsuppliers), 
           caption = "Has AM Suppliers Distribution")

# 5. Interviewer Comments
cat("<h2>5. Interviewer Comments Review</h2>\n")

meaningful_end_comments <- outlet_data %>% 
  filter(end3 != "." & !is.na(end3)) %>%
  count(end3, sort = TRUE) %>%
  slice_head(n = 10)

meaningful_business_comments <- outlet_data %>% 
  filter(p_cmts != "." & !is.na(p_cmts)) %>%
  count(p_cmts, sort = TRUE) %>%
  slice_head(n = 10)

cat("<h3>5.1 End-of-Interview Comments</h3>\n")
if (nrow(meaningful_end_comments) > 0) {
  html_table(meaningful_end_comments, caption = "Top 10 Common End Comments")
} else {
  cat("<p>No common end-of-interview comments found.</p>\n")
}

cat("<h3>5.2 Business Practice Comments</h3>\n")
if (nrow(meaningful_business_comments) > 0) {
  html_table(meaningful_business_comments, caption = "Top 10 Common Business Comments")
} else {
  cat("<p>No common business practice comments found.</p>\n")
}

# 6. Data Quality Assurance - Metadata
cat("<h2>6. Data Quality Assurance - Metadata Review</h2>\n")

cat("<h3>6.1 Data Collection Date Distribution</h3>\n")
html_table(
  outlet_data %>% count(date, sort = TRUE) %>% 
    mutate(percent = round(n/sum(n)*100, 1)) %>% 
    slice_head(n = 10),
  caption = "Top 10 Collection Dates"
)

date_stats <- outlet_data %>%
  summarise(
    min_date = min(date, na.rm = TRUE),
    max_date = max(date, na.rm = TRUE),
    unique_dates = n_distinct(date, na.rm = TRUE),
    total_records = n()
  )

html_table(date_stats, caption = "Date Range Summary")

# 7. Census and Screening Module
cat("<h2>7. Census and Screening Module</h2>\n")

cat("<h3>7.1 Team Distribution</h3>\n")
html_table(create_summary_table(outlet_data, team), caption = "Data Collection Teams")

dc_by_team <- outlet_data %>%
  rename(enumerator_id = c1a) %>%
  group_by(team) %>%
  count(enumerator_id, sort = TRUE) %>%
  slice_head(n = 5) %>%
  ungroup() %>%
  arrange(team, desc(n))

cat("<h3>7.2 Data Collectors by Team</h3>\n")
html_table(dc_by_team, caption = "Top 5 Data Collectors per Team")

cat("<h3>7.3 Location Data Distribution</h3>\n")
for (loc_var in c("c2", "c3", "c4")) {
  loc_name <- c("c2" = "Region", "c3" = "District", "c4" = "Cluster")[loc_var]
  cat(glue("<h4>{loc_name} ({loc_var})</h4>\n"))
  html_table(
    outlet_data %>% count(.data[[loc_var]], sort = TRUE) %>% slice_head(n = 10),
    caption = paste("Top 10", loc_name)
  )
}

# GPS analysis
gps_dup_summary <- outlet_data %>%
  group_by(gpslatitude, gpslongitude) %>%
  mutate(dup_count = n()) %>%
  ungroup() %>%
  filter(!is.na(gpslatitude), !is.na(gpslongitude)) %>%
  count(dup_count, name = "freq") %>%
  mutate(percent = round(freq/sum(freq)*100, 1))

missing_gps_count <- sum(outlet_data$gps_missing, na.rm = TRUE)

cat("<h3>7.4 GPS Coordinate Duplicates</h3>\n")
html_table(gps_dup_summary, caption = "GPS Duplicate Frequency (excluding missing coordinates)")

if (missing_gps_count > 0) {
  cat(glue("<p><strong>Note:</strong> {missing_gps_count} outlets have missing GPS coordinates</p>\n"))
}

cat("<h3>7.5 Outlet Type Distribution</h3>\n")
html_table(create_summary_table(outlet_data, c7_labels), caption = "Outlet Types")

other_outlets <- outlet_data %>%
  filter(c7 == 96 & !is.na(c7_other) & c7_other != "") %>%
  count(c7_other, sort = TRUE)

if (nrow(other_outlets) > 0) {
  cat("<h4>7.5.1 'Other' Outlet Type Specifications</h4>\n")
  html_table(other_outlets, caption = "Other Outlet Type Responses")
}

# 8. Screening and Eligibility
cat("<h2>8. Screening and Eligibility Analysis</h2>\n")

cat("<h3>8.1 Screening Status</h3>\n")
html_table(create_summary_table(outlet_data, canscreen), caption = "Ability to Screen Outlets")

if (any(outlet_data$canscreen == 0, na.rm = TRUE)) {
  cat("<h4>8.1.1 Reasons for Not Screening</h4>\n")
  html_table(
    create_summary_table(outlet_data %>% filter(canscreen == 0), cantscreen),
    caption = "Reasons Outlets Could Not Be Screened"
  )
}

cat("<h3>8.2 Inclusion Criteria Results</h3>\n")
for (criteria in list(
  list(var = "am_stockcurrent", label = "AM Stock Current (s3/am_stockcurrent)"),
  list(var = "am_stockpast", label = "AM Stock Past (s4/am_stockpast)"),
  list(var = "rdt_stock", label = "RDT Stock (s5b/rdt_stock)"),
  list(var = "micro_stockcurrent", label = "Microscopy Current (s5a/micro_stockcurrent)"),
  list(var = "testing_stock", label = "Testing Stock (calculated)")
)) {
  if (criteria$var %in% names(outlet_data)) {
    cat(glue("<h4>{criteria$label}</h4>\n"))
    html_table(create_summary_table(outlet_data, .data[[criteria$var]]), caption = criteria$label)
  }
}

cat("<h3>8.3 Eligibility by Screening Status</h3>\n")
html_table(
  outlet_data %>%
    group_by(canscreen) %>%
    count(eligible, sort = TRUE) %>%
    mutate(percent = round(n/sum(n)*100, 1)) %>%
    ungroup(),
  caption = "Eligibility Distribution by Screening Status"
)

if (any(outlet_data$eligible == 1, na.rm = TRUE)) {
  cat("<h3>8.4 Consent Status (Eligible Outlets Only)</h3>\n")
  html_table(
    create_summary_table(outlet_data %>% filter(eligible == 1), consented),
    caption = "Consent Rates Among Eligible Outlets"
  )
}

cat("<h3>8.5 Study Component Eligibility</h3>\n")
for (comp_var in c("no_prov_int", "prov_int", "do_audit")) {
  if (comp_var %in% names(outlet_data)) {
    html_table(create_summary_table(outlet_data, .data[[comp_var]]), 
               caption = comp_var)
  }
}

cat("<h3>8.6 Wholesale/Resale Activities</h3>\n")
for (ws_var in c("retws1", "retws", "retws_confirmdk")) {
  if (ws_var %in% names(outlet_data)) {
    html_table(create_summary_table(outlet_data, .data[[ws_var]]), caption = ws_var)
  }
}

cat("<h4>8.6.1 Resale Activity by Outlet Type</h4>\n")
html_table(
  outlet_data %>%
    count(c7, retws) %>%
    group_by(c7) %>%
    mutate(percent = round(n/sum(n)*100, 1)) %>%
    ungroup() %>%
    arrange(c7, retws),
  caption = "Resale Distribution by Outlet Type"
)

# 9. Final Interview Status
cat("<h2>9. Final Interview Status</h2>\n")

cat("<h3>9.1 Final Status Distribution</h3>\n")
html_table(create_summary_table(outlet_data, finalintstat_labels), 
           caption = "Final Interview Status")

# 10. Interview Duration
cat("<h2>10. Interview Duration Analysis</h2>\n")

duration_summary <- outlet_data %>%
  summarise(
    mean_duration = round(mean(duration_min, na.rm = TRUE), 2),
    median_duration = round(median(duration_min, na.rm = TRUE), 2),
    min_duration = round(min(duration_min, na.rm = TRUE), 2),
    max_duration = round(max(duration_min, na.rm = TRUE), 2)
  )

cat(glue("<p><strong>Mean duration:</strong> {duration_summary$mean_duration} minutes</p>\n"))
cat(glue("<p><strong>Median duration:</strong> {duration_summary$median_duration} minutes</p>\n"))
cat(glue("<p><strong>Range:</strong> {duration_summary$min_duration} - {duration_summary$max_duration} minutes</p>\n"))

cat("<h3>10.1 Duration Issues Detected</h3>\n")
cat(glue("<p>Negative durations: {duration_stats$negative}</p>\n"))
cat(glue("<p>Very short interviews (&lt;2 min): {duration_stats$very_short}</p>\n"))
cat(glue("<p>Very long interviews (&gt;2 hours): {duration_stats$very_long}</p>\n"))
cat(glue("<p>Missing duration: {duration_stats$missing}</p>\n"))

# 11. GPS Validation
cat("<h2>11. GPS Coordinate Validation</h2>\n")

gps_summary <- outlet_data %>%
  summarise(
    total = n(),
    missing_gps = sum(is.na(gpslatitude) | is.na(gpslongitude)),
    invalid_gps = sum(
      (!is.na(gpslatitude) & (gpslatitude < -90 | gpslatitude > 90)) |
        (!is.na(gpslongitude) & (gpslongitude < -180 | gpslongitude > 180)),
      na.rm = TRUE
    ),
    valid_gps = sum(
      !is.na(gpslatitude) & !is.na(gpslongitude) & 
        gpslatitude >= -90 & gpslatitude <= 90 & 
        gpslongitude >= -180 & gpslongitude <= 180,
      na.rm = TRUE
    )
  )

cat(glue("<p><strong>Total outlets:</strong> {gps_summary$total}</p>\n"))
cat(glue("<p><strong>Missing GPS:</strong> {gps_summary$missing_gps}</p>\n"))
cat(glue("<p><strong>Invalid GPS:</strong> {gps_summary$invalid_gps}</p>\n"))
cat(glue("<p><strong>Valid GPS:</strong> {gps_summary$valid_gps}</p>\n"))

# 12. Final Dataset Summary
cat("<h2>12. Final Dataset Summary</h2>\n")

final_summary <- data.frame(
  Metric = c("Total Observations", "Total Variables", "Complete Cases",
             "Consented Participants", "Completed Interviews"),
  Count = c(
    nrow(outlet_data),
    ncol(outlet_data),
    sum(complete.cases(outlet_data)),
    sum(outlet_data$consented == 1, na.rm = TRUE),
    sum(outlet_data$finalintstat == "1", na.rm = TRUE)
  ),
  Percentage = c(
    100, NA,
    round(sum(complete.cases(outlet_data))/nrow(outlet_data)*100, 1),
    round(sum(outlet_data$consented == 1, na.rm = TRUE)/nrow(outlet_data)*100, 1),
    round(sum(outlet_data$finalintstat == "1", na.rm = TRUE)/nrow(outlet_data)*100, 1)
  )
)

html_table(final_summary, caption = "Final Dataset Summary Statistics")

quality_indicators <- data.frame(
  Indicator = c("Duplicate Keys", "Duplicate Outlet IDs", "Missing Final Status", 
                "Duration Issues", "GPS Issues"),
  Count = c(
    sum(duplicated(outlet_data$key) | duplicated(outlet_data$key, fromLast = TRUE)),
    sum(duplicated(outlet_data$outletid) | duplicated(outlet_data$outletid, fromLast = TRUE)),
    sum(is.na(outlet_data$finalintstat)),
    duration_stats$negative + duration_stats$very_short + duration_stats$very_long,
    gps_summary$invalid_gps + gps_summary$missing_gps
  ),
  Status = c(
    if_else(sum(duplicated(outlet_data$key) | duplicated(outlet_data$key, fromLast = TRUE)) > 0, "Review", "OK"),
    if_else(sum(duplicated(outlet_data$outletid) | duplicated(outlet_data$outletid, fromLast = TRUE)) > 0, "Review", "OK"),
    if_else(sum(is.na(outlet_data$finalintstat)) > 0, "Review", "OK"),
    if_else(duration_stats$negative + duration_stats$very_short + duration_stats$very_long > 0, "Review", "OK"),
    if_else(gps_summary$invalid_gps + gps_summary$missing_gps > 0, "Review", "OK")
  )
)

cat("<h3>12.1 Data Quality Indicators</h3>\n")
html_table(quality_indicators, caption = "Data Quality Assessment")

# Closing section
cat(glue("<div class='section'>
<h2>13. Cleaning Process Complete</h2>
<p><strong>Clean dataset saved to:</strong> {output_file}</p>
<p><strong>Final dataset dimensions:</strong> {nrow(outlet_data)} rows × {ncol(outlet_data)} columns</p>
<p><strong>Processing completed:</strong> {Sys.time()}</p>
</div>\n"))

sink()
cat("</body></html>", file = log_file, append = TRUE)
options(warn = 0)

################################################################################
# CONSOLE OUTPUT
################################################################################

cat("\n=== OUTLET CLEANING COMPLETE ===\n")
cat("Total outlet entries:", nrow(outlet_data), "\n")
cat("Clean dataset saved to:", output_file, "\n")
cat("Data cleaning log saved to:", log_file, "\n")
cat("\n=== PLEASE REVIEW DATA CLEANING LOG PRIOR TO RUNNING ADDITIONAL SCRIPTS ===\n")

console_issues <- character()

if (nrow(duplicate_keys) > 0) {
  console_issues <- c(console_issues, glue("{nrow(duplicate_keys)} duplicate keys found"))
}

if (nrow(duplicate_outletids) > 0) {
  console_issues <- c(console_issues, glue("{nrow(duplicate_outletids)} duplicate outlet IDs found"))
}

if (duration_stats$negative > 0) {
  console_issues <- c(console_issues, glue("{duration_stats$negative} interviews with negative duration"))
}
if (duration_stats$very_short > 0) {
  console_issues <- c(console_issues, glue("{duration_stats$very_short} interviews < 2 minutes"))
}
if (duration_stats$very_long > 0) {
  console_issues <- c(console_issues, glue("{duration_stats$very_long} interviews > 2 hours"))
}

if (length(console_issues) > 0) {
  cat("\nDATA ISSUES DETECTED:\n")
  for (issue in console_issues) cat("• ", issue, "\n")
  cat("\nPlease review the HTML log for detailed analysis.\n")
} else {
  cat("\nNo major data quality issues detected.\n")
}

# =============================================================================
# ####               END                   ####
# =============================================================================