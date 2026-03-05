################################################################################
# ACTwatch LITE 
# Step 1.1 Product list cleaning
################################################################################

# Product lists were used to search through a list of known antimalarial and 
# RDTs during data collection instead of manually entering all product 
# information. This will be used to merge/fix any errors amended 
# during data collection and to determine which products are PQ.

# THIS ONLY NEEDS TO BE RUN ONCE

# This R script prepares product lists for antimalarials and rapid diagnostic 
# tests (RDTs) by reading CSV files, cleaning variable names, standardizing text 
# to uppercase, and renaming ID variables (amcode for antimalarials and rdtcode 
# for RDTs). It checks for duplicate IDs and counts observations before saving 
# cleaned datasets to specified directories. The script ensures standardized and 
# deduplicated master lists for further data validation or analysis.

# The following flags are used throughout the syntax:
# $$$ = Breaks the script to remind analyst to modify syntax
# # EXAMPLE: = Sample syntax from pilot studies for reference
# Please initial all comments/responses and make note of changes

# NOTE 
# *EACH* STEP FLAGGED WITH "$$$" HAS SECTIONS WHERE MANUAL INPUTS OR EDITS ARE REQUIRED
# REVIEW LINES OF SYNTAX MARKED WITH "$$$". MANAGE/CLEAN DATA ACCORDINGLY


################################################################################
# STEP 0.3 - PREPARE PRODUCT LISTS
################################################################################

# ANTIMALARIALS 
################################################################################
# CONVERT CSV TO DATA FRAME	
cat("Processing antimalarial masterlist...\n")

# Construct antimalarial masterlist filename dynamically
am_masterlist_file <- paste0("AwL Antimalarial Masterlist-", country, "-", year, ".csv")

antimalarial_data <- fread(
  here("Data", "Product lists", am_masterlist_file)) %>% 
  filter(product_id!="" & !is.na(product_id))

# CLEAN
# Rename all variables with _master suffix
antimalarial_data <- antimalarial_data %>%
  rename_with(~ paste0(.x, "_master"), .cols = everything())

# Convert all character variables to uppercase
antimalarial_data <- antimalarial_data %>%
  mutate(across(where(is.character), toupper))

# Rename identification ID variable in the antimalarial dataset to amcode
antimalarial_data <- antimalarial_data %>%
  rename(amcode = product_id_master)

# Clean the data
antimalarial_data = antimalarial_data %>% 
  mutate(across(everything(), ~ ifelse(.x == ".", NA, .x))) %>% 
  rename_with(tolower)

# Output the antimalarial masterlist to .csv
fwrite(antimalarial_data, file = here("Data", "Product lists", "antimalarial_masterlist_clean.csv"),
          row.names=F, na="")


# RDTs
################################################################################
# CONVERT CSV TO DATA FRAME
# EXAMPLE:
# rdt_data <- read_csv(file.path(prodlistdir, "rdt_masterlist.csv"))

cat("Processing RDT masterlist...\n")

# Construct RDT masterlist filename dynamically
rdt_masterlist_file <- paste0("AwL RDT Masterlist-", country, "-", year, ".csv")

rdt_data <- fread(
  here("Data", "Product lists", rdt_masterlist_file)) %>% 
  filter(product_id!="" & !is.na(product_id))


# CLEAN
# Rename all variables with _master suffix 
rdt_data <- rdt_data %>%
  rename_with(~ paste0(.x, "_master"), .cols = everything())

# Convert all character variables to uppercase
rdt_data <- rdt_data %>%
  mutate(across(where(is.character), toupper))

# Rename identification ID variable in the RDT dataset to rdtcode
rdt_data <- rdt_data %>%
  rename(rdtcode = product_id_master)


# final cleaning
rdt_data = rdt_data %>% 
  mutate(across(everything(), ~ ifelse(.x == ".", NA, .x))) %>% 
  rename_with(tolower)

# SAVE
fwrite(rdt_data, file = here("Data", "Product lists", "rdt_masterlist_clean.csv"),
          row.names=F, na="")

################################################################################
# CONSOLE OUTPUT
################################################################################
cat("\n=== PRODUCT LISTS PROCESSING COMPLETE ===\n")

## Antimalarial output summary
cat("Antimalarial records processed:", nrow(antimalarial_data), "\n")

duplicates_am <- antimalarial_data %>%
  filter(duplicated(amcode) | duplicated(amcode, fromLast = TRUE)) %>%
  arrange(amcode)

if (nrow(duplicates_am) > 0) {
  cat("Warning: Found", nrow(duplicates_am), "duplicate amcode entries:\n")
  print(duplicates_am %>% select(amcode))
} else {
  cat("No duplicate amcode entries found.\n")
}

## RDT output summary
cat("RDT records processed:", nrow(rdt_data), "\n")

# Check and identify duplicates in rdtcode
duplicates_rdt <- rdt_data %>%
  filter(duplicated(rdtcode) | duplicated(rdtcode, fromLast = TRUE)) %>%
  arrange(rdtcode)

if (nrow(duplicates_rdt) > 0) {
  cat("Warning: Found", nrow(duplicates_rdt), "duplicate rdtcode entries:\n")
  print(duplicates_rdt %>% select(rdtcode))
} else {
  cat("No duplicate rdtcode entries found.\n")
}

# Output summary
cat("Files saved to folder:", "Product lists", "\n")

#########################
######	END 		#####
#########################