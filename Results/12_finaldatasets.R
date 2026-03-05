################################################################################
# ACTwatch LITE 
# Step 2.12 Finalize and save datasets for results generation
################################################################################

# This script prepares and saves the final cleaned datasets for analysis. 
# It merges in supplier data, and generates survey-ready datasets at both 
# the full product level and the outlet level. It also creates RDT-only and 
# antimalarial-only subsets for specific analyses.

################################################################################
# SETUP
################################################################################

# Load the latest cleaned data
data_file <- here("Data", "Management data", paste0(country, "_", year, "_am_rdt_os_cleaned_long_wt_provider.csv"))
long_data <- fread(data_file)

# Create directories for final data storage
finaldata_dir <- here("Data", "Final data")
if (!dir.exists(finaldata_dir)) dir.create(finaldata_dir, recursive = TRUE)

mngmtdata_dir <- here("Data", "Management data")

################################################################################
# MERGE IN SUPPLIER DATASET
################################################################################

# # Load supplier dataset
# supplier_file <- here("Data", "Cleaned data", paste0("AwL_", country, "_", year, "_suppliers_clean.csv"))
# 
# if (!file.exists(supplier_file)) {
#   stop("Supplier file not found: ", supplier_file)
# }
# 
# supplier_data <- fread(supplier_file)
# 
# # # Process supplier data
# # supplier_data <- supplier_data %>%
# #   select(-any_of("key")) %>%
# #   rename(key = supplierkey)
# 
# # Check for duplicates
# supplier_duplicates <- supplier_data %>%
#   count(key) %>%
#   filter(n > 1)
# 
# # if (nrow(supplier_duplicates) > 0) {
# #   cat("WARNING: Duplicate keys found in supplier data:\n")
# #   print(supplier_duplicates)
# # }
# 
# # Merge supplier data (only those variables not already in long_data)
# long_data <- long_data %>%
#   left_join(supplier_data %>% select(key, all_of(setdiff(names(supplier_data), names(long_data))), -supplierkey), 
#             by = "key", relationship = "many-to-many")

################################################################################
# SAVE FULL DATA SET
################################################################################

# Save full dataset for final storage (final data)
full_data_final <- file.path(finaldata_dir, paste0(country, "_", year, "_full_data.csv"))
fwrite(long_data %>%   rename_with(tolower), full_data_final, row.names = FALSE, na = "")

################################################################################
# SAVE OUTLET-LEVEL DATA SET
################################################################################

# Create outlet-level dataset (keep 1 observation from each outlet)
outlet_data <- long_data %>%
  filter(nOut == 1) %>% 
  rename_with(tolower)

# Save outlet dataset
outlet_data_mgmt <- file.path(mngmtdata_dir, paste0(country, "_", year, "_outlet_data.csv"))
outlet_data_final <- file.path(finaldata_dir, paste0(country, "_", year, "_outlet_data.csv"))

fwrite(outlet_data, outlet_data_mgmt, row.names = FALSE, na = "")
fwrite(outlet_data, outlet_data_final, row.names = FALSE, na = "")

################################################################################
# SAVE RDT DATA SET
################################################################################

# Create RDT-only dataset
rdt_data <- long_data %>%
  filter(producttype == 3) %>%
  mutate(qardt = replace_na(qardt, 0)) %>% 
  rename_with(tolower)

# Save RDT dataset
rdt_data_mgmt <- file.path(mngmtdata_dir, paste0(country, "_", year, "_rdt_data.csv"))
rdt_data_final <- file.path(finaldata_dir, paste0(country, "_", year, "_rdt_data.csv"))

fwrite(rdt_data, rdt_data_mgmt, row.names = FALSE, na = "")
fwrite(rdt_data, rdt_data_final, row.names = FALSE, na = "")

################################################################################
# SAVE RDT_MICRO_DATA (COMBINED DIAGNOSTICS DATASET)
################################################################################

# Start with microscopy data (1 observation per outlet)
diag_data <- long_data %>%
  filter(nOut == 1, vf_micro == 1) %>%
  rename_with(tolower) %>%
  mutate(
    across(c(rdt_true, starts_with("rdtmanu_"), st_rdt, st_qardt, 
             starts_with("st_rdtmanu_"), vd_rdt, vf_rdt), ~ifelse(.x == 1, 0, .x)),
    test_type = 1
  )

# Add RDT data if it exists
if (exists("rdt_data") && nrow(rdt_data) > 0) {
  rdt_data_temp <- rdt_data %>% 
    rename_with(tolower) %>%
    mutate(test_type = 2)
  diag_data <- bind_rows(diag_data, rdt_data_temp)
}

# Generate volume and test type flags
diag_data <- diag_data %>%
  mutate(
    d4 = as.numeric(d4),
    r13 = as.numeric(r13),
    volume = case_when(test_type == 1 ~ d4, test_type == 2 ~ r13, TRUE ~ NA_real_),
    volume = if_else(volume %in% c(99998, 998, 99), NA_real_, volume),
    total = 1,
    microscopy = if_else(test_type == 1, 1, NA_real_),
    rdt = if_else(test_type == 2, 1, NA_real_)
  )

# Save combined diagnostics dataset
rdt_micro_mgmt <- file.path(mngmtdata_dir, paste0(country, "_", year, "_rdt_micro_data.csv"))
rdt_micro_final <- file.path(finaldata_dir, paste0(country, "_", year, "_rdt_micro_data.csv"))
fwrite(diag_data, rdt_micro_mgmt, row.names = FALSE, na = "")
fwrite(diag_data, rdt_micro_final, row.names = FALSE, na = "")

################################################################################
# SAVE ANTIMALARIAL DATA SET
################################################################################

# Create antimalarial-only dataset
am_data <- long_data %>%
  filter(producttype %in% c(1, 2)) %>%
  mutate(qardt = replace_na(qardt, 0)) %>% 
  rename_with(tolower)

# Save antimalarial dataset
am_data_final <- file.path(finaldata_dir, paste0(country, "_", year, "_antimalarial_data.csv"))
fwrite(am_data, am_data_final, row.names = FALSE, na = "")

################################################################################
# FINAL SUMMARY
################################################################################

cat("\n=== FINAL DATASETS CREATION COMPLETE ===\n")

cat("\nDatasets created:\n")
cat(sprintf("  Full dataset: %d observations\n", nrow(long_data)))
cat(sprintf("  Outlet dataset: %d outlets\n", nrow(outlet_data)))
cat(sprintf("  RDT dataset: %d products\n", nrow(rdt_data)))
cat(sprintf("  Antimalarial dataset: %d products\n", nrow(am_data)))

cat("\nFinal data (for analysis):\n")
cat(sprintf("  %s\n", basename(full_data_final)))
cat(sprintf("  %s\n", basename(outlet_data_final)))
cat(sprintf("  %s\n", basename(rdt_data_final)))
cat(sprintf("  %s\n", basename(am_data_final)))

cat("\nAll datasets successfully created and saved\n")

################################################################################
# END
################################################################################