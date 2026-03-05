################################################################################
# ACTWATCH LITE 
# MASTER R SCRIPT 
################################################################################
# The purpose of this R script is to set up the ACTwatch analysis for this 
# country/implementation and then run all subsequent R scripts to prep, clean, 
# and manage market survey data then output tables

# The following flags are used throughout the syntax:
# $$$ = Breaks the script to remind analyst to modify syntax
# # EXAMPLE: = Sample syntax from pilot studies for reference
# Please initial all comments/responses and make note of changes

# NOTE ALL R SCRIPTS CALLED HERE REQUIRE MANUAL INPUTS OR EDITS BEFORE FINALIZING
# WITHIN EACH R SCRIPT CALLED HERE, REVIEW LINES OF SYNTAX MARKED WITH "$$$"
# MANAGE/CLEAN DATA ACCORDINGLY
# ONCE ALL $$$ ARE ADDRESSED IN ALL R SCRIPTS -> 
# THIS MASTERFILE CAN BE USED TO RUN DATA CLEANING, MANAGEMENT, THEN PRODUCE OUTPUT TABLES

# STEPS: 
# SECTION A - Basic set-up
#   *INITIALIZE R
#   **INITIATE FORM-SPECIFIC PARAMETERS 
#   *SET USER AND FILEPATHWAYS 

# SECITON B - Execute R scripts 
#   *0  SET UP
#   *1	CLEANING
#   *2	MANAGEMENT 
#   *3 	RESULTS 

# See the data analysis guidelines for a more detailed description and 
# instructions for specific steps

################################################################################
#######################      SECTION A      ##################################

# INITIALIZE R
################################################################################
# Clear environment
rm(list = ls())
while(sink.number() > 0) sink()


#NEW 
#Install packages (this only needs to be run once)
#install.packages(c("tidyverse", "readxl", "writexl", "srvyr",  "purrr", "here", "fs",  "labelled",  "data.table",  "glue",  "rmarkdown",  "knitr", "rio", "openxlsx"))

# Load required packages
library(tidyverse)
library(readxl)
library(writexl)
library(srvyr)
library(purrr)
library(here)
library(fs)
library(labelled)
library(data.table)
library(writexl)
library(glue)

# Set options
options(width = 250)
options(dplyr.width = Inf)

# INITIATE FORM-SPECIFIC PARAMETERS 
################################################################################
# $$$ Set specific global parameters for this study. Ensure that the [country] 
# and [year] for your study are included below:

# EXAMPLE:
# country <- "NGA"
# year <- 2024

country <- ""
year <- 


################################################################################
#######################      SECTION B      ##################################

# $$$ The syntax below can be used to run all other R scripts for the data 
# cleaning and analysis required for assessment of the core indicators for 
# ACTwatch Lite included in the toolkit. Each R script requires review and 
# manual inputs/edits where indicated (with "$$$")

## STEP 0	SET UP
################################################################################
# The purpose of these R scripts are to perform additional set up for the 
# ACTwatch Lite analysis including setting variables, running functions, and 
# creating data frames of product lists


# 0.1 Functions
source(here("Scripts", "00_functions.R"))


## STEP 1	CLEANING
################################################################################
# These R scripts prepare raw data, converting .csv to data frames, adding 
# variable labels, checking unique identifiers, joining and merging data

# 1.1 product lists 
# Creates cleaned product lists (antimalarial and RDTs) used alongside the quantitative ODK form
# This script should not require edits 
# This script only needs to be run once
source(here("Scripts", "1_Cleaning", "01_product_list_cleaning.R"))


# 1.2 outlet data
# Prepares and cleans outlet level data 
# Requires manual cleaning of each variable 
source(here("Scripts", "1_Cleaning", "02_outlet_cleaning.R"))

# 1.3 antimalarial cleaning
# Prepares and cleans antimalarial audit data 
# Requires manual review of data merges and cleaning of each variable 
source(here("Scripts", "1_Cleaning", "03_antimalarial_cleaning.R"))

# 1.4 RDT cleaning
# Prepares and cleans RDT audit data 
# Requires manual review of data merges and cleaning of each variable 
source(here("Scripts", "1_Cleaning", "04_rdt_cleaning.R"))

# 1.5 supplier data 
# Prepares and cleans reported supplier information  
# Requires manual review of data merges and cleaning of each variable 
# If this implementation is using reported supplier information to inform data 
# collection at higher levels of the supply chain, this cleaning should be done 
# **during** data collection 
source(here("Scripts", "1_Cleaning", "05_supplier_cleaning.R"))

## STEP 2	DATA MANAGEMENT
################################################################################
# These R scripts merge data into one analysis dataset, weight data, and add 
# variables needed for results generation

# 2.1 long dataset
# Merges cleaned outlet and product data into long format analysis datasets
# Creates separate antimalarial and RDT product-level datasets
source(here("Scripts", "2_Management", "01_long_dataset.R"))

# 2.2 weights
# Calculates survey weights including outlet weights, market share weights, and FPC
# Applies sampling design parameters to create analysis weights
source(here("Scripts", "2_Management", "02_weights.R"))

# 2.3 denominators
# Creates denominator variables for availability and stocking indicators
# Generates outlet-level and product-level denominator flags
source(here("Scripts", "2_Management", "03_denoms.R"))

# 2.4 outlet categories
# Creates outlet type classification variables and aggregated outlet categories
# Defines retail/wholesale categories and outlet type groupings
source(here("Scripts", "2_Management", "04_outlet_categories.R"))

# 2.5 blood test categories
# Creates diagnostic/blood test product category variables (RDT, microscopy, quality)
# Defines variables for diagnostic product analysis
source(here("Scripts", "2_Management", "05_bloodtest_categories.R"))

# 2.6 antimalarial categories
# Creates antimalarial product category variables (ACT types, formulations, quality)
# Defines variables for antimalarial product analysis including QA indicators
source(here("Scripts", "2_Management", "06_antimalarial_categories.R"))

# 2.7 stocking
# Creates stocking and availability indicator variables
# Defines stockout indicators and product availability flags
source(here("Scripts", "2_Management", "07_stocking.R"))

# 2.8 AETD
# Calculates Adult Equivalent Treatment Doses (AETDs) for antimalarial products
# Standardizes antimalarial volumes to AETD units for comparability
source(here("Scripts", "2_Management", "08_aetd.R"))

# 2.9 macro and prices
# Adds macro-level data (exchange rates, inflation) and processes price variables
# Converts prices to USD and adjusts for purchasing power parity
source(here("Scripts", "2_Management", "09_macro_and_prices.R"))

# 2.10 volume
# Calculates sales volume variables for antimalarial and diagnostic products
# Processes reported sales data and creates volume indicators
source(here("Scripts", "2_Management", "10_volume.R"))
#NEW
#commented out lines 91-107 that were calling a dataflie that does not exist 
#Error: No such file: C:/Users/asecor/OneDrive - Population Services International/Documents/Projects/Misc/ACTWatch Stata to R (old version)/Existing files (Stata .do etc)/12 Analysis syntax/data/2_MANAGEMENT/TST_2040_am_rdt_os_cleaned_long_wt.dta

# 2.11 provider
# Processes provider/prescriber information and related variables
# Creates variables for provider type and prescribing patterns
source(here("Scripts", "2_Management", "11_provider.R"))

# 2.12 final datasets
# Creates final analysis datasets with all variables and applies final filters
# Outputs clean datasets ready for indicator calculation
source(here("Scripts", "2_Management", "12_finaldatasets.R"))

# 2.13 sensitivity analyses
# Runs sensitivity analyses on key indicators with alternative specifications
# Tests robustness of results to methodological choices
source(here("Scripts", "2_Management", "13_sensitivity_analyses.R"))

## STEP 3	RESULTS TABLES
################################################################################
# These R Markdown scripts calculate and output results tables for each core indicator
# Each script reads the final analysis datasets, applies survey design, calculates 
# estimates, and outputs results to Excel and Word documents

# 1.1 Market Composition among antimalarial-stocking outlets
rmd_file <- here("Scripts", "3_Analysis", "Indicator 1.1.Rmd")
info <- extract_indicator_info(rmd_file)
rmarkdown::render(
  rmd_file,
  params = list(country = country, year = year),
  output_file = paste0("Indicator ",info$indicator_id, "_", country, "_", year, ".docx"),
  output_dir = here("Results", paste("Indicator", info$indicator_id))
)


# 1.2 Market Composition among outlets with malaria blood testing
rmd_file <- here("Scripts", "3_Analysis", "Indicator 1.2.Rmd")
info <- extract_indicator_info(rmd_file)
rmarkdown::render(
  rmd_file,
  params = list(country = country, year = year),
  output_file = paste0("Indicator ",info$indicator_id, "_", country, "_", year, ".docx"),
  output_dir = here("Results", paste("Indicator", info$indicator_id))
)

# 2.1 Availability of antimalarial types in all screened outlets
rmd_file <- here("Scripts", "3_Analysis", "Indicator 2.1.Rmd")
info <- extract_indicator_info(rmd_file)
rmarkdown::render(
  rmd_file,
  params = list(country = country, year = year),
  output_file = paste0("Indicator ",info$indicator_id, "_", country, "_", year, ".docx"),
  output_dir = here("Results", paste("Indicator", info$indicator_id))
)

# 2.2 Availability of antimalarial types in all antimalarial-stocking outlets
rmd_file <- here("Scripts", "3_Analysis", "Indicator 2.2.Rmd")
info <- extract_indicator_info(rmd_file)
rmarkdown::render(
  rmd_file,
  params = list(country = country, year = year),
  output_file = paste0("Indicator ",info$indicator_id, "_", country, "_", year, ".docx"),
  output_dir = here("Results", paste("Indicator", info$indicator_id))
)

# 2.3 Availability of malaria blood testing in all screened outlets
rmd_file <- here("Scripts", "3_Analysis", "Indicator 2.3.Rmd")
info <- extract_indicator_info(rmd_file)
rmarkdown::render(
  rmd_file,
  params = list(country = country, year = year),
  output_file = paste0("Indicator ",info$indicator_id, "_", country, "_", year, ".docx"),
  output_dir = here("Results", paste("Indicator", info$indicator_id))
)

# 2.4 Availability of malaria blood testing in all antimalarial-stocking outlets
rmd_file <- here("Scripts", "3_Analysis", "Indicator 2.4.Rmd")
info <- extract_indicator_info(rmd_file)
rmarkdown::render(
  rmd_file,
  params = list(country = country, year = year),
  output_file = paste0("Indicator ",info$indicator_id, "_", country, "_", year, ".docx"),
  output_dir = here("Results", paste("Indicator", info$indicator_id))
)

# 3.1 Median sales volume of antimalarial AETDs
rmd_file <- here("Scripts", "3_Analysis", "Indicator 3.1.Rmd")
info <- extract_indicator_info(rmd_file)
rmarkdown::render(
  rmd_file,
  params = list(country = country, year = year),
  output_file = paste0("Indicator ",info$indicator_id, "_", country, "_", year, ".docx"),
  output_dir = here("Results", paste("Indicator", info$indicator_id))
)

# 3.2 Median sales volume of antimalarial AETDs among outlets with any sales of that antimalarial type
rmd_file <- here("Scripts", "3_Analysis", "Indicator 3.2.Rmd")
info <- extract_indicator_info(rmd_file)
rmarkdown::render(
  rmd_file,
  params = list(country = country, year = year),
  output_file = paste0("Indicator ",info$indicator_id, "_", country, "_", year, ".docx"),
  output_dir = here("Results", paste("Indicator", info$indicator_id))
)

# 3.3 Median sales volume of malaria blood tests
rmd_file <- here("Scripts", "3_Analysis", "Indicator 3.3.Rmd")
info <- extract_indicator_info(rmd_file)
rmarkdown::render(
  rmd_file,
  params = list(country = country, year = year),
  output_file = paste0("Indicator ",info$indicator_id, "_", country, "_", year, ".docx"),
  output_dir = here("Results", paste("Indicator", info$indicator_id))
)

# 3.4 Median sales volume of malaria blood tests among outlets with any sales of that test type
rmd_file <- here("Scripts", "3_Analysis", "Indicator 3.4.Rmd")
info <- extract_indicator_info(rmd_file)
rmarkdown::render(
  rmd_file,
  params = list(country = country, year = year),
  output_file = paste0("Indicator ",info$indicator_id, "_", country, "_", year, ".docx"),
  output_dir = here("Results", paste("Indicator", info$indicator_id))
)

# 4.1 Market share of antimalarials
rmd_file <- here("Scripts", "3_Analysis", "Indicator 4.1.Rmd")
info <- extract_indicator_info(rmd_file)
rmarkdown::render(
  rmd_file,
  params = list(country = country, year = year),
  output_file = paste0("Indicator ",info$indicator_id, "_", country, "_", year, ".docx"),
  output_dir = here("Results", paste("Indicator", info$indicator_id))
)

# 4.2 Market share of malaria blood testing overall
rmd_file <- here("Scripts", "3_Analysis", "Indicator 4.2.Rmd")
info <- extract_indicator_info(rmd_file)
rmarkdown::render(
  rmd_file,
  params = list(country = country, year = year),
  output_file = paste0("Indicator ",info$indicator_id, "_", country, "_", year, ".docx"),
  output_dir = here("Results", paste("Indicator", info$indicator_id))
)

# 4.3 Market share of antimalarials by brand and manufacturer
rmd_file <- here("Scripts", "3_Analysis", "Indicator 4.3.Rmd")
info <- extract_indicator_info(rmd_file)
rmarkdown::render(
  rmd_file,
  params = list(country = country, year = year),
  output_file = paste0("Indicator ",info$indicator_id, "_", country, "_", year, ".docx"),
  output_dir = here("Results", paste("Indicator", info$indicator_id))
)

# 5.1a Sales price of antimalarial tablet AETDs to customers
rmd_file <- here("Scripts", "3_Analysis", "Indicator 5.1a.Rmd")
info <- extract_indicator_info(rmd_file)
rmarkdown::render(
  rmd_file,
  params = list(country = country, year = year),
  output_file = paste0("Indicator ",info$indicator_id, "_", country, "_", year, ".docx"),
  output_dir = here("Results", paste("Indicator", info$indicator_id))
)

# 5.2a Sales price of pre-packaged ACTs to customer
rmd_file <- here("Scripts", "3_Analysis", "Indicator 5.2a.Rmd")
info <- extract_indicator_info(rmd_file)
rmarkdown::render(
  rmd_file,
  params = list(country = country, year = year),
  output_file = paste0("Indicator ",info$indicator_id, "_", country, "_", year, ".docx"),
  output_dir = here("Results", paste("Indicator", info$indicator_id))
)

# 5.3 Sales price of malaria blood testing to customers
rmd_file <- here("Scripts", "3_Analysis", "Indicator 5.3a.Rmd")
info <- extract_indicator_info(rmd_file)
rmarkdown::render(
  rmd_file,
  params = list(country = country, year = year),
  output_file = paste0("Indicator ",info$indicator_id, "_", country, "_", year, ".docx"),
  output_dir = here("Results", paste("Indicator", info$indicator_id))
)

# 6.1b Purchase price of antimalarial AETDs from suppliers
rmd_file <- here("Scripts", "3_Analysis", "Indicator 6.1b.Rmd")
info <- extract_indicator_info(rmd_file)
rmarkdown::render(
  rmd_file,
  params = list(country = country, year = year),
  output_file = paste0("Indicator ",info$indicator_id, "_", country, "_", year, ".docx"),
  output_dir = here("Results", paste("Indicator", info$indicator_id))
)

# 6.2 Purchase price of malaria RDTs from suppliers
rmd_file <- here("Scripts", "3_Analysis", "Indicator 6.2b.Rmd")
info <- extract_indicator_info(rmd_file)
rmarkdown::render(
  rmd_file,
  params = list(country = country, year = year),
  output_file = paste0("Indicator ",info$indicator_id, "_", country, "_", year, ".docx"),
  output_dir = here("Results", paste("Indicator", info$indicator_id))
)

# 7.1 & 7.2 Stock outs of antimalarials and RDTs
rmd_file <- here("Scripts", "3_Analysis", "Indicator 7.1 & 7.2.Rmd")
info <- extract_indicator_info(rmd_file)
rmarkdown::render(
  rmd_file,
  params = list(country = country, year = year),
  output_file = paste0("Indicator ",info$indicator_id, "_", country, "_", year, ".docx"),
  output_dir = here("Results", paste("Indicator", info$indicator_id))
)
