################################################################################
# ACTwatch LITE 
# Step 2.9 Generate Price Variables
# REORGANIZED VERSION - HTML logging at end
################################################################################

################################################################################
# SET MACRO VALUES - USERS MUST MODIFY THESE!
################################################################################

# $$$ CRITICAL USER INPUT REQUIRED $$$
# These values must be updated for each country and survey round
# based on World Bank data and your data collection period

# Example macros - USERS MUST MODIFY THESE VALUES
cpiFIRST <- 1.1786345   # inflation rate since first year of time series
cpiCURRENT <- 0.997     # inflation rate for year of data collection  
exFIRST <- 610.12       # exchange rate for first year in time series
exCURRENT <- 574.02     # exchange rate for year of data collection
exDC <- 582.19          # exchange rate for data collection period

################################################################################
# INTRODUCTION
################################################################################
#
# This script generates comprehensive price and markup variables for antimalarials,
# RDTs, and microscopy services, enabling standardized price comparisons across
# time periods, currencies, and product formulations. Price calculations proceed
# through multiple steps: (1) calculating unit and package prices in local currency
# during data collection period, (2) converting to USD using period-specific
# exchange rates, (3) adjusting for inflation to enable time-series comparisons,
# (4) computing AETD-adjusted prices to standardize across different package sizes,
# (5) calculating percentage markups by comparing retail to wholesale/supplier
# prices, and (6) generating diagnostic test prices (microscopy and RDT) with
# parallel currency conversions. All price variables are created in multiple
# currencies and time adjustments: DC LCU (data collection local currency),
# FIRST (inflation-adjusted to baseline year), CURRENT (current year), and USD
# equivalents at different points. The script requires user specification of
# macro values (CPI and exchange rates) at the top and includes quality checks
# for extreme values, negative markups, and currency conversion validation.
#
# The following flags are used throughout the syntax:
# $$$ = Breaks the script to remind analyst to modify syntax
# # EXAMPLE: = Sample syntax from pilot studies for reference

################################################################################
# SECTION 1: LOAD DATA
################################################################################

long_data <- fread(here("Data", "Management data", 
                        paste0(country, "_", year, "_am_rdt_os_cleaned_long_wt_aetd.csv")))

# SAVE FOR LOGGING
initial_records <- nrow(long_data)

################################################################################
# SECTION 2: ANTIMALARIAL SELLING PRICES
################################################################################

# Calculate base prices
long_data <- long_data %>%
  mutate(
    # 1. Per unit, DC LCU
    unitPrice = if_else(!is.na(rts_price) & !is.na(size) & 
                          rts_price < 99995 & size < 9999 & rts_price > 0,
                        rts_price / size, NA_real_),
    unitPrice = if_else(is.infinite(unitPrice), NA_real_, unitPrice),
    
    # 2. Per package, DC LCU
    packagePrice = if_else(!is.na(size) & !is.na(unitPrice), 
                           unitPrice * size, NA_real_),
    
    # 3-4. Per package, USD conversions
    usppriceperpackageDC = packagePrice / exDC,
    usppriceperpackageFIRST = packagePrice * (1 / cpiFIRST) / exFIRST,
    
    # 5. Per AETD, DC LCU
    ppriceperaetd = if_else(!is.na(packagePrice) & !is.na(packageaetd) & packageaetd != 0,
                            packagePrice / packageaetd, NA_real_),
    
    # 6-9. Per AETD, various conversions
    ppriceperaetdFIRST = ppriceperaetd * (1 / cpiFIRST),
    usppriceperaetdDC = ppriceperaetd / exDC,
    usppriceperaetdCURRENT = ppriceperaetd / exCURRENT,
    usppriceperaetdFIRST = ppriceperaetdFIRST / exFIRST
  )

################################################################################
# SECTION 3: MICROSCOPY PRICES
################################################################################

long_data <- long_data %>%
  mutate(
    # Base prices
    pptestAdultTotal = if_else(!is.na(d5) & !d5 %in% c(-9555, -9777, -9888), d5, NA_real_),
    pptestChildTotal = if_else(!is.na(d6) & !d6 %in% c(-9555, -9777, -9888), d6, NA_real_),
    
    # Conversions - Adult
    pptestAdultTotalFIRST = pptestAdultTotal * (cpiFIRST / cpiCURRENT),
    uspptestAdultTotalDC = pptestAdultTotal / exDC,
    uspptestAdultTotalCURRENT = pptestAdultTotal / exCURRENT,
    uspptestAdultTotalFIRST = pptestAdultTotalFIRST / exFIRST,
    
    # Conversions - Child
    pptestChildTotalFIRST = pptestChildTotal * (cpiFIRST / cpiCURRENT),
    uspptestChildTotalDC = pptestChildTotal / exDC,
    uspptestChildTotalCURRENT = pptestChildTotal / exCURRENT,
    uspptestChildTotalFIRST = pptestChildTotalFIRST / exFIRST
  )

################################################################################
# SECTION 4: RDT PRICES
################################################################################

long_data <- long_data %>%
  mutate(
    # Base prices
    pprdtAdultOutlet = if_else(!is.na(r15b) & r15b < 99995, r15b, NA_real_),
    pprdtAdultAway = if_else(!is.na(r16b) & r16b < 99995, r16b, NA_real_),
    
    # Conversions - Outlet
    pprdtAdultOutletFIRST = pprdtAdultOutlet * (100 / cpiFIRST),
    uspprdtAdultOutletDC = pprdtAdultOutlet / exDC,
    uspprdtAdultOutletCURRENT = pprdtAdultOutlet / exCURRENT,
    uspprdtAdultOutletFIRST = pprdtAdultOutletFIRST / exFIRST,
    
    # Conversions - Away
    pprdtAdultAwayFIRST = pprdtAdultAway * (100 / cpiFIRST),
    uspprdtAdultAwayDC = pprdtAdultAway / exDC,
    uspprdtAdultAwayCURRENT = pprdtAdultAway / exCURRENT,
    uspprdtAdultAwayFIRST = pprdtAdultAwayFIRST / exFIRST
  )

################################################################################
# SECTION 5: ANTIMALARIAL MARK-UPS
################################################################################

long_data <- long_data %>%
  mutate(
    # Calculate supplier prices
    supplier_price_all = supplier_price_tab,
    tempsup_packprice = if_else(!is.na(supplier_price) & !is.na(supplier_amt_pack) & 
                                  supplier_amt_pack != 0,
                                supplier_price / supplier_amt_pack, NA_real_),
    tempsup_indtabprice = if_else(!is.na(supplier_price) & !is.na(supplier_amt_unit) & 
                                    supplier_amt_unit != 0,
                                  supplier_price / supplier_amt_unit, NA_real_),
    tempsup_price_all = coalesce(tempsup_packprice, tempsup_indtabprice),
    ws_price_all = ws_price_tab,
    
    # Supplier price per AETD
    suppriceperaetd = if_else(!is.na(tempsup_price_all) & !is.na(packageaetd) & 
                                !is.na(supplier_price) & !is.na(supplier_amt) & packageaetd != 0,
                              tempsup_price_all / packageaetd, NA_real_),
    
    # Unit supplier price
    unitsuppPrice = if_else(!is.na(supplier_price) & !is.na(supplier_amt) & supplier_amt != 0,
                            supplier_price / supplier_amt, NA_real_),
    
    # Markup percentages
    markup = case_when(
      !is.na(unitsuppPrice) & unitsuppPrice > 0 & !is.na(rts_price) ~ 
        (rts_price - unitsuppPrice) / unitsuppPrice,
      unitsuppPrice == 0 & unitPrice == 0 ~ 0,
      unitsuppPrice == 0 & unitPrice > 0 ~ NA_real_,
      TRUE ~ NA_real_
    ),
    
    markupaetd = case_when(
      !is.na(suppriceperaetd) & suppriceperaetd != 0 & !is.na(ppriceperaetd) ~ 
        (ppriceperaetd - suppriceperaetd) / suppriceperaetd,
      suppriceperaetd == 0 & ppriceperaetd == 0 ~ 0,
      suppriceperaetd == 0 & ppriceperaetd > 0 ~ NA_real_,
      TRUE ~ NA_real_
    ),
    
    # Raw markup values
    ppricemarkupaetd = if_else(!is.na(ppriceperaetd) & !is.na(suppriceperaetd),
                               ppriceperaetd - suppriceperaetd, NA_real_),
    ppricemarkupval = if_else(!is.na(packageaetd) & !is.na(ppriceperaetd) & !is.na(suppriceperaetd),
                              (packageaetd * ppriceperaetd) - (packageaetd * suppriceperaetd), NA_real_)
  )

################################################################################
# SECTION 6: RDT MARK-UPS
################################################################################

long_data <- long_data %>%
  mutate(
    # RDT wholesale price
    unitRdtWholePrice = if_else(!is.na(r17p) & !is.na(r17n) & r17n != 0 & 
                                  !r17p %in% c(-9555, -9777, -9888) & 
                                  !r17n %in% c(-9555, -9777, -9888),
                                r17p / r17n, NA_real_),
    
    # Adult markup
    markupAdult = case_when(
      !is.na(unitRdtWholePrice) & !is.na(r15b) & unitRdtWholePrice != 0 ~ 
        (r15b - unitRdtWholePrice) / unitRdtWholePrice,
      unitRdtWholePrice == 0 & r15b == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    
    # USD markup value
    usppricemarkupvalDCrdt = if_else(!is.na(r15b) & !is.na(unitRdtWholePrice),
                                     (r15b / exDC) - (unitRdtWholePrice / exDC), NA_real_),
    usppricemarkupvalDCrdt = if_else(usppricemarkupvalDCrdt < 0, NA_real_, usppricemarkupvalDCrdt)
  )

################################################################################
# SECTION 7: SAVE PROCESSED DATA
################################################################################

output_file <- here("Data", "Management data", 
                    paste0(country, "_", year, "_am_rdt_os_cleaned_long_wt_prices.csv"))
fwrite(long_data, output_file, row.names = FALSE, na = "")

################################################################################
# GENERATE HTML LOG
################################################################################

# Setup log file
log_dir <- here("Data", "Management data", "Data cleaning notes")
if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE)
log_file <- file.path(log_dir, paste0("AwL_", country, "_", year, "_price_notes.html"))

sink(log_file, type = "output")

cat("<html><head><title>Price Variables Generation</title>")
cat("<style>
table { border-collapse: collapse; width: 100%; margin: 10px 0; }
th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }
th { background-color: #f2f2f2; }
.warning { color: red; font-weight: bold; }
.note { background-color: #fff3cd; padding: 10px; margin: 10px 0; }
.important { background-color: #f8d7da; padding: 10px; margin: 10px 0; border-left: 5px solid #dc3545; }
</style></head><body>")

cat("<h1>ACTwatch LITE - Price Variables Generation</h1>")
cat(paste("<h2>Country:", country, "| Year:", year, "</h2>"))

# =============================================================================
# HTML CONTENT GENERATION - All calculated from final data
# =============================================================================

cat("<p><strong>Current macro values (MODIFY THESE):</strong></p>")
cat("<table>")
cat("<tr><th>Macro</th><th>Value</th><th>Description</th></tr>")
cat(paste0("<tr><td>cpiFIRST</td><td>", cpiFIRST, "</td><td>Cumulative inflation since first survey year</td></tr>"))
cat(paste0("<tr><td>cpiCURRENT</td><td>", cpiCURRENT, "</td><td>Inflation rate for current survey year</td></tr>"))
cat(paste0("<tr><td>exFIRST</td><td>", exFIRST, "</td><td>Exchange rate for first survey year</td></tr>"))
cat(paste0("<tr><td>exCURRENT</td><td>", exCURRENT, "</td><td>Exchange rate for current survey year</td></tr>"))
cat(paste0("<tr><td>exDC</td><td>", exDC, "</td><td>Exchange rate for data collection period</td></tr>"))
cat("</table>")

cat("<p>Data loaded successfully. Number of observations:", initial_records, "</p>")

cat("<h4>ANTIMALARIAL SELLING PRICES</h4>")

# Quality checks
size_summary <- long_data %>% 
  filter(rts_price < 150) %>% 
  count(size, sort = TRUE) %>% 
  head(10)

html_table(size_summary, "Package Sizes (rts_price < 150)")

aetd_summary <- long_data %>%
  filter(producttype %in% c(1, 2)) %>%
  summarise(
    count = sum(!is.na(ppriceperaetd)),
    mean_price = round(mean(ppriceperaetd, na.rm = TRUE), 2),
    median_price = round(median(ppriceperaetd, na.rm = TRUE), 2)
  )

html_table(aetd_summary, "AETD Price Summary (DC LCU)")

cat("<h4>MICROSCOPY PRICES</h4>")

cat("<h4>RDT PRICES</h4>")

cat("<h3>PERCENTAGE MARK-UPS</h3>")
cat("<h4>ANTIMALARIAL MARK-UPS</h4>")

# Markup summaries
markup_summary <- long_data %>%
  filter(!is.na(markup)) %>%
  summarise(
    count = n(),
    mean_markup = round(mean(markup, na.rm = TRUE), 3),
    median_markup = round(median(markup, na.rm = TRUE), 3),
    q25 = round(quantile(markup, 0.25, na.rm = TRUE), 3),
    q75 = round(quantile(markup, 0.75, na.rm = TRUE), 3)
  )

html_table(markup_summary, "Package Markup Summary Statistics")

markupaetd_summary <- long_data %>%
  filter(!is.na(markupaetd)) %>%
  summarise(
    count = n(),
    mean_markup = round(mean(markupaetd, na.rm = TRUE), 3),
    median_markup = round(median(markupaetd, na.rm = TRUE), 3),
    q25 = round(quantile(markupaetd, 0.25, na.rm = TRUE), 3),
    q75 = round(quantile(markupaetd, 0.75, na.rm = TRUE), 3)
  )

html_table(markupaetd_summary, "AETD Markup Summary Statistics")

cat("<h4>RDT MARK-UPS</h4>")

# Negative markup checks
cat("<div class='important'>")
cat("<h4>$$$ INVESTIGATE NEGATIVE MARKUPS</h4>")
cat("</div>")

negative_markups <- long_data %>%
  filter(markupAdult < 0) %>%
  count(c7, sort = TRUE)

if (nrow(negative_markups) > 0) {
  html_table(negative_markups, "Cases with Negative RDT Markups by Outlet Type (c7)")
  cat("<p class='warning'>Found", sum(negative_markups$n), "cases with negative markups.</p>")
}

rdt_markup_summary <- long_data %>%
  filter(!is.na(markupAdult)) %>%
  summarise(
    count = n(),
    mean_markup = round(mean(markupAdult, na.rm = TRUE), 3),
    median_markup = round(median(markupAdult, na.rm = TRUE), 3),
    negative_count = sum(markupAdult < 0, na.rm = TRUE)
  )

html_table(rdt_markup_summary, "RDT Adult Markup Summary")

cat("<h3>QUALITY CHECKS</h3>")

# Extreme value checks
extreme_checks <- bind_rows(
  long_data %>%
    filter(!is.na(unitPrice)) %>%
    summarise(
      Variable = "unitPrice",
      Count_Total = nrow(long_data),
      Count_NonMissing = n(),
      Min_Value = round(min(unitPrice, na.rm = TRUE), 4),
      Max_Value = round(max(unitPrice, na.rm = TRUE), 2),
      Potential_Issues = paste0("High (>1000): ", sum(unitPrice > 1000, na.rm = TRUE),
                                "; Low (<0.01): ", sum(unitPrice < 0.01, na.rm = TRUE))
    ),
  
  long_data %>%
    filter(!is.na(ppriceperaetd)) %>%
    summarise(
      Variable = "ppriceperaetd",
      Count_Total = nrow(long_data),
      Count_NonMissing = n(),
      Min_Value = round(min(ppriceperaetd, na.rm = TRUE), 4),
      Max_Value = round(max(ppriceperaetd, na.rm = TRUE), 2),
      Potential_Issues = paste0("High (>10000): ", sum(ppriceperaetd > 10000, na.rm = TRUE),
                                "; Low (<1): ", sum(ppriceperaetd < 1, na.rm = TRUE))
    ),
  
  long_data %>%
    filter(!is.na(markup)) %>%
    summarise(
      Variable = "markup",
      Count_Total = nrow(long_data),
      Count_NonMissing = n(),
      Min_Value = round(min(markup, na.rm = TRUE), 4),
      Max_Value = round(max(markup, na.rm = TRUE), 2),
      Potential_Issues = paste0("Very high (>10): ", sum(markup > 10, na.rm = TRUE),
                                "; Negative: ", sum(markup < 0, na.rm = TRUE))
    )
)

html_table(extreme_checks, "Extreme Value Quality Checks")

# Currency conversion check
conversion_summary <- long_data %>%
  filter(!is.na(packagePrice)) %>%
  summarise(
    DC_LCU_mean = round(mean(packagePrice, na.rm = TRUE), 2),
    DC_USD_mean = round(mean(usppriceperpackageDC, na.rm = TRUE), 4),
    exchange_rate_check = round(DC_LCU_mean / DC_USD_mean, 2),
    expected_rate = exDC,
    rate_difference = abs(exchange_rate_check - exDC)
  )

html_table(conversion_summary, "Currency Conversion Validation")

if (conversion_summary$rate_difference > 1) {
  cat("<p class='warning'>WARNING: Exchange rate check shows significant difference.</p>")
}

cat("<h3>SAVING PROCESSED DATA</h3>")

cat("<p>Data saved successfully to:", basename(output_file), "</p>")

cat("<div class='important'>")
cat("<h4>$$$ REQUIRED ACTIONS:</h4>")
cat("<ol>")
cat("<li>Verify Macro Values</li>")
cat("<li>Review Extreme Values</li>")
cat("<li>Validate Exchange Rates</li>")
cat("<li>Document Negative Markups</li>")
cat("<li>Review Missing Data</li>")
cat("</ol>")
cat("</div>")

cat("<hr><h3>PROCESSING COMPLETE</h3>")
cat("</body></html>")
if (sink.number() > 0) sink()

################################################################################
# CONSOLE OUTPUT
################################################################################

cat("\n=== PRICE VARIABLES PROCESSING COMPLETE ===\n")
cat("Records processed:", nrow(long_data), "\n")

# Price calculation summary
price_summary <- long_data %>%
  summarise(
    am_with_prices = sum(!is.na(unitPrice), na.rm = TRUE),
    am_with_aetd_price = sum(!is.na(ppriceperaetd), na.rm = TRUE),
    am_with_markup = sum(!is.na(markup), na.rm = TRUE),
    rdt_with_prices = sum(!is.na(pprdtAdultOutlet), na.rm = TRUE),
    microscopy_with_prices = sum(!is.na(pptestAdultTotal), na.rm = TRUE)
  )

cat("\nPrice variables calculated:\n")
cat(sprintf("  Antimalarial unit prices: %d\n", price_summary$am_with_prices))
cat(sprintf("  AETD prices: %d\n", price_summary$am_with_aetd_price))
cat(sprintf("  Package markups: %d\n", price_summary$am_with_markup))
cat(sprintf("  RDT prices: %d\n", price_summary$rdt_with_prices))
cat(sprintf("  Microscopy prices: %d\n", price_summary$microscopy_with_prices))

# Median prices
if (price_summary$am_with_aetd_price > 0) {
  median_prices <- long_data %>%
    filter(!is.na(ppriceperaetd)) %>%
    summarise(
      median_lcu = round(median(ppriceperaetd, na.rm = TRUE), 2),
      median_usd = round(median(usppriceperaetdDC, na.rm = TRUE), 2)
    )
  
  cat(sprintf("\nMedian AETD price: %.2f LCU (%.2f USD)\n", 
              median_prices$median_lcu, median_prices$median_usd))
}

# Quality flags
quality_issues <- long_data %>%
  filter(!is.na(markup)) %>%
  summarise(
    negative_markup = sum(markup < 0, na.rm = TRUE),
    extreme_markup = sum(markup > 10, na.rm = TRUE)
  )

if (quality_issues$negative_markup > 0 || quality_issues$extreme_markup > 0) {
  cat("\nQuality alerts:\n")
  if (quality_issues$negative_markup > 0) {
    cat(sprintf("  Negative markups: %d (review required)\n", quality_issues$negative_markup))
  }
  if (quality_issues$extreme_markup > 0) {
    cat(sprintf("  Extreme markups (>1000%%): %d (review required)\n", quality_issues$extreme_markup))
  }
}

# Exchange rate validation
cat("\nMacro values used:\n")
cat(sprintf("  Exchange rate (DC): %.2f\n", exDC))
cat(sprintf("  CPI adjustment: %.4f\n", cpiFIRST))

cat("\nOutput files:\n")
cat("  Dataset:", output_file, "\n")
cat("  HTML log:", log_file, "\n")

cat("\nIMPORTANT: Verify macro values at top of script match your data collection period\n")

################################################################################
# END
################################################################################