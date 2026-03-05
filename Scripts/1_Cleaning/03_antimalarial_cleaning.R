################################################################################
# ANTIMALARIAL CLEANING (03_antimalarial_cleaning.R)
################################################################################

################################################################################
# ACTwatch LITE 
# Step 1.3 Antimalarial Product Audit Data Cleaning
# REORGANIZED VERSION - HTML logging at end
################################################################################
#
# This script cleans and validates antimalarial product audit data. It processes
# product characteristics including active ingredients, brand names, manufacturers,
# formulations, and packaging. The script validates dosage forms and strengths,
# processes pricing and availability data, and matches products to the antimalarial
# masterlist for quality classification. Extensive validation checks ensure data
# consistency across brands, manufacturers, and formulation details. Fixed-Dose
# Combinations (FDCs) receive special validation. After cleaning, products are
# assigned quality indicators (QAACT) and approval status flags for downstream
# analysis. The cleaned dataset is saved with comprehensive HTML documentation.
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
# SECTION 1: DATA IMPORT AND PREPARATION
################################################################################

# Import data
am_data <- fread(here("Data", "Raw data", paste0("AwL-", country, "-", year, "-amAudit.csv"))) %>%
  rename_with(tolower) %>%
  rename_with(~ gsub("-", "", .))

# SAVE FOR LOGGING
initial_count <- nrow(am_data)

# Add compatibility variables
am_data <- am_data %>%
  rename(
    hassalt = any_of("excip"),
    salt = any_of("excip_salt"), 
    salt_other = any_of("excip_salt_other"),
    reg_code = any_of("nafdac_code")
  ) %>%
  mutate(
    salton = if (!"salton" %in% names(.)) NA_real_ else salton,
    salton_other = if (!"salton_other" %in% names(.)) NA_character_ else salton_other
  )

# Text field preparation
text_fields1 <- c("tsgsearchtext", "tab_amcode", "tab_ambrand_search", "tab_brand_search", 
                  "tab_a3_search", "tab_a3_lbl_search", "tab_ai1_search", "tab_ai1_lbl_search", 
                  "tab_ai1_mg_search", "tab_ai1_ml_search", "tab_ai1_strength_search")

am_data <- am_data %>%
  mutate(across(any_of(text_fields1), ~ replace_na(as.character(.), "")))

# Remove accents and standardize
am_data <- am_data %>%
  mutate(across(where(is.character), ~ {
    .x %>%
      str_to_upper() %>%
      str_replace_all(c(
        "[\u00e9\u00c8\u00e8\u00c9]" = "E",
        "[\u00e0\u00c0]" = "A",
        "[\u00e7\u00c7]" = "C",
        "[\u00d1\u00f1]" = "N"
      )) %>%
      str_trim()
  }))

# Drop search variables
search_vars_to_drop <- c(
  "tab_brand_search", "tab_a3_search", "tab_a3_lbl_search", 
  "tab_ai1_search", "tab_ai1_lbl_search", "tab_ai2_search", 
  "tab_ai2_lbl_search", "tab_ai3_search", "tab_ai3_lbl_search", 
  "tab_manu_search", "ntab_searchtext", "ntab_amcode", 
  "ntab_ambrand_search", "ntab_brand_search", "ntab_a3_search", 
  "ntab_a3_lbl_search", "ntab_ai1_search", "ntab_ai1_lbl_search", 
  "ntab_ai2_search", "ntab_ai2_lbl_search", "ntab_ai3_search", 
  "ntab_ai3_lbl_search", "ntab_manu_search", "a3_lbl_search", 
  "ai1_lbl_search", "ai2_lbl_search", "ai3_lbl_search", 
  "a3_lbl_manual", "ai1_manual_lbl", "ai2_manual_lbl", "ai3_manual_lbl"
)

am_data <- am_data %>% select(-any_of(search_vars_to_drop))

# Combine searched and manual product info
strength_vars <- c("ai1_mg_search", "ai1_mg_manual", "ai1_ml_search", "ai1_ml_manual",
                   "ai2_mg_search", "ai2_mg_manual", "ai2_ml_search", "ai2_ml_manual", 
                   "ai3_mg_search", "ai3_mg_manual", "ai3_ml_search", "ai3_ml_manual")

am_data <- am_data %>%
  mutate(across(any_of(strength_vars), ~ as.numeric(as.character(.)))) %>%
  mutate(
    ai1_mg = coalesce(ai1_mg_search, ai1_mg_manual),
    ai1_ml = coalesce(ai1_ml_search, ai1_ml_manual),
    ai2_mg = coalesce(ai2_mg_search, ai2_mg_manual),
    ai2_ml = coalesce(ai2_ml_search, ai2_ml_manual),
    ai3_mg = coalesce(ai3_mg_search, ai3_mg_manual),
    ai3_ml = coalesce(ai3_ml_search, ai3_ml_manual)
  ) %>%
  select(-matches("ai.*_m.*_(search|manual)"), -matches("ai.*_strength"))

# Handle suspension info
if ("a3_searchl_5_detail" %in% names(am_data)) {
  am_data$suspensiontype <- coalesce(am_data$a3_searchl_5_detail, am_data$a3_manual_5_detail)
  am_data <- am_data %>% select(-any_of(c("a3_manual_5_detail", "a3_searchl_5_detail")))
}

# Destring and create dose form categories
am_data <- am_data %>%
  mutate(
    across(c(ai1_ing, ai2_ing, ai3_ing), ~ as.numeric(.)),
    a3_category = if_else(a3 %in% c(1,2,3), 1, 0)
  )

# Create cleaning variables
am_data <- am_data %>%
  mutate(
    amcode_orig = amcode,
    a3_orig = a3,
    brand_orig = brand,
    manu_orig = manu,
    amcountry_orig = amcountry,
    ai1_orig = ai1_ing,
    ai2_orig = ai2_ing,
    ai3_orig = ai3_ing,
    ai1mg_orig = ai1_mg,
    ai1ml_orig = ai1_ml,
    ai2mg_orig = ai2_mg,
    ai2ml_orig = ai2_ml,
    ai3mg_orig = ai3_mg,
    ai3ml_orig = ai3_ml
  )

# Merge outlet data
if ("key" %in% names(am_data)) {
  am_data$amauditkey <- am_data$key
  if ("parent_key" %in% names(am_data)) {
    am_data <- am_data %>% 
      select(-key) %>% 
      rename(key = parent_key)
  }
}

outlet_data <- fread(here("Data", "Cleaned data", paste0("AwL_", country, "_", year, "_outlet_clean.csv")))

outlet_vars <- c("key", "outletid", "auditlevel", "eligible", "consented", 
                 "am_stockcurrent", "n1", "amaudit_complete", "amaudit_incomplete", 
                 "hasamaudit", "status", "c2", "c3", "c4", "c7", "c6", "s3", 
                 "s4", "s5a", "s5b", "s6", "c9")

am_data <- am_data %>%
  left_join(outlet_data %>% select(any_of(outlet_vars)), by = "key")

am_data <- am_data %>% filter(!is.na(outletid))

# Merge antimalarial masterlist
am_masterlist <- fread(here("Data", "Product lists", "antimalarial_masterlist_clean.csv"))

am_data <- am_data %>% left_join(am_masterlist, by = "amcode")

# Process package variables
am_data <- am_data %>%
  mutate(
    packagetype = as.numeric(packagetype),
    size = coalesce(packagesize_blisterpacks, packagesize_loose, packagesize_bottle,
                    packagesize_vial, packagesize_sachets, packagesize_other),
    amsold = coalesce(amsold_unit, amsold_pack),
    retail_price = coalesce(packageprice_unit, packageprice_pack),
    ws_amt = coalesce(wsmin_unit, wsmin_pack)
  ) %>%
  rename(ws_price = any_of("wsmin_price")) %>%
  mutate(
    ws_amt = case_when(
      abs(ws_amt) %in% c(9888, 9988, 9998) ~ -9888,
      abs(ws_amt) %in% c(9777, 9977, 9997) ~ -9777,
      TRUE ~ ws_amt
    ),
    ws_price = if("ws_price" %in% names(.)) {
      case_when(
        abs(ws_price) %in% c(9888, 9988, 9998) ~ -9888,
        abs(ws_price) %in% c(9777, 9977, 9997) ~ -9777,
        TRUE ~ ws_price
      )
    } else NA_real_
  )

# Recode packagetype
if ("packagetype_other" %in% names(am_data)) {
  am_data$packagetype[am_data$packagetype_other == "BOX"] <- 3
  am_data$packagetype[am_data$packagetype_other == "SINGLE VIAL IN A COMBIPACK"] <- 5
}

# Handle missing size values
if ("size" %in% names(am_data)) {
  am_data$size[am_data$size == -9888] <- NA_real_
}

# Save prepared data
prep_file <- here("Data", "Cleaned data", paste0("AwL_", country, "_", year, "_amaudit_prepared.csv"))
fwrite(am_data, prep_file)

################################################################################
# SECTION 2: ANTIMALARIAL DATA CLEANING
################################################################################

# Load data if needed
if (!exists("am_data")) {
  am_data <- fread(here("Data", "Cleaned data", paste0("AwL_", country, "_", year, "_amaudit_prepared.csv")))
}

# Calculate strength
am_data <- am_data %>%
  mutate(across(matches("^ai[1-3]_mg$"), ~ if_else(.x == 9998, NA_real_, .x))) %>%
  mutate(sumstrength = rowSums(select(., ai1_mg, ai2_mg, ai3_mg), na.rm = TRUE))

# Clean comments
if ("amcomments" %in% names(am_data)) {
  standard_empty <- c("", "NONE", "NO COMMENT", "NO", "9998", "NA", "N/A", 
                      "NOT APPLICABLE", "NOTHING")
  
  am_data <- am_data %>%
    mutate(
      amcomments = if_else(amcomments %in% standard_empty | str_detect(amcomments, "NON|NO COMM"), 
                           ".", amcomments)
    )
}

# Brand name cleaning
am_data <- am_data %>%
  mutate(
    brand = str_trim(brand),
    brand = str_replace_all(brand, c(
      "," = "",
      " AND " = " & ",
      "MG|ML" = "",
      "GOUTTE" = "DROP",
      " - " = "",
      "FOR INJECTION|INJECTION|INJECTABLE" = "INJ",
      "AMODIAQINE" = "AMODIAQUINE",
      "ARTEMTHER" = "ARTEMETHER",
      "SOFT GEL" = "SOFTGEL",
      "9998" = ""
    ))
  )

# Manufacturer cleaning
am_data <- am_data %>%
  mutate(
    manu = case_when(
      manu_orig == "9998" ~ "-9998",
      manu_orig == "2" ~ "-9998", 
      manu_orig %in% c("NOT INDICATED", "NOT CLEAR", "NONE") ~ "-9888",
      TRUE ~ manu
    ),
    manu = str_trim(manu),
    manu = str_replace_all(manu, c(
      "PHARMACEUTICALS?|PHARMS?" = "PHARMA",
      "\\.|,|;|:" = "",
      "LIMITED|CO LTD|PVT LTD|PRIVATE LTD" = "LTD",
      "LTD|PVT|PUT" = "",
      " AND " = " & ",
      "HEALTH CARE" = "HEALTHCARE",
      "  +" = " "
    ))
  )

# Generate GNAME using helper function
am_data <- am_data %>%
  mutate(
    ai1_tmp = create_ai_labels(ai1_ing),
    ai2_tmp = create_ai_labels(ai2_ing),
    ai3_tmp = create_ai_labels(ai3_ing)
  )

am_data$gname_tmp <- am_data$ai1_tmp
am_data$gname_tmp[am_data$ai2_tmp != ""] <- paste(am_data$gname_tmp[am_data$ai2_tmp != ""], 
                                                  am_data$ai2_tmp[am_data$ai2_tmp != ""], sep = ", ")
am_data$gname_tmp[am_data$ai3_tmp != ""] <- paste(am_data$gname_tmp[am_data$ai3_tmp != ""], 
                                                  am_data$ai3_tmp[am_data$ai3_tmp != ""], sep = ", ")

# Generate sumcodes
am_data$sumcodes <- am_data$ai1_ing
am_data$sumcodes[!is.na(am_data$ai2_ing) & !am_data$ai2_ing %in% c(96,98)] <- 
  am_data$ai1_ing[!is.na(am_data$ai2_ing) & !am_data$ai2_ing %in% c(96,98)] + 
  am_data$ai2_ing[!is.na(am_data$ai2_ing) & !am_data$ai2_ing %in% c(96,98)]
am_data$sumcodes[!is.na(am_data$ai3_ing) & !am_data$ai3_ing %in% c(96,98)] <- 
  am_data$ai1_ing[!is.na(am_data$ai3_ing) & !am_data$ai3_ing %in% c(96,98)] + 
  am_data$ai2_ing[!is.na(am_data$ai3_ing) & !am_data$ai3_ing %in% c(96,98)] +
  am_data$ai3_ing[!is.na(am_data$ai3_ing) & !am_data$ai3_ing %in% c(96,98)]

# Assign gname codes
am_data$gname <- case_when(
  am_data$sumcodes == 60 ~ 1,    # amodiaquine
  am_data$sumcodes == 226 ~ 2,   # SP amodiaquine
  am_data$sumcodes == 145 & str_detect(am_data$gname_tmp, "Atovaquone.*Proguanil|Proguanil.*Atovaquone") ~ 3,
  am_data$sumcodes == 68 ~ 4,    # chloroquine
  am_data$sumcodes == 73 ~ 10,   # hydroxychloroquine
  am_data$sumcodes == 75 ~ 11,   # mefloquine
  am_data$sumcodes == 79 ~ 14,   # proguanil
  am_data$sumcodes == 81 ~ 15,   # pyrimethamine
  am_data$sumcodes == 82 ~ 22,   # quinacrine
  am_data$sumcodes == 83 ~ 19,   # quinine
  am_data$sumcodes == 166 ~ 21,  # SP
  am_data$sumcodes == 63 ~ 30,   # arteether
  am_data$sumcodes == 61 ~ 31,   # artemether
  am_data$sumcodes == 65 ~ 32,   # artesunate
  am_data$sumcodes == 135 ~ 40,  # AL
  am_data$sumcodes == 139 ~ 42,  # artemisinin-piperaquine
  am_data$sumcodes == 125 ~ 44,  # ASAQ
  am_data$sumcodes == 140 ~ 47,  # ASMQ
  am_data$sumcodes == 145 & str_detect(am_data$gname_tmp, "Artesunate.*Pyronaridine|Pyronaridine.*Artesunate") ~ 49,
  am_data$sumcodes == 146 ~ 61,  # arterolane-piperaquine
  am_data$sumcodes == 148 ~ 55,  # DHA-piperaquine
  am_data$sumcodes == 231 ~ 50,  # AS-SP
  am_data$sumcodes == 236 ~ 56,  # DHA-piperaquine-Trimethoprim
  TRUE ~ NA_real_
)

# Clean up
am_data <- am_data %>% select(-ai1_tmp, -ai2_tmp, -ai3_tmp, -gname_tmp, -sumcodes)

# Drop non-AMs
non_am_count <- sum(am_data$gname == 95, na.rm = TRUE)
if (non_am_count > 0) {
  am_data <- am_data %>% filter(gname != 95 | is.na(gname))
}

################################################################################
# SECTION 3: DOSE FORM AND STRENGTH VALIDATION
################################################################################

# Generate AI number
am_data$ainum <- case_when(
  !is.na(am_data$ai3_ing) ~ 3,
  !is.na(am_data$ai2_ing) ~ 2,
  !is.na(am_data$ai1_ing) ~ 1,
  TRUE ~ NA_real_
)

################################################################################
# SECTION 4: PACKAGE INFO, PRICES, AND SALT VALIDATION
################################################################################

# Salt validation
if (all(c("salt", "salton") %in% names(am_data))) {
  # Store original hassalt before modifications
  if ("hassalt" %in% names(am_data)) {
    am_data$hassalt_orig <- am_data$hassalt
  }
  
  # Clean inappropriate salts
  am_data <- am_data %>%
    mutate(
      salt = case_when(
        gname %in% c(40, 21) ~ NA_real_,  # No AL/SP salts
        salton == 71 & gname %in% c(55, 56) ~ salt,  # DHAPPQ
        salton == 65 & gname == 44 ~ salt,  # ASAQ
        TRUE ~ salt
      ),
      salton = case_when(
        gname %in% c(40, 21) ~ NA_real_,
        salton == 71 & gname %in% c(55, 56) ~ 77,
        salton == 65 & gname == 44 ~ 60,
        TRUE ~ salton
      )
    )
  
  # Recode hassalt based on salton
  if ("hassalt" %in% names(am_data)) {
    am_data$hassalt[am_data$hassalt == 0 & !is.na(am_data$salton)] <- 1
    am_data$hassalt[am_data$hassalt == 1 & is.na(am_data$salton)] <- 0
  }
  
  # Additional salt corrections
  am_data$salt[am_data$gname == 2 & am_data$brand == "SPAQ CO-DS"] <- NA_real_
  am_data$salton[am_data$gname == 2 & am_data$brand == "SPAQ CO-DS"] <- NA_real_
  
  am_data$salton[am_data$gname == 32 & am_data$salton %in% c(61, 65)] <- NA_real_
  am_data$salt[am_data$gname == 32 & am_data$salt %in% c(5, 96)] <- NA_real_
  
  am_data$salt[am_data$gname == 1 & !is.na(am_data$salt) & am_data$salt == 1] <- 2
  am_data$salt[am_data$gname == 4 & !am_data$salt %in% c(2, 5, 6) & !is.na(am_data$salt)] <- 5
  am_data$salt[am_data$ai2_ing == 77 & !am_data$salt %in% c(5) & !is.na(am_data$salt)] <- 5
  
  am_data$salt[(am_data$ai1_ing == 83 | am_data$ai2_ing == 83 | am_data$ai3_ing == 83) & 
                 !am_data$salt %in% c(1,2,3,4,6) & !is.na(am_data$salt) & 
                 am_data$salt == 5 & str_detect(am_data$brand, "SUL")] <- 6
  
  am_data$salt[am_data$ai1_ing == 68 & am_data$gname == 4 & am_data$hassalt == 1 & am_data$salt == 6] <- 5
  
  am_data$salt[(am_data$ai1_ing == 79 | am_data$ai2_ing == 79 | am_data$ai3_ing == 79) & 
                 !am_data$salt %in% c(2) & !is.na(am_data$salt) & am_data$salt == 3] <- NA_real_
  am_data$salton[(am_data$ai1_ing == 79 | am_data$ai2_ing == 79 | am_data$ai3_ing == 79) & 
                   !am_data$salt %in% c(2) & !is.na(am_data$salt)] <- NA_real_
  
  am_data$salt[(am_data$ai1_ing == 83 | am_data$ai2_ing == 83 | am_data$ai3_ing == 83) & 
                 !am_data$salt %in% c(1,2,3,4,6) & !is.na(am_data$salt) & 
                 am_data$salt %in% c(5, 96) & str_detect(am_data$brand, "TOBY")] <- 2
  
  am_data$salt[am_data$gname == 4 & am_data$salt == 6 & 
                 (str_detect(am_data$brand, "HCQS") | 
                    str_detect(am_data$brand, "MAXIQUINE") |
                    str_detect(am_data$brand, "FESTDRINE"))] <- 5
  
  am_data$salt[am_data$ai1_ing == 83 & am_data$salt == 96] <- 2
  am_data$salt[am_data$gname == 44 & am_data$salt == 96 & str_detect(am_data$brand, "CAMOSUNATE")] <- 2
  am_data$salt[am_data$ai1_ing == 79 & am_data$salt == 96] <- 2
}

# Package type and size
if ("packagetype" %in% names(am_data)) {
  am_data <- am_data %>%
    mutate(
      type = case_when(
        packagetype %in% c(1,2,6) ~ 1,  # package/sachet
        packagetype == 3 ~ 2,  # loose/pot
        packagetype == 4 ~ 3,  # bottle
        packagetype == 5 ~ 4,  # ampoule/vial
        TRUE ~ NA_real_
      )
    )
}

# Price processing
if ("retail_price" %in% names(am_data)) {
  am_data <- am_data %>%
    mutate(
      rts_price = retail_price,
      rts_price = case_when(
        rts_price %in% c(9888, 9988, 9998) ~ -9888,
        rts_price %in% c(9777, 9977, 9997) ~ -9777,
        TRUE ~ rts_price
      )
    )
}

# Amount sold
if ("amsold" %in% names(am_data)) {
  am_data <- am_data %>%
    mutate(
      sold = amsold,
      sold = case_when(
        sold %in% c(9888, 9988, 9998) ~ -9888,
        sold %in% c(9777, 9977, 9997) ~ -9777,
        TRUE ~ sold
      )
    )
}

# Calculate price per unit/pack
if ("rts_price" %in% names(am_data)) {
  am_data$rts_price_unit <- NA_real_
  am_data$rts_price_unit[!am_data$rts_price %in% c(-9777, -9888) & am_data$a3 == 1 & am_data$packagetype == 3] <- 
    am_data$rts_price[!am_data$rts_price %in% c(-9777, -9888) & am_data$a3 == 1 & am_data$packagetype == 3]
  
  am_data$rts_price_pack <- NA_real_
  am_data$rts_price_pack[!am_data$rts_price %in% c(-9777, -9888) & is.na(am_data$rts_price_unit)] <- 
    am_data$rts_price[!am_data$rts_price %in% c(-9777, -9888) & is.na(am_data$rts_price_unit)]
}

# Clean supplier price values
if ("supplier_amt" %in% names(am_data)) {
  am_data$supplier_amt <- case_when(
    am_data$supplier_amt %in% c(9888, 9988, 9998) ~ -9888,
    am_data$supplier_amt %in% c(9777, 9977, 9997) ~ -9777,
    TRUE ~ am_data$supplier_amt
  )
}

if ("supplier_price" %in% names(am_data)) {
  am_data$supplier_price <- case_when(
    am_data$supplier_price %in% c(9888, 9988, 9998) ~ -9888,
    am_data$supplier_price %in% c(9777, 9977, 9997) ~ -9777,
    TRUE ~ am_data$supplier_price
  )
}

# Supplier price per tablet
if (all(c("supplier_price", "supplier_amt") %in% names(am_data))) {
  am_data$supplier_price_tab <- NA_real_
  valid_rows <- !am_data$supplier_amt %in% c(-9777, -9888) & 
    !am_data$supplier_price %in% c(-9777, -9888) &
    !is.na(am_data$supplier_amt) & !is.na(am_data$supplier_price) &
    am_data$supplier_amt != 0
  
  am_data$supplier_price_tab[valid_rows] <- am_data$supplier_price[valid_rows] / am_data$supplier_amt[valid_rows]
}

# Wholesale price per tablet
if (all(c("ws_price", "ws_amt") %in% names(am_data))) {
  am_data$ws_price_tab <- NA_real_
  valid_rows <- !am_data$ws_amt %in% c(-9777, -9888, NA, 0) & 
    !am_data$ws_price %in% c(-9777, -9888, NA, 0)
  
  am_data$ws_price_tab[valid_rows] <- am_data$ws_price[valid_rows] / am_data$ws_amt[valid_rows]
}

# FDC validation
if (all(c("gname", "fdc", "a3_category") %in% names(am_data))) {
  # Store original
  am_data$fdc_orig <- am_data$fdc
  
  # Monotherapies are not FDC
  am_data$fdc[am_data$gname %in% c(1,4,9,10,11,13,14,15,18,19,30,31,32,33) & 
                am_data$fdc %in% c(1, 0)] <- 97
  
  # Brand name has FDC
  if ("brand_orig" %in% names(am_data)) {
    am_data$fdc_inbrand <- as.numeric(str_detect(am_data$brand_orig, "FDC"))
    am_data$fdc[am_data$fdc_inbrand == 1 & am_data$a3_category == 1 & am_data$fdc == 0] <- 1
  }
  
  # FDC only applies to tablets, suppositories, granules
  am_data$fdc[am_data$a3 > 3 & am_data$fdc %in% c(1, 0)] <- -97
  
  # Recode values to negative
  am_data$fdc[am_data$fdc == 97] <- -97
  am_data$fdc[am_data$fdc == 98] <- -98
  am_data$fdc[am_data$fdc == 99] <- -99
  
  # FDC should be consistent within brand/manu/gname
  am_data <- am_data %>%
    group_by(brand, manu, gname) %>%
    mutate(temp_fdc_max = max(fdc, na.rm = TRUE)) %>%
    ungroup()
  
  am_data$fdc[am_data$fdc %in% c(-98, -97, 0) & am_data$temp_fdc_max == 1] <- 1
  am_data <- am_data %>% select(-temp_fdc_max)
  
  # Brand-specific corrections
  am_data$fdc[(str_detect(am_data$brand, "COARINATE") | str_detect(am_data$brand, "CO-ARINATE")) & 
                !str_detect(am_data$brand, "FDC")] <- 0
  am_data$fdc[str_detect(am_data$brand, "ARSUCAM")] <- 0
  
  # Combination therapy suppositories
  am_data$fdc[am_data$a3 == 2 & 
                am_data$gname %in% c(2,3,5,6,7,12,16,20,21,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58) &
                is.na(am_data$fdc)] <- 1
  
  # Combination therapy granules
  am_data$fdc[am_data$a3 == 3 & 
                am_data$gname %in% c(2,3,5,6,7,12,16,20,21,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58) &
                is.na(am_data$fdc)] <- 1
}

# Additional brand corrections
am_data$fdc[(str_detect(am_data$brand, "COARINATE") | str_detect(am_data$brand, "CO-ARINATE")) & 
              !str_detect(am_data$brand, "FDC")] <- 0
am_data$fdc[str_detect(am_data$brand, "ARSUCAM")] <- 0


################################################################################
# SECTION 5: PRODUCT MATCHING AND FINAL PROCESSING
################################################################################

# Create composite identifier
am_data <- am_data %>%
  mutate(
    amcode = if_else(amcode == "", NA_character_, amcode),
    summary = paste(
      as.character(a3), as.character(gname), brand, manu,
      paste0(ai1_mg, "/", ai1_ml),
      paste0(ai2_mg, "/", ai2_ml), 
      paste0(ai3_mg, "/", ai3_ml),
      sep = "-"
    )
  ) %>%
  group_by(summary) %>%
  mutate(summary_code = cur_group_id()) %>%
  ungroup()

# Create product codes lookup
product_codes <- am_data %>%
  filter(fillmethod == 1, !is.na(amcode) & amcode != "") %>%
  group_by(summary_code) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  select(summary_code, amcode_list = amcode)

# Merge back
am_data <- am_data %>%
  left_join(product_codes, by = "summary_code") %>%
  mutate(amcode_new = case_when(
    fillmethod == 1 ~ amcode,
    TRUE ~ amcode_list
  ))

# QAACT and NATAPP
am_masterlist_full <- fread(here("Data", "Product lists", "antimalarial_masterlist_clean.csv"))
am_data <- am_data %>%
  left_join(am_masterlist_full %>% select(amcode, qaact_master, natapp_master), 
            by = "amcode", suffix = c("", "_master"))

am_data$qaact <- 0
am_data$qaact[am_data$qaact_master == 1 & 
                am_data$gname %in% c(40, 42, 47, 49, 55, 44, 50, 56, 61)] <- 1

am_data$natapp <- 0
am_data$natapp[am_data$natapp_master == 1] <- 1

# AM TRUE
am_data$am_true <- 0
am_data$am_true[am_data$gname > 0 & am_data$gname < 62] <- 1

# Final cleanup
am_data <- am_data %>%
  filter(!is.na(key) & key != "", !is.na(amauditkey) & amauditkey != "") %>%
  mutate(
    sumstrength_orig = sumstrength,
    sumstrength = rowSums(select(., ai1_mg, ai2_mg, ai3_mg), na.rm = TRUE)
  )

# Drop unnecessary variables
drop_vars <- c("tab_amcode", "tab_ambrand_search", "search_typ_am", "tabsearchtext",
               "brand_search", "brand_manual", "manu_manual", "manu_search", "a3_search", 
               "a3_manual", "ai1_search", "ai1_manual", "ai2_search", "ai2_manual", 
               "ai3_search", "ai3_manual", "hasml")

am_data <- am_data %>% select(-any_of(drop_vars))

# Save final dataset
final_file <- here("Data", "Cleaned data", paste0("AwL_", country, "_", year, "_amaudit_clean.csv"))
fwrite(am_data, final_file)

final_count <- nrow(am_data)

################################################################################
# GENERATE HTML LOG
################################################################################

# Helper functions for HTML logging
create_summary_table <- function(data, var) {
  var_enquo <- enquo(var)
  data %>%
    count(!!var_enquo) %>%
    mutate(percent = round(n/sum(n)*100, 1)) %>%
    arrange(desc(n))
}

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
  
  # Rows (limit to first 100)
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

log_file <- file.path(log_dir, paste0("AwL_", country, "_", year, "_amaudit_clean_notes.html"))

# Initialize HTML
cat('<!DOCTYPE html>
<html><head>
<title>Antimalarial Audit Cleaning Log</title>
<style>
body {font-family: Arial, sans-serif; margin: 20px;}
h1, h2, h3, h4 {color: #2E86AB;}
table {border-collapse: collapse; width: 100%; margin: 10px 0;}
th, td {border: 1px solid #ddd; padding: 8px; text-align: left;}
th {background-color: #f2f2f2;}
.section {margin: 20px 0; padding: 15px; border-left: 4px solid #2E86AB;}
.warning {background-color: #fff3cd; border-color: #ffc107;}
.error {background-color: #f8d7da; border-color: #dc3545;}
.success {background-color: #d4edda; border-color: #28a745;}
.required-action {background-color: #fff3cd; border-left: 4px solid #dc3545; padding: 15px; margin: 15px 0; font-weight: bold;}
</style>
</head><body>', file = log_file)

sink(log_file, append = TRUE)

cat("<h1>ACTwatch Lite Antimalarial Audit Data Cleaning Log</h1>\n")
cat(glue("<div class='section'>
<h2>Cleaning Session Information</h2>
<p><strong>Date/Time:</strong> {Sys.time()}</p>
<p><strong>Country:</strong> {country}</p>
<p><strong>Year:</strong> {year}</p>
</div>\n"))

# =============================================================================
# HTML CONTENT GENERATION
# =============================================================================

cat("<h2>1.2.1 DATA PREPARATION</h2>\n")

cat("<div class='required-action'>
<h3>$$$ REQUIRED ACTION: Import AM Audit Data</h3>
<p>Confirm AM audit data filename and record count</p>
</div>\n")

cat(glue("<p><strong>Initial record count:</strong> {initial_count}</p>\n"))

cat("<div class='required-action'>
<h3>$$$ REQUIRED ACTION: Text Field Configuration</h3>
<p>Edit text field list if variable names have changed</p>
</div>\n")

cat("<p>Text fields converted to uppercase and trimmed, accents removed</p>\n")

cat("<h3>1.2.1.4: Combine Searched and Manual Product Info</h3>\n")
cat("<p>Product information combined and dose form categories created</p>\n")

cat("<div class='required-action'>
<h3>$$$ REQUIRED ACTION: Check Duplicates</h3>
<p>Drop duplicates found in outlet cleaning if any</p>
</div>\n")

if ("key" %in% names(am_data)) {
  duplicates <- sum(duplicated(am_data$key) | duplicated(am_data$key, fromLast = TRUE))
  cat(glue("<p>Duplicate keys found: {duplicates}</p>\n"))
}

cat("<div class='required-action'>
<h3>$$$ REQUIRED ACTION: Known Errors</h3>
<p>Address any errors noted during data collection or in comments</p>
</div>\n")

cat("<div class='required-action'>
<h3>$$$ REQUIRED ACTION: Outlet Data Merge</h3>
<p>All AM audit forms should be linked to an outlet form. Check unmatched records.</p>
</div>\n")

cat(glue("<p>Records after merging and filtering: {nrow(am_data)}</p>\n"))

cat("<div class='required-action'>
<h3>$$$ REQUIRED ACTION: Masterlist Merge</h3>
<p>Review merge results and fix errors based on product review</p>
</div>\n")

if ("fillmethod" %in% names(am_data)) {
  merge_check <- am_data %>%
    count(fillmethod, is.na(amcode))
  
  cat("<p>Merge results by fillmethod:</p>\n")
  html_table(merge_check, "Merge Check by Fill Method")
}

cat(glue("<p><strong>Prepared data saved</strong></p>\n"))
cat(glue("<p><strong>Records in dataset:</strong> {nrow(am_data)}</p>\n"))

# Section 2: Data Cleaning
cat("<h2>1.2.2 ANTIMALARIAL DATA CLEANING</h2>\n")

cat("<div class='required-action'>
<h3>$$$ REQUIRED ACTION: Record Count Verification</h3>
<p>Confirm number of products matches expected from preparation stage</p>
</div>\n")

cat(glue("<p><strong>Record count:</strong> {nrow(am_data)}</p>\n"))

cat("<div class='required-action'>
<h3>$$$ REQUIRED ACTION: Manual Entry Strength Check</h3>
<p>Before sum of strengths, check manually entered product strengths for obvious errors</p>
</div>\n")

if ("fillmethod" %in% names(am_data)) {
  for (i in 1:3) {
    ai_check <- am_data %>%
      filter(fillmethod == 2) %>%
      count(!!sym(paste0("ai", i, "_ing")), !!sym(paste0("ai", i, "_mg"))) %>%
      filter(n > 0) %>%
      slice_head(n = 5)
    
    if (nrow(ai_check) > 0) {
      cat(glue("<p>AI{i} manual entry check:</p>\n"))
      html_table(ai_check, paste0("AI", i, " Manual Strengths"))
    }
  }
}

html_table(
  am_data %>%
    summarise(
      count = sum(sumstrength > 0, na.rm = TRUE),
      mean = round(mean(sumstrength[sumstrength > 0], na.rm = TRUE), 1),
      median = median(sumstrength[sumstrength > 0], na.rm = TRUE)
    ),
  "Strength Summary Statistics"
)

cat("<h3>CHECKS</h3>\n")

cat("<div class='required-action'>
<h3>$$$ REQUIRED ACTION: Completeness Review</h3>
<p>Add results and make changes based on careful review</p>
</div>\n")

if ("amaudit_complete" %in% names(am_data)) {
  html_table(create_summary_table(am_data, amaudit_complete), "AM Audit Completeness")
}

if ("fillmethod" %in% names(am_data)) {
  html_table(create_summary_table(am_data, fillmethod), "Fill Method Distribution")
}

cat("<div class='required-action'>
<h3>$$$ REQUIRED ACTION: Comment Review</h3>
<p>Check remaining comments and address any data cleaning required</p>
</div>\n")

if ("amcomments" %in% names(am_data)) {
  meaningful_comments <- am_data %>% 
    filter(amcomments != "." & !is.na(amcomments)) %>%
    count(amcomments, sort = TRUE) %>%
    slice_head(n = 10)
  
  if (nrow(meaningful_comments) > 0) {
    html_table(meaningful_comments, "Meaningful Comments for Review")
  }
}

cat("<div class='required-action'>
<h3>$$$ REQUIRED ACTION: Non-Antimalarial Check</h3>
<p>Add results and make changes based on careful review. Drop non-antimalarials.</p>
</div>\n")

non_am_check <- am_data %>%
  filter(ai1_ing > 90 & ai1_ing < 999) %>%
  select(brand, manu, ai1_ing, ai2_ing, ai3_ing, amcomments, amauditkey) %>%
  slice_head(n = 10)

if (nrow(non_am_check) > 0) {
  html_table(non_am_check, "Potential Non-Antimalarials")
}

cat("<div class='required-action'>
<h3>$$$ REQUIRED ACTION: Missing Brand Names</h3>
<p>Add missing brand names from product photos where possible</p>
</div>\n")

cat("<div class='required-action'>
<h3>$$$ REQUIRED ACTION: Missing Manufacturers</h3>
<p>Check product pictures for missing manufacturer names and correct where possible</p>
</div>\n")

html_table(
  am_data %>% count(manu, sort = TRUE) %>% slice_head(n = 10),
  "Top Manufacturers"
)

cat("<div class='required-action'>
<h3>$$$ REQUIRED ACTION: Country Recoding</h3>
<p>Recode all 'other' countries and check consistency by manufacturer/brand</p>
</div>\n")

if ("amcountry" %in% names(am_data)) {
  html_table(
    am_data %>% count(amcountry, sort = TRUE) %>% slice_head(n = 10),
    "Country Distribution"
  )
}

cat("<div class='required-action'>
<h3>$$ REQUIRED ACTION: AI Error Check</h3>
<p>Spot check and fix errors with AI numbers. Address known errors or obvious data entry errors.</p>
</div>\n")

for (i in 1:3) {
  html_table(
    am_data %>% count(!!sym(paste0("ai", i, "_ing")), sort = TRUE) %>% slice_head(n = 10),
    paste0("AI", i, " Distribution")
  )
}

html_table(create_summary_table(am_data, gname), "Generic Name Distribution")

if (non_am_count > 0) {
  cat(glue("<p>Dropped {non_am_count} non-antimalarial products</p>\n"))
}

# Section 3: Dose Form and Strength Validation
cat("<h3>DOSE FORM AND STRENGTH VALIDATION</h3>\n")

if ("suspensiontype" %in% names(am_data)) {
  html_table(create_summary_table(am_data, suspensiontype), "Suspension Type Distribution")
}

html_table(
  am_data %>% count(gname, a3) %>% arrange(gname, a3) %>% slice_head(n = 15),
  "Dose Form by Generic Name"
)

cat("<div class='required-action'>
<h3>$$$ REQUIRED ACTION: Strength/Dose Form Consistency</h3>
<p>Review product photos and fix errors. Liquids have mg/ml, solids have mg only.</p>
</div>\n")

for (form_type in list(
  list(a3 = 1, name = "Tablets", should_have_ml = FALSE),
  list(a3 = 4, name = "Syrups", should_have_ml = TRUE),
  list(a3 = 5, name = "Suspensions", should_have_ml = TRUE),
  list(a3 = 6, name = "Liquid Injections", should_have_ml = TRUE)
)) {
  if (form_type$should_have_ml) {
    check <- am_data %>%
      filter(a3 == form_type$a3 & is.na(ai1_ml) & !is.na(ai1_mg)) %>%
      select(amauditkey, gname, brand, ai1_mg, ai1_ml) %>%
      slice_head(n = 10)
  } else {
    check <- am_data %>%
      filter(a3 == form_type$a3 & !is.na(ai1_ml)) %>%
      select(amauditkey, gname, brand, ai1_mg, ai1_ml) %>%
      slice_head(n = 10)
  }
  
  if (nrow(check) > 0) {
    html_table(check, paste(form_type$name, "with ML Issues"))
  }
}

# Section 4: Salt, Package, Price Validation
cat("<h3>SALT VALIDATION</h3>\n")

if ("packagetype" %in% names(am_data)) {
  html_table(
    am_data %>% count(a3, type),
    "Package Type by Dose Form"
  )
}

cat("<h3>FDC STATUS VALIDATION</h3>\n")

if (all(c("gname", "fdc", "a3_category") %in% names(am_data))) {
  html_table(
    am_data %>% count(a3, fdc),
    "FDC by Dose Form"
  )
}

# Final Summary
cat("<h2>FINAL DATASET SUMMARY</h2>\n")

final_summary <- data.frame(
  Metric = c("Total Products", "Valid Antimalarials", "WHO Prequalified ACTs",
             "Nationally Approved", "Database Products", "Manual Entry Products"),
  Count = c(
    nrow(am_data),
    sum(am_data$am_true == 1, na.rm = TRUE),
    sum(am_data$qaact == 1, na.rm = TRUE),
    sum(am_data$natapp == 1, na.rm = TRUE),
    sum(am_data$fillmethod == 1, na.rm = TRUE),
    sum(am_data$fillmethod == 2, na.rm = TRUE)
  )
)

html_table(final_summary, "Final Dataset Summary")

cat(glue("<div class='section'>
<h2>CLEANING PROCESS COMPLETE</h2>
<p><strong>Initial records:</strong> {initial_count}</p>
<p><strong>Final records:</strong> {final_count}</p>
<p><strong>Valid antimalarials:</strong> {sum(am_data$am_true == 1, na.rm = TRUE)}</p>
</div>\n"))

cat("</body></html>", file = log_file, append = TRUE)
sink()

options(warn = 0)

################################################################################
# CONSOLE OUTPUT
################################################################################

cat("\n=== ANTIMALARIAL AUDIT DATA CLEANING COMPLETE ===\n")
cat("Initial antimalarial records:", initial_count, "\n")
cat("Final antimalarial records:", nrow(am_data), "\n") 
cat("Valid antimalarials:", sum(am_data$am_true == 1, na.rm = TRUE), "\n")
cat("Clean dataset saved to:", final_file, "\n")
cat("Data cleaning log saved to:", log_file, "\n")

# =============================================================================
# ####               END                   ####
# =============================================================================