################################################################################
# ACTwatch LITE 
# Step 2.6 Generate Antimalarial Category Variables
# REORGANIZED VERSION - HTML logging at end
################################################################################
#
# This script generates a comprehensive set of antimalarial classification 
# variables for market analysis and treatment guideline compliance assessment.
# Variables are created across multiple dimensions including broad drug categories
# (ACT, artemisinin monotherapy, non-artemisinin therapy, prophylaxis), quality
# assurance classifications (QAACT, non-QAACT), national registration status,
# dosage appropriateness for pediatric and adult patients, specific drug types
# (AL, ASAQ, DHAPPQ, etc.), formulation categories (tablet, non-tablet), and
# compliance with national treatment guidelines. The script automatically 
# generates binary indicators for all major antimalarial types found in the 
# dataset, enabling detailed market composition analysis and identification of
# products meeting WHO and national quality standards.
#
# The following flags are used throughout the syntax:
# $$$ = Breaks the script to remind analyst to modify syntax
# # EXAMPLE: = Sample syntax from pilot studies for reference

################################################################################
# SECTION 1: LOAD AND PREPARE DATA
################################################################################

long_data <- fread(here("Data", "Management data", 
                        paste0(country, "_", year, "_am_rdt_os_cleaned_long_wt_bloodtest.csv"))) %>%
  mutate(
    ai1_mg = as.numeric(ai1_mg),
    ai1_ml = as.numeric(ai1_ml),
    ai2_mg = as.numeric(ai2_mg),
    ai2_ml = as.numeric(ai2_ml),
    ai3_mg = as.numeric(ai3_mg),
    ai3_ml = as.numeric(ai3_ml)
  ) %>%
  mutate(across(where(is.character) & !matches("ai\\d+_m[lg]"), ~ str_trim(str_to_upper(.)))) %>% 
  select(-natapp_master) %>%
  rename(natapp_master = natapp)

# SAVE FOR LOGGING (only what we need)
initial_records <- nrow(long_data)

################################################################################
# SECTION 2: GENERATE BROAD DRUG CATEGORIES
################################################################################

long_data_processed <- long_data %>%
  mutate(
    drugcat1 = case_when(
      producttype %in% c(1, 2) & gname %in% c(1,2,3,4,6,9,10,11,12,13,17,18,19,20,21,22) ~ 3,
      producttype %in% c(1, 2) & gname %in% c(30,31,32,33) ~ 2,
      producttype %in% c(1, 2) & gname >= 40 & gname < 90 ~ 1,
      producttype %in% c(1, 2) ~ 4,
      TRUE ~ NA_real_
    )
  )

val_labels(long_data_processed$drugcat1) <- c("ACT" = 1, "Artemisinin monotherapy" = 2, 
                                              "Non-artemisinin therapy" = 3, "Prophylaxis" = 4)

################################################################################
# SECTION 3: CORE ANTIMALARIAL CLASSIFICATION
################################################################################

long_data_processed <- long_data_processed %>%
  mutate(
    amCategory = case_when(
      !is.na(gname) & gname >= 40 & qaact == 1 ~ 1,
      !is.na(gname) & gname >= 40 & !gname %in% c(5,7,8,14,15,16) ~ 2,
      !is.na(gname) & gname %in% c(30,31,32,33) ~ 3,
      !is.na(gname) & !gname %in% c(5,7,8,14,15,16) ~ 4,
      TRUE ~ NA_real_
    ),
    
    anyAM = if_else(!is.na(drugcat1) & drugcat1 < 4, 1, 
                    if_else(!is.na(drugcat1) & drugcat1 < 5, 0, NA_real_)),
    
    anyACT = if_else(!is.na(drugcat1) & drugcat1 == 1, 1,
                     if_else(!is.na(drugcat1) & drugcat1 < 4, 0, NA_real_))
  )

val_labels(long_data_processed$amCategory) <- c("QAACT" = 1, "Non-QAACT" = 2, 
                                                "Artemisinin monotherapy" = 3, "Non-artemisinin therapy" = 4)
var_label(long_data_processed$amCategory) <- "AMFm antimalarial categories"
val_labels(long_data_processed$anyAM) <- c("No" = 0, "Yes" = 1)
var_label(long_data_processed$anyAM) <- "Any Antimalarial"
val_labels(long_data_processed$anyACT) <- c("No" = 0, "Yes" = 1)
var_label(long_data_processed$anyACT) <- "Any ACT"

################################################################################
# SECTION 4: NATIONAL REGISTRATION & QAACT VARIABLES
################################################################################

long_data_processed <- long_data_processed %>%
  mutate(
    natapp = if_else(natapp_master == 999, 0, as.numeric(natapp_master)),
    
    notnatapp = if_else(!is.na(drugcat1) & drugcat1 == 1 & natapp == 0, 1,
                        if_else(!is.na(drugcat1) & drugcat1 < 4, 0, NA_real_)),
    
    flact = if_else(!is.na(drugcat1) & anyACT == 1 & gname %in% c(40, 44, 49, 55), 1,
                    if_else(!is.na(drugcat1) & drugcat1 < 4, 0, NA_real_)),
    
    QA_all = if_else(!is.na(drugcat1) & natapp == 1 & qaact == 1 & drugcat1 == 1, 1,
                     if_else(!is.na(drugcat1) & drugcat1 < 4, 0, NA_real_)),
    
    QA_WHO = if_else(!is.na(drugcat1) & natapp == 0 & qaact == 1 & drugcat1 == 1, 1,
                     if_else(!is.na(drugcat1) & drugcat1 < 4, 0, NA_real_)),
    
    QA_NAT = if_else(!is.na(drugcat1) & natapp == 1 & qaact == 0 & drugcat1 == 1, 1,
                     if_else(!is.na(drugcat1) & drugcat1 < 4, 0, NA_real_)),
    
    QA_none = if_else(!is.na(drugcat1) & natapp == 0 & qaact == 0 & drugcat1 == 1, 1,
                      if_else(!is.na(drugcat1) & drugcat1 < 4, 0, NA_real_)),
    
    faact = if_else(!is.na(drugcat1) & qaact == 1 & gname %in% c(40, 44, 49, 55), 1,
                    if_else(!is.na(drugcat1) & drugcat1 < 4, 0, NA_real_))
  ) %>%
  mutate(
    naact = if_else(!is.na(drugcat1) & qaact == 1 & faact == 0, 1,
                    if_else(!is.na(drugcat1) & drugcat1 < 4, 0, NA_real_))
  )

# Add labels
val_labels(long_data_processed$natapp) <- c("No" = 0, "Yes" = 1)
var_label(long_data_processed$natapp) <- "Nat registered ACT"
val_labels(long_data_processed$notnatapp) <- c("No" = 0, "Yes" = 1)
var_label(long_data_processed$notnatapp) <- "Not nat registered ACT"
val_labels(long_data_processed$flact) <- c("No" = 0, "Yes" = 1)
var_label(long_data_processed$flact) <- "First-line ACT (AL, ASAQ, DHAPPQ, ASPY)"

qa_vars <- c("QA_all", "QA_WHO", "QA_NAT", "QA_none", "faact", "naact")
for(var in qa_vars) {
  val_labels(long_data_processed[[var]]) <- c("No" = 0, "Yes" = 1)
}

var_label(long_data_processed$QA_all) <- "WHO PQ and Nationally approved ACT"
var_label(long_data_processed$QA_WHO) <- "WHO PQ ACT, not Nat. Ap."
var_label(long_data_processed$QA_NAT) <- "Nat approved but not WHO PQ ACT"
var_label(long_data_processed$QA_none) <- "Not WHO PQ or Nat approved ACT"
var_label(long_data_processed$faact) <- "FAACT - First-line QAACT"
var_label(long_data_processed$naact) <- "NAACT - QA but not 1st-line ACT"

################################################################################
# SECTION 5: DOSAGE-SPECIFIC QA VARIABLES
################################################################################

long_data_processed <- long_data_processed %>%
  mutate(
    base_val = if_else(!is.na(drugcat1) & drugcat1 < 4, 0, NA_real_),
    
    pqaact = case_when(
      base_val == 0 & qaact == 1 & gname == 40 & sumstrength == 140 & size %in% c(6, 180) ~ 1,
      base_val == 0 & qaact == 1 & gname == 44 & sumstrength %in% c(200, 203) & size %in% c(6, 60) ~ 1,
      base_val == 0 & qaact == 1 & gname == 44 & sumstrength == 185 & size %in% c(3, 30, 75) ~ 1,
      base_val == 0 & qaact == 1 & gname == 47 & sumstrength == 75 & size == 6 ~ 1,
      base_val == 0 & qaact == 1 & gname == 50 & sumstrength == 575 & size == 4 ~ 1,
      base_val == 0 & qaact == 1 & gname == 55 & sumstrength == 180 & size == 3 ~ 1,
      base_val == 0 & qaact == 1 & gname == 49 & sumstrength == 80 & size %in% c(1,2,3,90) ~ 1,
      TRUE ~ base_val
    ),
    
    pact_fl = case_when(
      base_val == 0 & gname == 40 & sumstrength == 140 & fdc == 1 & size %in% c(6, 180) ~ 1,
      base_val == 0 & gname == 49 & sumstrength == 80 & size %in% c(2, 90) ~ 1,
      base_val == 0 & gname == 44 & sumstrength == 185 & fdc == 1 & size %in% c(3, 30, 75) ~ 1,
      base_val == 0 & gname == 55 & sumstrength == 180 & fdc == 1 & size == 3 ~ 1,
      TRUE ~ base_val
    ),
    
    aqaact = case_when(
      base_val == 0 & qaact == 1 & gname == 40 & sumstrength == 140 & fdc == 1 & size %in% c(24, 720) ~ 1,
      base_val == 0 & qaact == 1 & gname == 40 & sumstrength == 280 & fdc == 1 & size == 12 ~ 1,
      base_val == 0 & qaact == 1 & gname == 40 & sumstrength == 560 & fdc == 1 & size == 6 ~ 1,
      base_val == 0 & qaact == 1 & gname == 44 & sumstrength %in% c(200, 203) & fdc == 0 & size %in% c(24, 240) ~ 1,
      base_val == 0 & qaact == 1 & gname == 44 & sumstrength == 370 & fdc == 1 & size %in% c(6, 60, 150) ~ 1,
      base_val == 0 & qaact == 1 & gname == 47 & sumstrength == 450 & fdc == 0 & size == 9 ~ 1,
      base_val == 0 & qaact == 1 & gname == 47 & sumstrength == 300 & fdc == 1 & size == 6 ~ 1,
      base_val == 0 & qaact == 1 & gname == 55 & sumstrength %in% c(360, 540, 720) & fdc == 1 ~ 1,
      base_val == 0 & qaact == 1 & gname == 49 & sumstrength == 240 & fdc == 1 & size %in% c(9, 12, 90) ~ 1,
      TRUE ~ base_val
    ),
    
    aact_fl = case_when(
      base_val == 0 & gname == 40 & sumstrength == 140 & fdc == 1 & size %in% c(24, 720) ~ 1,
      base_val == 0 & gname == 40 & sumstrength == 260 & fdc == 1 & size == 12 ~ 1,
      base_val == 0 & gname == 40 & sumstrength == 560 & fdc == 1 & size == 6 ~ 1,
      base_val == 0 & gname == 44 & sumstrength == 370 & fdc == 1 & size == 6 ~ 1,
      base_val == 0 & qaact == 1 & gname == 55 & sumstrength == 360 & fdc == 1 & size == 12 ~ 1,
      TRUE ~ base_val
    )
  ) %>%
  select(-base_val)

# Add labels
dosage_vars <- c("pqaact", "pact_fl", "aqaact", "aact_fl")
for(var in dosage_vars) {
  val_labels(long_data_processed[[var]]) <- c("No" = 0, "Yes" = 1)
}

var_label(long_data_processed$pqaact) <- "Pediatric QAACT (2 years, 10kg)"
var_label(long_data_processed$pact_fl) <- "Pediatric first-line ACT (2 years, 10kg)"
var_label(long_data_processed$aqaact) <- "Adult QAACT (>18 years, 60kg)"
var_label(long_data_processed$aact_fl) <- "Adult first-line ACT (18+ years, 60kg)"

################################################################################
# SECTION 6: COMMON ANTIMALARIAL BINARY VARIABLES
################################################################################

binary_conditions <- list(
  nonqaact = quote(anyACT == 1 & qaact == 0),
  nonart = quote(drugcat1 == 3),
  CQ = quote(gname %in% c(4, 10)),
  SP = quote(gname == 21),
  SPAQ = quote(gname == 2),
  ATOPRO = quote(gname == 3),
  MQ = quote(gname == 11),
  QN = quote(gname %in% c(18, 19)),
  oralQN = quote(QN == 1 & a3 %in% c(1,3,4,5,8)),
  injQN = quote(QN == 1 & a3 %in% c(6,7)),
  nonartoth = quote(drugcat1 == 3 & !gname %in% c(4,10,21,2,18,19)),
  artmono = quote(drugcat1 == 2),
  oartmono = quote(gname %in% c(30,31,32,33) & a3 %in% c(1,3,4,5,9)),
  noartmono = quote(drugcat1 == 2 & oartmono != 1),
  AS = quote(gname == 32),
  injAS = quote(gname == 32 & a3 %in% c(6,7)),
  recAS = quote(gname == 32 & a3 == 2),
  AR = quote(gname == 31),
  injAR = quote(gname == 31 & a3 %in% c(6,7)),
  AE = quote(gname == 30),
  injAE = quote(gname == 30 & a3 %in% c(6,7))
)

long_data_processed <- long_data_processed %>%
  mutate(
    base_val = if_else(!is.na(drugcat1) & drugcat1 < 4, 0, NA_real_),
    !!!map(binary_conditions, ~quo(if_else(base_val == 0 & !!., 1, base_val)))
  ) %>%
  select(-base_val)

# Add labels
for(var in names(binary_conditions)) {
  val_labels(long_data_processed[[var]]) <- c("No" = 0, "Yes" = 1)
}

var_labels <- c(
  nonqaact = "Non-QAACT ACT", nonart = "Non-artemsinin therapy",
  CQ = "Chloroquine - packaged alone", SP = "Sulfadoxine pyrimethamine",
  SPAQ = "SP-Amodiaquine", ATOPRO = "Atovaquone-Proguanil", MQ = "Mefloquine",
  QN = "Quinine", oralQN = "Oral quinine", injQN = "Injectable quinine",
  nonartoth = "Other non-artemsinin therapy", artmono = "Artemisinin monotherapy",
  oartmono = "Oral artemisinin monotherapy", noartmono = "Non-oral artemisinin monotherapy",
  AS = "artesunate", injAS = "Injectable artesunate", recAS = "Rectal artesunate",
  AR = "artemether", injAR = "Injectable artemether", 
  AE = "arteether/artemotil", injAE = "Injectable arteether/artemotil"
)

for(var in names(var_labels)) {
  var_label(long_data_processed[[var]]) <- var_labels[[var]]
}

################################################################################
# SECTION 7: SEVERE MALARIA & SPECIFIC ACT VARIABLES
################################################################################

long_data_processed <- long_data_processed %>%
  mutate(
    base_val = if_else(!is.na(drugcat1) & drugcat1 < 4, 0, NA_real_),
    
    severe = if_else(base_val == 0 & (injQN == 1 | injAS == 1 | recAS == 1 | injAR == 1 | injAE == 1), 
                     1, base_val),
    
    sev_fl = base_val,  # USER INPUT REQUIRED - update based on national guidelines
    
    AL = if_else(gname == 40, 1, base_val),
    ASAQ = if_else(gname == 44, 1, base_val),
    APPQ = if_else(gname == 42, 1, base_val),
    ASSP = if_else(gname == 50, 1, base_val),
    DHAPPQ = if_else(gname == 55, 1, base_val),
    DHAPPQTRI = if_else(gname == 56, 1, base_val),
    ARPPQ = if_else(gname == 61, 1, base_val),
    
    ALqaact = if_else(gname == 40 & qaact == 1, 1, base_val),
    ASAQqaact = if_else(gname == 44 & qaact == 1, 1, base_val),
    DHAPPQqaact = if_else(gname == 55 & qaact == 1, 1, base_val),
    
    ALnonqaact = if_else(gname == 40 & nonqaact == 1, 1, base_val),
    DHAPPQnonqaact = if_else(gname == 55 & nonqaact == 1, 1, base_val),
    ASAQnonqaact = if_else(gname == 44 & nonqaact == 1, 1, base_val),
    
    otherACT = if_else(anyACT == 1 & !gname %in% c(40, 55, 61, 42, 44), 1, base_val),
    otherNonart = if_else(nonart == 1 & gname %in% c(1, 3, 14, 15, 22), 1, base_val)
  ) %>%
  select(-base_val)

val_labels(long_data_processed$severe) <- c("No" = 0, "Yes" = 1)
var_label(long_data_processed$severe) <- "Any severe malaria treatment"

act_vars <- c("AL", "ASAQ", "APPQ", "ASSP", "DHAPPQ", "DHAPPQTRI", "ARPPQ", 
              "ALqaact", "ASAQqaact", "DHAPPQqaact", "ALnonqaact", "DHAPPQnonqaact", 
              "ASAQnonqaact", "otherACT", "otherNonart")

for(var in act_vars) {
  val_labels(long_data_processed[[var]]) <- c("No" = 0, "Yes" = 1)
}

act_labels <- c(
  AL = "Artemether lumefantrine", ASAQ = "Artesunate amodiaquine", 
  APPQ = "Artemisinin-PPQ", ASSP = "AS-SP", DHAPPQ = "Dihydroartemisinin-Piperaquine",
  DHAPPQTRI = "DHA-PPQ-Trimethoprim", ARPPQ = "Arterolane PPQ",
  ALqaact = "AL QAACT", ASAQqaact = "ASAQ QAACT", DHAPPQqaact = "DHAPPQ QAACT",
  ALnonqaact = "AL non-QAACT", DHAPPQnonqaact = "DHAPPQ non-QAACT", 
  ASAQnonqaact = "ASAQ non-QAACT", 
  otherACT = "Other ACTs not reported individually",
  otherNonart = "Other non-artemisinin therapy not reported individually"
)

for(var in names(act_labels)) {
  var_label(long_data_processed[[var]]) <- act_labels[[var]]
}

################################################################################
# SECTION 8: COMBINATION VARIABLES
################################################################################

cmb_conditions <- list(
  cmb_tab_AL = quote(gname == 40 & a3 == 1),
  cmb_tab_ANAP = quote(gname == 41 & a3 == 1),
  cmb_tab_APPQ = quote(gname == 42 & a3 == 1),
  cmb_tab_APPQPRI = quote(gname == 43 & a3 == 1),
  cmb_tab_ASAQ = quote(gname == 44 & a3 == 1),
  cmb_tab_ASHAL = quote(gname == 45 & a3 == 1),
  cmb_tab_ASL = quote(gname == 46 & a3 == 1),
  cmb_tab_ASMQ = quote(gname == 47 & a3 == 1),
  cmb_tab_ASPPQ = quote(gname == 48 & a3 == 1),
  cmb_tab_ASPYR = quote(gname == 49 & a3 == 1),
  cmb_tab_ASSP = quote(gname == 50 & a3 == 1),
  cmb_tab_DHA = quote(gname == 51 & a3 == 1),
  cmb_tab_DHAHAL = quote(gname == 52 & a3 == 1),
  cmb_tab_DHAL = quote(gname == 53 & a3 == 1),
  cmb_tab_DHAMQ = quote(gname == 54 & a3 == 1),
  cmb_tab_DHAPPQ = quote(gname == 55 & a3 == 1),
  cmb_tab_DHAPPQTRI = quote(gname == 56 & a3 == 1),
  cmb_tab_DHAPYR = quote(gname == 57 & a3 == 1),
  cmb_tab_DHASP = quote(gname == 58 & a3 == 1),
  cmb_tab_ARPPQ = quote(gname == 61 & a3 == 1),
  
  cmb_ntb_AL = quote(gname %in% c(40, 60) & a3 %in% c(2,9)),
  cmb_ntb_ANAP = quote(gname == 41 & a3 %in% c(2,9)),
  cmb_ntb_APPQ = quote(gname == 42 & a3 %in% c(2,9)),
  cmb_ntb_APPQPRI = quote(gname == 43 & a3 %in% c(2,9)),
  cmb_ntb_ASAQ = quote(gname == 44 & a3 %in% c(2,9)),
  cmb_ntb_ASHAL = quote(gname == 45 & a3 %in% c(2,9)),
  cmb_ntb_ASL = quote(gname == 46 & a3 %in% c(2,9)),
  cmb_ntb_ASMQ = quote(gname == 47 & a3 %in% c(2,9)),
  cmb_ntb_ASPPQ = quote(gname == 48 & a3 %in% c(2,9)),
  cmb_ntb_ASPYR = quote(gname == 49 & a3 %in% c(2,9)),
  cmb_ntb_ASSP = quote(gname == 50 & a3 %in% c(2,9)),
  cmb_ntb_DHA = quote(gname == 51 & a3 %in% c(2,9)),
  cmb_ntb_DHAHAL = quote(gname == 52 & a3 %in% c(2,9)),
  cmb_ntb_DHAL = quote(gname == 53 & a3 %in% c(2,9)),
  cmb_ntb_DHAMQ = quote(gname == 54 & a3 %in% c(2,9)),
  cmb_ntb_DHAPPQ = quote(gname == 55 & a3 %in% c(2,9)),
  cmb_ntb_DHAPPQTRI = quote(gname == 56 & a3 %in% c(2,9)),
  cmb_ntb_DHAPYR = quote(gname == 57 & a3 %in% c(2,9)),
  cmb_ntb_DHASP = quote(gname == 58 & a3 %in% c(2,9)),
  cmb_ntb_ARPPQ = quote(gname == 61 & a3 %in% c(2,9)),
  cmb_ntb_otherACT = quote(!gname %in% c(40,42,44,55,61) & drugcat1 == 1 & a3 %in% c(2,9))
)

long_data_processed <- long_data_processed %>%
  mutate(
    base_val = if_else(!is.na(drugcat1) & drugcat1 < 4, 0, NA_real_),
    !!!map(cmb_conditions, ~quo(if_else(base_val == 0 & !!., 1, base_val))),
    # Exact original logic: just check gname not in list and a3==1
    cmb_tab_otherACT = case_when(
      !gname %in% c(40,42,44,55,61) & a3 == 1 ~ 1,
      TRUE ~ base_val
    )
  ) %>%
  select(-base_val)

################################################################################
# SECTION 9: STRENGTH & GUIDELINES VARIABLES
################################################################################

long_data_processed <- long_data_processed %>%
  mutate(
    # Active ingredient strengths
    aiStrength_a = case_when(
      !is.na(ai1_mg) & producttype == 1 ~ ai1_mg,
      !is.na(ai1_mg) & a3 == 7 ~ ai1_mg,
      !is.na(ai1_mg) & !is.na(ai1_ml) & ai1_ml != 0 & a3 != 7 & producttype != 1 ~ ai1_mg/ai1_ml,
      TRUE ~ NA_real_
    ),
    aiStrength_b = case_when(
      !is.na(ai2_mg) & producttype == 1 ~ ai2_mg,
      !is.na(ai2_mg) & a3 == 7 ~ ai2_mg,
      !is.na(ai2_mg) & !is.na(ai2_ml) & ai2_ml != 0 & a3 != 7 & producttype != 1 ~ ai2_mg/ai2_ml,
      TRUE ~ NA_real_
    ),
    aiStrength_c = case_when(
      !is.na(ai3_mg) & producttype == 1 ~ ai3_mg,
      !is.na(ai3_mg) & a3 == 7 ~ ai3_mg,
      !is.na(ai3_mg) & !is.na(ai3_ml) & ai3_ml != 0 & a3 != 7 & producttype != 1 ~ ai3_mg/ai3_ml,
      TRUE ~ NA_real_
    ),
    sumstrength = pmap_dbl(list(aiStrength_a, aiStrength_b, aiStrength_c), 
                           ~ sum(c(...), na.rm = TRUE)),
    
    # National treatment guidelines
    base_val = if_else(!is.na(drugcat1) & drugcat1 < 4, 0, NA_real_),
    
    natrxg = case_when(
      base_val == 0 & a3 == 1 & gname == 40 & sumstrength %in% c(140, 280, 560) & fdc == 1 ~ 1,
      base_val == 0 & a3 == 1 & gname == 44 & sumstrength %in% c(92.5, 185, 370) & fdc == 1 ~ 1,
      base_val == 0 & a3 == 1 & gname == 55 & sumstrength %in% c(180, 360, 720) & fdc == 1 ~ 1,
      base_val == 0 & a3 == 3 & gname == 49 & sumstrength == 80 ~ 1,
      base_val == 0 & a3 %in% c(6,7) & gname %in% c(32, 31, 19) ~ 1,
      base_val == 0 & a3 == 2 & gname == 32 ~ 1,
      TRUE ~ base_val
    ),
    
    notnatrxg = if_else(base_val == 0 & natrxg == 0, 1, base_val),
    
    # QA AL pack sizes - exactly match original with separate conditions
    qaal_1 = if_else(a3 == 1 & qaact == 1 & gname == 40 & sumstrength == 140 & size %in% c(6, 60), 1, 0),
    qaal_2 = case_when(
      a3 == 1 & qaact == 1 & gname == 40 & sumstrength == 280 & size %in% c(6, 60) ~ 1,
      a3 == 1 & qaact == 1 & gname == 40 & sumstrength == 140 & size == 12 ~ 1,
      TRUE ~ 0
    ),
    qaal_3 = if_else(a3 == 1 & qaact == 1 & gname == 40 & sumstrength == 140 & size == 18, 1, 0, missing = 0),
    qaal_4 = case_when(
      a3 == 1 & qaact == 1 & gname == 40 & sumstrength == 560 & size %in% c(6, 60) ~ 1,
      a3 == 1 & qaact == 1 & gname == 40 & sumstrength == 140 & size == 240 ~ 1,
      TRUE ~ 0
    ),
    
    nqaal_1 = if_else(a3 == 1 & qaact == 0 & gname == 40 & sumstrength == 140 & size %in% c(6, 60), 1, 0),
    nqaal_2 = case_when(
      a3 == 1 & qaact == 0 & gname == 40 & sumstrength == 280 & size %in% c(6, 60) ~ 1,
      a3 == 1 & qaact == 0 & gname == 40 & sumstrength == 140 & size == 12 ~ 1,
      TRUE ~ 0
    ),
    nqaal_3 = if_else(a3 == 1 & qaact == 0 & gname == 40 & sumstrength == 140 & size == 18, 1, 0, missing = 0),
    nqaal_4 = case_when(
      a3 == 1 & qaact == 0 & gname == 40 & sumstrength == 560 & size %in% c(6, 60) ~ 1,
      a3 == 1 & qaact == 0 & gname == 40 & sumstrength == 140 & size == 240 ~ 1,
      TRUE ~ 0
    ),
    
    # Product count
    countprod = if_else(anyAM == 1 | rdt_true == 1, 1, NA_real_)
  ) %>%
  select(-base_val)

# Add labels
val_labels(long_data_processed$natrxg) <- c("No" = 0, "Yes" = 1)
var_label(long_data_processed$natrxg) <- "Antimalarial included in National Malaria Treatment Guidelines"
val_labels(long_data_processed$notnatrxg) <- c("No" = 0, "Yes" = 1)
var_label(long_data_processed$notnatrxg) <- "Antimalarial not included in National Malaria Treatment Guidelines"

qa_al_vars <- c("qaal_1", "qaal_2", "qaal_3", "qaal_4", "nqaal_1", "nqaal_2", "nqaal_3", "nqaal_4")
for(var in qa_al_vars) {
  val_labels(long_data_processed[[var]]) <- c("No" = 0, "Yes" = 1)
}

var_label(long_data_processed$qaal_1) <- "QA AL pack size 1 (for an infant 5-15kg)"
var_label(long_data_processed$qaal_2) <- "QA AL pack size 2 (for a child 15-25 kgs)"
var_label(long_data_processed$qaal_3) <- "QA AL pack size 3 (for an adolescent 25-35 kgs)"
var_label(long_data_processed$qaal_4) <- "QA AL pack size 4 (for an adult 35+ kgs)"
var_label(long_data_processed$nqaal_1) <- "Non-QA AL pack size 1 (for an infant 5-15kg)"
var_label(long_data_processed$nqaal_2) <- "Non-QA AL pack size 2 (for a child 15-25 kgs)"
var_label(long_data_processed$nqaal_3) <- "Non-QA AL pack size 3 (for an adolescent 25-35 kgs)"
var_label(long_data_processed$nqaal_4) <- "Non-QA AL pack size 4 (for an adult 35+ kgs)"

val_labels(long_data_processed$countprod) <- c("No" = 0, "Yes" = 1)
var_label(long_data_processed$countprod) <- "Product count (AM or RDT)"

################################################################################
# SECTION 10: SAVE PROCESSED DATA
################################################################################

output_file <- here("Data", "Management data", paste0(country, "_", year, "_am_rdt_os_cleaned_long_wt_amcat.csv"))
fwrite(long_data_processed, output_file, row.names = FALSE, na = "")

################################################################################
# GENERATE HTML LOG
################################################################################

# Setup log file
log_dir <- here("Data", "Management data", "Data cleaning notes")
if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE)
log_file <- file.path(log_dir, paste0("AwL_", country, "_", year, "_antimalarial_categories_notes.html"))

sink(log_file, type = "output")
cat("<html><head><title>Antimalarial Categories Analysis</title>")
cat("<style>
table { border-collapse: collapse; width: 100%; margin: 10px 0; }
th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }
th { background-color: #f2f2f2; }
.warning { color: red; font-weight: bold; }
.note { background-color: #fff3cd; padding: 10px; margin: 10px 0; }
</style></head><body>")

cat("<h1>ACTwatch LITE - Antimalarial Category Variables</h1>")
cat(paste("<h2>Country:", country, "| Year:", year, "</h2>"))

# =============================================================================
# HTML CONTENT GENERATION - All calculated from final data
# =============================================================================

cat("<h3>GENERATING BROAD DRUG CATEGORIES</h3>")

gname_freq <- long_data_processed %>% 
  filter(!is.na(gname)) %>%
  count(gname, sort = TRUE) %>%
  slice_head(n = 20)

html_table(gname_freq, "Generic Name (gname) Frequencies (Top 20)")

cat("<h3>CORE ANTIMALARIAL CLASSIFICATION</h3>")
cat("<h3>NATIONAL REGISTRATION STATUS</h3>")
cat("<div class='note'>USER INPUT REQUIRED: Review first-line ACT selection according to national guidelines.</div>")

cat("Generating dosage-specific variables...\n")
cat("Generating common antimalarial variables...\n")
cat("Generating severe malaria and specific ACT variables...\n")
cat("Generating combination variables...\n")
cat("Generating strength and guidelines variables...\n")
cat("Finalizing and saving data...\n")

cat("ANTIMALARIAL CATEGORIES PROCESSING COMPLETE!\n")
cat("Final data saved to:", basename(output_file), "\n")

if (sink.number() > 0) sink()
cat("</body></html>")

################################################################################
# CONSOLE OUTPUT
################################################################################

cat("\n=== ANTIMALARIAL CATEGORIES PROCESSING COMPLETE ===\n")
cat("Records processed:", nrow(long_data_processed), "\n")

# Count antimalarial products
am_count <- long_data_processed %>% 
  filter(!is.na(drugcat1) & drugcat1 < 4) %>% 
  nrow()

cat("Antimalarial products:", am_count, "\n")

# Show breakdown by drug category
if (am_count > 0) {
  drugcat_summary <- long_data_processed %>%
    filter(!is.na(drugcat1)) %>%
    count(drugcat1) %>%
    arrange(drugcat1)
  
  cat("\nDrug category breakdown:\n")
  for (i in 1:nrow(drugcat_summary)) {
    cat_label <- case_when(
      drugcat_summary$drugcat1[i] == 1 ~ "ACT",
      drugcat_summary$drugcat1[i] == 2 ~ "Artemisinin monotherapy",
      drugcat_summary$drugcat1[i] == 3 ~ "Non-artemisinin therapy",
      drugcat_summary$drugcat1[i] == 4 ~ "Prophylaxis",
      TRUE ~ "Other"
    )
    cat(sprintf("  %s: %d\n", cat_label, drugcat_summary$n[i]))
  }
}

# Show top antimalarial types
top_am <- long_data_processed %>%
  filter(!is.na(drugcat1) & drugcat1 < 4) %>%
  summarise(
    ACTs = sum(anyACT == 1, na.rm = TRUE),
    AL = sum(AL == 1, na.rm = TRUE),
    ASAQ = sum(ASAQ == 1, na.rm = TRUE),
    DHAPPQ = sum(DHAPPQ == 1, na.rm = TRUE),
    QAACT = sum(qaact == 1 & anyACT == 1, na.rm = TRUE)
  )

if (am_count > 0) {
  cat("\nTop antimalarial types:\n")
  cat(sprintf("  ACTs: %d (%.1f%%)\n", top_am$ACTs, top_am$ACTs/am_count*100))
  cat(sprintf("  - AL: %d\n", top_am$AL))
  cat(sprintf("  - ASAQ: %d\n", top_am$ASAQ))
  cat(sprintf("  - DHAPPQ: %d\n", top_am$DHAPPQ))
  cat(sprintf("  QAACTs: %d (%.1f%% of ACTs)\n", top_am$QAACT, 
              ifelse(top_am$ACTs > 0, top_am$QAACT/top_am$ACTs*100, 0)))
}

# Count variables generated
var_count <- sum(c(
  "drugcat1", "amCategory", "anyAM", "anyACT", "natapp", "notnatapp", "flact",
  "QA_all", "QA_WHO", "QA_NAT", "QA_none", "faact", "naact",
  "pqaact", "pact_fl", "aqaact", "aact_fl",
  "nonqaact", "nonart", "CQ", "SP", "SPAQ", "ATOPRO", "MQ", "QN", "oralQN", "injQN",
  "nonartoth", "artmono", "oartmono", "noartmono", "AS", "injAS", "recAS",
  "AR", "injAR", "AE", "injAE", "severe", "sev_fl",
  "AL", "ASAQ", "APPQ", "ASSP", "DHAPPQ", "DHAPPQTRI", "ARPPQ",
  "ALqaact", "ASAQqaact", "DHAPPQqaact", "ALnonqaact", "DHAPPQnonqaact", "ASAQnonqaact",
  "otherACT", "otherNonart", "natrxg", "notnatrxg", "countprod"
) %in% names(long_data_processed))

cat("\nVariables generated:", var_count, "antimalarial category variables\n")
cat("\nOutput files:\n")
cat("  Dataset:", output_file, "\n")
cat("  HTML log:", log_file, "\n")

################################################################################
# END
################################################################################