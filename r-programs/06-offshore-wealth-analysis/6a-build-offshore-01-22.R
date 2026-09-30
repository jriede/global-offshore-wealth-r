# ==============================================================================
# REPL: Global Offshore Wealth Using R, 2001-2021
# jlenke, 2026
# This program merges Swiss fiduciary and BIS bank deposits; constructs country
# shares (using a 5-year smoothing method) and values of offshore wealth in
# Switzerland and several haven groups.
#
# databases used: - bis-deposits-all-01-22.dta
#                 - bis-interbank-all-01-22.dta
#                 - fiduciary-87-22.dta
#                 - gdp_current.csv
#                 - isocodes.xlsx
#
# outputs:        - offshore2001.dta ... offshore2022.dta
# ==============================================================================

library(tidyverse)
library(readxl)
library(haven)


# ==============================================================================
# Helper functions
# ==============================================================================

# Stata's sum() ignores missing values. This helper reproduces that behaviour.
stata_sum <- function(x) {
  sum(x, na.rm = TRUE)
}

# Stata's mean in collapse ignores missing values.
stata_mean <- function(x) {
  if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}

# Stata "first" equivalent used below.
first_non_missing <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) NA else x[1]
}


# ==============================================================================
# Read data used repeatedly
# ==============================================================================
bis_deposits <- read_work_data("bis-deposits-all-01-22")
#bis_deposits <- read_dta(file.path(work2, "bis-deposits-all-01-22.dta"))

bis_interbank <- read_work_data("bis-interbank-all-01-22")
#bis_interbank <- read_dta(file.path(work2, "bis-interbank-all-01-22.dta"))

fiduciary_all <- read_work_data("fiduciary-87-22")
# fiduciary_all <- read_dta(file.path(work2, "fiduciary-87-22.dta"))

isocodes <- read_excel(
  file.path(raw, "isocodes.xlsx"),
  sheet = "iso"
)

gdp_current <- read_csv(
  file.path(raw, "gdp_current.csv"),
  show_col_types = FALSE
)

household_shares <- read_excel(
  file.path(raw, "FGZ-raw-data.xlsx"),
  sheet = "sharehouseholddep",
  range = "A3:W25"
)


# ==============================================================================
# Storage for country-level annual shares
# ==============================================================================

countries <- vector(
  "list",
  length = 22
)

names(countries) <- as.character(
  2001:2022
)


# ==============================================================================
# I–III ---- Construct annual offshore datasets
# ==============================================================================

for (i in 2001:2022) {
  
  message("Processing year ", i, " ...")
  
  
  # ============================================================================
  # I ---- Load BIS locational banking statistics
  # ============================================================================
  
  # ---------------------------------------------------------------------------
  # I.1 Adjustments to non-bank and bank deposit data
  # ---------------------------------------------------------------------------
  
  bisbilat <- full_join(
    bis_deposits,
    bis_interbank,
    by = c("bank", "saver", "year"),
    suffix = c("", "_interbank")
  ) %>%
    mutate(
      # Stata: replace totdep = totdep - dep
      interbank = totdep - dep,
      
      interbank = if_else(
        bank %in% c("1N", "5A", "1R"),
        0,
        interbank
      ),
      
      saver = case_when(
        saver == "1W" ~ "AG",
        saver == "1Z" ~ "VG",
        TRUE          ~ saver
      )
    ) %>%
    filter(
      !saver %in% c(
        "DD",
        "YU",
        "SU",
        "C9",
        "CS",
        "5J"
      ),
      year == i
    ) %>%
    group_by(
      bank,
      saver
    ) %>%
    summarise(
      dep       = stata_mean(dep),
      interbank = stata_mean(interbank),
      OFC       = stata_mean(OFC),
      year      = stata_mean(year),
      .groups   = "drop"
    )
  
  
  # ---------------------------------------------------------------------------
  # Reshape wide
  # ---------------------------------------------------------------------------
  
  bis_wide <- bisbilat %>%
    select(
      saver,
      bank,
      dep,
      interbank
    ) %>%
    pivot_wider(
      names_from = bank,
      values_from = c(dep, interbank),
      names_sep = ""
    )
  
  
  # Stata replaces missing values with zero for these banks
  banks_zero <- c(
    "CH", "KY", "MY", "PA", "GG", "IM", "JE", "LU",
    "CL", "MO", "BE", "AT", "US", "GB", "1N", "5A",
    "1R", "CY"
  )
  
  for (deposit in c("dep", "interbank")) {
    
    for (bank_code in banks_zero) {
      
      var <- paste0(
        deposit,
        bank_code
      )
      
      if (!var %in% names(bis_wide)) {
        bis_wide[[var]] <- 0
      }
      
      bis_wide[[var]][is.na(bis_wide[[var]])] <- 0
    }
  }
  
  
  # ---------------------------------------------------------------------------
  # Asian haven aggregate
  # ---------------------------------------------------------------------------
  
  bis_wide <- bis_wide %>%
    mutate(
      depAS =
        dep1N +
        depMY -
        depKY -
        depGG -
        depIM -
        depJE -
        depPA,
      
      interbankAS = interbankMY
    )
  
  
  # ---------------------------------------------------------------------------
  # Household shares
  # ---------------------------------------------------------------------------
  
  hh_i <- household_shares %>%
    filter(
      year == i
    ) %>%
    mutate(
      AS = 0.7
    )
  
  
  # Stata merge 1:m year using bisbilat
  bis_wide <- bis_wide %>%
    mutate(
      year = i
    ) %>%
    left_join(
      hh_i,
      by = "year"
    )
  
  
  household_banks <- c(
    "CH", "AS", "GG", "IM", "JE", "PA", "LU",
    "CY", "MO", "MY", "KY", "BE", "AT", "BH",
    "BM", "BS", "HK", "SG", "GB", "US", "CL",
    "AN", "CW"
  )
  
  
  for (bank_code in household_banks) {
    
    dep_var <- paste0(
      "dep",
      bank_code
    )
    
    if (
      dep_var %in% names(bis_wide) &&
      bank_code %in% names(bis_wide)
    ) {
      
      bis_wide[[dep_var]] <-
        bis_wide[[bank_code]] *
        bis_wide[[dep_var]]
    }
  }
  
  
  # ---------------------------------------------------------------------------
  # Caribbean and European haven aggregates
  # ---------------------------------------------------------------------------
  
  # Ensure required variables exist
  required_vars <- c(
    "depKY", "depPA", "depCL", "depUS",
    "depGG", "depIM", "depJE", "depLU",
    "depAT", "depBE", "depGB", "depCY",
    "interbankKY", "interbankPA",
    "interbankCL", "interbankUS",
    "interbankGG", "interbankIM",
    "interbankJE", "interbankLU",
    "interbankAT", "interbankBE",
    "interbankGB", "interbankCY"
  )
  
  for (v in required_vars) {
    if (!v %in% names(bis_wide)) {
      bis_wide[[v]] <- 0
    }
  }
  
  
  bis_wide <- bis_wide %>%
    mutate(
      depCR =
        depKY +
        depPA +
        depCL +
        depUS,
      
      depEU =
        depGG +
        depIM +
        depJE +
        depLU +
        depAT +
        depBE +
        depGB +
        depCY,
      
      interbankCR =
        interbankKY +
        interbankPA +
        interbankCL +
        interbankUS,
      
      interbankEU =
        interbankGG +
        interbankIM +
        interbankJE +
        interbankLU +
        interbankAT +
        interbankBE +
        interbankGB +
        interbankCY
    )
  
  
  # Drop household-share columns after they have been applied
  bis_wide <- bis_wide %>%
    select(
      -any_of(
        household_banks
      )
    )
  
  
  # ---------------------------------------------------------------------------
  # Back to long format
  # ---------------------------------------------------------------------------
  
  dep_long <- bis_wide %>%
    select(
      saver,
      year,
      starts_with("dep")
    ) %>%
    pivot_longer(
      cols = starts_with("dep"),
      names_to = "bank",
      values_to = "dep"
    ) %>%
    mutate(
      bank = sub(
        "^dep",
        "",
        bank
      )
    )
  
  
  interbank_long <- bis_wide %>%
    select(
      saver,
      year,
      starts_with("interbank")
    ) %>%
    pivot_longer(
      cols = starts_with("interbank"),
      names_to = "bank",
      values_to = "interbank"
    ) %>%
    mutate(
      bank = sub(
        "^interbank",
        "",
        bank
      )
    )
  
  
  bisbilat <- full_join(
    dep_long,
    interbank_long,
    by = c(
      "saver",
      "year",
      "bank"
    )
  )
  
  
  # ============================================================================
  # I.2 ---- Merge ISO country codes
  # ============================================================================
  
  saver_codes <- isocodes %>%
    rename(
      saver     = iso2,
      namesaver = isoname,
      iso3saver = iso3
    )
  
  
  bisbilat <- bisbilat %>%
    left_join(
      saver_codes,
      by = "saver"
    ) %>%
    mutate(
      iso3saver = if_else(
        saver == "PU",
        "UMI",
        iso3saver
      ),
      
      saver = if_else(
        saver == "PU",
        "UM",
        saver
      )
    )
  
  
  # ---------------------------------------------------------------------------
  # Add GDP
  # ---------------------------------------------------------------------------
  
  gdp_i <- gdp_current %>%
    filter(
      year == i
    ) %>%
    rename(
      iso3saver = iso3,
      gdp = gdp_current_dollars
    ) %>%
    select(
      year,
      iso3saver,
      gdp
    )
  
  
  bisbilat <- bisbilat %>%
    left_join(
      gdp_i,
      by = c(
        "year",
        "iso3saver"
      )
    )
  
  
  # Stata:
  # su gdp if bank == "5A"
  # local worldgdp = r(sum)
  
  worldgdp <- bisbilat %>%
    filter(
      bank == "5A"
    ) %>%
    summarise(
      value = stata_sum(gdp)
    ) %>%
    pull(value)
  
  
  bisbilat <- bisbilat %>%
    mutate(
      shgdp = gdp / worldgdp
    )
  
  
  # ---------------------------------------------------------------------------
  # Add bank ISO codes
  # ---------------------------------------------------------------------------
  
  bank_codes <- isocodes %>%
    rename(
      bank     = iso2,
      namebank = isoname,
      iso3bank = iso3
    )
  
  
  bisbilat <- bisbilat %>%
    left_join(
      bank_codes,
      by = "bank"
    ) %>%
    mutate(
      namebank = case_when(
        bank == "CR" ~ "Caribbean havens",
        bank == "AS" ~ "Asian havens",
        bank == "EU" ~ "European havens",
        bank == "1N" ~ "Haven aggregate",
        bank == "5A" ~ "All BIS-reporting banks",
        bank == "1R" ~ "Residual countries",
        TRUE         ~ namebank
      ),
      
      iso3bank = if_else(
        bank %in% c(
          "CR",
          "AS",
          "EU",
          "HA",
          "OC"
        ),
        "",
        iso3bank
      )
    )
  
  
  # ============================================================================
  # II ---- Load fiduciary data
  # ============================================================================
  
  fiduciary <- fiduciary_all %>%
    filter(
      year == i
    ) %>%
    rename(
      iso3 = ccode
    ) %>%
    filter(
      nchar(iso3) <= 3
    ) %>%
    select(
      -any_of(
        c(
          "continent",
          "group"
        )
      )
    ) %>%
    rename(
      haven = ofc
    )
  
  
  # ---------------------------------------------------------------------------
  # Country classifications
  # ---------------------------------------------------------------------------
  
  eu_codes <- c(
    "BEL", "FRA", "ITA", "LUX", "NLD", "DEU",
    "DNK", "IRL", "GBR", "GRC", "PRT", "ESP",
    "AUT", "FIN", "SWE", "HUN", "CYP", "CZE",
    "EST", "LVA", "LTU", "MLT", "POL", "SVK",
    "SVN"
  )
  
  
  fiduciary <- fiduciary %>%
    mutate(
      gcc = if_else(
        iso3 %in% c(
          "SAU",
          "ARE",
          "KWT",
          "QAT",
          "OMN"
        ),
        1,
        0
      ),
      
      eu = if_else(
        iso3 %in% eu_codes,
        1,
        0
      ),
      
      africa = if_else(
        iso3 %in% c(
          "EGY",
          "IRN",
          "IRQ",
          "ISR",
          "JOR",
          "SYR",
          "YEM"
        ),
        1,
        africa
      ),
      
      africa = if_else(
        iso3 %in% c(
          "DJI",
          "GMB"
        ),
        0,
        africa
      ),
      
      haven = if_else(
        iso3 %in% c(
          "DJI",
          "GMB"
        ),
        1,
        haven
      ),
      
      latin_am = if_else(
        caribbean == 1,
        1,
        latin_am
      ),
      
      latin_am = if_else(
        iso3 == "GUY",
        0,
        latin_am
      ),
      
      haven = if_else(
        iso3 == "GUY",
        1,
        haven
      ),
      
      asia = if_else(
        iso3 == "RUS",
        0,
        asia
      ),
      
      russia = if_else(
        iso3 == "RUS",
        1,
        0
      ),
      
      asia = if_else(
        iso3 %in% c(
          "BRN",
          "MDV",
          "SLB",
          "PNG"
        ),
        0,
        asia
      ),
      
      haven = if_else(
        iso3 %in% c(
          "BRN",
          "MDV",
          "SLB",
          "PNG"
        ),
        1,
        haven
      )
    ) %>%
    select(
      -any_of(
        c(
          "middle_east",
          "caribbean"
        )
      )
    )
  
  
  # ---------------------------------------------------------------------------
  # Add Swiss fiduciary deposits
  #
  # Stata:
  # expand 2 if iso3 == "LIE"
  # CHE = (1 - 0.45) / 0.45 * LIE
  # ---------------------------------------------------------------------------
  
  liechtenstein_che <- fiduciary %>%
    filter(
      iso3 == "LIE"
    ) %>%
    mutate(
      iso3    = "CHE",
      ifscode = 146,
      cn      = "Switzerland"
    )
  
  
  fiduvar <- c(
    "lfidu",
    "lfidudol",
    "lfidu2",
    "lfidu2dol"
  )
  
  
  for (v in fiduvar) {
    
    liechtenstein_che[[v]] <-
      ((1 - 0.45) / 0.45) *
      liechtenstein_che[[v]]
  }
  
  
  fiduciary <- bind_rows(
    fiduciary,
    liechtenstein_che
  ) %>%
    rename(
      iso3saver = iso3,
      amt_fidu  = lfidu2dol
    ) %>%
    select(
      -any_of(
        c(
          "lfidu",
          "lfidudol",
          "lfidu2"
        )
      )
    )
  
  
  # Stata collapse
  fiduciary <- fiduciary %>%
    group_by(
      iso3saver
    ) %>%
    summarise(
      amt_fidu = stata_mean(amt_fidu),
      
      across(
        all_of(
          c(
            "euro16",
            "rich",
            "developing",
            "haven",
            "north_am",
            "latin_am",
            "gcc",
            "russia",
            "asia",
            "africa",
            "europe",
            "eu"
          )
        ),
        first_non_missing
      ),
      
      .groups = "drop"
    ) %>%
    mutate(
      bank = "CH"
    )
  
  
  # ============================================================================
  # II.2 ---- Merge BIS and fiduciary data
  # ============================================================================
  
  offshore <- full_join(
    bisbilat,
    fiduciary,
    by = c(
      "iso3saver",
      "bank"
    )
  ) %>%
    mutate(
      namebank = if_else(
        bank == "CH",
        "Switzerland",
        namebank
      ),
      
      iso3bank = if_else(
        bank == "CH",
        "CHE",
        iso3bank
      )
    ) %>%
    filter(
      !is.na(bank),
      bank != ""
    )
  
  
  # Save intermediate version, as in Stata
  saveRDS(offshore, file = file.path(work,paste0("offshore",i,".rds")))
  #write_dta(offshore,file.path(work,paste0("offshore",i,".dta")))
  
  
  # ---------------------------------------------------------------------------
  # Update country classifications for every bank-saver pair
  # ---------------------------------------------------------------------------
  
  fidu_classes <- fiduciary %>%
    select(
      iso3saver,
      euro16,
      rich,
      developing,
      haven,
      north_am,
      latin_am,
      gcc,
      russia,
      asia,
      africa,
      europe,
      eu
    ) %>%
    distinct(
      iso3saver,
      .keep_all = TRUE
    )
  
  
  offshore <- offshore %>%
    left_join(
      fidu_classes,
      by = "iso3saver",
      suffix = c("", "_fidu")
    )
  
  
  classification_vars <- c(
    "euro16",
    "rich",
    "developing",
    "haven",
    "north_am",
    "latin_am",
    "gcc",
    "russia",
    "asia",
    "africa",
    "europe",
    "eu"
  )
  
  
  # Reproduce Stata merge ..., update:
  # only replace an existing value when it is missing.
  
  for (v in classification_vars) {
    
    v_fidu <- paste0(
      v,
      "_fidu"
    )
    
    if (v_fidu %in% names(offshore)) {
      
      offshore[[v]] <- coalesce(
        offshore[[v]],
        offshore[[v_fidu]]
      )
      
      offshore[[v_fidu]] <- NULL
    }
  }
  
  
  offshore <- offshore %>%
    rename(
      amt_bis   = dep,
      amt_inter = interbank
    )
  
  
  # OFC and haven
  if ("OFC" %in% names(offshore)) {
    
    offshore <- offshore %>%
      mutate(
        OFC = if_else(
          is.na(OFC) & !is.na(haven),
          haven,
          OFC
        ),
        
        haven = if_else(
          is.na(haven) & !is.na(OFC),
          OFC,
          haven
        )
      ) %>%
      select(
        -OFC
      )
  }
  
  
  offshore <- offshore %>%
    mutate(
      europe = if_else(
        iso3saver %in% c(
          "MNE",
          "GRL"
        ),
        1,
        europe
      ),
      
      africa = if_else(
        iso3saver == "PSE",
        1,
        africa
      ),
      
      haven = if_else(
        iso3saver %in% c(
          "BLM",
          "PUS",
          "FRO",
          "AIA"
        ),
        1,
        haven
      )
    )
  
  
  # ANT, CHE, ATG and KNA treated as havens
  vars_zero_haven <- c(
    "europe",
    "developing",
    "africa",
    "rich",
    "euro16",
    "north_am",
    "latin_am",
    "russia",
    "asia",
    "eu",
    "gcc"
  )
  
  
  for (v in vars_zero_haven) {
    
    offshore[[v]] <- if_else(
      offshore$iso3saver %in% c(
        "ANT",
        "CHE",
        "ATG",
        "KNA"
      ),
      0,
      offshore[[v]]
    )
  }
  
  
  offshore <- offshore %>%
    mutate(
      haven = if_else(
        iso3saver %in% c(
          "ANT",
          "CHE",
          "ATG",
          "KNA"
        ),
        1,
        haven
      )
    )
  
  
  # Serbia and Montenegro
  vars_scg_zero <- c(
    "africa",
    "rich",
    "euro16",
    "north_am",
    "latin_am",
    "russia",
    "asia",
    "eu",
    "gcc",
    "haven"
  )
  
  
  for (v in vars_scg_zero) {
    
    offshore[[v]] <- if_else(
      offshore$iso3saver == "SCG",
      0,
      offshore[[v]]
    )
  }
  
  
  offshore <- offshore %>%
    mutate(
      europe = if_else(
        iso3saver == "SCG",
        1,
        europe
      ),
      
      developing = if_else(
        iso3saver == "SCG",
        1,
        developing
      )
    )
  
  
  # ============================================================================
  # III ---- Compute and merge shares of deposits
  # ============================================================================
  
  offshore %>%
    filter(is.na(saver)) %>%
    select(
      iso3saver,
      namesaver,
      bank,
      namebank,
      amt_bis,
      amt_inter,
      amt_fidu
    ) %>%
    distinct()
  
  # ---------------------------------------------------------------------------
  # III.1 Shell-company adjustment
  # ---------------------------------------------------------------------------
  
  shell_shares <- c(
    CH = 1.00,
    IE = 0.85,
    GB = 0.65,
    NL = 0.75,
    BE = 0.50,
    US = 0.20
  )
  
  
  for (saver_code in names(shell_shares)) {
    
    share_shell <- shell_shares[[saver_code]]
    
    original <- offshore %>%
      filter(
        saver == saver_code
      )
    
    shell <- original %>%
      mutate(
        saver     = paste0(saver_code, "H"),
        namesaver = paste0("Shell corp ", saver_code),
        gdp       = 0,
        shgdp     = 0,
        haven     = 1,
        iso3saver = paste0(saver_code, "H")
      )
    
    
    for (v in c(
      "north_am",
      "europe",
      "rich"
    )) {
      
      shell[[v]] <- 0
    }
    
    
    amount_vars <- names(shell)[
      startsWith(
        names(shell),
        "amt"
      )
    ]
    
    
    for (v in amount_vars) {
      
      shell[[v]] <-
        shell[[v]] *
        share_shell
    }
    
    
    idx_original <-
      !is.na(offshore$saver) &
      offshore$saver == saver_code
    
    
    for (v in amount_vars) {
      
      offshore[[v]][idx_original] <-
        offshore[[v]][idx_original] *
        (1 - share_shell)
    }
    
    
    offshore <- bind_rows(
      offshore,
      shell
    )
  }
  
  
  # ---------------------------------------------------------------------------
  # Create raw and corrected shares
  # ---------------------------------------------------------------------------
  
  banks_share <- c(
    "1N", "5A", "1R", "US", "GB", "CL",
    "GG", "IM", "JE", "KY", "LU", "MO",
    "MY", "PA", "CH", "AT", "BE", "EU",
    "CR", "AS", "HA", "OC", "CY"
  )
  
  
  for (y in c(
    "fidu",
    "bis",
    "inter"
  )) {
    
    amt_var <- paste0(
      "amt_",
      y
    )
    
    raw_var <- paste0(
      "rawsh_",
      y
    )
    
    sh_var <- paste0(
      "sh_",
      y
    )
    
    
    offshore[[raw_var]] <- 0
    offshore[[sh_var]]  <- 0
    
    
    for (b in banks_share) {
      
      idx_bank <-
        offshore$bank ==
        b
      
      tot_y_b <- stata_sum(
        offshore[[amt_var]][idx_bank]
      )
      
      tothaven_y_b <- stata_sum(
        offshore[[amt_var]][
          idx_bank &
            !is.na(offshore$haven) &
            offshore$haven == 1
        ]
      )
      
      totgcc_y_b <- stata_sum(
        offshore[[amt_var]][
          idx_bank &
            !is.na(offshore$gcc) &
            offshore$gcc == 1
        ]
      )
      
      toteu_y_b <- stata_sum(
        offshore[[amt_var]][
          idx_bank &
            !is.na(offshore$eu) &
            offshore$eu == 1 &
            (
              is.na(offshore$haven) |
                offshore$haven != 1
            )
        ]
      )
      
      
      if (tot_y_b != 0) {
        
        offshore[[raw_var]][idx_bank] <-
          offshore[[amt_var]][idx_bank] /
          tot_y_b
      }
      
      
      stddep <- 0
      
      if (y != "fidu" && b == "CH") {
        
        stddep <-
          0.35 *
          tothaven_y_b
      }
      
      
      # EU non-haven countries
      idx_eu <-
        idx_bank &
        !is.na(offshore$haven) &
        offshore$haven != 1 &
        !is.na(offshore$eu) &
        offshore$eu == 1
      
      
      denom_nonhaven <-
        tot_y_b -
        tothaven_y_b
      
      
      if (
        denom_nonhaven != 0 &&
        toteu_y_b != 0
      ) {
        
        offshore[[sh_var]][idx_eu] <-
          offshore[[raw_var]][idx_eu] *
          (
            1 +
              (
                tothaven_y_b -
                  stddep
              ) /
              denom_nonhaven +
              stddep /
              toteu_y_b
          )
      }
      
      
      # Other non-haven countries
      idx_other <-
        idx_bank &
        !is.na(offshore$haven) &
        offshore$haven != 1 &
        (
          is.na(offshore$eu) |
            offshore$eu != 1
        )
      
      
      if (denom_nonhaven != 0) {
        
        offshore[[sh_var]][idx_other] <-
          offshore[[raw_var]][idx_other] *
          (
            1 +
              (
                tothaven_y_b -
                  stddep
              ) /
              denom_nonhaven
          )
      }
    }
  }
  
  
  # ---------------------------------------------------------------------------
  # Compute share of BIS deposits in haven aggregates
  # ---------------------------------------------------------------------------
  
  total_wide <- offshore %>%
    select(
      bank,
      iso3saver,
      sh_bis,
      amt_bis,
      amt_inter,
      sh_inter
    ) %>%
    filter(
      !is.na(iso3saver),
      iso3saver != ""
    ) %>%
    pivot_wider(
      names_from = bank,
      values_from = c(
        sh_bis,
        amt_bis,
        amt_inter,
        sh_inter
      ),
      names_sep = ""
    )
  
  
  # Missing values become zero, as in Stata
  share_amount_vars <- names(total_wide)[
    startsWith(names(total_wide), "sh") |
      startsWith(names(total_wide), "amt")
  ]
  
  
  total_wide <- total_wide %>%
    mutate(
      across(
        all_of(share_amount_vars),
        ~ replace_na(.x, 0)
      )
    )
  
  
  # Ensure variables needed below exist
  needed_total_vars <- c(
    "sh_bisAS",
    "sh_bisCR",
    "sh_bisEU",
    "sh_bisCH",
    "amt_bisAS",
    "amt_bisCR",
    "amt_bisEU",
    "amt_bisCH",
    "sh_interAS",
    "sh_interCR",
    "sh_interEU",
    "amt_interAS",
    "amt_interCR",
    "amt_interEU"
  )
  
  
  for (v in needed_total_vars) {
    
    if (!v %in% names(total_wide)) {
      total_wide[[v]] <- 0
    }
  }
  
  
  # Totals used by the original Stata locals
  get_total <- function(bank_code) {
    
    stata_sum(
      offshore$amt_bis[
        offshore$bank ==
          bank_code
      ]
    )
  }
  
  
  totbisAS <- get_total("AS")
  totbisCR <- get_total("CR")
  totbisEU <- get_total("EU")
  totbisCH <- get_total("CH")
  
  
  total_wide <- total_wide %>%
    mutate(
      sh_bisOC =
        (
          sh_bisAS * totbisAS +
            sh_bisCR * totbisCR +
            sh_bisEU * totbisEU
        ) /
        (
          totbisAS +
            totbisCR +
            totbisEU
        ),
      
      sh_bisHA =
        (
          sh_bisOC *
            (
              totbisAS +
                totbisCR +
                totbisEU
            ) +
            sh_bisCH *
            totbisCH
        ) /
        (
          totbisAS +
            totbisCR +
            totbisEU +
            totbisCH
        ),
      
      amt_bisOC =
        amt_bisAS +
        amt_bisCR +
        amt_bisEU,
      
      amt_bisHA =
        amt_bisOC +
        amt_bisCH,
      
      sh_interOC =
        (
          sh_interAS * totbisAS +
            sh_interCR * totbisCR +
            sh_interEU * totbisEU
        ) /
        (
          totbisAS +
            totbisCR +
            totbisEU
        )
    )
  
  
  # Construct OC and HA observations
  total_long <- total_wide %>%
    select(
      iso3saver,
      ends_with("OC"),
      ends_with("HA")
    ) %>%
    pivot_longer(
      cols = -iso3saver,
      names_to = c(".value", "bank"),
      names_pattern = "(.*)(OC|HA)$"
    ) %>%
    filter(
      bank %in% c(
        "OC",
        "HA"
      )
    )
  
  
  # Add country information from CH observations
  country_info <- offshore %>%
    filter(
      bank == "CH"
    ) %>%
    select(
      iso3saver,
      saver,
      namesaver,
      gdp,
      shgdp,
      rich,
      developing,
      starts_with("gdp"),
      north_am,
      latin_am,
      gcc,
      russia,
      asia,
      africa,
      europe,
      eu,
      euro16,
      haven
    ) %>%
    distinct(
      iso3saver,
      .keep_all = TRUE
    )
  
  
  total_long <- total_long %>%
    left_join(
      country_info,
      by = "iso3saver"
    ) %>%
    mutate(
      namebank = case_when(
        bank == "HA" ~ "All havens",
        bank == "OC" ~ "Havens other than CH",
        TRUE         ~ NA_character_
      ),
      
      iso3bank = ""
    )
  
  
  # Update original dataset with OC and HA rows
  offshore <- offshore %>%
    filter(
      !(
        bank %in% c(
          "OC",
          "HA"
        )
      )
    ) %>%
    bind_rows(
      total_long
    ) %>%
    mutate(
      namesaver = if_else(
        iso3saver == "UMI",
        "US Minor Islands",
        namesaver
      ),
      
      year = i
    ) %>%
    filter(
      !is.na(namesaver),
      namesaver != ""
    )
  
  
  # Save annual offshore dataset before smoothing
  saveRDS(offshore, file = file.path(work, paste0("offshore",i,".rds")))
  
  # ============================================================================
  # III.2 ---- Country-level share dataset
  # ============================================================================
  
  countries_i <- offshore %>%
    filter(
      bank == "CH"
    ) %>%
    mutate(
      continent =
        1 * coalesce(africa, 0) +
        2 * coalesce(europe, 0) +
        3 * coalesce(gcc, 0) +
        4 * coalesce(asia, 0) +
        5 * coalesce(russia, 0) +
        6 * coalesce(latin_am, 0) +
        7 * coalesce(north_am, 0) +
        8 * coalesce(haven, 0),
      
      continent = if_else(
        continent == 0,
        8,
        continent
      ),
      
      continent = if_else(
        saver == "NO",
        9,
        continent
      ),
      
      continent = if_else(
        saver == "CR",
        10,
        continent
      )
    ) %>%
    select(
      saver,
      iso3saver,
      namesaver,
      sh_fidu,
      sh_inter,
      shgdp,
      gdp,
      sh_bis,
      continent
    )
  
  
  # Add OC, AS, EU and CR shares
  for (
    b in c(
      "OC",
      "AS",
      "EU",
      "CR"
    )
  ) {
    
    tmp <- offshore %>%
      filter(
        bank == b
      ) %>%
      select(
        iso3saver,
        sh_bis
      ) %>%
      rename(
        !!paste0(
          "sh_",
          b
        ) := sh_bis
      )
    
    
    countries_i <- full_join(
      countries_i,
      tmp,
      by = "iso3saver"
    )
  }
  
  
  # Year-specific names used in the smoothing section
  countries_i <- countries_i %>%
    rename(
      !!paste0("sh_fidu", i)  := sh_fidu,
      !!paste0("sh_AS", i)    := sh_AS,
      !!paste0("sh_CR", i)    := sh_CR,
      !!paste0("sh_EU", i)    := sh_EU,
      !!paste0("sh_OC", i)    := sh_OC,
      !!paste0("sh_bis", i)   := sh_bis,
      !!paste0("sh_inter", i) := sh_inter,
      !!paste0("gdp", i)      := gdp
    )
  
  
  countries[[as.character(i)]] <-
    countries_i
}


# ==============================================================================
# IV ---- Compute 5-year smoothed estimates
# ==============================================================================

share_types <- c(
  "fidu",
  "CR",
  "EU",
  "AS",
  "OC",
  "bis",
  "inter"
)


# ------------------------------------------------------------------------------
# IV.1 Interior years: 2003–2020
# ------------------------------------------------------------------------------

for (x in 2003:2020) {
  
  message(
    "Smoothing year ",
    x,
    " ..."
  )
  
  
  years_window <- c(
    x - 2,
    x - 1,
    x,
    x + 1,
    x + 2
  )
  
  
  smooth_data <-
    countries[[
      as.character(
        years_window[1]
      )
    ]]
  
  
  for (
    yy in years_window[-1]
  ) {
    
    smooth_data <- full_join(
      smooth_data,
      countries[[
        as.character(yy)
      ]],
      by = "iso3saver",
      suffix = c(
        "",
        paste0(
          "_",
          yy
        )
      )
    )
  }
  
  
  # Because descriptive columns occur in multiple years, retain the central
  # year's country information separately.
  central_info <- countries[[as.character(x)]] %>%
    select(
      any_of(
        c(
          "iso3saver",
          "saver",
          "namesaver",
          "shgdp",
          "continent",
          paste0(
            "gdp",
            x
          )
        )
      )
    )
  
  
  # Construct a clean share panel by ISO3
  share_panel <- reduce(
    lapply(
      years_window,
      function(yy) {
        
        countries[[as.character(yy)]] %>%
          select(
            iso3saver,
            all_of(
              paste0(
                "sh_",
                share_types,
                yy
              )
            )
          )
      }
    ),
    full_join,
    by = "iso3saver"
  )
  
  
  for (b in share_types) {
    
    v_m2 <- paste0(
      "sh_",
      b,
      x - 2
    )
    
    v_m1 <- paste0(
      "sh_",
      b,
      x - 1
    )
    
    v_0 <- paste0(
      "sh_",
      b,
      x
    )
    
    v_p1 <- paste0(
      "sh_",
      b,
      x + 1
    )
    
    v_p2 <- paste0(
      "sh_",
      b,
      x + 2
    )
    
    v_sm <- paste0(
      "sh_",
      b,
      "_smthg"
    )
    
    
    share_panel[[v_sm]] <-
      (
        share_panel[[v_m2]] +
          share_panel[[v_p2]]
      ) * 0.1 +
      (
        share_panel[[v_m1]] +
          share_panel[[v_p1]]
      ) * 0.2 +
      share_panel[[v_0]] *
      0.4
    
    
    # Stata fallback:
    # replace smoothed share = current-year share if missing
    share_panel[[v_sm]] <- if_else(
      is.na(
        share_panel[[v_sm]]
      ),
      share_panel[[v_0]],
      share_panel[[v_sm]]
    )
  }
  
  
  smooth_keep <- share_panel %>%
    select(
      iso3saver,
      ends_with("_smthg")
    )
  
  
  # Merge 1:m namesaver using offshore[x].
  # ISO3 is safer in R because it is the actual country identifier.
  #offshore_x <- read_dta(file.path(work2, paste0("offshore",x,".dta")))
  offshore_x <- read_work_data(paste0("offshore", x))
  
  
  offshore_x <- offshore_x %>%
    left_join(
      smooth_keep,
      by = "iso3saver"
    )
  
  
  # Remove euro16 and eu as in Stata final output
  offshore_x <- offshore_x %>%
    select(
      -any_of(
        c(
          "eu",
          "euro16"
        )
      )
    ) %>%
    arrange(
      namesaver
    )
  
  saveRDS(offshore_x, file = file.path(work, paste0("offshore",x,".rds")))
  write.csv(offshore_x,file = file.path(work, paste0("offshore", x, ".csv")),row.names = FALSE)
  
}


# ==============================================================================
# IV.2 Boundary years: 2001, 2002, 2021, 2022
# ==============================================================================

boundary_years <- c(
  2001,
  2002,
  2021,
  2022
)


# Merge the years needed for boundary smoothing
boundary_panel <- reduce(
  lapply(
    c(
      2001,
      2002,
      2003,
      2004,
      2019,
      2020,
      2021,
      2022
    ),
    function(yy) {
      
      countries[[as.character(yy)]] %>%
        select(
          iso3saver,
          any_of(
            paste0(
              "sh_",
              share_types,
              yy
            )
          )
        )
    }
  ),
  full_join,
  by = "iso3saver"
)


for (b in share_types) {
  
  # ---------------------------------------------------------------------------
  # 2001
  # ---------------------------------------------------------------------------
  
  boundary_panel[[paste0("sh_",b,"_smthg2001")]] <- (boundary_panel[[paste0("sh_",b,"2003")]] * 0.1 + boundary_panel[[paste0("sh_",b,"2002")]] *0.2 +boundary_panel[[paste0("sh_",b,"2001")]] *0.4) /0.7
  
  
  
  # ---------------------------------------------------------------------------
  # 2001: Fallback
  # ---------------------------------------------------------------------------
  
  boundary_panel[[paste0("sh_", b, "_smthg2001")]] <- if_else(
    is.na(boundary_panel[[paste0("sh_", b, "_smthg2001")]]),
    boundary_panel[[paste0("sh_", b, "2001")]],
    boundary_panel[[paste0("sh_", b, "_smthg2001")]]
  )
  
  
  # ---------------------------------------------------------------------------
  # 2002
  # ---------------------------------------------------------------------------
  
  boundary_panel[[paste0("sh_", b, "_smthg2002")]] <- (
    boundary_panel[[paste0("sh_", b, "2004")]] * 0.1 +
      (
        boundary_panel[[paste0("sh_", b, "2003")]] +
          boundary_panel[[paste0("sh_", b, "2001")]]
      ) * 0.2 +
      boundary_panel[[paste0("sh_", b, "2002")]] * 0.4
  ) / 0.9
  
  
  # First Stata fallback for 2002
  idx_missing <- is.na(
    boundary_panel[[paste0("sh_", b, "_smthg2002")]]
  )
  
  boundary_panel[[paste0("sh_", b, "_smthg2002")]][idx_missing] <- (
    boundary_panel[[paste0("sh_", b, "2002")]][idx_missing] * 0.4 +
      boundary_panel[[paste0("sh_", b, "2004")]][idx_missing] * 0.1
  ) / 0.5
  
  
  # Second Stata fallback for 2002
  idx_missing <- is.na(
    boundary_panel[[paste0("sh_", b, "_smthg2002")]]
  )
  
  boundary_panel[[paste0("sh_", b, "_smthg2002")]][idx_missing] <-
    boundary_panel[[paste0("sh_", b, "2002")]][idx_missing]
  
  
  # ---------------------------------------------------------------------------
  # 2021
  # ---------------------------------------------------------------------------
  
  boundary_panel[[paste0("sh_", b, "_smthg2021")]] <- (
    boundary_panel[[paste0("sh_", b, "2019")]] * 0.1 +
      (
        boundary_panel[[paste0("sh_", b, "2022")]] +
          boundary_panel[[paste0("sh_", b, "2020")]]
      ) * 0.2 +
      boundary_panel[[paste0("sh_", b, "2021")]] * 0.4
  ) / 0.9
  
  
  # ---------------------------------------------------------------------------
  # 2022
  # ---------------------------------------------------------------------------
  
  boundary_panel[[paste0("sh_", b, "_smthg2022")]] <- (
    boundary_panel[[paste0("sh_", b, "2020")]] * 0.1 +
      boundary_panel[[paste0("sh_", b, "2021")]] * 0.2 +
      boundary_panel[[paste0("sh_", b, "2022")]] * 0.4
  ) / 0.7
  
}

# ==============================================================================
# V ---- Merge boundary-year smoothed shares into offshore datasets
# ==============================================================================

for (i in boundary_years) {
  
  smooth_vars <- paste0(
    "sh_",
    share_types,
    "_smthg",
    i
  )
  
  
  smooth_i <- boundary_panel %>%
    select(
      iso3saver,
      all_of(
        smooth_vars
      )
    )
  
  
  # Remove year suffix to reproduce final Stata variable names
  names(smooth_i) <- gsub(
    paste0(
      i,
      "$"
    ),
    "",
    names(smooth_i)
  )
  
  
  #offshore_i <- read_dta(file.path(work,paste0("offshore",i,".dta")))
  offshore_i <- read_work_data(paste0("offshore", i))
  
  
  offshore_i <- offshore_i %>%
    left_join(
      smooth_i,
      by = "iso3saver"
    ) %>%
    select(
      -any_of(
        c(
          "eu",
          "euro16"
        )
      )
    ) %>%
    arrange(
      namesaver
    )
  
  saveRDS(offshore_i, file = file.path(work, paste0("offshore",i,".rds")))
  write.csv(offshore_i,file = file.path(work, paste0("offshore", i, ".csv")),row.names = FALSE)
  
}


# ==============================================================================
# VI ---- Checks
# ==============================================================================

cat(
  "\nFinished building offshore datasets for 2001-2022.\n"
)

cat(
  "Files saved in:\n",
  work,
  "\n"
)

for (i in 2001:2022) {
  
  file_i <- file.path(
    work,
    paste0(
      "offshore",
      i,
      ".rds"
    )
  )
  
  cat(
    i,
    ": ",
    ifelse(
      file.exists(file_i),
      "OK",
      "MISSING"
    ),
    "\n"
  )
}