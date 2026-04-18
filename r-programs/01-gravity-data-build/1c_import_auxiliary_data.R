# ------------------------------------------------------------------------------
# Project: Offshore financial wealth database - update 2023
# Title: 1c_import_auxiliary_data.R
# Purpose: import from different sources and formats
# This version: translated from Stata
# ------------------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(stringr)
library(haven)
library(purrr)
library(janitor)

# Assumed path objects:
# raw  <- "path/to/raw"
# work <- "path/to/work"

matching_iso_ifscode <- read_dta(file.path(raw, "dta", "matching_iso_ifscode.dta"))

# ------------------------------------------------------------------------------
# import aggregates from CPIS data
# ------------------------------------------------------------------------------


for (asset in c("eq", "debt")) {

  df <- read_excel(
    file.path(raw, paste0("IMF_2023_Table_15_All_Economies_Reported_Por_", asset, ".xlsx")),
    sheet = "Table 15",
    col_names = FALSE
  )

  # drop A and first 3 rows
  # -jlenke drop first 2 rows
  df <- df %>%
    #select(-1) %>%
    #slice(-(1:3))
    slice(-(1:2))

  # rename columns C:AG using first row values after replacing ". " with "_"
  name_idx <- 2:ncol(df)
  new_names <- names(df)
  first_row <- df[1, ]

  for (j in name_idx) {
    nm <- as.character(first_row[[j]])
    nm <- str_replace_all(nm, fixed(". "), "_")
    new_names[j] <- nm
  }
  names(df) <- new_names

  df <- df %>%
    slice(-1) %>%
    rename(B = 1) %>%
    pivot_longer(
      cols = starts_with("DEC_"),
      names_to = "year",
      values_to = "value"
    ) %>%
    mutate(
      year = as.integer(str_remove(year, "^DEC_"))
    ) %>%
    filter(B != "") %>%
    rename(country = B)

  # first merge on country
  m1 <- df %>%
    left_join(matching_iso_ifscode, by = "country")
  
  m1 <- m1 %>%
    rename(
      our_code_orig = our_code
#      country_v2 = country
    ) %>%
    select(-ifscode)

  # second merge on country_v2
  m2 <- m1 %>%
    left_join(matching_iso_ifscode, by = c("country_v2" = "country"),
              suffix = c("", "_m2")) %>%
    mutate(
      our_code_orig = if_else(is.na(our_code_orig) & !is.na(our_code), our_code, our_code_orig),
      our_code_orig = if_else(country_v2 == "SEFER + SSIO (**)", 9999, our_code_orig)
    ) %>%
    filter(!is.na(our_code_orig)) %>%
    transmute(
      source = our_code_orig,
      cname = country_v2,
      year = year,
      !!paste0("sum", asset, "asset") := as.numeric(value)
    )

  write_dta(m2, file.path(work, paste0("data_tot", asset, "_update.dta")))
}

# adjustment factor for equity growth between June and December
adj_df <- read_excel(
  file.path(raw, "IMF_2023_Table_15_All_Economies_Reported_Por_eq.xlsx"),
  sheet = "Table 15",
  col_names = FALSE
)
adj_df <- adj_df %>%
  #select(-1) %>%
  slice(-(1:2))

name_idx <- 2:ncol(adj_df)
first_row <- adj_df[1, ]
new_names <- names(adj_df)
for (j in name_idx) {
  nm <- as.character(first_row[[j]])
  nm <- str_replace_all(nm, fixed(". "), "_")
  new_names[j] <- nm
}
names(adj_df) <- new_names

adjustfactor_cpis <- adj_df %>%
  #slice(-1) %>%
  rename(B = 1) %>%
  filter(B %in% c("SEFER + SSIO (**)", "Value of Total Investment")) %>%
  select(-matches("^DEC_200"), -matches("^DEC_2010$"), -matches("^DEC_2011$"), -matches("^DEC_2012$")) %>%
  mutate(across(matches("^(DEC|JUN)_"), as.numeric)) %>%
  mutate(
    B = c("SEFER_SSIO", "Total")
  )

for (j in 2013:2021) {
  adjustfactor_cpis[[paste0("adj_", j)]] <- adjustfactor_cpis[[paste0("DEC_", j)]] /
    adjustfactor_cpis[[paste0("JUN_", j)]]
}

adjustfactor_cpis <- adjustfactor_cpis %>%
  select(B, starts_with("adj_")) %>%
  pivot_longer(
    cols = starts_with("adj_"),
    names_to = "year",
    values_to = "adj"
  ) %>%
  mutate(year = as.integer(str_remove(year, "^adj_"))) %>%
  pivot_wider(
    names_from = B,
    values_from = adj,
    names_prefix = "adj_"
  )

write_dta(adjustfactor_cpis, file.path(work, "adjustfactor_cpis.dta"))
##################

# total liabilities
for (liab in c("eq", "debt")) {

  df <- read_excel(
    file.path(raw, paste0("IMF_2023_International_Investment_Position_", liab, ".xlsx")),
    sheet = "Annual",
    col_names = FALSE
  ) %>%
    select(1:23) %>%
    slice(-(1:4))

  # first row becomes names for B:W
  new_names <- names(df)
  first_row <- df[1, ]
  for (j in 2:ncol(df)) {
    nm <- paste0("v_", as.character(first_row[[j]]))
    new_names[j] <- nm
  }
  names(df) <- new_names

  df <- df %>%
    slice(-1) %>%
    pivot_longer(
      cols = starts_with("v_"),
      names_to = "year",
      values_to = "value"
    ) %>%
    mutate(year = as.integer(str_remove(year, "^v_"))) %>%
    filter(.data[[names(df)[1]]] != "") %>%
    rename(country = 1)

  m1 <- df %>%
    left_join(matching_iso_ifscode, by = "country") %>%
    rename(
      our_code_orig = our_code
      #country_v2 = country
    ) %>%
    select(-ifscode, -iso3)

  m2 <- m1 %>%
    left_join(matching_iso_ifscode, by = c("country_v2" = "country"),
              suffix = c("", "_m2")) %>%
    mutate(
      our_code = case_when(
        country_v2 == "Curaçao, Kingdom of the Netherlands" ~ 355,
        country_v2 == "Sint Maarten, Kingdom of the Netherlands" ~ 355,
        country_v2 == "Türkiye, Rep. of" ~ 186,
        TRUE ~ our_code
      ),
      our_code_orig = if_else(!is.na(our_code), our_code, our_code_orig)
    ) %>%
    filter(!is.na(our_code_orig)) %>%
    transmute(
      host = our_code_orig,
      cname = country_v2,
      year = year,
      value = as.character(value)
    ) %>%
    mutate(
      value = na_if(value, "..."),
      value = str_replace_all(value, "K", ""),
      value = str_replace_all(value, ",", ""),
      value = as.numeric(value)
    )

  host_df <- m2 %>%
    group_by(host, year) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    rename(!!paste0(liab, "liab_IIP_host") := value)

  write_dta(host_df, file.path(work, paste0("IIP_", liab, "liab_host.dta")))

  total_df <- host_df %>%
    group_by(year) %>%
    summarise(!!paste0(liab, "liab_IIP") := sum(.data[[paste0(liab, "liab_IIP_host")]], na.rm = TRUE),
              .groups = "drop")

  write_dta(total_df, file.path(work, paste0("IIP_", liab, "liab.dta")))
}

# ------------------------------------------------------------------------------
# import U.S. data from TIC
# ------------------------------------------------------------------------------

tic <- read_csv(
  file.path(raw, "TIC_2022_foreign_portfolio_holdings_of_US_securities.csv"),
  show_col_types = FALSE,
  col_names = FALSE
)

# approximate translation of keep v1-v5 v*3 v*4 v*5 then keep v1-v205
tic <- tic %>%
  mutate(nvals = row_number()) %>%
  filter(nvals == 9 | nvals > 10) %>%
  select(1:205)

# row 2 contains part of variable labels
for (j in seq_along(tic)) {
  if (is.character(tic[[j]]) || is.list(tic[[j]])) {
    tic[[j]][2] <- str_replace_all(as.character(tic[[j]][2]), "Total securities", "Total")
    tic[[j]][2] <- str_replace_all(as.character(tic[[j]][2]), "Total long-term Debt", "Debtl")
  }
}

names(tic)[1:2] <- c("countryid", "country")

# first renaming pass from row 2
nm <- names(tic)
for (j in seq_along(tic)) {
  if (j >= 3) {
    suffix <- make_clean_names(as.character(tic[[j]][2]))
    nm[j] <- paste0(nm[j], "_", suffix)
  }
}

names(tic) <- nm

# strip Jun / Mar
tic <- tic %>%
  mutate(across(everything(), ~ ifelse(row_number() == 1, str_replace(., "Jun ", ""), .)))

# second renaming pass from row 1
nm <- names(tic)
for (j in seq_along(tic)) {
  if (j >= 3) {
    suffix <- make_clean_names(as.character(tic[[j]][1]))
    nm[j] <- paste0(nm[j], suffix)
  }
}
names(tic) <- nm
tic <- tic[-1, ]

names(tic)[names(tic) == "v203_Total__1__2000"] <- "v203_Total_2000"
TIC_sub <- tic

tic_list <- list()


for (i in c(2000, 2002:2021)) {
  sub <- TIC_sub %>%
    select(countryid, country, matches(paste0("_", i, "$"))) %>%
    mutate(group = i)

  names(sub) <- names(sub) %>%
    str_replace(paste0("^.*_Total_", i, "$"), "Total") %>%
    str_replace(paste0("^.*_Equity_", i, "$"), "Equity") %>%
    str_replace(paste0("^.*_Debtl_", i, "$"), "Debtl")

  tic_list[[as.character(i)]] <- sub
}



data_TIC_update <- bind_rows(tic_list) %>%
  mutate(
    countryid = as.character(countryid),
    Equity = as.numeric(str_replace_all(Equity, ",", "")),
    Debtl = as.numeric(str_replace_all(Debtl, ",", "")),
    Total = as.numeric(str_replace_all(Total, ",", ""))
  ) %>%
  pivot_wider(
    names_from = group,
    values_from = c(Total, Equity, Debtl)
  ) %>%
  mutate(
    Equity_2001 = (Equity_2000 + Equity_2002) / 2,
    Debtl_2001 = (Debtl_2000 + Debtl_2002) / 2,
    Total_2001 = (Total_2000 + Total_2002) / 2
  ) %>%
  pivot_longer(
    cols = matches("^(Total|Equity|Debtl)_"),
    names_to = c(".value", "year"),
    names_sep = "_"
  ) %>%
  mutate(
    year = as.integer(year),
    flag_TIC = if_else(year == 2001, "2001 is estimated as the mean of 2000 and 2002", NA_character_)
  )

write_dta(data_TIC_update, file.path(work, "data_TIC_update.dta"))

# ------------------------------------------------------------------------------
# import China's assets
# ------------------------------------------------------------------------------

china_imf <- read_excel(
  file.path(raw, "IMF_IIP_China.xlsx"),
  sheet = "Annual",
  col_names = FALSE
)

# This block depends on row-relative Stata filtering.
# Closest translation uses row context from the imported sheet.
china_imf <- china_imf %>%
  mutate(rowid = row_number()) %>%
  filter(
    .[[1]] == "Other reserve assets" |
      .[[2]] == "2004" |
      (. [[1]] == "Equity and investment fund shares" & lag(.[[1]], 1) == "Portfolio investment" & lag(.[[1]], 11) == "Assets") |
      (. [[1]] == "Debt securities" & lag(.[[1]], 7) == "Portfolio investment" & lag(.[[1]], 17) == "Assets")
  ) %>%
  select(1:19)

for (j in 2:ncol(china_imf)) {
  for (k in 2004:2021) {
    china_imf[[j]][1] <- str_replace_all(as.character(china_imf[[j]][1]), as.character(k), paste0("v_", k))
  }
}

nm <- names(china_imf)
for (j in 2:ncol(china_imf)) {
  nm[j] <- make_clean_names(as.character(china_imf[[j]][1]))
}
names(china_imf) <- nm

data_IMF_China <- china_imf %>%
  slice(-1) %>%
  rename(A = 1) %>%
  mutate(
    A = case_when(
      A == "Equity and investment fund shares" ~ "Equity",
      A == "Debt securities" ~ "Debt",
      A == "Other reserve assets" ~ "Reserves",
      TRUE ~ A
    )
  ) %>%
  pivot_longer(
    cols = starts_with("v_"),
    names_to = "year",
    values_to = "v_"
  ) %>%
  mutate(
    year = as.integer(str_remove(year, "^v_")),
    v_ = as.numeric(str_replace_all(str_replace_all(as.character(v_), ",", ""), "K ", ""))
  ) %>%
  pivot_wider(
    names_from = A,
    values_from = v_,
    names_prefix = "",
    names_glue = "{A}_IMF"
  )

# foreign exchange reserves pre-2004
reserves_2001 <- read_excel(
  file.path(raw, "IMF_2023_IFS_China_reserves.xlsx"),
  col_names = FALSE
) %>%
  slice(-1)

nm <- names(reserves_2001)
for (j in 3:24) {
  nm[j] <- paste0("v", make_clean_names(as.character(reserves_2001[[j]][1])))
}
names(reserves_2001) <- nm

reserves_2001 <- reserves_2001 %>%
  slice(-1) %>%
  pivot_longer(
    cols = starts_with("v_"),
    names_to = "year",
    values_to = "reserves_2001"
  ) %>%
  mutate(
    year = as.integer(str_remove(year, "^v_")),
    reserves_2001 = as.numeric(reserves_2001)
  ) %>%
  select(year, reserves_2001)

data_IMF_China <- data_IMF_China %>%
  left_join(reserves_2001, by = "year") %>%
  mutate(
    Reserves_IMF = if_else(year < 2004, reserves_2001, Reserves_IMF)
  ) %>%
  select(-reserves_2001) %>%
  filter(year <= 2021)

write_dta(data_IMF_China, file.path(work, "data_IMF_China.dta"))

# ------------------------------------------------------------------------------
# import foreign exchange data
# ------------------------------------------------------------------------------

foreignexchange <- read_excel(
  file.path(raw, "IMF_2023_IFS_Foreign_exchange.xlsx"),
  col_names = FALSE
) %>%
  select(-2) %>%
  slice(-1)

nm <- names(foreignexchange)
for (j in 3:24) {
  nm[j] <- paste0("v", make_clean_names(as.character(foreignexchange[[j]][1])))
}
names(foreignexchange) <- nm

data_foreignexchange_update <- foreignexchange %>%
  slice(-1) %>%
  rename(A = 1) %>%
  pivot_longer(
    cols = starts_with("v_"),
    names_to = "year",
    values_to = "reserveIFS"
  ) %>%
  mutate(
    year = as.integer(str_remove(year, "^v_")),
    reserveIFS = as.numeric(reserveIFS)
  ) %>%
  rename(country = A) %>%
  left_join(matching_iso_ifscode, by = "country") %>%
  rename(
    our_code_orig = our_code,
    country_v2 = country
  ) %>%
  select(-ifscode) %>%
  left_join(matching_iso_ifscode, by = c("country_v2" = "country"),
            suffix = c("", "_m2")) %>%
  mutate(
    our_code_orig = if_else(is.na(our_code_orig) & !is.na(our_code), our_code, our_code_orig)
  ) %>%
  filter(!is.na(our_code_orig), year <= 2021) %>%
  transmute(
    source = our_code_orig,
    year = year,
    reserveIFS = reserveIFS
  )

write_dta(data_foreignexchange_update, file.path(work, "data_foreignexchange_update.dta"))

# ------------------------------------------------------------------------------
# import TIC U.S. cross-border securities positions
# ------------------------------------------------------------------------------

# Most recent monthly liabilities
TIC_liab_monthly_2020f <- read_delim(
  file.path(raw, "slt_table1.txt"),
  delim = ",",
  show_col_types = FALSE,
  col_names = FALSE
) %>%
  select(1:4, 7, 10, 13, 16) %>%
  mutate(nvals = row_number()) %>%
  filter(nvals > 8) %>%
  select(-nvals)

nm <- names(TIC_liab_monthly_2020f)
for (j in seq_along(TIC_liab_monthly_2020f)) {
  nm[j] <- make_clean_names(as.character(TIC_liab_monthly_2020f[[j]][1]))
}
names(TIC_liab_monthly_2020f) <- nm

TIC_liab_monthly_2020f <- TIC_liab_monthly_2020f %>%
  slice(-1) %>%
  separate(date, into = c("year", "month"), sep = "-", remove = TRUE) %>%
  mutate(
    year = as.integer(year),
    month = as.integer(month)
  ) %>%
  filter(month %in% c(6, 12)) %>%
  mutate(across(starts_with("for_lt_"), ~ na_if(.x, "n.a."))) %>%
  mutate(
    country_code = as.numeric(country_code)
  ) %>%
  filter(country_code != 72907, country_code != 76929) %>%
  filter(!(country_code > 79995 & country_code < 99996)) %>%
  rename(country_name = country)

# 2011-2020
TIC_liab_monthly_2011f <- read_csv(
  file.path(raw, "ifdp1113_data", "bertaut_judson_positions_liabs_2021.csv"),
  show_col_types = FALSE
) %>%
  separate(date, into = c("month", "day", "year"), sep = "/", remove = TRUE) %>%
  mutate(
    month = as.integer(month),
    year = as.integer(year)
  ) %>%
  select(year, month, country_code, country_name, ends_with("est_pos")) %>%
  rename_with(~ str_remove(.x, "^ftot_")) %>%
  filter(month %in% c(6, 12)) %>%
  full_join(TIC_liab_monthly_2020f, by = c("country_code", "month", "year", "country_name")) %>%
  arrange(country_code, year, month)

# 2001-2011
TIC_liab_monthly_complete <- read_csv(
  file.path(raw, "ticdata", "ticdata.liabilities.ftot.txt"),
  show_col_types = FALSE
) %>%
  separate(date, into = c("month", "day", "year"), sep = "/", remove = TRUE) %>%
  mutate(
    month = as.integer(month),
    year = as.integer(year)
  ) %>%
  filter(month %in% c(6, 12), year > 2000) %>%
  select(countrycode, countryname, ends_with("est_pos"), month, year) %>%
  mutate(across(c(ftot_agcy_est_pos, ftot_corp_est_pos, ftot_stk_est_pos, ftot_treas_est_pos),
                ~ as.numeric(na_if(.x, "        ND")))) %>%
  rename(
    country_name = countryname,
    country_code = countrycode
  ) %>%
  rename_with(~ str_remove(.x, "^ftot_")) %>%
  filter(year != 2011) %>%
  bind_rows(TIC_liab_monthly_2011f) %>%
  arrange(country_code, year, month) %>%
  mutate(
    for_lt_treas_pos = coalesce(for_lt_treas_pos, treas_est_pos),
    for_lt_agcy_pos = coalesce(for_lt_agcy_pos, agcy_est_pos),
    for_lt_corp_pos = coalesce(for_lt_corp_pos, corp_est_pos),
    for_lt_eqty_pos = coalesce(for_lt_eqty_pos, stk_est_pos)
  ) %>%
  select(-matches("_est_"), -starts_with("for_lt_tot")) %>%
  rename(
    treas = for_lt_treas_pos,
    agcy = for_lt_agcy_pos,
    corp = for_lt_corp_pos,
    equity = for_lt_eqty_pos
  ) %>%
  mutate(
    debtl = treas + agcy + corp
  ) %>%
  select(country_code, country_name, year, month, equity, debtl)

write_dta(TIC_liab_monthly_complete, file.path(work, "TIC_liab_monthly_complete.dta"))

# ------------------------------------------------------------------------------
# Cayman Islands
# ------------------------------------------------------------------------------

TIC_lt_monthly_Cayman_complete <- TIC_liab_monthly_complete %>%
  filter(country_code == 36137, month == 12) %>%
  mutate(
    source = 377,
    host = 111
  ) %>%
  select(-country_code, -country_name, -month)

# short-term debt for Cayman
cayman_short <- read_excel(
  file.path(raw, "TIC_US_Financial_Firms_Liabilities_Cayman.xlsx"),
  col_names = FALSE
) %>%
  select(1, 2, 9, 10) %>%
  rename(
    countrycode = 1,
    date = 2,
    shortterm_official = 3,
    shortterm_other = 4
  ) %>%
  slice(-(1:2)) %>%
  separate(date, into = c("year", "month"), sep = "-", remove = FALSE) %>%
  filter(month == "12") %>%
  transmute(
    year = as.integer(year),
    shortterm_official = as.numeric(shortterm_official),
    shortterm_other = as.numeric(shortterm_other)
  ) %>%
  mutate(shortterm_debt = rowSums(across(c(shortterm_official, shortterm_other)), na.rm = TRUE)) %>%
  select(year, shortterm_debt)

Cayman_TIC_Dec <- cayman_short %>%
  left_join(TIC_lt_monthly_Cayman_complete, by = "year") %>%
  mutate(
    shortterm_debt = if_else(year == 2001, 4712, shortterm_debt),
    shortterm_debt = if_else(year == 2002, 11018, shortterm_debt),
    debt = shortterm_debt + debtl
  ) %>%
  rename(
    debts_KY_TIC = shortterm_debt,
    debtl_KY_TIC = debtl,
    eq_KY_TIC = equity,
    debt_KY_TIC = debt
  )

write_dta(Cayman_TIC_Dec, file.path(work, "Cayman_TIC_Dec.dta"))

# CPIS banking and insurance holdings
KY_banks <- read_excel(file.path(raw, "IMF_2023_assets_Cayman_banking.xlsx"), col_names = FALSE) %>%
  slice(-(1:2))

nm <- names(KY_banks)
for (j in 2:18) {
  header <- as.character(KY_banks[[j]][1])
  for (k in 2004:2021) {
    header <- str_replace_all(header, paste0("Dec. ", k), paste0("v_", k))
  }
  nm[j] <- make_clean_names(header)
}
names(KY_banks) <- nm

KY_banks <- KY_banks %>%
  slice(-1) %>%
  rename(A = 1) %>%
  pivot_longer(cols = starts_with("v_"), names_to = "year", values_to = "bank") %>%
  mutate(
    year = as.integer(str_remove(year, "^v_")),
    bank = as.numeric(bank)
  ) %>%
  select(year, bank)

KY_ins <- read_excel(file.path(raw, "IMF_2023_assets_Cayman_ins.xlsx"), col_names = FALSE) %>%
  slice(-(1:2))

nm <- names(KY_ins)
for (j in 2:7) {
  header <- as.character(KY_ins[[j]][1])
  for (k in 2016:2021) {
    header <- str_replace_all(header, paste0("Dec. ", k), paste0("v_", k))
  }
  nm[j] <- make_clean_names(header)
}
names(KY_ins) <- nm

KY_banks_final <- KY_ins %>%
  slice(-1) %>%
  rename(A = 1) %>%
  pivot_longer(cols = starts_with("v_"), names_to = "year", values_to = "ins") %>%
  mutate(
    year = as.integer(str_remove(year, "^v_")),
    ins = as.numeric(ins)
  ) %>%
  select(year, ins) %>%
  left_join(KY_banks, by = "year") %>%
  arrange(year) %>%
  mutate(
    KY_assets = ins + bank,
    KY_assets = if_else(is.na(ins), bank, KY_assets),
    host = 377
  ) %>%
  select(year, KY_assets, host)

write_dta(KY_banks_final, file.path(work, "KY_banks.dta"))

# estimate equity liabilities of non-financial corporations located in Cayman Islands
xrates <- read_dta(file.path(raw, "dta", "xrates.dta")) %>%
  mutate(
    currency = case_when(
      B == "United States" ~ "USD",
      B == "Australia" ~ "AUD",
      B == "Brazil" ~ "BRL",
      B == "China, P.R.: Mainland" ~ "CNY",
      B == "Euro Area" ~ "EUR",
      B == "United Kingdom" ~ "GBP",
      B == "China, P.R.: Hong Kong" ~ "HKD",
      B == "Israel" ~ "ILS",
      B == "Japan" ~ "JPY",
      B == "Korea, Rep. of" ~ "KRW",
      B == "Mexico" ~ "MXN",
      B == "Norway" ~ "NOK",
      B == "Singapore" ~ "SGD",
      B == "Taiwan Province of China" ~ "TWD",
      TRUE ~ ""
    )
  ) %>%
  filter(currency != "")

KY_liab_nfc <- read_csv(
  file.path(raw, "compustat_cayman.csv"),
  locale = locale(grouping_mark = ","),
  show_col_types = FALSE
) %>%
  mutate(
    mktcap = cshoc * prccd,
    issue = as.numeric(substr(iid, 1, 2))
  ) %>%
  filter(issue < 90) %>%
  arrange(gvkey, datadate, issue) %>%
  distinct(gvkey, datadate, .keep_all = TRUE) %>%
  mutate(
    year = as.integer(substr(datadate, 1, 4)),
    month = as.integer(substr(datadate, 6, 7))
  ) %>%
  filter(month == 12) %>%
  rename(currency = curcdd) %>%
  filter(!(currency == "" & is.na(cshoc) & is.na(prccd))) %>%
  left_join(xrates %>% select(currency, year, xrate), by = c("currency", "year")) %>%
  mutate(
    mktcap_usd = mktcap / xrate
  ) %>%
  group_by(year) %>%
  summarise(mktcap_usd = sum(mktcap_usd, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    eqliab_nfc = 0.75 * mktcap_usd
  )

write_dta(KY_liab_nfc, file.path(work, "KY_liab_nfc.dta"))

# ------------------------------------------------------------------------------
# China's liabilities
# ------------------------------------------------------------------------------

TIC_longterm_monthly_China <- TIC_liab_monthly_complete %>%
  filter(country_code == 41408, month == 12)

TIC_China_short <- read_excel(
  file.path(raw, "TIC_US_Financial_Firms_Liabilities_China.xlsx"),
  col_names = FALSE
) %>%
  select(1, 2, 9, 10) %>%
  rename(
    countrycode = 1,
    date = 2,
    shortterm_official = 3,
    shortterm_other = 4
  ) %>%
  slice(-(1:2)) %>%
  separate(date, into = c("year", "month"), sep = "-", remove = FALSE) %>%
  filter(month == "12") %>%
  transmute(
    year = as.integer(year),
    shortterm_official = as.numeric(shortterm_official),
    shortterm_other = as.numeric(shortterm_other)
  ) %>%
  mutate(shortterm_debt = rowSums(across(c(shortterm_official, shortterm_other)), na.rm = TRUE)) %>%
  select(year, shortterm_debt)

TIC_China_short_pre <- read_excel(
  file.path(raw, "TIC_US_Financial_Firms_Liabilities_China.xlsx"),
  sheet = "before2003",
  col_names = FALSE
) %>%
  select(1, 2, 9, 10) %>%
  rename(
    countrycode = 1,
    date = 2,
    shortterm_official = 3,
    shortterm_other = 4
  ) %>%
  slice(-(1:2)) %>%
  separate(date, into = c("year", "month"), sep = "-", remove = FALSE) %>%
  filter(month == "12") %>%
  mutate(year = as.integer(year)) %>%
  filter(year > 2000) %>%
  transmute(
    year = year,
    shortterm_official = as.numeric(shortterm_official),
    shortterm_other = as.numeric(shortterm_other)
  ) %>%
  mutate(shortterm_debt = rowSums(across(c(shortterm_official, shortterm_other)), na.rm = TRUE)) %>%
  select(year, shortterm_debt)

TIC_China_Dec <- bind_rows(TIC_China_short_pre, TIC_China_short) %>%
  arrange(year) %>%
  left_join(TIC_longterm_monthly_China, by = "year") %>%
  mutate(
    source = 924,
    host = 111,
    total_China_TIC = equity + debtl + shortterm_debt,
    total_lt_China_TIC = equity + debtl,
    eq_China_TIC = equity,
    debtl_China_TIC = debtl,
    debts_China_TIC = shortterm_debt,
    debt_China_TIC = debtl_China_TIC + debts_China_TIC
  ) %>%
  select(-equity, -debtl)

write_dta(TIC_China_Dec, file.path(work, "TIC_China_Dec.dta"))

# ------------------------------------------------------------------------------
# Assets of Middle Eastern Oil Exporters
# ------------------------------------------------------------------------------

TIC_update_middleast <- read_dta(file.path(work, "data_TIC_update.dta")) %>%
  filter(country %in% c(
    " Middle Eastern Oil Exporters", "Kuwait", "Saudi Arabia", "Bahrain",
    "Iran", "Iraq", "Oman", "Qatar", "United Arab Emirates"
  ))

write_dta(TIC_update_middleast, file.path(work, "TIC_update_middleast.dta"))

TIC_update_middleeast_total <- TIC_update_middleast %>%
  group_by(year) %>%
  summarise(
    Total = sum(Total, na.rm = TRUE),
    Equity = sum(Equity, na.rm = TRUE),
    Debtl = sum(Debtl, na.rm = TRUE),
    .groups = "drop"
  )

# short-term liabilities of foreign official institutions
TIC_shortterm_FOI <- read_csv(
  file.path(raw, "tic_historic", "bltype_history.csv"),
  show_col_types = FALSE,
  col_names = FALSE
) %>%
  select(1, 6, 7) %>%
  separate(X1, into = c("year", "month"), sep = "-", remove = TRUE) %>%
  mutate(year = as.integer(year)) %>%
  filter(!is.na(year), year > 2000, month %in% c("Dec", "Jun")) %>%
  mutate(
    v6 = as.numeric(X6),
    v7 = as.numeric(X7),
    shortterm_FOI = rowSums(across(c(v6, v7)), na.rm = TRUE),
    month = recode(month, Dec = "12", Jun = "6"),
    month = as.integer(month)
  ) %>%
  select(year, month, shortterm_FOI)

write_dta(TIC_shortterm_FOI, file.path(work, "TIC_shortterm_FOI.dta"))

# long-term liabilities of foreign official institutions
TIC_longterm_FOI_2001_11 <- read_csv(
  file.path(raw, "ticdata", "ticdata.liabilities.foiadj.txt"),
  show_col_types = FALSE
) %>%
  select(date, starts_with("foi_")) %>%
  mutate(
    longterm_debt_FOI = rowSums(across(matches("^foi_(agcy|corp|treas)_est_pos$")), na.rm = TRUE),
    longterm_FOI = rowSums(across(matches("^foi_(agcy|corp|treas|stk)_est_pos$")), na.rm = TRUE)
  ) %>%
  separate(date, into = c("month", "day", "year"), sep = "/", remove = TRUE) %>%
  filter(month %in% c("12", "06")) %>%
  mutate(
    month = if_else(month == "06", "6", month),
    year = as.integer(year)
  ) %>%
  filter(year > 2000) %>%
  select(year, month, longterm_debt_FOI, longterm_FOI)

TIC_liabs_monthly_FOIs <- read_csv(
  file.path(raw, "tic_historic", "slt2d_history.csv"),
  show_col_types = FALSE,
  col_names = FALSE
) %>%
  select(1, 3, 6, 9, 14, 27) %>%
  rename(
    v1 = 1, foi_total = 2, foi_treas = 3, foi_agcy = 4, foi_corp = 5, foi_stk = 6
  ) %>%
  mutate(nvals = row_number()) %>%
  filter(nvals > 17) %>%
  select(-nvals) %>%
  separate(v1, into = c("year", "month"), sep = "-", remove = TRUE) %>%
  mutate(
    year = as.integer(year)
  ) %>%
  filter(!is.na(year), month %in% c("Jun", "Dec")) %>%
  mutate(
    month = recode(month, Jun = "6", Dec = "12"),
    across(starts_with("foi_"), as.numeric)
  ) %>%
  bind_rows(TIC_longterm_FOI_2001_11 %>% mutate(month = as.character(month))) %>%
  mutate(month = as.integer(month)) %>%
  arrange(year, month) %>%
  mutate(
    longterm_FOI = coalesce(longterm_FOI, foi_total)
  ) %>%
  select(-foi_total, -longterm_debt_FOI) %>%
  left_join(TIC_shortterm_FOI, by = c("year", "month"))

shortterm_ratio_FOI <- TIC_liabs_monthly_FOIs %>%
  mutate(
    short_long_ratio = shortterm_FOI / longterm_FOI
  ) %>%
  filter(month == 12) %>%
  select(year, month, longterm_FOI, short_long_ratio)

write_dta(shortterm_ratio_FOI, file.path(work, "shortterm_ratio_FOI.dta"))

# adjustment ratio based on Saudi Arabia & Kuwait
adjustfactor <- read_dta(file.path(work, "TIC_liab_monthly_complete.dta")) %>%
  filter(country_code %in% c(46612, 45608, 43109, 46604)) %>%
  mutate(help = if_else(country_code == 46612, 1, 0)) %>%
  group_by(year, month, help) %>%
  summarise(equity = sum(equity, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = month, values_from = equity, names_prefix = "equity") %>%
  mutate(adj_eq = equity12 / equity6) %>%
  filter(help == 0) %>%
  select(year, adj_eq)

adjust_period <- TIC_liabs_monthly_FOIs %>%
  select(year, month, foi_stk) %>%
  pivot_wider(names_from = month, values_from = foi_stk, names_prefix = "foi_stk") %>%
  mutate(
    adj_eq_foiUS = foi_stk12 / foi_stk6
  ) %>%
  select(year, adj_eq_foiUS) %>%
  left_join(adjustfactor, by = "year") %>%
  mutate(
    adj_eq = if_else(year == 2011, adj_eq_foiUS, adj_eq)
  ) %>%
  select(year, adj_eq)

write_dta(adjust_period, file.path(work, "adjust_period.dta"))

Bertaut_Judson_middleeast_Dec <- read_dta(file.path(work, "TIC_liab_monthly_complete.dta")) %>%
  filter(country_name == " Middle Eastern Oil Exporters", month == 12) %>%
  mutate(
    country = country_name,
    Equity = equity,
    Debtl = debtl,
    Total = Equity + Debtl
  )

write_dta(Bertaut_Judson_middleeast_Dec, file.path(work, "Bertaut_Judson_middleeast_Dec.dta"))

# ------------------------------------------------------------------------------
# BIS securities of international organizations
# ------------------------------------------------------------------------------

BIS_total_debt <- read_csv(
  file.path(raw, "BIS_2023_table-c1.csv"),
  show_col_types = FALSE,
  col_names = FALSE
) %>%
  select(2, 4, 6, 234:321) %>%
  filter(X6 %in% c("Issue market", "C:International markets")) %>%
  filter(X4 %in% c("Issuer sector - immediate borrower", "1:All issuers")) %>%
  filter(X2 %in% c(
    "Issuer residence", "1C:International organisations", "KY:Cayman Islands",
    "BS:Bahamas", "BM:Bermuda", "CW:Curacao", "LI:Liechtenstein"
  )) %>%
  rename(A = X2)

for (j in seq_along(BIS_total_debt)) {
  BIS_total_debt[[j]][1] <- paste0("v_", BIS_total_debt[[j]][1])
}

nm <- names(BIS_total_debt)
for (j in seq_along(BIS_total_debt)) {
  nm[j] <- make_clean_names(as.character(BIS_total_debt[[j]][1]))
}
names(BIS_total_debt) <- nm

BIS_total_debt <- BIS_total_debt %>%
  slice(-1) %>%
  pivot_longer(
    cols = starts_with("v_"),
    names_to = "date",
    values_to = "total_debt_BIS"
  ) %>%
  separate(date, into = c("date1", "date2", "date3", "date4"), sep = "_", remove = TRUE) %>%
  filter(date3 == "12") %>%
  transmute(
    A = A,
    year = as.integer(date4),
    total_debt_BIS = as.numeric(na_if(na_if(total_debt_BIS, ""), "...")),
    host = case_when(
      A == "1C:International organisations" ~ 9998,
      A == "KY:Cayman Islands" ~ 377,
      A == "BS:Bahamas" ~ 313,
      A == "BM:Bermuda" ~ 319,
      A == "CW:Curacao" ~ 355,
      A == "LI:Liechtenstein" ~ 9006,
      TRUE ~ NA_real_
    )
  ) %>%
  select(year, host, total_debt_BIS)

write_dta(BIS_total_debt %>% filter(host == 9998),
          file.path(work, "BIS_total_debt_IO.dta"))
write_dta(BIS_total_debt %>% filter(host != 9998),
          file.path(work, "BIS_total_debt_ofc.dta"))

# ------------------------------------------------------------------------------
# Dutch SFIs
# ------------------------------------------------------------------------------

dnb <- read_csv(
  file.path(raw, "DNB_2023_Cross-border_securities_holdings_(Quarter).csv"),
  show_col_types = FALSE
) %>%
  filter(
    hoofdpost == "Dutch portfolio investment in foreign securities ",
    subpost1 %in% c("Foreign equity and shares in foreign investment funds ",
                    "Foreign debt securities ")
  )

# assets
DNB_assets_total <- dnb %>%
  filter(subpost2 %in% c("Total ", "Long term foreign debt securities ", "Short term foreign debt securities ")) %>%
  arrange(subpost2, sector) %>%
  filter(
    (sector == "Total " & subpost1 == "Foreign equity and shares in foreign investment funds ") |
      (sector != "Total " & subpost1 == "Foreign debt securities ")
  ) %>%
  filter(!(sector == "Other sectors " & subsector == "Total ")) %>%
  filter(label7 != "Of which SFIs ") %>%
  group_by(subpost2, period) %>%
  summarise(
    waarde = sum(waarde, na.rm = TRUE),
    subpost1 = first(subpost1),
    .groups = "drop"
  ) %>%
  separate(period, into = c("year", "quarter"), sep = "Q", remove = TRUE) %>%
  mutate(year = as.integer(year)) %>%
  group_by(subpost1, year, quarter) %>%
  summarise(waarde = sum(waarde, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    subpost1 = if_else(subpost1 == "Foreign debt securities ", "debt", "equity")
  ) %>%
  pivot_wider(
    names_from = subpost1,
    values_from = waarde,
    names_prefix = ""
  ) %>%
  filter(quarter == "4") %>%
  rename(
    debt = debt,
    equity = equity
  )

# SFIs
DNB_assets <- dnb %>%
  filter(subpost2 %in% c("Foreign equity ", "Long term foreign debt securities ", "Short term foreign debt securities ")) %>%
  filter(label7 == "Of which SFIs ") %>%
  separate(period, into = c("year", "quarter"), sep = "Q", remove = TRUE) %>%
  mutate(year = as.integer(year)) %>%
  group_by(subpost1, year, quarter) %>%
  summarise(
    label7 = first(label7),
    waarde = sum(waarde, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    var = case_when(
      subpost1 == "Foreign equity and shares in foreign investment funds " ~ "equity_SFI",
      subpost1 == "Foreign debt securities " ~ "debt_SFI",
      TRUE ~ NA_character_
    )
  ) %>%
  select(-subpost1, -label7) %>%
  pivot_wider(
    names_from = var,
    values_from = waarde
  ) %>%
  filter(quarter == "4") %>%
  left_join(DNB_assets_total %>% select(year, debt, equity), by = "year")

eur <- read_dta(file.path(raw, "dta", "xrates.dta")) %>%
  filter(B == "Euro Area")

DNB_assets <- DNB_assets %>%
  left_join(eur, by = "year") %>%
  rename(
    debt_NL = debt,
    equity_NL = equity
  )

for (v in c("debt_SFI", "debt_NL", "equity_SFI", "equity_NL")) {
  DNB_assets[[v]] <- DNB_assets[[v]] / DNB_assets$xrate
}

DNB_assets <- DNB_assets %>%
  select(year, debt_SFI, equity_SFI, debt_NL, equity_NL) %>%
  filter(year < 2015) %>%
  mutate(source = 138)

write_dta(DNB_assets, file.path(work, "DNB_assets.dta"))
