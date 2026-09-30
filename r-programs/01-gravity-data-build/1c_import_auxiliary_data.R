# ==============================================================================
# REPL: Global Offshore Wealth Using R, 2001-2021
# jlenke, 2026
# Purpose: import from different sources and formats
# ------------------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(stringr)
library(haven)
library(purrr)
library(janitor)

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
  
  # Mapping über beide Namensvarianten der Zuordnungstabelle
  mapping <- matching_iso_ifscode %>%
    select(country, country_v2, our_code)
  
  mapping_long <- bind_rows(
    mapping %>%
      transmute(
        country = country,
        our_code = our_code
      ),
    mapping %>%
      transmute(
        country = country_v2,
        our_code = our_code
      )
  ) %>%
    filter(!is.na(country), country != "") %>%
    distinct(country, our_code)
  
  # Prüfen, ob ein Name mehreren Codes zugeordnet ist
  mapping_conflicts <- mapping_long %>%
    count(country) %>%
    filter(n > 1)
  
  print(mapping_conflicts, n = Inf)
  
  # CPIS-Daten zuordnen
  m2 <- df %>%
    left_join(
      mapping_long,
      by = "country",
      relationship = "many-to-one"
    ) %>%
    mutate(
      our_code = if_else(
        country == "SEFER + SSIO (**)",
        9999,
        our_code
      )
    ) %>%
    filter(!is.na(our_code)) %>%
    transmute(
      source = our_code,
      cname = country,
      year = year,
      !!paste0("sum", asset, "asset") := as.numeric(value)
    )
  saveRDS(m2, file = file.path(work, paste0("data_tot", asset, "_update.rds")))
  #write_dta(m2, file.path(work, paste0("data_tot", asset, "_update.dta")))
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

saveRDS(adjustfactor_cpis, file.path(work, "adjustfactor_cpis.rds"))
#write_dta(adjustfactor_cpis, file.path(work, "adjustfactor_cpis.dta"))
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

  # --- FIXME Korrektur für unterschiedliche Namen ...
  df <- df %>%
    mutate(
      country = if_else(
        country == "United States",
        "United States of America",
        country
      )
    )
  
  
  df <- df %>%
    mutate(
      country = case_when(
        country == "United States" ~ "United States of America",
        country == "Türkiye, Rep. of" ~ "Turkey",
        country == "Curaçao, Kingdom of the Netherlands" ~ "Curacao and Sint Maarten",
        country == "Sint Maarten, Kingdom of the Netherlands" ~ "Curacao and Sint Maarten",
        TRUE ~ country
      )
    )
  
  
  # ------------------------------------------------------------
  # IIP: Länderzuordnung über beide Namensvarianten
  # ------------------------------------------------------------
  
  # Mapping über die erste Namensspalte
  mapping_country <- matching_iso_ifscode %>%
    select(
      country,
      code_country = our_code
    )
  
  # Mapping über die alternative Namensspalte
  mapping_country_v2 <- matching_iso_ifscode %>%
    select(
      country_v2,
      code_country_v2 = our_code
    )
  
  # Beide Zuordnungswege verwenden
  m2 <- df %>%
    left_join(
      mapping_country,
      by = "country",
      relationship = "many-to-one"
    ) %>%
    left_join(
      mapping_country_v2,
      by = c("country" = "country_v2"),
      relationship = "many-to-one"
    ) %>%
    mutate(
      host = case_when(
        country == "United States" ~ 111,
        country == "Türkiye, Rep. of" ~ 186,
        country == "Curaçao, Kingdom of the Netherlands" ~ 355,
        country == "Sint Maarten, Kingdom of the Netherlands" ~ 355,
        TRUE ~ coalesce(code_country, code_country_v2)
      )
    ) %>%
    filter(!is.na(host)) %>%
    transmute(
      host = host,
      cname = country,
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

  saveRDS(host_df, file = file.path(work, paste0("IIP_", liab, "liab_host.rds")))
  #write_dta(host_df, file.path(work, paste0("IIP_", liab, "liab_host.dta")))


  total_df <- m2 %>%
    group_by(year) %>%
    summarise(
      !!paste0(liab, "liab_IIP") :=
        sum(value, na.rm = TRUE),
      .groups = "drop"
    )

  saveRDS(total_df, file = file.path(work, paste0("IIP_", liab, "liab.rds")))
  #write_dta(total_df, file.path(work, paste0("IIP_", liab, "liab.dta")))
}

# ------------------------------------------------------------------------------
# import U.S. data from TIC
# ------------------------------------------------------------------------------

tic_raw <- read_csv(
  file.path(raw, "TIC_2022_foreign_portfolio_holdings_of_US_securities.csv"),
  col_names = FALSE,
  show_col_types = FALSE
)

tic <- tic_raw %>%
  mutate(nvals = row_number()) %>%
  filter(nvals == 9 | nvals > 10) %>%
  select(1:205) %>%
  mutate(across(everything(), as.character))

# Replace text in row 2
for (j in seq_along(tic)) {
  tic[[j]][2] <- str_replace_all(tic[[j]][2], fixed("Total securities"), "Total")
  tic[[j]][2] <- str_replace_all(tic[[j]][2], fixed("Total long-term Debt"), "Debtl")
}

# rename (v1 v2) (countryid country)
names(tic)[1:2] <- c("countryid", "country")

# First renaming pass: append cleaned row-2 labels
nm <- names(tic)
for (j in seq_along(tic)) {
  if (j >= 3) {
    vname <- make_clean_names(tic[[j]][2])
    nm[j] <- paste0(nm[j], "_", vname)
  }
}
names(tic) <- nm

# drop in 2
tic <- tic[-2, ]

# Remove "Jun " and "Mar "
for (j in seq_along(tic)) {
  if (j >= 3) {
    tic[[j]] <- str_replace_all(tic[[j]], fixed("Jun "), "")
    tic[[j]] <- str_replace_all(tic[[j]], fixed("Mar "), "")
  }
}

# Second renaming pass: append cleaned row-1 labels
nm <- names(tic)
for (j in seq_along(tic)) {
  if (j >= 3) {
    vname <- make_clean_names(tic[[j]][1])
    nm[j] <- paste0(nm[j], vname)
  }
}
names(tic) <- nm

# drop in 1
tic <- tic[-1, ] %>%
  filter(country != "", !is.na(country))

# fix known bad column name
names(tic)[names(tic) == "v203_Total__1__2000"] <- "v203_Total_2000"

TIC_sub <- tic

# Build yearly files 2002-2021
#####
tic_list <- list()

for (i in 2002:2021) {
  
  sub <- TIC_sub %>%
    select(countryid, country, matches(paste0(i, "$"))) %>%
    mutate(group = i)
  
  # identify the three asset columns for this year
  total_col  <- names(sub)[str_detect(names(sub), regex("total", ignore_case = TRUE))]
  equity_col <- names(sub)[str_detect(names(sub), regex("equity", ignore_case = TRUE))]
  debt_col   <- names(sub)[str_detect(names(sub), regex("debtl", ignore_case = TRUE))]
  
  sub <- sub %>%
    rename(
      Total  = all_of(total_col[1]),
      Equity = all_of(equity_col[1]),
      Debtl  = all_of(debt_col[1])
    )
  
  tic_list[[as.character(i)]] <- sub
}

# 2000
tic_2000 <- TIC_sub %>%
  select(countryid, country, matches("2000$")) %>%
  mutate(group = 2000)

total_col  <- names(tic_2000)[str_detect(names(tic_2000), regex("total", ignore_case = TRUE))]
equity_col <- names(tic_2000)[str_detect(names(tic_2000), regex("equity", ignore_case = TRUE))]
debt_col   <- names(tic_2000)[str_detect(names(tic_2000), regex("debtl", ignore_case = TRUE))]

tic_2000 <- tic_2000 %>%
  rename(
    Total  = all_of(total_col[1]),
    Equity = all_of(equity_col[1]),
    Debtl  = all_of(debt_col[1])
  )


# Combine all years

data_TIC_update <- bind_rows(
  c(list(tic_2000), tic_list)
) %>%
  select(countryid, country, group, Total, Equity, Debtl) %>%
  mutate(
    countryid = as.character(countryid),
    across(
      c(Total, Equity, Debtl),
      ~ as.numeric(
        na_if(
          na_if(str_replace_all(.x, ",", ""), "*"),
          "n.a."
        )
      )
    )
  )

tic_duplicates <- data_TIC_update %>%
  count(countryid, country, group) %>%
  filter(n > 1)

print(tic_duplicates)
####


names(data_TIC_update)
names(tic_2000)
names(tic_list[[1]])

data_TIC_update <- data_TIC_update %>%
  pivot_wider(
    id_cols = c(countryid, country),
    names_from = group,
    values_from = c(Total, Equity, Debtl)
  ) %>%
  mutate(
    Equity_2001 = (Equity_2000 + Equity_2002) / 2,
    Debtl_2001  = (Debtl_2000 + Debtl_2002) / 2,
    Total_2001  = (Total_2000 + Total_2002) / 2
  ) %>%
  pivot_longer(
    cols = matches("^(Total|Equity|Debtl)_"),
    names_to = c(".value", "year"),
    names_sep = "_"
  ) %>%
  mutate(
    year = as.integer(year),
    flag_TIC = if_else(
      year == 2001,
      "2001 is estimated as the mean of 2000 and 2002",
      NA_character_
    )
  ) %>%
  arrange(countryid, country, year)

saveRDS(data_TIC_update, file.path(work, "data_TIC_update.rds"))
#write_dta(data_TIC_update, file.path(work, "data_TIC_update.dta"))

# ------------------------------------------------------------------------------
# import China's assets
# ------------------------------------------------------------------------------

# 1. Public assets: est. 85-95% of foreign exchange reserves
china_imf_raw <- read_excel(
  file.path(raw, "IMF_IIP_China.xlsx"),
  sheet = "Annual",
  col_names = FALSE
)

# Entspricht dem Stata-keep if ...
china_imf <- china_imf_raw %>%
  mutate(
    A = .[[1]],
    B = .[[2]]
  ) %>%
  filter(
    A == "Other reserve assets" |
      B == "2004" |
      (A == "Equity and investment fund shares" &
         lag(A, 1) == "Portfolio investment" &
         lag(A, 11) == "Assets") |
      (A == "Debt securities" &
         lag(A, 7) == "Portfolio investment" &
         lag(A, 17) == "Assets")
  ) %>%
  select(1:19)   # A-S

# Stata:
# foreach v of varlist B - S {
#   forvalues k = 2004/2021{
#     replace `v' = subinstr(`v',"`k'","v_`k'",.) in 1
#   }
# }
for (j in 2:ncol(china_imf)) {
  for (k in 2004:2021) {
    china_imf[[j]][1] <- str_replace_all(
      as.character(china_imf[[j]][1]),
      as.character(k),
      paste0("v_", k)
    )
  }
}

# Stata:
# foreach v of varlist B - S {
#    local vname = strtoname(`v'[1])
#    rename `v' `vname'
# }
nm <- names(china_imf)
for (j in 2:ncol(china_imf)) {
  nm[j] <- make_clean_names(as.character(china_imf[[j]][1]))
}
names(china_imf) <- nm

china_imf <- china_imf %>%
  slice(-1) %>%
  rename(A = 1) %>%
  mutate(
    A = case_when(
      A == "Equity and investment fund shares" ~ "Equity",
      A == "Debt securities" ~ "Debt",
      A == "Other reserve assets" ~ "Reserves",
      TRUE ~ A
    )
  )

# Robuste Auswahl der Jahr-Spalten
year_cols <- names(china_imf)[str_detect(names(china_imf), "^v_?[0-9]{4}$")]

data_IMF_China_main <- china_imf %>%
  pivot_longer(
    cols = all_of(year_cols),
    names_to = "year",
    values_to = "value"
  ) %>%
  mutate(
    year = as.integer(str_extract(year, "[0-9]{4}")),
    value = str_replace_all(as.character(value), ",", ""),
    value = str_replace_all(value, "K\\s*", ""),
    value = as.numeric(value)
  ) %>%
  pivot_wider(
    names_from = A,
    values_from = value
  ) %>%
  rename_with(~ paste0(.x, "_IMF"), -year)

# ------------------------------------------------------------------------------
# foreign exchange reserves pre-2004
# ------------------------------------------------------------------------------
reserves_2001_raw <- read_excel(
  file.path(raw, "IMF_2023_IFS_China_reserves.xlsx"),
  col_names = FALSE
)

reserves_2001 <- reserves_2001_raw %>%
  slice(-1)

nm <- names(reserves_2001)
for (j in 3:ncol(reserves_2001)) {
  nm[j] <- paste0("v", make_clean_names(as.character(reserves_2001[[j]][1])))
}
names(reserves_2001) <- nm

reserves_2001 <- reserves_2001 %>%
  slice(-1) %>%
  rename(A = 1, B = 2)

# Statt Regex: alle Daten-Spalten außer A und B nehmen
reserve_year_cols <- setdiff(names(reserves_2001), c("A", "B"))

reserves_2001 <- reserves_2001 %>%
  pivot_longer(
    cols = all_of(reserve_year_cols),
    names_to = "year",
    values_to = "reserves_2001"
  ) %>%
  select(-A, -B) %>%
  mutate(
    year = as.integer(str_extract(year, "[0-9]{4}")),
    reserves_2001 = as.numeric(reserves_2001)
  ) %>%
  filter(!is.na(year))

names(reserves_2001)
head(reserves_2001)

data_IMF_China <- data_IMF_China_main %>%
  left_join(reserves_2001, by = "year") %>%
  mutate(
    Reserves_IMF = if_else(year < 2004, reserves_2001, Reserves_IMF)
  ) %>%
  select(-reserves_2001) %>%
  filter(year <= 2021)

saveRDS(data_IMF_China, file.path(work, "data_IMF_China.rds"))
#write_dta(data_IMF_China, file.path(work, "data_IMF_China.dta"))

# ------------------------------------------------------------------------------
# import foreign exchange data
# ------------------------------------------------------------------------------

matching_iso_ifscode <- read_dta(file.path(raw, "dta", "matching_iso_ifscode.dta"))

foreignexchange_raw <- read_excel(
  file.path(raw, "IMF_2023_IFS_Foreign_exchange.xlsx"),
  col_names = FALSE
)

data_foreignexchange_update <- foreignexchange_raw %>%
  # drop B
  select(-2) %>%
  # drop in 1
  slice(-1)

# Stata:
# foreach v of varlist C-X{
#    local vname = strtoname(`v'[1])
#    rename `v' v`vname'
# }
nm <- names(data_foreignexchange_update)
for (j in 3:ncol(data_foreignexchange_update)) {
  nm[j] <- paste0("v", make_clean_names(as.character(data_foreignexchange_update[[j]][1])))
}
names(data_foreignexchange_update) <- nm

data_foreignexchange_update <- data_foreignexchange_update %>%
  # drop in 1
  slice(-1) %>%
  rename(A = 1)

# robust statt starts_with("v_")
year_cols <- setdiff(names(data_foreignexchange_update), "A")

data_foreignexchange_update <- data_foreignexchange_update %>%
  pivot_longer(
    cols = all_of(year_cols),
    names_to = "year",
    values_to = "reserveIFS"
  ) %>%
  mutate(
    year = as.integer(str_extract(year, "[0-9]{4}")),
    reserveIFS = as.numeric(reserveIFS)
  ) %>%
  rename(country = A) %>%
  filter(!is.na(year))

# erste Zuordnung über country
m1 <- data_foreignexchange_update %>%
  left_join(matching_iso_ifscode, by = "country") %>%
  rename(
    our_code_orig = our_code
    #country_v2 = country
  ) %>%
  select(-ifscode)

# zweite Zuordnung über country_v2
data_foreignexchange_update <- m1 %>%
  left_join(
    matching_iso_ifscode,
    by = c("country_v2" = "country"),
    suffix = c("", "_m2")
  ) %>%
  mutate(
    our_code_orig = if_else(
      is.na(our_code_orig) & !is.na(our_code),
      our_code,
      our_code_orig
    )
  ) %>%
  filter(!is.na(our_code_orig)) %>%   # drop BEAC etc.
  transmute(
    source = our_code_orig,
    year,
    reserveIFS
  ) %>%
  filter(year <= 2021)

saveRDS(data_foreignexchange_update, file = file.path(work, "data_foreignexchange_update.rds"))
#write_dta(data_foreignexchange_update, file.path(work, "data_foreignexchange_update.dta"))


# ------------------------------------------------------------------------------
# import TIC U.S. cross-border securities positions
# (Bertaut and Judson 2001-2020 + new monthly TIC data for 2021f)
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# U.S. long-term securities held by foreign residents
# most recent
# ------------------------------------------------------------------------------

#TIC_liab_monthly_2020f_raw <- read_delim(
#  "https://ticdata.treasury.gov/resource-center/data-chart-center/tic/Documents/slt_table1.txt",
#  delim = " ",
#  col_names = FALSE,
#  show_col_types = FALSE
#)

tic_recent_raw <- readr::read_tsv(
  file.path(raw, "slt_table1.txt"),
  col_names = FALSE,
  col_types = readr::cols(.default = readr::col_character()),
  show_col_types = FALSE
)

# --- von hier

tic_recent <- tic_recent_raw %>%
  select(1:4, 7, 10, 13, 16) %>%
  setNames(c(
    "country_name",
    "country_code",
    "date_raw",
    "for_lt_total_pos",
    "for_lt_treas_pos",
    "for_lt_agcy_pos",
    "for_lt_corp_pos",
    "for_lt_eqty_pos"
  )) %>%
  mutate(
    year = as.integer(str_extract(date_raw, "^[0-9]{4}")),
    month = as.integer(str_extract(date_raw, "(?<=-)[0-9]{1,2}$")),
    country_code = as.numeric(country_code),
    across(
      starts_with("for_lt_"),
      ~ suppressWarnings(as.numeric(na_if(as.character(.x), "n.a.")))
    )
  ) %>%
  filter(
    !is.na(year),
    month %in% c(6, 12),
    country_code != 72907,
    country_code != 76929,
    !(country_code > 79995 & country_code < 99996)
  ) %>%
  select(-date_raw)
### bis hier

tic_recent_raw %>%
  select(1:3) %>%
  setNames(c("country_name", "country_code", "date_raw")) %>%
  filter(
    !is.na(country_code),
    is.na(suppressWarnings(as.numeric(country_code)))
  ) %>%
  print(n = 30)
tic_recent %>%
  filter(
    country_code == 36137,
    year >= 2019,
    month == 12
  ) %>%
  select(
    year,
    country_name,
    for_lt_eqty_pos,
    for_lt_treas_pos,
    for_lt_agcy_pos,
    for_lt_corp_pos
  ) %>%
  arrange(year) %>%
  print(n = Inf)

# ------------------------------------------------------------------------------
# 2) Bertaut & Judson 2011-2020
# ------------------------------------------------------------------------------

tic_2011_2020 <- read_csv(
  file.path(raw, "ifdp1113_data", "bertaut_judson_positions_liabs_2021.csv"),
  show_col_types = FALSE
) %>%
  mutate(
    date = as.character(date),
    month = str_extract(date, "^[0-9]{1,2}"),
    year = str_extract(date, "[0-9]{4}$")
  ) %>%
  mutate(
    month = as.integer(month),
    year = as.integer(year)
  ) %>%
  filter(!is.na(month), !is.na(year)) %>%
  filter(month %in% c(6, 12)) %>%
  select(year, month, country_code, country_name, ends_with("est_pos")) %>%
  rename_with(~ str_remove(.x, "^ftot_"))


# Historische Schätzungen mit aktuellen TIC-Beständen verknüpfen.
# Join-Schlüssel entsprechend Stata: country_code, month, year.

tic_2011_plus <- tic_2011_2020 %>%
  rename(country_name_hist = country_name) %>%
  full_join(
    tic_recent %>%
      rename(country_name_recent = country_name),
    by = c("country_code", "month", "year")
  ) %>%
  mutate(
    country_name = coalesce(
      country_name_recent,
      country_name_hist
    )
  ) %>%
  select(
    -country_name_recent,
    -country_name_hist
  ) %>%
  arrange(country_code, year, month)
# ------------------------------------------------------------------------------
# 3) Historische TIC-Datei 2001-2011
# ------------------------------------------------------------------------------

tic_2001_2011 <- read_csv(
  file.path(raw, "ticdata", "ticdata.liabilities.ftot.txt"),
  show_col_types = FALSE
)

tic_2001_2011 <- tic_2001_2011 %>%
  mutate(
    date = as.character(date),
    month = str_extract(date, "^[0-9]{1,2}"),
    year = str_extract(date, "[0-9]{4}$")
  ) 

tic_2001_2011 <- tic_2001_2011 %>%
  mutate(
    month = as.integer(month),
    year = as.integer(year)
  ) 

tic_2001_2011 <- tic_2001_2011 %>%
  filter(!is.na(month), !is.na(year))

tic_2001_2011 <- tic_2001_2011 %>%
  filter(month %in% c(6, 12), year > 2000)

tic_2001_2011 <- tic_2001_2011 %>%
  rename(countrycode = `country code`) 

tic_2001_2011 <- tic_2001_2011 %>%
  rename(countryname = `country name`) 

tic_2001_2011 <- tic_2001_2011 %>%
  select(countrycode, countryname, ends_with("est_pos"), month, year)

### test
tic_2001_2011 %>%
  select(
    ftot_agcy_est_pos,
    ftot_corp_est_pos,
    ftot_stk_est_pos,
    ftot_treas_est_pos
  ) %>%
  pivot_longer(
    everything(),
    names_to = "variable",
    values_to = "raw_value"
  ) %>%
  filter(
    !is.na(raw_value),
    is.na(suppressWarnings(
      as.numeric(as.character(raw_value))
    ))
  ) %>%
  count(variable, raw_value, sort = TRUE)
## ende test



tic_2001_2011 <- tic_2001_2011 %>%
  mutate(
    across(
      c(
        ftot_agcy_est_pos,
        ftot_corp_est_pos,
        ftot_stk_est_pos,
        ftot_treas_est_pos
      ),
      ~ as.numeric(
        na_if(str_trim(as.character(.x)), "ND")
      )
    )
  ) %>%
  rename(
    country_code = countrycode,
    country_name = countryname
  ) %>%
  rename_with(~ str_remove(.x, "^ftot_")) %>%
  filter(year != 2011)


TIC_liab_monthly_complete <- tic_2001_2011 %>%
  bind_rows(tic_2011_plus) %>%
  arrange(country_code, year, month) %>%
  mutate(
    for_lt_treas_pos = coalesce(for_lt_treas_pos, treas_est_pos),
    for_lt_agcy_pos  = coalesce(for_lt_agcy_pos, agcy_est_pos),
    for_lt_corp_pos  = coalesce(for_lt_corp_pos, corp_est_pos),
    for_lt_eqty_pos  = coalesce(for_lt_eqty_pos, stk_est_pos)
  ) %>%
  select(
    -matches("_est_"),
    -starts_with("for_lt_total")
  ) %>%
  rename(
    treas = for_lt_treas_pos,
    agcy  = for_lt_agcy_pos,
    corp  = for_lt_corp_pos,
    eqty  = for_lt_eqty_pos
  ) %>%
  mutate(
    debtl = treas + agcy + corp
  ) %>%
  rename(equity = eqty) %>%
  select(country_code, country_name, year, month, equity, debtl)

saveRDS(TIC_liab_monthly_complete, file = file.path(work, "TIC_liab_monthly_complete.rds"))
#write_dta(TIC_liab_monthly_complete, file.path(work, "TIC_liab_monthly_complete.dta"))


# ------------------------------------------------------------------------------
# Cayman Islands
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# TIC: langfristige US-Wertpapiere, gehalten von den Cayman Islands
# ------------------------------------------------------------------------------
# FIXME work2
#TIC_lt_monthly_Cayman_complete <- read_dta(file.path(work, "TIC_liab_monthly_complete.dta")) %>%
#TIC_lt_monthly_Cayman_complete <- read_dta(file.path(work2, "TIC_liab_monthly_complete.dta")) %>%
TIC_lt_monthly_Cayman_complete <- read_work_data("TIC_liab_monthly_complete") %>%
  filter(country_code == 36137, month == 12) %>%
  mutate(
    source = 377,
    host = 111
  ) %>%
  select(-country_code, -country_name, -month)

# ------------------------------------------------------------------------------
# TIC: kurzfristige Schuldtitel der Cayman Islands
# ------------------------------------------------------------------------------

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
  slice(-1, -2) %>%
  mutate(
    date = as.character(date),
    year = str_extract(date, "^[0-9]{4}"),
    month = str_extract(date, "(?<=-)[0-9]{2}$")
  ) %>%
  filter(month == "12") %>%
  transmute(
    year = as.integer(year),
    shortterm_official = as.numeric(shortterm_official),
    shortterm_other = as.numeric(shortterm_other)
  ) %>%
  mutate(
    shortterm_debt = rowSums(across(c(shortterm_official, shortterm_other)), na.rm = TRUE)
  ) %>%
  select(year, shortterm_debt)


cayman_short %>%
  filter(year >= 2019) %>%
  print(n = Inf)

TIC_lt_monthly_Cayman_complete %>%
  filter(year >= 2019) %>%
  print(n = Inf)

Cayman_TIC_Dec <- cayman_short %>%
  full_join(TIC_lt_monthly_Cayman_complete, by = "year") %>%
  mutate(
    # Werte für 2001 und 2002 wie im Stata-Skript gesetzt
    shortterm_debt = if_else(year == 2001, 4712, shortterm_debt),
    shortterm_debt = if_else(year == 2002, 11018, shortterm_debt),
    
    # Stata hat hier vermutlich einen Tippfehler:
    # gen debt = shortterm + debtl
    # gemeint ist fast sicher shortterm_debt + debtl
    debt = shortterm_debt + debtl
  ) %>%
  arrange(year) %>%
  rename(
    debts_KY_TIC = shortterm_debt,
    debtl_KY_TIC = debtl,
    eq_KY_TIC = equity,
    debt_KY_TIC = debt
  )

#---
Cayman_TIC_Dec %>%
  filter(year >= 2019) %>%
  select(year, source, host, eq_KY_TIC, debt_KY_TIC) %>%
  print(n = Inf)
TIC_lt_monthly_Cayman_complete %>%
  summarise(
    n = n(),
    min_year = min(year, na.rm = TRUE),
    max_year = max(year, na.rm = TRUE)
  )

cayman_short %>%
  summarise(
    n = n(),
    min_year = min(year, na.rm = TRUE),
    max_year = max(year, na.rm = TRUE)
  )

###
saveRDS(Cayman_TIC_Dec, file = file.path(work, "Cayman_TIC_Dec.rds"))
#write_dta(Cayman_TIC_Dec, file.path(work, "Cayman_TIC_Dec.dta"))




# ------------------------------------------------------------------------------
# CPIS banking holdings Cayman
# ------------------------------------------------------------------------------

KY_banks <- read_excel(
  file.path(raw, "IMF_2023_assets_Cayman_banking.xlsx"),
  col_names = FALSE
) %>%
  slice(-1, -2)

# Jahres-Spaltennamen erzeugen
nm <- names(KY_banks)
for (j in 2:ncol(KY_banks)) {
  header <- as.character(KY_banks[[j]][1])
  for (k in 2004:2021) {
    header <- str_replace_all(header, fixed(paste0("Dec. ", k)), paste0("v_", k))
  }
  nm[j] <- make_clean_names(header)
}
names(KY_banks) <- nm

bank_cols <- setdiff(names(KY_banks), names(KY_banks)[1])

KY_banks <- KY_banks %>%
  slice(-1) %>%
  rename(A = 1) %>%
  pivot_longer(
    cols = all_of(bank_cols),
    names_to = "year",
    values_to = "bank"
  ) %>%
  mutate(
    year = as.integer(str_extract(year, "[0-9]{4}")),
    bank = as.numeric(bank)
  ) %>%
  filter(!is.na(year)) %>%
  select(year, bank)

# ------------------------------------------------------------------------------
# CPIS insurance holdings Cayman
# ------------------------------------------------------------------------------

KY_ins <- read_excel(
  file.path(raw, "IMF_2023_assets_Cayman_ins.xlsx"),
  col_names = FALSE
) %>%
  slice(-1, -2)

nm <- names(KY_ins)
for (j in 2:ncol(KY_ins)) {
  header <- as.character(KY_ins[[j]][1])
  for (k in 2016:2021) {
    header <- str_replace_all(header, fixed(paste0("Dec. ", k)), paste0("v_", k))
  }
  nm[j] <- make_clean_names(header)
}
names(KY_ins) <- nm

ins_cols <- setdiff(names(KY_ins), names(KY_ins)[1])

KY_banks_final <- KY_ins %>%
  slice(-1) %>%
  rename(A = 1) %>%
  pivot_longer(
    cols = all_of(ins_cols),
    names_to = "year",
    values_to = "ins"
  ) %>%
  mutate(
    year = as.integer(str_extract(year, "[0-9]{4}")),
    ins = as.numeric(ins)
  ) %>%
  filter(!is.na(year)) %>%
  select(year, ins) %>%
  left_join(KY_banks, by = "year") %>%
  arrange(year) %>%
  mutate(
    # Stata ist hier inkonsistent:
    # gen KY_assets_bank = ins + bank
    # replace KY_assets = bank if ins==.
    # keep year KY_assets
    # gemeint ist offenbar KY_assets
    KY_assets = ins + bank,
    KY_assets = if_else(is.na(ins), bank, KY_assets),
    host = 377
  ) %>%
  select(year, KY_assets, host)

saveRDS(KY_banks_final, file = file.path(work, "KY_banks.rds"))
#write_dta(KY_banks_final, file.path(work, "KY_banks.dta"))

# ------------------------------------------------------------------------------
# Schätzung der Equity-Liabilities nichtfinanzieller Cayman-Unternehmen
# ------------------------------------------------------------------------------

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
    
    # erste zwei Stellen von iid
    issue = as.numeric(substr(iid, 1, 2))
  ) %>%
  # ADRs entfernen
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
  left_join(
    xrates %>% select(currency, year, xrate),
    by = c("currency", "year")
  ) %>%
  mutate(
    mktcap_usd = mktcap / xrate
  ) %>%
  group_by(year) %>%
  summarise(
    mktcap_usd = sum(mktcap_usd, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    eqliab_nfc = 0.75 * mktcap_usd
  )

saveRDS(KY_liab_nfc, file = file.path(work, "KY_liab_nfc.rds"))
#write_dta(KY_liab_nfc, file.path(work, "KY_liab_nfc.dta"))
#####



# ------------------------------------------------------------------------------
# China's liabilities
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# long-term
# ------------------------------------------------------------------------------
# FIXME
#TIC_longterm_monthly_China <- read_dta(file.path(work, "TIC_liab_monthly_complete.dta")) %>%
#TIC_longterm_monthly_China <- read_dta(file.path(work2, "TIC_liab_monthly_complete.dta")) %>%
TIC_longterm_monthly_China <- read_work_data("TIC_liab_monthly_complete") %>% 
  filter(country_code == 41408, month == 12)

# ------------------------------------------------------------------------------
# short-term 2003-2023
# ------------------------------------------------------------------------------

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
  slice(-1, -2) %>%
  mutate(
    date = as.character(date),
    year = str_extract(date, "^[0-9]{4}"),
    month = str_extract(date, "(?<=-)[0-9]{2}$")
  ) %>%
  filter(month == "12") %>%
  transmute(
    year = as.integer(year),
    shortterm_official = as.numeric(shortterm_official),
    shortterm_other = as.numeric(shortterm_other)
  ) %>%
  mutate(
    shortterm_debt = rowSums(across(c(shortterm_official, shortterm_other)), na.rm = TRUE)
  ) %>%
  select(year, shortterm_debt)

# ------------------------------------------------------------------------------
# short-term 2001-2003
# ------------------------------------------------------------------------------


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
  slice(-1, -2) %>%
  mutate(
    date = as.character(date),
    year = as.integer(str_extract(date, "^[0-9]{4}")),
    month = str_extract(date, "(?<=-)[0-9]{2}$")
  ) %>%
  filter(month == "12", year > 2000) %>%
  transmute(
    year,
    across(
      c(shortterm_official, shortterm_other),
      ~ as.numeric(
        na_if(
          na_if(str_trim(as.character(.x)), "-"),
          "*"
        )
      )
    )
  ) %>%
  mutate(
    shortterm_debt = shortterm_official + shortterm_other
  ) %>%
  select(year, shortterm_debt)

# ------------------------------------------------------------------------------
# merge short-term and long-term
# ------------------------------------------------------------------------------

TIC_China_Dec <- bind_rows(TIC_China_short_pre, TIC_China_short) %>%
  arrange(year) %>%
  left_join(
    TIC_longterm_monthly_China %>% select(year, equity, debtl),
    by = "year"
  ) %>%
  mutate(
    source = 924,   # IFS country code for China
    host = 111,     # IFS country code for USA
    
    total_China_TIC = equity + debtl + shortterm_debt,
    total_lt_China_TIC = equity + debtl,
    
    eq_China_TIC = equity,
    debtl_China_TIC = debtl,
    debts_China_TIC = shortterm_debt,
    debt_China_TIC = debtl_China_TIC + debts_China_TIC
  ) %>%
  select(
    year, source, host,
    eq_China_TIC, debtl_China_TIC, debts_China_TIC,
    debt_China_TIC, total_lt_China_TIC, total_China_TIC
  )

saveRDS(TIC_China_Dec, file = file.path(work, "TIC_China_Dec.rds"))
#write_dta(TIC_China_Dec, file.path(work, "TIC_China_Dec.dta"))

####

# ------------------------------------------------------------------------------
# Assets of Middle Eastern Oil Exporters
# ------------------------------------------------------------------------------

# Hinweis:
# Bertaut & Judson berichten "Middle Eastern Oil Exporters" zunächst als Aggregat.
# Ab 2010 wird auf Länderebene umgestellt, umfasst aber nur Kuwait und Saudi-Arabien.
# Deshalb wird später ab 2010 mit Juni-Werten gearbeitet und auf Dezember hochgerechnet.

# ------------------------------------------------------------------------------
# 1) Relevante TIC-Daten für Middle East speichern
# ------------------------------------------------------------------------------
# FIXME
#TIC_update_middleast <- read_dta(file.path(work, "data_TIC_update.dta")) %>%
#TIC_update_middleast <- read_dta(file.path(work2, "data_TIC_update.dta")) %>%
TIC_update_middleast <- read_work_data("data_TIC_update") %>%
  filter(country %in% c(
    " Middle Eastern Oil Exporters",
    "Kuwait",
    "Saudi Arabia",
    "Bahrain",
    "Iran",
    "Iraq",
    "Oman",
    "Qatar",
    "United Arab Emirates"
  ))

saveRDS(TIC_update_middleast, file = file.path(work, "TIC_update_middleast.rds"))
#write_dta(TIC_update_middleast, file.path(work, "TIC_update_middleast.dta"))

TIC_update_middleeast_total <- TIC_update_middleast %>%
  group_by(year) %>%
  summarise(
    Total = sum(Total, na.rm = TRUE),
    Equity = sum(Equity, na.rm = TRUE),
    Debtl = sum(Debtl, na.rm = TRUE),
    .groups = "drop"
  )

saveRDS(TIC_update_middleeast_total, file = file.path(work, "TIC_update_middleeast_total.rds"))
# optional wie im Stata-tempfile
# write_dta(TIC_update_middleeast_total, file.path(work, "TIC_update_middleeast_total.dta"))

# ------------------------------------------------------------------------------
# 2) Kurzfristig / langfristig-Verhältnis der Bestände offizieller Institutionen
# ------------------------------------------------------------------------------

# 2a) kurzfristige liabilities of foreign official institutions

TIC_shortterm_FOI <- read_csv(
  file.path(raw, "tic_historic", "bltype_history.csv"),
  col_names = FALSE,
  col_types = cols(.default = col_character()),
  skip = 18,
  skip_empty_rows = TRUE,
  show_col_types = FALSE
)

TIC_shortterm_FOI <- TIC_shortterm_FOI %>%
  select(1, 6, 7) %>%
  rename(v1 = 1, v6 = 2, v7 = 3) %>%
  mutate(
    v1 = as.character(v1),
    year = str_extract(v1, "^[0-9]{4}"),
    month = str_extract(v1, "(Dec|Jun)$")
  ) %>%
  mutate(
    year = as.integer(year),
    v6 = as.numeric(v6),
    v7 = as.numeric(v7)
  ) %>%
  filter(!is.na(year), year > 2000, month %in% c("Dec", "Jun"))

TIC_shortterm_FOI <- TIC_shortterm_FOI %>%
  mutate(
    shortterm_FOI = rowSums(across(c(v6, v7)), na.rm = TRUE),
    month = recode(month, Dec = "12", Jun = "6"),
    month = as.integer(month)
  ) %>%
  select(year, month, shortterm_FOI)

saveRDS(TIC_shortterm_FOI, file = file.path(work, "TIC_shortterm_FOI.rds"))
#write_dta(TIC_shortterm_FOI, file.path(work, "TIC_shortterm_FOI.dta"))

# 2b) langfristige liabilities of foreign official institutions, 2001-2011
TIC_longterm_FOI_2001_11 <- read_csv(
  file.path(raw, "ticdata", "ticdata.liabilities.foiadj.txt"),
  show_col_types = FALSE
) %>%
  select(date, starts_with("foi_"), ends_with("_est_pos")) %>%
  mutate(
    longterm_debt_FOI = rowSums(
      across(matches("^foi_(agcy|corp|treas)_est_pos$")),
      na.rm = TRUE
    ),
    longterm_FOI = rowSums(
      across(matches("^foi_(agcy|corp|treas|stk)_est_pos$")),
      na.rm = TRUE
    ),
    date = as.character(date),
    month = str_extract(date, "^[0-9]{2}"),
    year = str_extract(date, "[0-9]{4}$")
  ) %>%
  mutate(
    month = if_else(month == "06", "6", month),
    year = as.integer(year)
  ) %>%
  filter(month %in% c("6", "12"), year > 2000) %>%
  transmute(
    year = year,
    month = as.integer(month),
    longterm_debt_FOI = longterm_debt_FOI,
    longterm_FOI = longterm_FOI
  )

###################
# 2c) langfristige liabilities of foreign official institutions, neuere Historie

TIC_liabs_monthly_FOIs <- read_csv(
  file.path(raw, "tic_historic", "slt2d_history.csv"),
  col_names = FALSE,
  col_types = cols(.default = col_character()),
  show_col_types = FALSE,
  skip = 17,
  n_max = 137
)

TIC_liabs_monthly_FOIs <- TIC_liabs_monthly_FOIs %>%
  select(1, 3, 6, 9, 14, 27) %>%
  rename(
    v1 = 1,
    foi_total = 2,
    foi_treas = 3,
    foi_agcy = 4,
    foi_corp = 5,
    foi_stk = 6
  ) %>%
  mutate(nvals = row_number()) %>%
  filter(nvals > 17) %>%
  select(-nvals) %>%
  mutate(
    v1 = as.character(v1),
    year = str_extract(v1, "^[0-9]{4}"),
    month = str_extract(v1, "(Jun|Dec)$")
  ) %>%
  mutate(
    year = as.integer(year)
  ) %>%
  filter(!is.na(year), month %in% c("Jun", "Dec")) %>%
  mutate(
    month = recode(month, Jun = "6", Dec = "12"),
    across(c(foi_total, foi_treas, foi_agcy, foi_corp, foi_stk), as.numeric),
    month = as.integer(month)
  ) %>%
  bind_rows(TIC_longterm_FOI_2001_11) %>%
  arrange(year, month) %>%
  mutate(
    longterm_FOI = coalesce(longterm_FOI, foi_total)
  ) %>%
  select(-foi_total, -longterm_debt_FOI) %>%
  left_join(TIC_shortterm_FOI, by = c("year", "month"))

# Stata hat hier:
# gen short_long_ratio = shortterm / longterm_FOI
# Nach dem Merge heißt die Variable aber shortterm_FOI.
shortterm_ratio_FOI <- TIC_liabs_monthly_FOIs %>%
  mutate(
    short_long_ratio = shortterm_FOI / longterm_FOI
  ) %>%
  filter(month == 12) %>%
  select(year, month, longterm_FOI, short_long_ratio)

saveRDS(shortterm_ratio_FOI, file = file.path(work, "shortterm_ratio_FOI.rds"))
#write_dta(shortterm_ratio_FOI, file.path(work, "shortterm_ratio_FOI.dta"))

# ------------------------------------------------------------------------------
# 3) Anpassungsfaktor Juni -> Dezember
# ------------------------------------------------------------------------------

# 3a) Faktor aus Saudi-Arabien und Kuwait
# FIXME
#adjustfactor <- read_dta(file.path(work, "TIC_liab_monthly_complete.dta")) %>%
#adjustfactor <- read_dta(file.path(work2, "TIC_liab_monthly_complete.dta")) %>%
adjustfactor <- read_work_data("TIC_liab_monthly_complete") %>%
  filter(country_code %in% c(46612, 45608, 43109, 46604)) %>%
  mutate(
    help = if_else(country_code == 46612, 1, 0)
  ) %>%
  group_by(year, month, help) %>%
  summarise(
    equity = sum(equity, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = month,
    values_from = equity,
    names_prefix = "equity"
  ) %>%
  mutate(
    adj_eq = equity12 / equity6
  ) %>%
  filter(help == 0) %>%
  select(year, adj_eq)

# 3b) Faktor aus FOI-Beständen für 2011
adjust_period <- TIC_liabs_monthly_FOIs %>%
  select(year, month, foi_stk) %>%
  pivot_wider(
    names_from = month,
    values_from = foi_stk,
    names_prefix = "foi_stk"
  ) %>%
  mutate(
    adj_eq_foiUS = foi_stk12 / foi_stk6
  ) %>%
  select(year, adj_eq_foiUS) %>%
  left_join(adjustfactor, by = "year") %>%
  mutate(
    adj_eq = if_else(year == 2011, adj_eq_foiUS, adj_eq)
  ) %>%
  select(year, adj_eq)

saveRDS(adjust_period, file = file.path(work, "adjust_period.rds"))
#write_dta(adjust_period, file.path(work, "adjust_period.dta"))

# ------------------------------------------------------------------------------
# 4) Middle East aggregate aus Bertaut & Judson extrahieren
# ------------------------------------------------------------------------------
# FIXME
#Bertaut_Judson_middleeast_Dec <- read_dta(file.path(work, "TIC_liab_monthly_complete.dta")) %>%
#Bertaut_Judson_middleeast_Dec <- read_dta(file.path(work2, "TIC_liab_monthly_complete.dta")) %>%
Bertaut_Judson_middleeast_Dec <- read_work_data("TIC_liab_monthly_complete") %>%
  filter(country_name == " Middle Eastern Oil Exporters", month == 12) %>%
  transmute(
    country = country_name,
    year = year,
    Equity = equity,
    Debtl = debtl,
    Total = Equity + Debtl
  )

saveRDS( Bertaut_Judson_middleeast_Dec, file = file.path(work, "Bertaut_Judson_middleeast_Dec.rds"))
#write_dta(  Bertaut_Judson_middleeast_Dec,  file.path(work, "Bertaut_Judson_middleeast_Dec.dta"))
#####

# ------------------------------------------------------------------------------
# BIS securities of international organizations
# ------------------------------------------------------------------------------

bis_dates <- read_csv(
  file.path(raw, "BIS_2023_table-c1.csv"),
  skip = 5,
  n_max = 0,
  show_col_types = FALSE
) %>%
  names()

# Erste und letzte Zeitreihenspalten
head(bis_dates[-(1:16)], 8)
tail(bis_dates[-(1:16)], 8)

# Import BIS data
bis_file <- file.path(raw, "BIS_2023_table-c1.csv")

bis_raw <- read.csv(
  bis_file,
  skip = 5,
  header = TRUE,
  fill = TRUE,
  colClasses = "character",
  check.names = FALSE,
  na.strings = c("", "NA"),
  stringsAsFactors = FALSE
)


dim(bis_raw)
names(bis_raw)[1:20]

# Metadaten der ersten Beobachtungen
head(bis_raw[, 1:16], 3)

# Letzte Zeitreihenspalten
tail(names(bis_raw), 5)

bis_long <- bis_raw %>%
  tidyr::pivot_longer(
    cols = -(1:16),
    names_to = "date",
    values_to = "value_raw"
  ) %>%
  dplyr::mutate(
    date = as.Date(date, format = "%d/%m/%Y"),
    value = suppressWarnings(as.numeric(value_raw)),
    year = as.integer(format(date, "%Y")),
    quarter = lubridate::quarter(date)
  )


bis_long <- bis_long %>%
  dplyr::filter(
    year >= 2001,
    year <= 2021
  )

### test

# Anzahl der Beobachtungen
dim(bis_long)

# Zeitlicher Umfang
range(bis_long$date, na.rm = TRUE)

# Anzahl unterschiedlicher Quartale
n_distinct(bis_long$date)

# Anteil fehlender numerischer Werte
mean(is.na(bis_long$value))
#####

bis_annual <- bis_long %>%
  filter(
    quarter == 4,
    year >= 2001,
    year <= 2021
  )

BIS_total_debt <- bis_raw %>%
  select(2, 4, 6, 234:321) %>%
  rename(
    v2 = 1,
    v4 = 2,
    v6 = 3
  ) %>%
  filter(
    v6 %in% c("Issue market", "C:International markets"),
    v4 %in% c("Issuer sector - immediate borrower", "1:All issuers"),
    v2 %in% c(
      "Issuer residence",
      "1C:International organisations",
      "KY:Cayman Islands",
      "BS:Bahamas",
      "BM:Bermuda",
      "CW:Curacao",
      "LI:Liechtenstein"
    )
  ) %>%
  rename(A = v2) %>%
  select(-v4, -v6)

# Stata:
# foreach var of varlist v*{
#   replace `var'="v_"+`var' in 1
# }
for (j in 2:ncol(BIS_total_debt)) {
  BIS_total_debt[[j]][1] <- paste0("v_", as.character(BIS_total_debt[[j]][1]))
}

# Stata:
# foreach v of varlist v* {
#   local vname = strtoname(`v'[1])
#   rename `v' `vname'
# }
nm <- names(BIS_total_debt)
for (j in 2:ncol(BIS_total_debt)) {
  nm[j] <- make_clean_names(as.character(BIS_total_debt[[j]][1]))
}
names(BIS_total_debt) <- nm

BIS_total_debt <- BIS_total_debt %>%
  slice(-1)

# robuste Auswahl aller Zeitspalten außer A
date_cols <- setdiff(names(BIS_total_debt), "A")

BIS_total_debt <- BIS_total_debt %>%
  pivot_longer(
    cols = all_of(date_cols),
    names_to = "date",
    values_to = "total_debt_BIS"
  ) %>%
  mutate(
    year = str_extract(date, "[0-9]{4}$"),
    month = str_extract(date, "(?<=_)[0-9]{2}(?=_[0-9]{4}$)")
  ) %>%
  filter(month == "12") %>%
  mutate(
    year = as.integer(year),
    total_debt_BIS = as.character(total_debt_BIS),
    total_debt_BIS = na_if(total_debt_BIS, ""),
    total_debt_BIS = na_if(total_debt_BIS, "..."),
    total_debt_BIS = as.numeric(total_debt_BIS),
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

saveRDS(BIS_total_debt %>% filter(host == 9998), file = file.path(work, "BIS_total_debt_IO.rds"))
#write_dta(  BIS_total_debt %>% filter(host == 9998),  file.path(work, "BIS_total_debt_IO.dta"))

saveRDS(BIS_total_debt %>% filter(host != 9998), file = file.path(work, "BIS_total_debt_ofc.rds"))
#write_dta( BIS_total_debt %>% filter(host != 9998), file.path(work, "BIS_total_debt_ofc.dta"))

#####

# ------------------------------------------------------------------------------
# Dutch SFIs
# ------------------------------------------------------------------------------

dnb_file <- file.path(
  raw,
  "DNB_2023_Cross-border_securities_holdings_(Quarter).csv"
)

# Originaldatei einlesen
dnb_lines <- readLines(dnb_file)

# Quellen- und Disclaimer-Zeilen entfernen
dnb_lines <- dnb_lines[
  !grepl("^\\s*\"?(Source: DNB|Disclaimer:)", dnb_lines)
]

# Bereinigte Daten importieren
dnb_raw <- readr::read_csv(
  I(paste(dnb_lines, collapse = "\n")),
  col_types = readr::cols(.default = readr::col_character()),
  show_col_types = FALSE
)

dnb_raw <- dnb_raw %>%
  rename(
    hoofdpost = Hoofdpost,
    subpost1  = `Subpost 1`,
    subpost2  = `Subpost 2`,
    sector    = Sector,
    subsector = Subsector,
    label7    = `&label7`,
    period    = Period,
    waarde    = waarde
  ) %>%
  mutate(
    waarde = as.numeric(waarde)
  )

dnb <- dnb_raw %>%
  filter(
    hoofdpost == "Dutch portfolio investment in foreign securities",
    subpost1 %in% c(
      "Foreign equity and shares in foreign investment funds",
      "Foreign debt securities"
    )
  )


# ------------------------------------------------------------------------------
# Gesamtvermögen
# ------------------------------------------------------------------------------

DNB_total <- dnb %>%
  filter(
    subpost2 %in% c(
      "Total",
      "Long term foreign debt securities",
      "Short term foreign debt securities"
    )
  ) 

DNB_total <- DNB_total %>%
  arrange(subpost2, sector) %>%
  filter(
    (sector == "Total" & subpost1 == "Foreign equity and shares in foreign investment funds") |
      (sector != "Total" & subpost1 == "Foreign debt securities")
  ) %>%
  filter(!(sector == "Other sectors" & subsector == "Total")) %>%
  filter(label7 != "Of which SFIs") 

DNB_total <- DNB_total %>%
  group_by(subpost2, period) %>%
  summarise(
    waarde = sum(waarde, na.rm = TRUE),
    subpost1 = first(subpost1),
    .groups = "drop"
  ) %>%
  mutate(
    year = as.integer(str_extract(period, "^[0-9]{4}")),
    quarter = str_extract(period, "(?<=Q)[1-4]$")
  ) %>%
  select(-period) 

DNB_total <- DNB_total %>%
  group_by(subpost1, year, quarter) %>%
  summarise(
    waarde = sum(waarde, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    subpost1 = if_else(subpost1 == "Foreign debt securities", "debt", "equity")
  ) %>%
  pivot_wider(
    names_from = subpost1,
    values_from = waarde
  ) 
  
DNB_total <- DNB_total %>%
  filter(quarter == "4") %>%
  rename(
    debt = debt,
    equity = equity
  )

# ------------------------------------------------------------------------------
# SFI-Vermögen
# ------------------------------------------------------------------------------

DNB_assets <- dnb 

DNB_assets <- DNB_assets%>%
  filter(
    subpost2 %in% c(
      "Foreign equity",
      "Long term foreign debt securities",
      "Short term foreign debt securities"
    ),
    label7 == "Of which SFIs"
  ) %>%
  mutate(
    year = as.integer(str_extract(period, "^[0-9]{4}")),
    quarter = str_extract(period, "(?<=Q)[1-4]$")
  ) %>%
  select(-period) %>%
  group_by(subpost1, year, quarter) %>%
  summarise(
    label7 = first(label7),
    waarde = sum(waarde, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    var = case_when(
      subpost1 == "Foreign equity and shares in foreign investment funds" ~ "equity_SFI",
      subpost1 == "Foreign debt securities" ~ "debt_SFI",
      TRUE ~ NA_character_
    )
  ) %>%
  select(-subpost1, -label7) %>%
  pivot_wider(
    names_from = var,
    values_from = waarde
  ) %>%
  filter(quarter == "4") %>%
  left_join(
    DNB_total %>% select(year, debt, equity),
    by = "year"
  )

# ------------------------------------------------------------------------------
# Wechselkurs Euro Area
# ------------------------------------------------------------------------------

eur <- read_dta(file.path(raw, "dta", "xrates.dta")) %>%
  filter(B == "Euro Area")

DNB_assets <- DNB_assets %>%
  left_join(eur, by = "year") %>%
  rename(
    debt_NL = debt,
    equity_NL = equity
  ) %>%
  mutate(
    debt_SFI = debt_SFI / xrate,
    debt_NL = debt_NL / xrate,
    equity_SFI = equity_SFI / xrate,
    equity_NL = equity_NL / xrate
  ) %>%
  select(year, debt_SFI, equity_SFI, debt_NL, equity_NL) %>%
  filter(year < 2015) %>%
  mutate(source = 138)

saveRDS(DNB_assets, file.path(work, "DNB_assets.rds"))
#write_dta(DNB_assets, file.path(work, "DNB_assets.dta"))

