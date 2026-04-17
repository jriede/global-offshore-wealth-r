# ------------------------------------------------------------------------------
# Project: Offshore financial wealth database - update 2023
# Title: 2_do_full_matrices.R
# Purpose: construct exhaustive matrices of identifiable bilateral portfolio assets
# This version: translated from Stata
# ------------------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(haven)
library(purrr)
library(stringr)
library(zoo)

# Assumed path objects:
# raw  <- "path/to/raw"
# work <- "path/to/work"

# ------------------------------------------------------------------------------
# Helpers
# ------------------------------------------------------------------------------

fill_from_next <- function(x, n = 1) {
  out <- x
  for (i in seq_len(n)) {
    idx <- which((is.na(out) | out == 0) & dplyr::lead(out) != 0 & !is.na(dplyr::lead(out)))
    out[idx] <- dplyr::lead(out)[idx]
  }
  out
}

fill_from_prev <- function(x, n = 1) {
  out <- x
  for (i in seq_len(n)) {
    idx <- which((is.na(out) | out == 0) & dplyr::lag(out) != 0 & !is.na(dplyr::lag(out)))
    out[idx] <- dplyr::lag(out)[idx]
  }
  out
}

# ------------------------------------------------------------------------------
# Read main gravity dataset
# ------------------------------------------------------------------------------

df <- read_dta(file.path(work, "data_gravity_update.dta"))

# ------------------------------------------------------------------------------
# Predicted shares from bilateral model
# ------------------------------------------------------------------------------

df <- df %>%
  filter(host != source)

# Fixed effects: host and year
# In R, we use factors directly in lm() instead of generating dummy variables.

# OFC classification
df <- df %>%
  mutate(
    ofc_source = if_else(sifc_source == 1, 1, 0)
  ) %>%
  mutate(
    ofc_source = if_else(source %in% c(312, 9006, 1200, 1003, 815, 1100, 178, 137, 532, 576, 423), 1, ofc_source),
    ofc_source = if_else(source == 355, 1, ofc_source),
    ofc_source = if_else(source %in% c(372, 321, 328, 668, 135, 298, 351, 565), 0, ofc_source)
  )

ofc <- df %>%
  filter(ofc_source == 1) %>%
  distinct(source, ofc_source) %>%
  rename(
    host = source,
    ofc_host = ofc_source
  )

df <- df %>%
  left_join(ofc, by = "host") %>%
  mutate(
    ofc_host = if_else(ofc_host == 1, 1, 0),
    logeqasset = if_else(eqasset == 0, 0, logeqasset),
    logdebtasset = if_else(debtasset == 0, 0, logdebtasset)
  )

# Benchmark regressions
reg_eq_bench <- lm(
  logeqasset ~ logdist + gap_lon + comlang_off + col45 + industrial +
    loggap_gdp + loggap_gdppc + lat_source + landlocked_source +
    logpop_source + loggdppc_source + factor(year) + factor(host),
  data = df %>% filter(ofc_source == 0, ofc_host == 0)
)

reg_debt_bench <- lm(
  logdebtasset ~ logdist + gap_lon + comlang_off + col45 + industrial +
    loggap_gdp + loggap_gdppc + lat_source + landlocked_source +
    logpop_source + loggdppc_source + factor(year) + factor(host),
  data = df %>% filter(ofc_source == 0, ofc_host == 0)
)

# Augmented regressions with interaction between OFC source and host
reg_eq <- lm(
  logeqasset ~ logdist + comlang_off + col45 + industrial +
    loggap_gdp + loggap_gdppc + ofc_source + lat_source +
    landlocked_source + logpop_source + loggdppc_source +
    factor(year) + factor(host) + factor(host):ofc_source,
  data = df
)

reg_debt <- lm(
  logdebtasset ~ logdist + comlang_off + col45 + industrial +
    loggap_gdp + loggap_gdppc + ofc_source + lat_source +
    landlocked_source + logpop_source + loggdppc_source +
    factor(year) + factor(host) + factor(host):ofc_source,
  data = df
)

df <- df %>%
  mutate(
    logeqp = predict(reg_eq, newdata = df),
    logdebtp = predict(reg_debt, newdata = df),
    eqp = exp(logeqp) - 1,
    debtp = exp(logdebtp) - 1,
    eqp = if_else(eqp < 0, 0, eqp),
    debtp = if_else(debtp < 0, 0, debtp)
  )

# Comparison of predicted shares and true shares
df <- df %>%
  group_by(source, year) %>%
  mutate(
    toteqalloc = sum(eqasset[host != 983], na.rm = TRUE),
    totdebtalloc = sum(debtasset[host != 983], na.rm = TRUE),
    shareeqalloc = if_else(toteqalloc == 0, 0, eqasset / toteqalloc),
    sharedebtalloc = if_else(totdebtalloc == 0, 0, debtasset / totdebtalloc),
    toteqp = sum(eqp, na.rm = TRUE),
    totdebtp = sum(debtp, na.rm = TRUE),
    shareeqp = eqp / toteqp,
    sharedebtp = debtp / totdebtp
  ) %>%
  ungroup()

# ------------------------------------------------------------------------------
# Allocation of confidential + unallocated claims
# ------------------------------------------------------------------------------

df <- df %>%
  mutate(
    eqasset = coalesce(eqasset, 0),
    debtasset = coalesce(debtasset, 0)
  ) %>%
  group_by(source, year) %>%
  mutate(
    toteqasset = sum(eqasset, na.rm = TRUE),
    totdebtasset = sum(debtasset, na.rm = TRUE)
  ) %>%
  ungroup()

# Compare raw sums with aggregates from CPIS data
data_toteq_update <- read_dta(file.path(work, "data_toteq_update.dta"))
data_totdebt_update <- read_dta(file.path(work, "data_totdebt_update.dta"))

df <- df %>%
  left_join(data_toteq_update %>% select(source, year, sumeqasset = sumeqasset), by = c("source", "year")) %>%
  left_join(data_totdebt_update %>% select(source, year, sumdebtasset = sumdebtasset), by = c("source", "year")) %>%
  group_by(year) %>%
  mutate(
    help_total_eq = sum(sumeqasset, na.rm = TRUE),
    help_total_debt = sum(sumdebtasset, na.rm = TRUE)
  ) %>%
  ungroup()

df <- df %>%
  mutate(
    missingeq = sumeqasset - toteqasset,
    missingdebt = sumdebtasset - totdebtasset,
    eqasset = if_else(host == 983 & !is.na(missingeq), eqasset + missingeq, eqasset),
    debtasset = if_else(host == 983 & !is.na(missingdebt), debtasset + missingdebt, debtasset)
  ) %>%
  select(-sumeqasset, -sumdebtasset, -missingeq, -missingdebt)

# Confidential + unallocated
df <- df %>%
  mutate(
    confidentialeq = 0,
    ignore_jur = if_else(
      (host == 353 & year > 2009) |
        (host == 355 & year < 2010) |
        (host == 357 & year < 2010) |
        (host == 537 & year < 2002) |
        (host == 733 & year < 2011) |
        (host == 943 & year < 2006),
      1, NA_real_
    ),
    confidentialeq = if_else(
      (eqasset == 0 | host %in% c(1001, 9006, 1003)) &
        !is.na(toteqasset) & toteqasset != 0 &
        host != 135 & ignore_jur != 1,
      1, confidentialeq
    ),
    confidentialdebt = if_else(
      (debtasset == 0 | host %in% c(1001, 9006, 1003)) &
        !is.na(totdebtasset) & totdebtasset != 0 &
        host != 135 & ignore_jur != 1,
      1, 0
    ),
    eqpconf = eqp * confidentialeq,
    debtpconf = debtp * confidentialdebt
  ) %>%
  group_by(source, year) %>%
  mutate(
    toteqpconf = sum(eqpconf, na.rm = TRUE),
    totdebtpconf = sum(debtpconf, na.rm = TRUE),
    shareeqpconf = if_else(toteqpconf == 0, 0, eqpconf / toteqpconf),
    sharedebtpconf = if_else(totdebtpconf == 0, 0, debtpconf / totdebtpconf)
  ) %>%
  ungroup()

unalloc <- df %>%
  filter(host == 983) %>%
  transmute(
    source, year,
    totunalloceq = eqasset,
    totunallocdebt = debtasset
  )

df <- df %>%
  left_join(unalloc, by = c("source", "year")) %>%
  mutate(
    unalloceq = if_else(source != 9999, shareeqpconf * totunalloceq, 0),
    unallocdebt = if_else(source != 9999, sharedebtpconf * totunallocdebt, 0),
    augmeqasset = eqasset,
    augmeqasset = if_else(host != 983 & source != 9999, eqasset + unalloceq, augmeqasset),
    augmeqasset = if_else(host == 983, 0, augmeqasset),
    augmdebtasset = debtasset,
    augmdebtasset = if_else(host != 983 & source != 9999, debtasset + unallocdebt, augmdebtasset),
    augmdebtasset = if_else(host == 983, 0, augmdebtasset),
    augmeqasset = if_else(source == 9999, augmeqasset + shareeqalloc * totunalloceq, augmeqasset),
    augmdebtasset = if_else(source == 9999, augmdebtasset + sharedebtalloc * totunallocdebt, augmdebtasset)
  )

write_dta(df, file.path(work, "temp.dta"))
write_dta(df, file.path(work, "temp_30.dta"))

# ------------------------------------------------------------------------------
# Allocation of Cayman Islands non-bank sector
# ------------------------------------------------------------------------------

df <- read_dta(file.path(work, "temp_30.dta"))
Cayman_TIC_Dec <- read_dta(file.path(work, "Cayman_TIC_Dec.dta"))

df <- df %>%
  left_join(Cayman_TIC_Dec, by = c("year", "source", "host")) %>%
  filter(!(is.na(eq_KY_TIC) & is.na(debt_KY_TIC) & is.na(debtl_KY_TIC) & is.na(debts_KY_TIC) & year > 2021)) %>%
  mutate(
    augmeqasset = if_else(source == 377 & host == 111 & augmeqasset < eq_KY_TIC, eq_KY_TIC, augmeqasset),
    augmdebtasset = if_else(source == 377 & host == 111 & year < 2015, debt_KY_TIC, augmdebtasset)
  ) %>%
  group_by(year, source) %>%
  mutate(
    help_toteq_KY = sum(if_else(source == 377, augmeqasset, 0), na.rm = TRUE),
    share_US_KY = if_else(source == 377 & host == 111 & year > 2014, augmeqasset / help_toteq_KY, NA_real_),
    help_totdebt_KY = sum(if_else(source == 377, augmdebtasset, 0), na.rm = TRUE),
    share_debt_US_KY = if_else(source == 377 & host == 111 & year > 2014, augmdebtasset / help_totdebt_KY, NA_real_)
  ) %>%
  ungroup() %>%
  arrange(source, host, year)

# fill pre-2015 with next available value
for (i in 1:14) {
  df <- df %>%
    group_by(source, host) %>%
    arrange(year, .by_group = TRUE) %>%
    mutate(
      share_US_KY = if_else(is.na(share_US_KY) & year < 2015 & source == 377 & host == 111,
                            lead(share_US_KY), share_US_KY),
      share_debt_US_KY = if_else(is.na(share_debt_US_KY) & year < 2015 & source == 377 & host == 111,
                                 lead(share_debt_US_KY), share_debt_US_KY)
    ) %>%
    ungroup()
}

caymantot <- df %>%
  filter(host == 111, source == 377, year >= 2001, year <= 2014) %>%
  transmute(
    source, year,
    caymantoteq = augmeqasset / share_US_KY,
    caymantotdebt = augmdebtasset / share_debt_US_KY
  ) %>%
  filter(!is.na(caymantoteq), !is.na(caymantotdebt))

df <- df %>%
  left_join(caymantot, by = c("source", "year")) %>%
  group_by(year, source) %>%
  mutate(
    share_nonus_eq_old = sum(shareeqp[host != 111], na.rm = TRUE),
    share_nonus_eq_new = mean(share_US_KY, na.rm = TRUE),
    share_nonus_eq_new = 1 - share_nonus_eq_new,
    share_nonus_eq_new = if_else(host == 111, NA_real_, share_nonus_eq_new),
    rescale_eq = share_nonus_eq_new / share_nonus_eq_old,
    shareeq_KY = if_else(host != 111, shareeqp * rescale_eq, NA_real_),
    share_nonus_debt_old = sum(sharedebtp[host != 111], na.rm = TRUE),
    share_nonus_debt_new = mean(share_debt_US_KY, na.rm = TRUE),
    share_nonus_debt_new = 1 - share_nonus_debt_new,
    share_nonus_debt_new = if_else(host == 111, NA_real_, share_nonus_debt_new),
    rescale_debt = share_nonus_debt_new / share_nonus_debt_old,
    sharedebt_KY = if_else(host != 111, sharedebtp * rescale_debt, NA_real_)
  ) %>%
  ungroup() %>%
  mutate(
    augmeqasset = if_else(source == 377 & host != 111 & year < 2015, caymantoteq * shareeq_KY, augmeqasset),
    augmdebtasset = if_else(source == 377 & host != 111 & year < 2015, caymantotdebt * sharedebt_KY, augmdebtasset)
  ) %>%
  select(-matches("_KY_"), -help_toteq_KY, -help_totdebt_KY, -caymantoteq, -caymantotdebt)

write_dta(df, file.path(work, "temp.dta"))
df <- read_dta(file.path(work, "temp.dta"))

# ------------------------------------------------------------------------------
# Allocation of other CPIS countries
# ------------------------------------------------------------------------------

df <- df %>%
  mutate(
    help_share_eq = toteqasset / help_total_eq,
    help_share_debt = totdebtasset / help_total_debt
  ) %>%
  arrange(source, host, year)

fill_country_missing <- function(data, code, years_condition = NULL, forward_n = 0, backward_n = 0, replace_augm = TRUE) {
  data <- data %>%
    group_by(source, host) %>%
    arrange(year, .by_group = TRUE) %>%
    mutate(
      help_share_eq = if_else(source == code, fill_from_next(help_share_eq, forward_n), help_share_eq),
      help_share_debt = if_else(source == code, fill_from_next(help_share_debt, forward_n), help_share_debt)
    ) %>%
    ungroup()

  if (backward_n > 0) {
    data <- data %>%
      group_by(source, host) %>%
      arrange(year, .by_group = TRUE) %>%
      mutate(
        help_share_eq = if_else(source == code, fill_from_prev(help_share_eq, backward_n), help_share_eq),
        help_share_debt = if_else(source == code, fill_from_prev(help_share_debt, backward_n), help_share_debt)
      ) %>%
      ungroup()
  }

  cond <- rep(TRUE, nrow(data))
  if (!is.null(years_condition)) cond <- years_condition(data)

  data <- data %>%
    mutate(
      toteqasset = if_else(source == code & cond & toteqasset == 0, help_share_eq * help_total_eq, toteqasset),
      totdebtasset = if_else(source == code & cond & totdebtasset == 0, help_share_debt * help_total_debt, totdebtasset)
    )

  if (replace_augm) {
    data <- data %>%
      mutate(
        augmeqasset = if_else(source == code & cond & augmeqasset == 0, toteqasset * shareeqp, augmeqasset),
        augmdebtasset = if_else(source == code & cond & augmdebtasset == 0, totdebtasset * sharedebtp, augmdebtasset)
      )
  }

  data
}

df <- fill_country_missing(df, 419, function(d) d$year %in% c(2002, 2003, 2016), forward_n = 2)
df <- fill_country_missing(df, 316, function(d) d$year < 2003 | d$year > 2015, forward_n = 2, backward_n = 1)
df <- fill_country_missing(df, 823, function(d) d$toteqasset == 0, forward_n = 3)
df <- fill_country_missing(df, 534, function(d) d$toteqasset == 0 | d$totdebtasset == 0, forward_n = 3)
df <- fill_country_missing(df, 443, function(d) d$toteqasset == 0 | d$totdebtasset == 0, forward_n = 2)
df <- fill_country_missing(df, 941, function(d) d$toteqasset == 0 | d$totdebtasset == 0, forward_n = 5)
df <- fill_country_missing(df, 273, function(d) d$toteqasset == 0 | d$totdebtasset == 0, forward_n = 2)

# Cayman: fill total only, do not replace augm already estimated
df <- df %>%
  group_by(source, host) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    help_share_eq = if_else(source == 377, fill_from_next(help_share_eq, 1), help_share_eq),
    help_share_debt = if_else(source == 377, fill_from_next(help_share_debt, 1), help_share_debt)
  ) %>%
  ungroup() %>%
  mutate(
    toteqasset = if_else(source == 377 & toteqasset == 0, help_share_eq * help_total_eq, toteqasset),
    totdebtasset = if_else(source == 377 & totdebtasset == 0, help_share_debt * help_total_debt, totdebtasset)
  )

df <- fill_country_missing(df, 283, function(d) d$toteqasset == 0 | d$totdebtasset == 0, backward_n = 1)
df <- fill_country_missing(df, 299, function(d) d$toteqasset == 0 | d$totdebtasset == 0, backward_n = 1)
df <- fill_country_missing(df, 313, function(d) d$toteqasset == 0 | d$totdebtasset == 0, backward_n = 1)
df <- fill_country_missing(df, 564, function(d) d$toteqasset == 0 | d$totdebtasset == 0, forward_n = 1)
df <- fill_country_missing(df, 1012, function(d) d$toteqasset == 0 | d$totdebtasset == 0, backward_n = 1)

# ------------------------------------------------------------------------------
# Missing Netherlands SFIs
# ------------------------------------------------------------------------------

df <- df %>%
  mutate(
    toteqasset = if_else(source == 138 & year == 2001, toteqasset + 3286, toteqasset),
    toteqasset = if_else(source == 138 & year == 2002, toteqasset + 2601, toteqasset),
    totdebtasset = if_else(source == 138 & year == 2001, totdebtasset + 20984, totdebtasset),
    totdebtasset = if_else(source == 138 & year == 2002, totdebtasset + 16611, totdebtasset),
    augmeqasset = if_else(source == 138, toteqasset * shareeqalloc, augmeqasset),
    augmdebtasset = if_else(source == 138, totdebtasset * sharedebtalloc, augmdebtasset)
  )

write_dta(df, file.path(work, "temp.dta"))

# ------------------------------------------------------------------------------
# Allocation of CHINA
# ------------------------------------------------------------------------------

china <- read_dta(file.path(work, "TIC_China_Dec.dta")) %>%
  left_join(read_dta(file.path(work, "data_IMF_China.dta")), by = "year") %>%
  arrange(year) %>%
  mutate(
    growth_equity = eq_China_TIC / lead(eq_China_TIC),
    Equity_IMF = if_else(year < 2007, NA_real_, Equity_IMF)
  )

for (i in 1:6) {
  china <- china %>%
    mutate(Equity_IMF = if_else(is.na(Equity_IMF), lead(Equity_IMF) * growth_equity, Equity_IMF))
}

china <- china %>%
  mutate(
    growth_Debt = debt_China_TIC / lead(debt_China_TIC)
  )

for (i in 1:3) {
  china <- china %>%
    mutate(Debt_IMF = if_else(is.na(Debt_IMF), lead(Debt_IMF) * growth_Debt, Debt_IMF))
}

china <- china %>%
  mutate(
    share = case_when(
      year == 2009 ~ 0.87,
      year == 2010 ~ 0.89,
      year == 2011 ~ 0.91,
      year == 2012 ~ 0.93,
      year > 2012 ~ 0.95,
      TRUE ~ 0.85
    ),
    reserves_China = share * Reserves_IMF,
    equity_ratio_TIC = eq_China_TIC / total_China_TIC,
    totaleq_China_public = equity_ratio_TIC * reserves_China,
    totaldebt_China_public = (1 - equity_ratio_TIC) * reserves_China,
    totaleq_China = equity_ratio_TIC * (reserves_China + Equity_IMF + Debt_IMF),
    totaldebt_China = (1 - equity_ratio_TIC) * (share * Reserves_IMF + Equity_IMF + Debt_IMF)
  ) %>%
  select(year, source, host, eq_China_TIC, debt_China_TIC, share, equity_ratio_TIC,
         starts_with("totaleq_China"), starts_with("totaldebt_China"), starts_with("reserves"))

df <- df %>%
  left_join(china, by = c("year", "source")) %>%
  filter(!(is.na(totaleq_China) & source == 924 & year > 2021)) %>%
  mutate(
    toteqasset = if_else(source == 924, totaleq_China, toteqasset),
    totdebtasset = if_else(source == 924, totaldebt_China, totdebtasset),
    useqassetchina = if_else(source == 924, eq_China_TIC, NA_real_),
    usdebtassetchina = if_else(source == 924, debt_China_TIC, NA_real_),
    nonuseqassetchina = if_else(source == 924, toteqasset - useqassetchina, NA_real_),
    nonusdebtassetchina = if_else(source == 924, totdebtasset - usdebtassetchina, NA_real_)
  )

sefernonus <- df %>%
  filter(source == 9999, host != 111, host != 924) %>%
  group_by(year) %>%
  mutate(
    toteqnonus = sum(augmeqasset, na.rm = TRUE),
    totdebtnonus = sum(augmdebtasset, na.rm = TRUE),
    shareeqsefernonus = augmeqasset / toteqnonus,
    sharedebtsefernonus = augmdebtasset / totdebtnonus
  ) %>%
  ungroup() %>%
  select(year, host, shareeqsefernonus, sharedebtsefernonus)

df <- df %>%
  left_join(sefernonus, by = c("host", "year")) %>%
  mutate(
    augmeqasset = if_else(source == 924 & host == 111, useqassetchina, augmeqasset),
    augmdebtasset = if_else(source == 924 & host == 111, usdebtassetchina, augmdebtasset),
    shareeqalloc = if_else(source == 924 & host == 111, augmeqasset / toteqasset, shareeqalloc),
    sharedebtalloc = if_else(source == 924 & host == 111, augmdebtasset / totdebtasset, sharedebtalloc),
    augmeqasset = if_else(source == 924 & host != 111, shareeqsefernonus * nonuseqassetchina, augmeqasset),
    augmdebtasset = if_else(source == 924 & host != 111, sharedebtsefernonus * nonusdebtassetchina, augmdebtasset)
  ) %>%
  select(-nonusdebtassetchina, -nonuseqassetchina, -usdebtassetchina, -useqassetchina,
         -shareeqsefernonus, -sharedebtsefernonus, -matches("_TIC$"), -share)

# ------------------------------------------------------------------------------
# Allocation of Middle East oil exporters
# ------------------------------------------------------------------------------

middle_east <- read_dta(file.path(work, "TIC_update_middleast.dta")) %>%
  bind_rows(read_dta(file.path(work, "Bertaut_Judson_middleeast_Dec.dta"))) %>%
  select(-any_of(c("flag_TIC", "country_code", "month"))) %>%
  mutate(
    source = case_when(
      country == " Middle Eastern Oil Exporters" ~ 4566,
      country == "Bahrain" & year > 2010 ~ 419,
      country == "Iran" & year > 2010 ~ 429,
      country == "Iraq" & year > 2010 ~ 433,
      country == "Kuwait" & year > 2010 ~ 443,
      country == "Oman" & year > 2010 ~ 449,
      country == "Qatar" & year > 2010 ~ 453,
      country == "United Arab Emirates" & year > 2010 ~ 466,
      country == "Saudi Arabia" & year > 2010 ~ 456,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(source)) %>%
  group_by(year) %>%
  summarise(
    Total = sum(Total, na.rm = TRUE),
    Equity = sum(Equity, na.rm = TRUE),
    Debtl = sum(Debtl, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(read_dta(file.path(work, "adjust_period.dta")), by = "year") %>%
  filter(!(is.na(adj_eq) & year > 2021)) %>%
  mutate(
    Equity = if_else(year > 2010, Equity * adj_eq, Equity)
  ) %>%
  left_join(read_dta(file.path(work, "shortterm_ratio_FOI.dta")), by = "year") %>%
  filter(!(is.na(short_long_ratio) & year > 2021)) %>%
  rename(ratio_shortlong = short_long_ratio) %>%
  mutate(
    Debt_est = Debtl + (ratio_shortlong * (Debtl + Equity)),
    Total_est = Equity + Debt_est,
    Total_TIC = Total,
    Equity_TIC = Equity,
    Debtl_TIC = Debtl,
    Debt_est_TIC = Debt_est,
    Total_TIC_ME = Total_est,
    toteqasset_ME = Equity_TIC / case_when(
      year == 2001 ~ 0.70,
      TRUE ~ NA_real_
    )
  )

# construct US share
middle_east <- middle_east %>%
  arrange(year) %>%
  mutate(
    USshare = NA_real_
  )
middle_east$USshare[middle_east$year == 2001] <- 0.7
for (i in seq_len(nrow(middle_east))) {
  if (is.na(middle_east$USshare[i]) && middle_east$year[i] < 2013 && i > 1) {
    middle_east$USshare[i] <- round(middle_east$USshare[i - 1] - 0.02, 2)
  }
  if (is.na(middle_east$USshare[i]) && i > 1) {
    middle_east$USshare[i] <- middle_east$USshare[i - 1]
  }
}

middle_east <- middle_east %>%
  mutate(
    toteqasset_ME = Equity_TIC / USshare,
    totasset_ME = Total_TIC_ME / USshare,
    totdebtasset_ME = totasset_ME - toteqasset_ME
  ) %>%
  select(year, totasset_ME, toteqasset_ME, totdebtasset_ME, USshare)

cpis_reported_ME_assets <- df %>%
  filter(source %in% c(419, 443, 456)) %>%
  group_by(source, year) %>%
  mutate(help = row_number()) %>%
  filter(help == 1) %>%
  ungroup() %>%
  group_by(year) %>%
  summarise(
    toteqasset_reported = sum(toteqasset, na.rm = TRUE),
    totdebtasset_reported = sum(totdebtasset, na.rm = TRUE),
    .groups = "drop"
  )

middle_east2 <- middle_east %>%
  left_join(cpis_reported_ME_assets, by = "year") %>%
  mutate(
    toteqasset_ME = toteqasset_ME - toteqasset_reported,
    totdebtasset_ME = totdebtasset_ME - totdebtasset_reported,
    source = 453
  )

df <- df %>%
  left_join(middle_east2 %>% select(source, year, toteqasset_ME, totdebtasset_ME, USshare), by = c("source", "year")) %>%
  mutate(
    toteqasset = if_else(source == 453, toteqasset_ME, toteqasset),
    totdebtasset = if_else(source == 453, totdebtasset_ME, totdebtasset),
    usoileqasset = if_else(source == 453, USshare * toteqasset, NA_real_),
    usoildebtasset = if_else(source == 453, USshare * totdebtasset, NA_real_),
    nonusoileqasset = if_else(source == 453, toteqasset - usoileqasset, NA_real_),
    nonusoildebtasset = if_else(source == 453, totdebtasset - usoildebtasset, NA_real_)
  )

shareusp <- df %>%
  filter(host == 111) %>%
  select(source, year, shareeqp, sharedebtp) %>%
  rename(
    shareequsp = shareeqp,
    sharedebtusp = sharedebtp
  )

df <- df %>%
  left_join(shareusp, by = c("source", "year")) %>%
  mutate(
    shareeqpnonus = shareeqp / (1 - shareequsp),
    sharedebtpnonus = sharedebtp / (1 - sharedebtusp),
    augmeqasset = if_else(source == 453 & host != 111, shareeqpnonus * nonusoileqasset, augmeqasset),
    augmdebtasset = if_else(source == 453 & host != 111, sharedebtpnonus * nonusoildebtasset, augmdebtasset),
    augmeqasset = if_else(source == 453 & host == 111, USshare * toteqasset, augmeqasset),
    augmdebtasset = if_else(source == 453 & host == 111, USshare * totdebtasset, augmdebtasset)
  ) %>%
  select(-USshare, -usoileqasset, -usoildebtasset, -nonusoileqasset, -nonusoildebtasset)

# ------------------------------------------------------------------------------
# Allocation of other non-CPIS
# ------------------------------------------------------------------------------

# EWN data with ifs mapping
data_ewn_update_ifs <- read_dta(file.path(work, "data_ewn_update.dta")) %>%
  rename(ifscode = source) %>%
  left_join(read_dta(file.path(work, "iso_ifscode.dta")), by = "ifscode") %>%
  filter(!is.na(our_code)) %>%
  mutate(
    our_code = if_else(ifscode == 355, 355, our_code)
  ) %>%
  rename(source = our_code)

write_dta(data_ewn_update_ifs, file.path(work, "data_ewn_update_ifs.dta"))

df <- df %>%
  left_join(data_ewn_update_ifs, by = c("source", "year")) %>%
  select(-lequity, -lportif_debt) %>%
  rename(ewn22_source = ewn22) %>%
  mutate(
    aportif_debt = if_else(is.na(aportif_debt) & !is.na(adebt), 0.2 * adebt, aportif_debt),
    toteqasset = if_else(cpis != 1 & !source %in% c(924, 449, 453, 456, 466, 429, 433), aequity, toteqasset),
    totdebtasset = if_else(cpis != 1 & !source %in% c(924, 449, 453, 456, 466, 429, 433), aportif_debt, totdebtasset)
  )

derived <- df %>%
  group_by(host, year) %>%
  summarise(
    augmeqliab = sum(augmeqasset, na.rm = TRUE),
    augmdebtliab = sum(augmdebtasset, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(source = host)

df <- df %>%
  left_join(derived, by = c("source", "year")) %>%
  mutate(
    toteqasset = if_else(cpis != 1 & !source %in% c(924, 449, 453, 456, 466, 429, 433, 9998) & is.na(aequity), augmeqliab, toteqasset),
    totdebtasset = if_else(cpis != 1 & !source %in% c(924, 449, 453, 456, 466, 429, 433, 9998) & is.na(aportif_debt), augmdebtliab, totdebtasset),
    augmeqasset = if_else(cpis != 1 & !source %in% c(924, 449, 453, 456, 466, 429, 433), shareeqp * toteqasset, augmeqasset),
    augmdebtasset = if_else(cpis != 1 & !source %in% c(924, 449, 453, 456, 466, 429, 433), sharedebtp * totdebtasset, augmdebtasset)
  )

# Reserves of non-CPIS countries
missingreserve <- read_dta(file.path(work, "data_foreignexchange_update.dta")) %>%
  filter(!source %in% c(163, 309, 758, 759, 967))

df <- df %>%
  left_join(missingreserve, by = c("source", "year")) %>%
  mutate(
    debtreserveIFS = 0.74 * reserveIFS,
    eqreserveIFS = 0.01 * reserveIFS
  )

missingreserve2 <- df %>%
  group_by(source, year) %>%
  slice(1) %>%
  ungroup() %>%
  filter(!cpis %in% 1, !source %in% c(924, 449, 453, 456, 466, 433, 429)) %>%
  group_by(year) %>%
  summarise(
    missingeqres = sum(eqreserveIFS, na.rm = TRUE),
    missingdebtres = sum(debtreserveIFS, na.rm = TRUE),
    .groups = "drop"
  )

df <- df %>%
  left_join(missingreserve2, by = "year")

sefer <- df %>%
  filter(source == 9999) %>%
  group_by(year) %>%
  mutate(
    toteq = sum(augmeqasset, na.rm = TRUE),
    totdebt = sum(augmdebtasset, na.rm = TRUE),
    shareeqsefer = augmeqasset / toteq,
    sharedebtsefer = augmdebtasset / totdebt
  ) %>%
  ungroup() %>%
  select(year, host, shareeqsefer, sharedebtsefer)

df <- df %>%
  left_join(sefer, by = c("host", "year")) %>%
  mutate(
    othereqreserve = shareeqsefer * missingeqres,
    otherdebtreserve = sharedebtsefer * missingdebtres
  )

otherreserve <- df %>%
  select(host, year, othereqreserve, otherdebtreserve) %>%
  distinct(host, year, .keep_all = TRUE) %>%
  filter(!is.na(host)) %>%
  mutate(
    source = 9994,
    augmeqasset = othereqreserve,
    augmdebtasset = otherdebtreserve
  ) %>%
  select(source, host, year, augmeqasset, augmdebtasset)

df <- bind_rows(df, otherreserve)

# ------------------------------------------------------------------------------
# EWNII and derived liabilities
# ------------------------------------------------------------------------------

# A - host liabilities
ewn22 <- read_dta(file.path(work, "data_ewn_update_ifs.dta")) %>%
  rename(host = source) %>%
  select(year, host, lequity, lportif_debt, ewn22)

df <- df %>%
  left_join(ewn22, by = c("host", "year")) %>%
  rename(
    ewn22_host = ewn22,
    lequity_host = lequity,
    lportif_debt_host = lportif_debt
  ) %>%
  mutate(
    eqliab_host = lequity_host,
    debtliab_host = lportif_debt_host,
    lequity_SFI = case_when(
      host == 138 & year == 2001 ~ 6635,
      host == 138 & year == 2002 ~ 8177,
      TRUE ~ NA_real_
    ),
    eqliab_host = if_else(host == 138 & year < 2003, lequity_host + lequity_SFI, eqliab_host),
    ldebt_SFI = case_when(
      host == 138 & year == 2001 ~ 258296,
      host == 138 & year == 2002 ~ 318308,
      TRUE ~ NA_real_
    ),
    debtliab_host = if_else(host == 138 & year < 2003, lportif_debt_host + ldebt_SFI, debtliab_host)
  )

KY_eqliab <- df %>%
  filter(source == 377) %>%
  group_by(year) %>%
  summarise(
    augmeqasset = sum(augmeqasset, na.rm = TRUE),
    augmdebtasset = sum(augmdebtasset, na.rm = TRUE),
    eqasset = sum(eqasset, na.rm = TRUE),
    debtasset = sum(debtasset, na.rm = TRUE),
    source = first(source),
    .groups = "drop"
  ) %>%
  mutate(
    eqliab_KY = augmeqasset + augmdebtasset,
    totliab_banks = if_else(year < 2015, eqasset + debtasset, NA_real_),
    host = source
  ) %>%
  select(year, host, eqliab_KY, totliab_banks) %>%
  left_join(read_dta(file.path(work, "KY_banks.dta")), by = c("year", "host")) %>%
  mutate(
    totliab_banks = if_else(year > 2014, KY_assets, totliab_banks)
  ) %>%
  left_join(read_dta(file.path(work, "KY_liab_nfc.dta")), by = "year") %>%
  filter(!(is.na(eqliab_nfc) & year > 2021)) %>%
  mutate(
    eqliab_nfc = eqliab_nfc / 1e6,
    eqliab_KY = eqliab_KY - totliab_banks + eqliab_nfc
  ) %>%
  select(year, host, eqliab_KY)

df <- df %>%
  left_join(KY_eqliab, by = c("year", "host")) %>%
  mutate(
    eqliab_host = if_else(host == 377, eqliab_KY, eqliab_host)
  ) %>%
  group_by(host, year) %>%
  mutate(
    rawderivedeq = sum(eqasset, na.rm = TRUE),
    rawderiveddebt = sum(debtasset, na.rm = TRUE),
    missingliabeq = if_else(rawderivedeq > eqliab_host, rawderivedeq - eqliab_host, 0),
    missingliabdebt = if_else(rawderiveddebt > debtliab_host, rawderiveddebt - debtliab_host, 0),
    eqliab_host = eqliab_host + missingliabeq,
    debtliab_host = debtliab_host + missingliabdebt
  ) %>%
  ungroup()

# International organizations
BIS_total_debt_IO <- read_dta(file.path(work, "BIS_total_debt_IO.dta"))

df <- df %>%
  left_join(BIS_total_debt_IO, by = c("year", "host")) %>%
  mutate(
    eqliab_host = if_else(host == 9998, rawderivedeq, eqliab_host),
    debtliab_host = if_else(host == 9998, total_debt_BIS, debtliab_host)
  ) %>%
  select(-total_debt_BIS)

# Derived liabilities for non-EWN22 nations
df <- df %>%
  group_by(host, year) %>%
  mutate(
    derivedeqliab_host = sum(augmeqasset, na.rm = TRUE),
    deriveddebtliab_host = sum(augmdebtasset, na.rm = TRUE),
    eqliab_host = if_else(is.na(eqliab_host) & !host %in% c(983, 9998, 9999), derivedeqliab_host, eqliab_host),
    debtliab_host = if_else(is.na(debtliab_host) & !host %in% c(983, 9998, 9999), deriveddebtliab_host, debtliab_host)
  ) %>%
  ungroup()

# B - liability data for source countries
liab <- df %>%
  select(host, year, lequity_host, lportif_debt_host, derivedeqliab_host,
         deriveddebtliab_host, eqliab_host, debtliab_host) %>%
  rename(
    source = host,
    lequity_source = lequity_host,
    lportif_debt_source = lportif_debt_host,
    derivedeqliab_source = derivedeqliab_host,
    deriveddebtliab_source = deriveddebtliab_host,
    eqliab_source = eqliab_host,
    debtliab_source = debtliab_host
  ) %>%
  group_by(year, source) %>%
  summarise(
    lequity_source = first(lequity_source),
    lportif_debt_source = first(lportif_debt_source),
    derivedeqliab_source = first(derivedeqliab_source),
    deriveddebtliab_source = first(deriveddebtliab_source),
    eqliab_source = first(eqliab_source),
    debtliab_source = first(debtliab_source),
    .groups = "drop"
  )

df <- df %>%
  left_join(liab, by = c("source", "year")) %>%
  mutate(
    gapeq_host = eqliab_host - derivedeqliab_host,
    gapdebt_host = debtliab_host - deriveddebtliab_host,
    gapeq_source = eqliab_source - derivedeqliab_source,
    gapdebt_source = debtliab_source - deriveddebtliab_source
  )

write_dta(df, file.path(work, "data_full_matrices.dta"))

# Host-gap summary
gap_host <- df %>%
  group_by(host, year) %>%
  summarise(
    hostname = first(hostname),
    eqliab_host = first(eqliab_host),
    derivedeqliab_host = first(derivedeqliab_host),
    gapeq_host = first(gapeq_host),
    debtliab_host = first(debtliab_host),
    deriveddebtliab_host = first(deriveddebtliab_host),
    gapdebt_host = first(gapdebt_host),
    .groups = "drop"
  )

# Source-gap summary
gap_source_update <- read_dta(file.path(work, "data_full_matrices.dta")) %>%
  group_by(source, year) %>%
  summarise(
    sourcename = first(sourcename),
    eqliab_source = first(eqliab_source),
    derivedeqliab_source = first(derivedeqliab_source),
    gapeq_source = first(gapeq_source),
    debtliab_source = first(debtliab_source),
    deriveddebtliab_source = first(deriveddebtliab_source),
    gapdebt_source = first(gapdebt_source),
    .groups = "drop"
  )

write_dta(gap_source_update, file.path(work, "gap_source_update.dta"))
