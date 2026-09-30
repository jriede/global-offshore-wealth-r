# ==============================================================================
# REPL: Global Offshore Wealth Using R, 2001-2021
# jlenke, 2026
# 7c – Sensitivity of pre-2015 Cayman portfolio equity and debt assets
# Run after 0a-setup.R and the original 2_do_full_matrices pipeline.
# Uses the saved pre-Cayman intermediate temp_30.dta; does NOT overwrite original data.
# Values retain the units of the original Stata matrices (typically USD million).

library(dplyr)
library(tidyr)
library(readr)
library(haven)
library(ggplot2)
library(knitr)

out <- file.path(work, "07-sensitivity-analysis", "cayman")
fig_out <- file.path(fig, "07-sensitivity-analysis", "cayman")

#sensitivity_dir <- if (exists("sensitivity_dir")) sensitivity_dir else
#  file.path(work, "07-sensitivity-analysis")
#dir.create(sensitivity_dir, recursive = TRUE, showWarnings = FALSE)
#dir.create(fig, recursive = TRUE, showWarnings = FALSE)

temp_30 <- read_work_data("df1")
Cayman_TIC_Dec <- read_work_data("Cayman_TIC_Dec")
#matrix_path <- file.path(work2, "temp_30.dta")
#tic_path <- file.path(work2, "Cayman_TIC_Dec.dta")

base <- temp_30 %>%
  mutate(across(c(year, source, host, augmeqasset, augmdebtasset,
                  eqasset, debtasset, shareeqp, sharedebtp), as.numeric))
tic <- Cayman_TIC_Dec %>%
  select(year, source, host, eq_KY_TIC, debt_KY_TIC) %>%
  mutate(across(everything(), as.numeric))
stopifnot(!anyDuplicated(tic[c("year", "source", "host")]))

# Mirror Stata's immediate TIC replacements at Cayman -> US (source 377, host 111).
ky <- base %>% filter(source == 377, between(year, 2001, 2021)) %>%
  left_join(tic, by = c("year", "source", "host")) %>%
  mutate(
    equity_pre = if_else(host == 111 & !is.na(eq_KY_TIC) &
                           (is.na(augmeqasset) | augmeqasset < eq_KY_TIC),
                         eq_KY_TIC, augmeqasset),
    debt_pre = if_else(host == 111 & year < 2015 & !is.na(debt_KY_TIC),
                       debt_KY_TIC, augmdebtasset)
  )

# 2015 benchmark shares: post-TIC US amount / sum of post-TIC Cayman assets.
bench <- ky %>% filter(year == 2015) %>% summarise(
  us_eq = sum(equity_pre[host == 111], na.rm = TRUE),
  us_debt = sum(debt_pre[host == 111], na.rm = TRUE),
  total_eq = sum(equity_pre, na.rm = TRUE),
  total_debt = sum(debt_pre, na.rm = TRUE)
) %>% mutate(eq_share = us_eq / total_eq, debt_share = us_debt / total_debt)
if (nrow(bench) != 1 || anyNA(bench) ||
    any(unlist(bench[c("eq_share", "debt_share")]) <= 0) ||
    any(unlist(bench[c("eq_share", "debt_share")]) >= 1)) {
  stop("Cannot calculate valid 2015 benchmark shares; inspect TIC and temp_30.")
}
print(bench)
write_csv(bench, file.path(out, "7c-cayman_2015_shares.csv"))

# Scenarios: alternative US shares are explicit hypotheses, not observed pre-2015 shares.
scenarios <- tibble(
  scenario = c("Baseline", "Lower US shares", "Higher US shares", "Rising equity US share"),
  equity_share_2001 = c(bench$eq_share, 0.40, 0.70, 0.40),
  equity_share_2014 = c(bench$eq_share, 0.40, 0.70, NA_real_),
  debt_share = c(bench$debt_share, 0.60, 0.70, bench$debt_share)
)
# In the rising scenario, equity share moves from 40% in 2001 to the 2015 benchmark.
shares <- crossing(year = 2001:2014, scenarios) %>% mutate(
  equity_share = if_else(scenario == "Rising equity US share",
    0.40 + (year - 2001) / (2015 - 2001) * (bench$eq_share - 0.40),
    equity_share_2001)
) %>% select(year, scenario, equity_share, debt_share)
stopifnot(all(shares$equity_share > 0 & shares$equity_share < 1),
          all(shares$debt_share > 0 & shares$debt_share < 1))

# Exactly the Stata reweighting: keep TIC US holdings fixed; distribute the residual
# over non-US destinations in proportion to gravity-predicted shares.
pre <- ky %>% filter(year < 2015) %>% group_by(year) %>% mutate(
  us_eq = sum(equity_pre[host == 111], na.rm = TRUE),
  us_debt = sum(debt_pre[host == 111], na.rm = TRUE),
  nonus_eq_weight = sum(shareeqp[host != 111], na.rm = TRUE),
  nonus_debt_weight = sum(sharedebtp[host != 111], na.rm = TRUE),
  bank_assets = sum(eqasset, na.rm = TRUE) + sum(debtasset, na.rm = TRUE)
) %>% ungroup()
if (any(pre %>% distinct(year, nonus_eq_weight, nonus_debt_weight) %>%
        transmute(bad = nonus_eq_weight <= 0 | nonus_debt_weight <= 0) %>% pull(bad))) {
  stop("Zero or invalid non-US gravity weight in pre-2015 Cayman matrix.")
}
if (anyNA(pre %>% filter(host == 111) %>% select(us_eq, us_debt))) {
  stop("Missing Cayman -> US TIC holdings.")
}



bilateral <- pre %>%
  inner_join(
    shares,
    by = "year",
    relationship = "many-to-many"
  ) %>%
  mutate(
    total_equity = us_eq / equity_share,
    total_debt   = us_debt / debt_share,
    
    equity_scenario = if_else(
      host == 111,
      us_eq,
      (total_equity - us_eq) *
        coalesce(shareeqp, 0) / nonus_eq_weight
    ),
    
    debt_scenario = if_else(
      host == 111,
      us_debt,
      (total_debt - us_debt) *
        coalesce(sharedebtp, 0) / nonus_debt_weight
    )
  )

# ---test
bilateral %>%
  filter(scenario %in% c("Baseline", "Rising equity US share")) %>%
  select(year, host, scenario, debt_scenario) %>%
  pivot_wider(
    names_from = scenario,
    values_from = debt_scenario
  ) %>%
  mutate(
    difference = `Rising equity US share` - Baseline
  ) %>%
  summarise(
    max_abs_difference = max(abs(difference), na.rm = TRUE),
    n_different = sum(abs(difference) > 1e-8, na.rm = TRUE)
  )


debt_check <- bilateral %>%
  group_by(year, scenario) %>%
  summarise(
    total_debt = sum(debt_scenario, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(scenario %in% c("Baseline", "Rising equity US share")) %>%
  pivot_wider(
    names_from = scenario,
    values_from = total_debt
  ) %>%
  mutate(
    difference = `Rising equity US share` - Baseline
  )

print(debt_check, n = Inf)

#---


stopifnot(
  nrow(shares) == 14 * 4,
  nrow(bilateral) == 4 * nrow(pre),
  !anyDuplicated(shares[c("year", "scenario")]),
  !anyDuplicated(bilateral[c("year", "scenario", "host")])
)


bilateral %>%
  filter(is.na(equity_scenario) | is.na(debt_scenario)) %>%
  summarise(
    n = n(),
    years = paste(sort(unique(year)), collapse = ", "),
    missing_shareeqp = sum(is.na(shareeqp)),
    missing_sharedebtp = sum(is.na(sharedebtp)),
    missing_us_eq = sum(is.na(us_eq)),
    missing_us_debt = sum(is.na(us_debt)),
    missing_nonus_eq_weight = sum(is.na(nonus_eq_weight)),
    missing_nonus_debt_weight = sum(is.na(nonus_debt_weight))
  )


if (anyNA(bilateral$equity_scenario) || anyNA(bilateral$debt_scenario)) {
  stop("Missing bilateral scenario values. Inspect gravity weights / TIC data.")
}

annual <- bilateral %>% group_by(year, scenario) %>% summarise(
  equity_assets = sum(equity_scenario),
  debt_assets = sum(debt_scenario),
  portfolio_assets = equity_assets + debt_assets,
  bank_assets = first(bank_assets),
  .groups = "drop"
) %>% mutate(
  # For pre-2015, FGZ: Cayman equity liabilities = portfolio assets
  # minus Cayman bank/insurance portfolio assets plus NFC equity liabilities.
  # Bank and NFC components are held fixed across scenarios.
  hedge_fund_equity_liabilities = portfolio_assets - bank_assets
)

baseline <- annual %>% filter(scenario == "Baseline") %>%
  select(year, baseline_equity = equity_assets,
         baseline_debt = debt_assets,
         baseline_portfolio = portfolio_assets,
         baseline_hedge_liabilities = hedge_fund_equity_liabilities)
annual <- annual %>% left_join(baseline, by = "year") %>% mutate(
  delta_equity_assets = equity_assets - baseline_equity,
  delta_debt_assets = debt_assets - baseline_debt,
  delta_portfolio_assets = portfolio_assets - baseline_portfolio,
  delta_equity_liabilities = hedge_fund_equity_liabilities - baseline_hedge_liabilities,
  # Other balance-sheet items unchanged; Cayman portfolio asset changes are
  # exactly offset by the linked Cayman equity-liability correction.
  delta_global_asset_liability_gap = delta_portfolio_assets - delta_equity_liabilities
)

validation <- annual %>% transmute(year, scenario,
  gap_change = delta_global_asset_liability_gap,
  cancellation_error = delta_portfolio_assets - delta_equity_liabilities)
stopifnot(all(abs(validation$cancellation_error) < 1e-6))

# Independent baseline check against final original bilateral matrix, if available.
final_path <- file.path(work, "data_full_matrices.dta")
if (file.exists(final_path)) {
  final <- read_dta(final_path)
  needed <- c("year", "source", "host", "augmeqasset", "augmdebtasset")
  if (all(needed %in% names(final))) {
    final_check <- final %>% filter(source == 377, year < 2015) %>%
      group_by(year) %>% summarise(
        original_equity = sum(as.numeric(augmeqasset), na.rm = TRUE),
        original_debt = sum(as.numeric(augmdebtasset), na.rm = TRUE), .groups = "drop")
    reconciliation <- baseline %>% left_join(final_check, by = "year") %>% mutate(
      equity_difference = baseline_equity - original_equity,
      debt_difference = baseline_debt - original_debt)
    write_csv(reconciliation, file.path(out,
                                      "7c-cayman_baseline_reconciliation.csv"))
    print(reconciliation %>% select(year, equity_difference, debt_difference), n = Inf)
    if (anyNA(reconciliation[c("equity_difference", "debt_difference")]) ||
        any(abs(as.matrix(reconciliation[c("equity_difference", "debt_difference")])) > 1e-5)) {
      warning("Baseline does not reproduce final original Cayman assets. Inspect reconciliation before interpreting scenarios.")
    }
  } else {
    message("Final matrix lacks required bilateral variables; independent baseline comparison skipped.")
  }
} else {
  message("data_full_matrices.dta unavailable; independent baseline comparison skipped.")
}

write_csv(shares, file.path(out, "7c-cayman_assumptions.csv"))
write_csv(annual, file.path(out, "7c-cayman_annual.csv"))
write_csv(validation, file.path(out, "7c-cayman_validation.csv"))
saveRDS(bilateral, file.path(out, "7c-cayman_bilateral.rds"))

plot_data <- annual %>%
  filter(scenario != "Baseline") %>%
  select(
    year, scenario,
    delta_equity_assets,
    delta_debt_assets
  ) %>%
  pivot_longer(
    cols = starts_with("delta_"),
    names_to = "asset_type",
    values_to = "delta"
  )

p <- ggplot(
  plot_data,
  aes(x = year, y = delta / 1000, linetype = scenario)
) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  geom_line(linewidth = 0.8) +
  facet_wrap(
    ~ asset_type,
    scales = "free_y",
    labeller = as_labeller(c(
      delta_debt_assets = "Debt securities",
      delta_equity_assets = "Equity securities"
    ))
  ) +
  labs(
    title = "Sensitivity of pre-2015 Cayman portfolio assets",
    x = NULL,
    y = "Change from baseline (USD billions)",
    linetype = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(
  file.path(fig_out, "7c-cayman_pre2015.pdf"),
  plot = p,
  width = 9,
  height = 5.5
)

ggsave(
  file.path(fig_out, "7c-cayman_pre2015.png"),
  plot = p,
  width = 9,
  height = 5.5,
  dpi = 300
)


# --- generating latex tables (Appendix A2)

# ---- Generate LaTeX tables for Appendix A2

# 1. Equity securities: deviations from baseline (USD billions)

appendix_cayman_equity <- annual %>%
  select(year, scenario, delta_equity_assets) %>%
  mutate(delta_equity_assets = delta_equity_assets / 1000) %>%
  pivot_wider(
    names_from = scenario,
    values_from = delta_equity_assets
  ) %>%
  transmute(
    Year = as.integer(year),
    Baseline,
    `Lower US shares`,
    `Higher US shares`,
    `Rising equity US share`
  )

latex_equity <- knitr::kable(
  appendix_cayman_equity,
  format = "latex",
  booktabs = TRUE,
  digits = 2,
  align = c("c", rep("r", 4)),
  caption = paste(
    "Sensitivity of Cayman Islands portfolio equity assets",
    "to alternative pre-2015 U.S. portfolio-share assumptions.",
    "Changes relative to the baseline, in USD billions."
  ),
  label = "tab:sensitivity_cayman_equity",
  escape = FALSE
)

writeLines(
  latex_equity,
  file.path(tables, "appendix_a2_cayman_equity.tex")
)


# 2. Debt securities: deviations from baseline (USD billions)

appendix_cayman_debt <- annual %>%
  select(year, scenario, delta_debt_assets) %>%
  mutate(delta_debt_assets = delta_debt_assets / 1000) %>%
  pivot_wider(
    names_from = scenario,
    values_from = delta_debt_assets
  ) %>%
  transmute(
    Year = as.integer(year),
    Baseline,
    `Lower US shares`,
    `Higher US shares`,
    `Rising equity US share`
  )

latex_debt <- knitr::kable(
  appendix_cayman_debt,
  format = "latex",
  booktabs = TRUE,
  digits = 2,
  align = c("c", rep("r", 4)),
  caption = paste(
    "Sensitivity of Cayman Islands portfolio debt assets",
    "to alternative pre-2015 U.S. portfolio-share assumptions.",
    "Changes relative to the baseline, in USD billions."
  ),
  label = "tab:sensitivity_cayman_debt",
  escape = FALSE
)

writeLines(
  latex_debt,
  file.path(tables, "appendix_a2_cayman_debt.tex")
)

# ---


message("7c complete. Inspect baseline reconciliation and validation before using results.")

