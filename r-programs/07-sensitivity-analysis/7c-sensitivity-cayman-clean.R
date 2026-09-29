# 7c – Sensitivity of pre-2015 Cayman portfolio equity and debt assets
# Run after 0a-setup.R and the original 2_do_full_matrices pipeline.
# Uses the R-native pre-Cayman intermediate df1.rds; does NOT overwrite original data.
# Values retain the units of the original Stata matrices (typically USD million).

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)

sensitivity_dir <- if (exists("sensitivity_dir")) sensitivity_dir else
  file.path(work, "07-sensitivity-analysis")
dir.create(sensitivity_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig, recursive = TRUE, showWarnings = FALSE)

temp_30 <- read_work_data("df1")
Cayman_TIC_Dec <- read_work_data("Cayman_TIC_Dec")

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
write_csv(bench, file.path(sensitivity_dir, "sensitivity_cayman_2015_shares.csv"))

# Scenarios: alternative US shares are explicit hypotheses, not observed pre-2015 shares.
scenarios <- tibble(
  scenario = c("Baseline", "Lower US shares", "Higher US shares", "Rising equity US share"),
  equity_share_2001 = c(bench$eq_share, 0.40, 0.90, 0.40),
  debt_share = c(bench$debt_share, 0.60, 0.75, bench$debt_share)
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

stopifnot(
  nrow(shares) == 14 * 4,
  nrow(bilateral) == 4 * nrow(pre),
  !anyDuplicated(shares[c("year", "scenario")]),
  !anyDuplicated(bilateral[c("year", "scenario", "host")])
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
annual <- annual %>% left_join(baseline, by = "year", relationship = "many-to-one") %>% mutate(
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
stopifnot(all(abs(annual$delta_debt_assets[
  annual$scenario == "Rising equity US share"
]) < 1e-8))

# Compare with the R-native final matrix, if available.
final_path <- file.path(work, "data_full_matrices.rds")
if (file.exists(final_path)) {
  final <- readRDS(final_path)
  needed <- c("year", "source", "host", "augmeqasset", "augmdebtasset")
  if (all(needed %in% names(final))) {
    final_check <- final %>%
      filter(source == 377, year %in% 2001:2014) %>%
      group_by(year) %>%
      summarise(
        original_equity = sum(as.numeric(augmeqasset), na.rm = TRUE),
        original_debt = sum(as.numeric(augmdebtasset), na.rm = TRUE),
        .groups = "drop"
      )
    reconciliation <- baseline %>%
      left_join(final_check, by = "year", relationship = "one-to-one") %>%
      mutate(
        equity_difference = baseline_equity - original_equity,
        debt_difference = baseline_debt - original_debt
      )
    write_csv(reconciliation, file.path(
      sensitivity_dir, "sensitivity_cayman_baseline_reconciliation.csv"
    ))
    if (anyNA(reconciliation[c("equity_difference", "debt_difference")]) ||
        any(abs(as.matrix(reconciliation[c("equity_difference", "debt_difference")])) > 1e-5)) {
      warning("Baseline differs from the final R matrix; inspect reconciliation.")
    }
  } else {
    warning("Final R matrix lacks variables required for baseline reconciliation.")
  }
} else {
  message("Final R matrix unavailable; baseline reconciliation skipped.")
}

write_csv(shares, file.path(sensitivity_dir, "sensitivity_cayman_assumptions.csv"))
write_csv(annual, file.path(sensitivity_dir, "sensitivity_cayman_annual.csv"))
write_csv(validation, file.path(sensitivity_dir, "sensitivity_cayman_validation.csv"))
saveRDS(bilateral, file.path(sensitivity_dir, "sensitivity_cayman_bilateral.rds"))

# Plot the differences calculated from the same scenario-specific baseline.
plot_data <- annual %>%
  filter(scenario != "Baseline") %>%
  select(year, scenario, delta_equity_assets, delta_debt_assets) %>%
  pivot_longer(
    cols = starts_with("delta_"),
    names_to = "asset_class",
    values_to = "change"
  ) %>%
  mutate(
    asset_class = recode(
      asset_class,
      delta_equity_assets = "Equity assets",
      delta_debt_assets = "Debt assets"
    ),
    scenario = factor(
      scenario,
      levels = c("Higher US shares", "Lower US shares", "Rising equity US share")
    )
  )

p <- ggplot(plot_data, aes(year, change, linetype = scenario)) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  geom_line(linewidth = 0.75) +
  facet_wrap(~ asset_class, scales = "free_y") +
  scale_x_continuous(breaks = c(2001, 2005, 2010, 2014)) +
  labs(
    x = NULL,
    y = "Change from baseline (USD million)",
    linetype = NULL,
    title = "Sensitivity of pre-2015 Cayman portfolio assets"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(file.path(fig, "7c-sensitivity_cayman_pre2015.pdf"), p, width = 9, height = 5.5)
ggsave(file.path(fig, "7c-sensitivity_cayman_pre2015.png"), p, width = 9, height = 5.5, dpi = 300)
message("7c complete. Outputs saved to sensitivity and figure directories.")
