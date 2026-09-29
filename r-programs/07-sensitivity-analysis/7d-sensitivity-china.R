# 7d: Sensitivity to China's assumed reserve portfolio share, 2001–2008
# Run after sourcing 0a-setup.R. The baseline inputs are read, never overwritten.
# Matrix units are inherited from the original replication data (USD millions).

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)

stopifnot(exists("work"), exists("fig"), exists("read_work_data"))
out <- file.path(work, "07-sensitivity-analysis", "china")
fig_out <- file.path(fig, "07-sensitivity-analysis", "china")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_out, recursive = TRUE, showWarnings = FALSE)

# df3 is the bilateral matrix immediately before the China allocation.
pre_china <- read_work_data("df3")
required <- c("year", "source", "host", "augmeqasset", "augmdebtasset")
stopifnot(all(required %in% names(pre_china)))
saveRDS(pre_china %>% filter(source %in% c(924, 9999)),
        file.path(out, "china_preallocation_input.rds"))

# Prepare the IMF/TIC input in the same order as the original China calculation:
# first backcast equity and debt, then infer missing early reserve observations.
tic_china <- read_work_data("TIC_China_Dec")
imf_china <- read_work_data("data_IMF_China")

# The stored baseline is used only to infer 2001–2003 reserve inputs. This is a
# baseline-consistent reconstruction, NOT independent verification of IMF data.
china_baseline <- read_work_data("df-with-china") %>%
  filter(source == 924, year %in% 2001:2003) %>%
  group_by(year) %>%
  summarise(total_assets = first(toteqasset) + first(totdebtasset),
            .groups = "drop")
stopifnot(nrow(china_baseline) == 3L,
          !anyNA(china_baseline$total_assets),
          !anyDuplicated(china_baseline$year))

china_input <- tic_china %>%
  left_join(imf_china, by = "year") %>%
  filter(between(year, 2001, 2021)) %>%
  arrange(year) %>%
  mutate(
    growth_equity = eq_China_TIC / lead(eq_China_TIC),
    growth_Debt = debt_China_TIC / lead(debt_China_TIC),
    Equity_IMF = if_else(year < 2007, NA_real_, Equity_IMF)
  )

# Sequential backward propagation: a newly filled year supplies the preceding year.
for (i in seq_len(6)) {
  china_input <- china_input %>%
    mutate(Equity_IMF = coalesce(Equity_IMF,
                                 lead(Equity_IMF) * growth_equity))
}
for (i in seq_len(3)) {
  china_input <- china_input %>%
    mutate(Debt_IMF = coalesce(Debt_IMF,
                               lead(Debt_IMF) * growth_Debt))
}

china_input <- china_input %>%
  left_join(china_baseline, by = "year", relationship = "many-to-one") %>%
  mutate(
    Reserves_IMF = coalesce(
      Reserves_IMF,
      if_else(year %in% 2001:2003,
              (total_assets - Equity_IMF - Debt_IMF) / 0.85,
              NA_real_)
    )
  ) %>%
  select(-total_assets)

stopifnot(
  nrow(china_input) == 21L,
  !anyDuplicated(china_input$year),
  identical(as.integer(china_input$year), 2001:2021),
  !anyNA(china_input$Equity_IMF),
  !anyNA(china_input$Debt_IMF),
  !anyNA(china_input$Reserves_IMF),
  all(is.finite(china_input$Reserves_IMF)),
  all(china_input$Reserves_IMF > 0)
)
print(china_input %>%
        filter(year %in% 2001:2005) %>%
        select(year, Equity_IMF, Debt_IMF, Reserves_IMF), n = Inf)
saveRDS(china_input, file.path(out, "china_imf_tic_prepared.rds"))

scenario_spec <- tibble(
  scenario = c("Baseline", "Lower portfolio share", "Higher portfolio share"),
  pre2009_share = c(0.85, 0.75, 0.95)
)
write_csv(scenario_spec, file.path(out, "china_scenario_assumptions.csv"))


# Replicate the original share schedule; only 2001–2008 differ.
china_scenarios <- tidyr::crossing(china_input, scenario_spec) %>%
  mutate(
    share = case_when(
      year == 2009 ~ 0.87, year == 2010 ~ 0.89,
      year == 2011 ~ 0.91, year == 2012 ~ 0.93,
      year > 2012 ~ 0.95, TRUE ~ pre2009_share
    ),
    reserves_China = share * Reserves_IMF,
    equity_ratio_TIC = eq_China_TIC / total_China_TIC,
    totaleq_China_public = equity_ratio_TIC * reserves_China,
    totaldebt_China_public = (1 - equity_ratio_TIC) * reserves_China,
    totaleq_China = equity_ratio_TIC *
      (reserves_China + Equity_IMF + Debt_IMF),
    totaldebt_China = (1 - equity_ratio_TIC) *
      (share * Reserves_IMF + Equity_IMF + Debt_IMF)
  )
stopifnot(!anyDuplicated(china_scenarios[c("year", "scenario")]))
saveRDS(china_scenarios, file.path(out, "china_scenario_totals.rds"))

# Preserve the baseline pipeline's non-US destination weights (source 9999).
weights <- pre_china %>%
  filter(source == 9999, host != 111, host != 924) %>%
  group_by(year) %>%
  mutate(
    shareeqsefernonus = augmeqasset / sum(augmeqasset, na.rm = TRUE),
    sharedebtsefernonus = augmdebtasset / sum(augmdebtasset, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  select(year, host, shareeqsefernonus, sharedebtsefernonus)
stopifnot(!anyDuplicated(weights[c("year", "host")]))

# One scenario-specific bilateral dataframe; source 924, same hosts as baseline.

china_bilateral <- pre_china %>%
  filter(source == 924) %>%
  select(year, source, host) %>%
  inner_join(
    china_scenarios %>% select(-any_of(c("host", "source"))),
    by = "year",
    relationship = "many-to-many"
  ) %>%
  left_join(
    weights,
    by = c("year", "host"),
    relationship = "many-to-one"
  ) %>%
  mutate(
    equity_assets = if_else(
      host == 111,
      eq_China_TIC,
      shareeqsefernonus * (totaleq_China - eq_China_TIC)
    ),
    debt_assets = if_else(
      host == 111,
      debt_China_TIC,
      sharedebtsefernonus * (totaldebt_China - debt_China_TIC)
    )
  )

stopifnot(!anyDuplicated(china_bilateral[c("year", "host", "scenario")]))
saveRDS(china_bilateral, file.path(out, "china_bilateral_scenarios.rds"))

# Only changes to the global asset side are calculated here. Liability totals
# and all non-China positions are held fixed. Gap convention: L - A.

annual <- china_scenarios %>%
  filter(between(year, 2001, 2021)) %>%
  group_by(year, scenario) %>%
  summarise(
    share = first(share),
    reserves_China = first(reserves_China),
    china_equity = first(totaleq_China),
    china_debt = first(totaldebt_China),
    china_portfolio = china_equity + china_debt,
    .groups = "drop"
  )

baseline <- annual %>%
  filter(scenario == "Baseline") %>%
  transmute(year, baseline_equity = china_equity,
            baseline_debt = china_debt,
            baseline_portfolio = china_portfolio)
annual <- annual %>%
  left_join(baseline, by = "year", relationship = "many-to-one") %>%
  mutate(
    delta_equity_assets = china_equity - baseline_equity,
    delta_debt_assets = china_debt - baseline_debt,
    delta_portfolio_assets = china_portfolio - baseline_portfolio,
    delta_global_gap_L_minus_A = -delta_portfolio_assets
  )

stopifnot(
  !anyNA(annual$delta_portfolio_assets),
  !anyNA(annual$delta_global_gap_L_minus_A),
  all(is.finite(annual$delta_portfolio_assets)),
  all(is.finite(annual$delta_global_gap_L_minus_A)),
  all(abs(
    annual$delta_portfolio_assets[annual$year >= 2009]
  ) < 1e-7),
  all(abs(
    annual$delta_global_gap_L_minus_A +
      annual$delta_portfolio_assets
  ) < 1e-7)
)

saveRDS(annual, file.path(out, "7d-china_annual_scenarios.rds"))
write_csv(annual, file.path(out, "7d-china_annual_scenarios.csv"))

# Validate the analytical scenario totals against the actual bilateral allocation.
check <- china_bilateral %>%
  group_by(year, scenario) %>%
  summarise(allocated_equity = sum(equity_assets, na.rm = TRUE),
            allocated_debt = sum(debt_assets, na.rm = TRUE),
            missing_equity = sum(is.na(equity_assets)),
            missing_debt = sum(is.na(debt_assets)),
            .groups = "drop") %>%
  left_join(annual, by = c("year", "scenario"), relationship = "one-to-one") %>%
  mutate(equity_residual = allocated_equity - china_equity,
         debt_residual = allocated_debt - china_debt)
saveRDS(check, file.path(out, "7d-china_allocation_reconciliation.rds"))
write_csv(check, file.path(out, "7d-china_allocation_reconciliation.csv"))
if (any(check$missing_equity > 0 | check$missing_debt > 0 |
        abs(check$equity_residual) > 1e-5 |
        abs(check$debt_residual) > 1e-5)) {
  warning("Bilateral allocation does not fully reconcile. Inspect reconciliation CSV.")
}

p <- annual %>%
  filter(year <= 2008, scenario != "Baseline") %>%
  ggplot(aes(year, y = delta_global_gap_L_minus_A / 1000, linetype = scenario)) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  geom_line(linewidth = 0.8) +
  scale_x_continuous(breaks = 2001:2008) +
  labs(title = "Sensitivity to China's reserve portfolio share",
       subtitle = "Global gap effect, holding liabilities and other inputs fixed",
       x = NULL, 
       y = "Change in global asset-liability gap (USD billions)",
       linetype = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")
ggsave(file.path(fig_out, "7d-china_global_gap_sensitivity.pdf"),
       p, width = 8, height = 5)
ggsave(file.path(fig_out, "7d-china_global_gap_sensitivity.png"),
       p, width = 8, height = 5, dpi = 300)
message("China sensitivity complete: ", out)
