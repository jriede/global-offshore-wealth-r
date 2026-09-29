# 7e: Propagate China gap changes to the global offshore-wealth figure.
# Run AFTER 7d-sensitivity-china.R and 0a-setup.R.
# Uses FGZ T.A1's published global wealth, securities and deposits as the
# reference series; holds the deposit share and world GDP fixed by year.
library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(ggplot2)
library(knitr)

stopifnot(exists("work"), exists("raw"), exists("fig"))
out <- file.path(work, "07-sensitivity-analysis", "china")
fig_out <- file.path(fig, "07-sensitivity-analysis", "china")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_out, recursive = TRUE, showWarnings = FALSE)
annual <- readRDS(file.path(out, "china_annual_scenarios.rds"))

input <- file.path(raw, "FGZ-raw-data.xlsx")
stopifnot(file.exists(input))
t_a1 <- read_excel(input, sheet = "T.A1", range = "A8:E28",
                   col_names = FALSE)
names(t_a1) <- c("year", "world_gdp", "published_wealth",
                 "securities", "deposits")
t_a1[] <- lapply(t_a1, as.numeric)
stopifnot(identical(as.integer(t_a1$year), 2001:2021),
          !anyNA(t_a1), all(t_a1$world_gdp > 0),
          all(t_a1$securities > 0),
          all(t_a1$deposits >= 0))
t_a1 <- t_a1 %>%
  mutate(deposit_share = deposits / (securities + deposits),
         component_wealth = securities + deposits,
         baseline_discrepancy = component_wealth - published_wealth)
saveRDS(t_a1, file.path(out, "china_offshore_reference_TA1.rds"))

# A change in the positive global gap (L-A) changes estimated offshore
# securities one-for-one; the deposit share is held constant. This is a
# conditional propagation, not a rerun of the entire FGZ production pipeline.

offshore <- offshore %>%
  mutate(
    delta_gap_bn = delta_global_gap_L_minus_A / 1000,
    
    scenario_securities = securities + delta_gap_bn,
    
    delta_offshore_wealth =
      delta_gap_bn / (1 - deposit_share),
    
    scenario_wealth =
      published_wealth + delta_offshore_wealth,
    
    scenario_percent_gdp =
      100 * scenario_wealth / world_gdp,
    
    baseline_percent_gdp =
      100 * published_wealth / world_gdp,
    
    delta_percentage_points_gdp =
      scenario_percent_gdp - baseline_percent_gdp
  )

stopifnot(
  !anyNA(offshore[c("scenario_wealth", "scenario_percent_gdp")]),
  all(offshore$scenario_securities > 0),
  all(abs(
    offshore$delta_offshore_wealth -
      offshore$delta_global_gap_L_minus_A /
      1000 / (1 - offshore$deposit_share)
  ) < 1e-5)
)

saveRDS(offshore, file.path(out, "china_offshore_scenarios.rds"))
write_csv(offshore, file.path(out, "china_offshore_scenarios.csv"))

# Full-period scenario series: the assumptions are identical from 2009 onward.
plot_data <- offshore %>%
  select(year, scenario, scenario_percent_gdp) %>%
  mutate(scenario = factor(
    scenario,
    levels = c("Baseline", "Lower portfolio share", "Higher portfolio share")
  ))
p <- ggplot(plot_data, aes(year, scenario_percent_gdp, linetype = scenario)) +
  geom_line(linewidth = 0.8) +
  scale_x_continuous(breaks = seq(2001, 2021, by = 2)) +
  labs(title = "Global offshore wealth: China reserve-share sensitivity",
       subtitle = "Conditional propagation; deposit share and GDP held fixed",
       x = NULL, y = "Offshore wealth (% of world GDP)", linetype = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")
ggsave(file.path(fig_out, "china_offshore_sensitivity.pdf"),
       p, width = 9, height = 5.5)
ggsave(file.path(fig_out, "china_offshore_sensitivity.png"),
       p, width = 9, height = 5.5, dpi = 300)
message("China offshore propagation complete: ", out)


# ---- Generate LaTeX tables for Appendix A2

china_dir <- file.path(
  work, "07-sensitivity-analysis", "china"
)

# ------------------------------------------------------------
# Table 1: Global asset-liability gap
# ------------------------------------------------------------

china_gap <- read_csv(
  file.path(china_dir, "china_annual_scenarios.csv"),
  show_col_types = FALSE
)

appendix_china_gap <- china_gap %>%
  filter(between(year, 2001, 2008)) %>%
  mutate(
    delta_gap_bn = delta_global_gap_L_minus_A / 1000
  ) %>%
  select(year, scenario, delta_gap_bn) %>%
  pivot_wider(
    names_from = scenario,
    values_from = delta_gap_bn
  ) %>%
  transmute(
    Year = as.integer(year),
    Baseline,
    `Lower portfolio share`,
    `Higher portfolio share`
  )

latex_china_gap <- knitr::kable(
  appendix_china_gap,
  format = "latex",
  booktabs = TRUE,
  digits = 2,
  align = c("c", rep("r", 3)),
  caption = paste(
    "Sensitivity of the global asset--liability gap to",
    "alternative assumptions about China's reserve portfolio share,",
    "2001--2008. Changes relative to the baseline, in USD billions."
  ),
  label = "tab:sensitivity_china_gap"
)

writeLines(
  latex_china_gap,
  file.path(tables, "appendix_a2_china_gap.tex")
)


# ------------------------------------------------------------
# Table 2: Global offshore wealth (% of world GDP)
# ------------------------------------------------------------

china_offshore <- read_csv(
  file.path(china_dir, "china_offshore_scenarios.csv"),
  show_col_types = FALSE
)

appendix_china_offshore <- china_offshore %>%
  select(year, scenario, scenario_percent_gdp) %>%
  pivot_wider(
    names_from = scenario,
    values_from = scenario_percent_gdp
  ) %>%
  transmute(
    Year = as.integer(year),
    Baseline,
    `Lower portfolio share`,
    `Higher portfolio share`
  )

latex_china_offshore <- knitr::kable(
  appendix_china_offshore,
  format = "latex",
  booktabs = TRUE,
  digits = 2,
  align = c("c", rep("r", 3)),
  caption = paste(
    "Sensitivity of estimated global offshore wealth to",
    "alternative assumptions about China's reserve portfolio share,",
    "2001--2021. Values are expressed as a percentage of world GDP."
  ),
  label = "tab:sensitivity_china_offshore"
)

writeLines(
  latex_china_offshore,
  file.path(tables, "appendix_a2_china_offshore.tex")
)

# ----


