# ==============================================================================
# REPL: Global Offshore Wealth Using R, 2001-2021
# jlenke, 2026
#
# Purpose:
#   Reconstruct the global offshore-wealth series underlying Figure 1 from
#   the R-replicated portfolio asset/liability data instead of importing the
#   published offshore-wealth series from FGZ-raw-data.xlsx.
#
# Required objects/functions from 0a-setup.R:
#   work, raw, fig, read_work_data()
#
# Main input:
#   data_full_matrices.rds
#
# Main outputs:
#   work/global_offshore_wealth.rds
#   work/global_offshore_wealth.csv
#   figures/global_offshore_wealth_figure1.pdf
#   figures/global_offshore_wealth_figure1.png
#
# Units:
#   data_full_matrices variables are in USD million.
#   Output monetary variables are in USD billion.
# ==============================================================================

library(dplyr)
library(readr)
library(readxl)
library(ggplot2)

# ------------------------------------------------------------------------------
# 1. Load final bilateral matrix
# ------------------------------------------------------------------------------

data_full_matrices <- read_work_data("data_full_matrices") %>%
  mutate(year = as.integer(year))

# ------------------------------------------------------------------------------
# 2. Global corrected portfolio assets
#
# augmeqasset and augmdebtasset are the final corrected bilateral asset
# variables produced in 2_do_full_matrices.R. Summing them over all
# source-host pairs gives corrected global portfolio assets.
# ------------------------------------------------------------------------------

global_assets <- data_full_matrices %>%
  filter(year %in% 2001:2021) %>%
  group_by(year) %>%
  summarise(
    equity_assets = sum(augmeqasset, na.rm = TRUE) / 1000,
    debt_assets   = sum(augmdebtasset, na.rm = TRUE) / 1000,
    .groups = "drop"
  ) %>%
  mutate(
    total_assets = equity_assets + debt_assets
  )

# ------------------------------------------------------------------------------
# 3. Global corrected portfolio liabilities
#
# Liability variables are host-year quantities repeated over bilateral source
# observations. Therefore first reduce data_full_matrices to one observation
# per host-year, exactly as in 3_do_table_A2.R, and only then aggregate globally.
# ------------------------------------------------------------------------------

host_year <- data_full_matrices %>%
  filter(year %in% 2001:2021) %>%
  arrange(host, year, source) %>%
  group_by(host, year) %>%
  summarise(
    equity_liabilities = first(eqliab_host),
    debt_liabilities   = first(debtliab_host),
    .groups = "drop"
  )

global_liabilities <- host_year %>%
  group_by(year) %>%
  summarise(
    equity_liabilities = sum(equity_liabilities, na.rm = TRUE) / 1000,
    debt_liabilities   = sum(debt_liabilities, na.rm = TRUE) / 1000,
    .groups = "drop"
  ) %>%
  mutate(
    total_liabilities = equity_liabilities + debt_liabilities
  )

# ------------------------------------------------------------------------------
# 4. Global portfolio assets-liabilities gap = offshore securities
# ------------------------------------------------------------------------------

global_gap <- global_assets %>%
  inner_join(global_liabilities, by = "year") %>%
  mutate(
    equity_gap = equity_liabilities - equity_assets,
    debt_gap   = debt_liabilities - debt_assets,
    securities_offshore = equity_gap + debt_gap
  )

# ------------------------------------------------------------------------------
# 5. Deposit share of total offshore wealth
#
# FGZ state:
#   - deposits = 25% of total offshore wealth in 2008 (Zucman 2013)
#   - deposits = 18% in 2013 (Zucman 2015)
#   - the share rises after 2013
#   - it is constant at 20% from 2016 onward
#
# The paper does not spell out every annual value. The interpolation below is
# therefore explicit rather than hidden:
#   2001-2008: 25%
#   2009-2013: linear interpolation from 25% to 18%
#   2014-2016: linear interpolation from 18% to 20%
#   2017-2021: 20%
#
# If the original replication code specifies different annual shares, replace
# ONLY this block; the rest of the reconstruction is unaffected.
# ------------------------------------------------------------------------------

deposit_shares <- tibble(
  year = 2001:2021
) %>%
  mutate(
    deposit_share = case_when(
      year <= 2008 ~ 0.25,
      year <= 2013 ~ 0.25 + (year - 2008) * (0.18 - 0.25) / (2013 - 2008),
      year <= 2016 ~ 0.18 + (year - 2013) * (0.20 - 0.18) / (2016 - 2013),
      TRUE         ~ 0.20
    )
  )

# ------------------------------------------------------------------------------
# 6. Total global offshore wealth
#
# securities_offshore = (1 - deposit_share) * total_offshore_wealth
#
# hence:
# total_offshore_wealth = securities_offshore / (1 - deposit_share)
# ------------------------------------------------------------------------------

global_offshore <- global_gap %>%
  left_join(deposit_shares, by = "year") %>%
  mutate(
    deposits_offshore = securities_offshore *
      deposit_share / (1 - deposit_share),

    offshore_wealth = securities_offshore / (1 - deposit_share)
  )

# ------------------------------------------------------------------------------
# 7. World GDP
#
# Construct world GDP from the replicated GDP data already merged into
# data_gravity_update. gdp_source is in USD million.
#
# GDP is repeated for every host of a source country, so keep one source-year
# observation before summing.
# ------------------------------------------------------------------------------

data_gravity_update <- read_work_data("data_gravity_update") %>%
  mutate(year = as.integer(year))

world_gdp <- data_gravity_update %>%
  filter(year %in% 2001:2021) %>%
  select(year, source, gdp_source) %>%
  distinct(year, source, .keep_all = TRUE) %>%
  group_by(year) %>%
  summarise(
    world_gdp = sum(gdp_source, na.rm = TRUE) / 1000,
    .groups = "drop"
  )

# ------------------------------------------------------------------------------
# 8. Figure-1 series
# ------------------------------------------------------------------------------

global_offshore_wealth <- global_offshore %>%
  left_join(world_gdp, by = "year") %>%
  mutate(
    offshore_share_world_gdp = 100 * offshore_wealth / world_gdp
  ) %>%
  select(
    year,
    equity_assets,
    debt_assets,
    total_assets,
    equity_liabilities,
    debt_liabilities,
    total_liabilities,
    equity_gap,
    debt_gap,
    securities_offshore,
    deposit_share,
    deposits_offshore,
    offshore_wealth,
    world_gdp,
    offshore_share_world_gdp
  ) %>%
  arrange(year)

# ------------------------------------------------------------------------------
# 9. Basic checks
# ------------------------------------------------------------------------------

stopifnot(
  nrow(global_offshore_wealth) == 21,
  identical(global_offshore_wealth$year, 2001:2021),
  all(global_offshore_wealth$deposit_share > 0 &
        global_offshore_wealth$deposit_share < 1),
  all(is.finite(global_offshore_wealth$offshore_share_world_gdp))
)

print(
  global_offshore_wealth %>%
    select(
      year,
      securities_offshore,
      deposit_share,
      offshore_wealth,
      world_gdp,
      offshore_share_world_gdp
    ),
  n = Inf
)

# ------------------------------------------------------------------------------
# 10. Optional validation against the published FGZ series
#
# IMPORTANT:
# The published series is used ONLY as a benchmark here. It does not enter the
# replicated offshore-wealth calculation above.
# ------------------------------------------------------------------------------

fgz_file <- file.path(raw, "FGZ-raw-data.xlsx")

if (file.exists(fgz_file)) {

  fgz_original <- read_excel(
    fgz_file,
    sheet = "T.A1",
    range = "A7:C28",
    col_names = FALSE
  )

  names(fgz_original)[1:3] <- c(
    "year",
    "world_gdp_fgz",
    "offshore_wealth_fgz"
  )

  fgz_original <- fgz_original %>%
    transmute(
      year = as.integer(year),
      world_gdp_fgz = as.numeric(world_gdp_fgz),
      offshore_wealth_fgz = as.numeric(offshore_wealth_fgz),
      offshore_share_world_gdp_fgz =
        100 * offshore_wealth_fgz / world_gdp_fgz
    ) %>%
    filter(year %in% 2001:2021)

  replication_comparison <- global_offshore_wealth %>%
    left_join(fgz_original, by = "year") %>%
    mutate(
      offshore_wealth_diff =
        offshore_wealth - offshore_wealth_fgz,

      offshore_wealth_pct_diff =
        100 * offshore_wealth_diff / offshore_wealth_fgz,

      share_diff_pp =
        offshore_share_world_gdp -
        offshore_share_world_gdp_fgz,

      world_gdp_diff =
        world_gdp - world_gdp_fgz
    )

  print(
    replication_comparison %>%
      select(
        year,
        offshore_wealth,
        offshore_wealth_fgz,
        offshore_wealth_diff,
        offshore_wealth_pct_diff,
        offshore_share_world_gdp,
        offshore_share_world_gdp_fgz,
        share_diff_pp
      ),
    n = Inf
  )

  saveRDS(
    replication_comparison,
    file.path(work, "global_offshore_wealth_comparison.rds")
  )

  write_csv(
    replication_comparison,
    file.path(work, "global_offshore_wealth_comparison.csv"),
    na = ""
  )
}

# ------------------------------------------------------------------------------
# 11. Save replicated data
# ------------------------------------------------------------------------------

saveRDS(
  global_offshore_wealth,
  file.path(work, "global_offshore_wealth.rds")
)

write_csv(
  global_offshore_wealth,
  file.path(work, "global_offshore_wealth.csv"),
  na = ""
)

# ------------------------------------------------------------------------------
# 12. Figure 1: Replication vs. original FGZ series
# ------------------------------------------------------------------------------

# Join replicated and original FGZ series
figure1_data <- global_offshore_wealth %>%
  select(
    year,
    replicated = offshore_share_world_gdp
  ) %>%
  left_join(
    fgz_original %>%
      select(
        year,
        original = offshore_share_world_gdp_fgz
      ),
    by = "year"
  ) %>%
  tidyr::pivot_longer(
    cols = c(replicated, original),
    names_to = "series",
    values_to = "offshore_share_world_gdp"
  ) %>%
  mutate(
    series = recode(
      series,
      replicated = "R replication",
      original   = "FGZ (2023)"
    )
  )


figure1 <- ggplot(
  figure1_data,
  aes(
    x = year,
    y = offshore_share_world_gdp,
    linetype = series,
    shape = series
  )
) +
  geom_line(
    linewidth = 0.7
  ) +
  geom_point(
    size = 2.2,
    stroke = 0.7
  ) +
  scale_linetype_manual(
    values = c(
      "R replication" = "solid",
      "FGZ (2023)"    = "dashed"
    )
  ) +
  scale_shape_manual(
    values = c(
      "R replication" = 16,
      "FGZ (2023)"    = 1
    )
  ) +
  scale_x_continuous(
    name = "Year",
    breaks = seq(2001, 2021, by = 2),
    limits = c(2000.5, 2021.5)
  ) +
  scale_y_continuous(
    name = "% of world GDP",
    breaks = seq(0, 16, by = 2),
    limits = c(0, 16),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    linetype = NULL,
    shape = NULL
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(
      angle = 0,
      hjust = 0.5
    ),
    legend.position = "bottom"
  )

print(figure1)


ggsave(
  file.path(fig, "global_offshore_wealth_comparison.pdf"),
  figure1,
  width = 7,
  height = 5
)

ggsave(
  file.path(fig, "global_offshore_wealth_comparison.png"),
  figure1,
  width = 7,
  height = 5,
  dpi = 300
)
# ------------------------------------------------------------------------------
# End
# ------------------------------------------------------------------------------
