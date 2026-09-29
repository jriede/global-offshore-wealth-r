# ==============================================================================
# REPL: Global Offshore Wealth, 2001-2021
#
# R translation of: 4c-graph-bis.do
#
# This program graphs the evolution of BIS bank deposits in selected countries
#
# databases used:
#   - locational.dta
#   - FGZ-raw-data.xlsx
#
# outputs:
#   - bisdepbyhaven sheet in FGZ-raw-data.xlsx
#   - shche-06-07.pdf
#   - shche-22.pdf
# ==============================================================================


# ------------------------------ PACKAGES --------------------------------------

library(dplyr)
library(tidyr)
library(readxl)
library(openxlsx)
library(haven)
library(ggplot2)


# --------------------------- HELPER OBJECTS -----------------------------------

havens <- c(
  "AN", "AT", "BE", "BM", "BH", "BS", "CH", "CL",
  "CW", "CY", "KY", "GB", "GG", "IM", "JE", "LU",
  "MO", "MC", "MY", "PA", "HK", "SG", "US"
)

plot_data_dir <- file.path(work, "plot-data", "r")

export_plot_data <- function(data, filename) {
  readr::write_csv(
    data,
    file.path(plot_data_dir, paste0(filename, ".csv")),
    na = ""
  )
}

# ==============================================================================
# I ---- TABLE: DEPOSITS IN EACH BIS-REPORTING COUNTRY
# ==============================================================================


# ----------------------------- IMPORT DATA ------------------------------------
locational <- read_work_data("locational")
#locational <- haven::read_dta(file.path(work2, "locational.dta"))


# ----------------------------- FILTER DATA ------------------------------------

bisdepbyhaven <- locational %>%
  filter(
    position == "L",
    instrument == "A",
    sector == "N",
    parent == "5J",
    quarter == 4,
    bank %in% c(
      "AN", "AT", "BE", "BH", "BM", "BS", "CH", "CL",
      "CW", "CY", "KY", "GB", "GG", "IM", "JE", "LU",
      "MO", "MY", "PA", "HK", "SG", "US"
    ),
    counter == "5J"
  )


# ------------------------- COLLAPSE TO YEAR -----------------------------------

# Stata:
# collapse (mean) value, by(bank year)

bisdepbyhaven <- bisdepbyhaven %>%
  group_by(bank, year) %>%
  summarise(
    value = mean(value, na.rm = TRUE),
    .groups = "drop"
  )


# ---------------------------- RESHAPE WIDE ------------------------------------

# Stata:
# reshape wide value, i(year) j(bank) string

bisdepbyhaven <- bisdepbyhaven %>%
  pivot_wider(
    names_from = bank,
    values_from = value,
    names_prefix = "value"
  ) %>%
  arrange(year)


# ---------------------------- EXPORT EXCEL ------------------------------------

# Stata:
# export excel using "$raw/FGZ-raw-data.xlsx",
# sheet(bisdepbyhaven) firstrow(variables) sheetreplace

openxlsx::write.xlsx(
  bisdepbyhaven,
  file = file.path(raw, "bisdepbyhaven.xlsx"),
  overwrite = TRUE
)

# NOTE:
# The Stata version writes directly into the existing workbook
# FGZ-raw-data.xlsx and replaces only the sheet "bisdepbyhaven".
#
# If the existing workbook must be modified instead of creating a separate
# file, use the following block:

wb <- openxlsx::loadWorkbook(
  file.path(raw, "FGZ-raw-data.xlsx")
)

if ("bisdepbyhaven" %in% names(wb)) {
  openxlsx::removeWorksheet(
    wb,
    "bisdepbyhaven"
  )
}

openxlsx::addWorksheet(
  wb,
  "bisdepbyhaven"
)

openxlsx::writeData(
  wb,
  sheet = "bisdepbyhaven",
  x = bisdepbyhaven
)

openxlsx::saveWorkbook(
  wb,
  file.path(raw, "FGZ-raw-data.xlsx"),
  overwrite = TRUE
)



# ==============================================================================
# II ---- GRAPHS USING AGGREGATE BIS DATA
# ==============================================================================


# ==============================================================================
# II.1 ---- Switzerland vs. other havens in BIS data, 2006-07
# ==============================================================================


# ----------------------------- IMPORT DATA ------------------------------------
# bereits geladen, nicht nochmal notwendig
#locational <- haven::read_dta(
#  file.path(work2, "locational.dta")
#)


# ----------------------------- FILTER DATA ------------------------------------

bis0607 <- locational %>%
  filter(
    year %in% c(2006, 2007),
    position == "L",
    instrument == "A",
    sector == "N",
    parent == "5J",
    counter == "5J"
  )


# ------------------------- COLLAPSE OBSERVATIONS ------------------------------

# Stata:
#
# collapse (mean) value
#          (first) namebank iso3bank year,
#          by(bank sector instrument position)

bis0607 <- bis0607 %>%
  group_by(
    bank,
    sector,
    instrument,
    position
  ) %>%
  summarise(
    value = mean(value, na.rm = TRUE),
    namebank = first(namebank),
    iso3bank = first(iso3bank),
    year = first(year),
    .groups = "drop"
  )


# ------------------------- HAVEN INDICATOR ------------------------------------

bis0607 <- bis0607 %>%
  mutate(
    haven = if_else(
      bank %in% havens,
      1,
      0
    )
  )


# ==============================================================================
# Household share of deposits
# ==============================================================================

share_household_dep <- readxl::read_excel(
  file.path(raw, "FGZ-raw-data.xlsx"),
  sheet = "sharehouseholddep",
  range = "A3:W25"
) %>%
  filter(
    year %in% c(2006, 2007)
  )


# Stata:
# merge 1:m year using "`bis0607'"

bis0607 <- bis0607 %>%
  left_join(
    share_household_dep,
    by = "year"
  )


# -------------------- ADJUST HOUSEHOLD DEPOSITS -------------------------------

adjustment_banks_0607 <- c(
  "BE", "CH", "GG", "IM", "JE", "PA", "LU", "CY",
  "MO", "MY", "KY", "AT", "AN", "CW", "BH", "BM",
  "BS", "HK", "SG", "GB", "US", "CL"
)

for (b in adjustment_banks_0607) {
  
  bis0607 <- bis0607 %>%
    mutate(
      value = if_else(
        bank == b & haven == 1,
        .data[[b]] * value,
        value
      )
    ) %>%
    select(
      -all_of(b)
    )
}


# ---------------------------- CLEAN DATA --------------------------------------

bis0607 <- bis0607 %>%
  filter(
    !is.na(iso3bank),
    iso3bank != "",
    !is.na(value)
  )


# ------------------------- COMPUTE SHARES -------------------------------------

# Stata:
#
# su value if haven==1
# local tothaven=r(sum)
# replace shvalue=value/`tothaven' if haven==1

tothaven_0607 <- bis0607 %>%
  filter(haven == 1) %>%
  summarise(
    total = sum(value, na.rm = TRUE)
  ) %>%
  pull(total)


bis0607 <- bis0607 %>%
  mutate(
    shvalue = if_else(
      haven == 1,
      value / tothaven_0607,
      0
    ),
    namebank = if_else(
      namebank ==
        "United Kingdom of Great Britain and Northern Ireland",
      "United Kingdom",
      namebank
    )
  )


# ------------------------------- GRAPH ----------------------------------------

graph_0607 <- bis0607 %>%
  filter(haven == 1) %>%
  arrange(shvalue) %>%
  mutate(
    namebank = factor(
      namebank,
      levels = namebank
    )
  ) %>%
  ggplot(
    aes(
      x = namebank,
      y = shvalue
    )
  ) +
  geom_col() +
  geom_text(
    aes(label = namebank),
    angle = 90,
    hjust = -0.05,
    size = 3
  ) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, 0.5)
  ) +
  labs(
    title =
      "Cross-border household deposits in BIS-reporting tax havens, 2006-07",
    x = NULL,
    y = "% of deposits in BIS-reporting tax havens",
    caption =
      paste0(
        "Data source: BIS\n",
        "Note: We assume that in Switzerland 100% of deposits belong ",
        "to households,\nand only a fraction in the other tax haven."
      )
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )


# ----------------------------- EXPORT GRAPH -----------------------------------

ggsave(
  filename = file.path(
    fig,
    "shche-06-07.pdf"
  ),
  plot = graph_0607,
  width = 8,
  height = 5
)



# ==============================================================================
# II.2 ---- Switzerland vs. other havens in BIS data, 2022
# ==============================================================================


# ----------------------------- IMPORT DATA ------------------------------------
# same
#locational <- haven::read_dta(
#  file.path(work, "locational.dta")
#)


# ----------------------------- FILTER DATA ------------------------------------

bis22 <- locational %>%
  filter(
    year == 2022,
    position == "L",
    instrument == "A",
    sector == "N",
    parent == "5J",
    counter == "5J"
  )


# ------------------------- COLLAPSE OBSERVATIONS ------------------------------

bis22 <- bis22 %>%
  group_by(
    bank,
    sector,
    instrument,
    position
  ) %>%
  summarise(
    value = mean(value, na.rm = TRUE),
    namebank = first(namebank),
    iso3bank = first(iso3bank),
    year = first(year),
    .groups = "drop"
  )


# ------------------------- HAVEN INDICATOR ------------------------------------

bis22 <- bis22 %>%
  mutate(
    haven = if_else(
      bank %in% havens,
      1,
      0
    )
  )


# ==============================================================================
# Household share of deposits
# ==============================================================================

share_household_dep <- readxl::read_excel(
  file.path(raw, "FGZ-raw-data.xlsx"),
  sheet = "sharehouseholddep",
  range = "A3:W25"
) %>%
  filter(
    year == 2022
  )


bis22 <- bis22 %>%
  left_join(
    share_household_dep,
    by = "year"
  )


# -------------------- ADJUST HOUSEHOLD DEPOSITS -------------------------------

adjustment_banks_22 <- c(
  "BE", "CH", "CW", "AN", "GG", "IM", "JE", "PA",
  "LU", "CY", "MO", "MY", "KY", "AT", "BH", "BM",
  "BS", "HK", "SG", "GB", "US", "CL"
)

for (b in adjustment_banks_22) {
  
  bis22 <- bis22 %>%
    mutate(
      value = if_else(
        bank == b & haven == 1,
        .data[[b]] * value,
        value
      )
    ) %>%
    select(
      -all_of(b)
    )
}


# ---------------------------- CLEAN DATA --------------------------------------

bis22 <- bis22 %>%
  filter(
    !is.na(iso3bank),
    iso3bank != "",
    !is.na(value)
  )


# ------------------------- COMPUTE SHARES -------------------------------------

tothaven_22 <- bis22 %>%
  filter(haven == 1) %>%
  summarise(
    total = sum(value, na.rm = TRUE)
  ) %>%
  pull(total)


bis22 <- bis22 %>%
  mutate(
    shvalue = if_else(
      haven == 1,
      value / tothaven_22,
      0
    ),
    namebank = if_else(
      namebank ==
        "United Kingdom of Great Britain and Northern Ireland",
      "United Kingdom",
      namebank
    )
  )


# ------------------------------- GRAPH ----------------------------------------

graph_22 <- bis22 %>%
  filter(haven == 1) %>%
  arrange(shvalue) %>%
  mutate(
    namebank = factor(
      namebank,
      levels = namebank
    )
  ) %>%
  ggplot(
    aes(
      x = namebank,
      y = shvalue
    )
  ) +
  geom_col() +
  geom_text(
    aes(label = namebank),
    angle = 90,
    hjust = -0.05,
    size = 3
  ) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, 0.5)
  ) +
  labs(
    title =
      "Cross-border household deposits in BIS-reporting tax havens, 2022",
    x = NULL,
    y = "% of deposits in BIS-reporting tax havens",
    caption =
      paste0(
        "Data source: BIS\n",
        "Note: We assume that in Switzerland 100% of deposits belong ",
        "to households,\nand only a fraction in the other tax haven."
      )
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )


# ----------------------------- EXPORT GRAPH -----------------------------------

ggsave(
  filename = file.path(
    fig,
    "shche-22.pdf"
  ),
  plot = graph_22,
  width = 8,
  height = 5
)

saveRDS(
  bis0607,
  file.path(work, "bis-graph-0607.rds")
)

saveRDS(
  bis22,
  file.path(work, "bis-graph-22.rds")
)

plot_data_0607 <- bis0607 %>%
  filter(haven == 1) %>%
  select(bank, namebank, value, shvalue)
saveRDS(plot_data_0607, file.path(work, "plot_data_0607.rds"))

plot_data_22 <- bis22 %>%
  filter(haven == 1) %>%
  select(bank, namebank, value, shvalue)
saveRDS(plot_data_22, file.path(work, "plot_data_22.rds"))

#2006–2007
bis0607 %>%
  filter(haven == 1) %>%
  select(year, bank, namebank, value, shvalue) %>%
  write.csv(
    file.path(work, "shche-06-07-r.csv"),
    row.names = FALSE,
    na = ""
  )

# 2022
bis22 %>%
  filter(haven == 1) %>%
  select(year, bank, namebank, value, shvalue) %>%
  write.csv(
    file.path(work, "shche-22-r.csv"),
    row.names = FALSE,
    na = ""
  )
