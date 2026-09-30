# ==============================================================================
# REPL: Global Offshore Wealth Using R, 2001-2021
# jlenke, 2026
# Translation of 6c-graph-offshore.do
# ==============================================================================

library(tidyverse)
library(readxl)
library(haven)
library(scales)


# ==============================================================================
# 0 ---- Helper functions
# ==============================================================================

plot_data_dir <- file.path(work, "plot-data", "r")
dir.create(plot_data_dir, recursive = TRUE, showWarnings = FALSE)

export_plot_data <- function(data, filename) {
  readr::write_csv(
    data,
    file.path(plot_data_dir, paste0(filename, ".csv")),
    na = ""
  )
}

# Read countries dataset and standardise GDP variable
read_countries <- function() {
  
  #x <- read_dta(file.path(work2, "countries.dta"))
  x <- read_work_data("countries")
  
  if ("gdp_current_dollars" %in% names(x)) {
    
    x <- x %>%
      mutate(
        gdp_used = gdp_current_dollars
      )
    
  } else if ("gdp" %in% names(x)) {
    
    x <- x %>%
      mutate(
        gdp_used = gdp
      )
    
  } else {
    
    stop(
      "Neither 'gdp_current_dollars' nor 'gdp' exists in countries.dta."
    )
  }
  
  x
}


# ---------------------------------------------------------------------------
# Country graph function
# ---------------------------------------------------------------------------

plot_country_offshore <- function(
    iso3_code,
    country_name,
    y_breaks,
    filename,
    point_shape = 16
) {
  
  data_country <- read_countries() %>%
    filter(
      iso3 == iso3_code,
      indicator == "total"
    ) %>%
    mutate(
      ratio_offshore_GDP =
        (value / (gdp_used / 1e9)) * 100
    )
  # export plot data files
  export_plot_data(
    data_country,
    tools::file_path_sans_ext(basename(filename))
  )
  #---
  p <- ggplot(
    data_country,
    aes(
      x = year,
      y = ratio_offshore_GDP
    )
  ) +
    geom_col(
      width = 0.7,
      fill = "#2E8B57"
    ) +
    geom_line(
      colour = "black",
      linewidth = 0.5
    ) +
    geom_point(
      colour = "black",
      shape = point_shape,
      size = 2.3
    ) +
    scale_y_continuous(
      breaks = y_breaks,
      labels = function(x) paste0(x, "%")
    ) +
    labs(
      x = NULL,
      y = paste0(
        "share of ",
        country_name,
        " GDP"
      )
    ) +
    theme_minimal() +
    
    scale_x_continuous(
      name = "Year",
      breaks = seq(2001, 2021, by = 2),
      limits = c(2000.5, 2021.5)
    ) +
    theme(
      axis.text.x = element_text(
        angle = 0,
        hjust = 0.5
      ),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = element_blank()
    )
  
  
  
  ggsave(
    file.path(
      fig,
      filename
    ),
    plot = p,
    device = "png",
    width = 7,
    height = 5
  )
}


# ==============================================================================
# I.1 ---- Country offshore wealth / GDP graphs
# ==============================================================================

# United States
plot_country_offshore(
  iso3_code = "USA",
  country_name = "United States",
  y_breaks = seq(4, 16, 2),
  filename = "offshore_gdpUSA.pdf",
  point_shape = 17
)


# United Kingdom
plot_country_offshore(
  iso3_code = "GBR",
  country_name = "United Kingdom",
  y_breaks = seq(10, 45, 5),
  filename = "offshore_gdpGBR.pdf",
  point_shape = 1
)


# Argentina
plot_country_offshore(
  iso3_code = "ARG",
  country_name = "Argentina",
  y_breaks = seq(0, 80, 10),
  filename = "offshore_gdpARG.pdf",
  point_shape = 18
)




# Colombia
plot_country_offshore(
  iso3_code = "COL",
  country_name = "Colombia",
  y_breaks = seq(0, 50, 10),
  filename = "offshore_gdpCOL.pdf",
  point_shape = 15
)


# Denmark
plot_country_offshore(
  iso3_code = "DNK",
  country_name = "Denmark",
  y_breaks = seq(0, 30, 5),
  filename = "offshore_gdpDNK.pdf",
  point_shape = 5
)


# South Africa
plot_country_offshore(
  iso3_code = "ZAF",
  country_name = "South Africa",
  y_breaks = seq(0, 25, 5),
  filename = "offshore_gdpZAF.pdf",
  point_shape = 4
)


# Taiwan
plot_country_offshore(
  iso3_code = "TWN",
  country_name = "Taiwan",
  y_breaks = seq(0, 120, 20),
  filename = "offshore_gdpTWN.pdf",
  point_shape = 17
)


# Israel
plot_country_offshore(
  iso3_code = "ISR",
  country_name = "Israel",
  y_breaks = seq(0, 80, 10),
  filename = "offshore_gdpISR.pdf",
  point_shape = 15
)


# Greece
plot_country_offshore(
  iso3_code = "GRC",
  country_name = "Greece",
  y_breaks = seq(0, 70, 10),
  filename = "offshore_gdpGRC.pdf",
  point_shape = 17
)


# Ireland
plot_country_offshore(
  iso3_code = "IRL",
  country_name = "Ireland",
  y_breaks = seq(0, 40, 5),
  filename = "offshore_gdpIRL.pdf",
  point_shape = 5
)


# Russia
plot_country_offshore(
  iso3_code = "RUS",
  country_name = "Russia",
  y_breaks = seq(0, 30, 5),
  filename = "offshore_gdpRUS.pdf",
  point_shape = 1
)


# ==============================================================================
# II ---- AJZ 2007 estimates vs FGZ smoothed 2007 estimates
# ==============================================================================

#offshore2007 <- read_dta(file.path(work2,"offshore2007.dta")) 
offshore2007 <- read_work_data("offshore2007")

offshore2007 <- offshore2007 %>%
  filter(
    bank == "CH",
    saver != "NG",
    !is.na(gdp),
    gdp > 200 * 1e9
  ) %>%
  mutate(
    global_offshore_wealth =
      5623.66447457748,
    
    ofw_in_switzerland =
      2666.95195303662,
    
    ofw_in_others_havens =
      2956.71252154086,
    
    offshore_smoothed2007 =
      sh_fidu_smthg *
      ofw_in_switzerland +
      sh_OC_smthg *
      ofw_in_others_havens
  ) %>%
  rename(
    iso3 = iso3saver
  ) %>%
  filter(
    offshore_smoothed2007 != 0
  )


ajz2007 <- read_excel(
  file.path(
    raw,
    "FGZ-raw-data.xlsx"
  ),
  sheet = "offshoreGDP2007",
  range = "A6:F44"
)


# Stata renames the imported variables manually.
# We first inspect/standardise them by position to avoid Excel-name differences.

names(ajz2007)[1:6] <- c(
  "namesaver",
  "country",
  "iso3",
  "Offshorewealthbn",
  "GDP",
  "OffshorewealthGDP2007"
)


ajz_comparison <- ajz2007 %>%
  select(
    -Offshorewealthbn
  ) %>%
  filter(
    !is.na(GDP)
  ) %>%
  rename(
    offshore_gdp_ajz2007 =
      OffshorewealthGDP2007
  ) %>%
  inner_join(
    offshore2007,
    by = "iso3"
  ) %>%
  select(
    iso3,
    country,
    GDP,
    offshore_smoothed2007,
    offshore_gdp_ajz2007
  ) %>%
  mutate(
    offshore_gdp_smoothed2007 =
      offshore_smoothed2007 / GDP
  )


ajz_long <- ajz_comparison %>%
  select(
    country,
    offshore_gdp_ajz2007,
    offshore_gdp_smoothed2007
  ) %>%
  pivot_longer(
    cols = -country,
    names_to = "estimate",
    values_to = "value"
  ) %>%
  mutate(
    estimate = recode(
      estimate,
      offshore_gdp_ajz2007 =
        "Alstadsæter, Johannesen, and Zucman (2018) Estimates",
      offshore_gdp_smoothed2007 =
        "Faye, Godar, and Zucman (2023) Weighted Moving Average Estimates"
    )
  )


country_order <- ajz_comparison %>%
  arrange(
    offshore_gdp_ajz2007
  ) %>%
  pull(country)


ajz_long <- ajz_long %>%
  mutate(
    country = factor(
      country,
      levels = country_order
    )
  )


p_ajz <- ggplot(
  ajz_long,
  aes(
    x = country,
    y = value,
    fill = estimate
  )
) +
  geom_col(
    position = position_dodge(
      width = 0.8
    ),
    width = 0.7,
    colour = "black",
    linewidth = 0.2
  ) +
  geom_hline(
    yintercept = 0.098,
    colour = "#2E8B57"
  ) +
  annotate(
    "text",
    x = Inf,
    y = 0.20,
    label = "World Average in 2007, AJZ & FGZ: 9.8%",
    hjust = 1.05,
    size = 3
  ) +
  scale_y_continuous(
    breaks = seq(0, 0.9, 0.1),
    labels = function(x) paste0(
      x * 100,
      "%"
    )
  ) +
  labs(
    x = NULL,
    y = "share of GDP",
    fill = NULL
  ) +
  theme_minimal() +
  
  scale_x_continuous(
    name = "Year",
    breaks = seq(2001, 2021, by = 2),
    limits = c(2000.5, 2021.5)
  ) 
  theme(
    axis.text.x = element_text(
      angle = 0,
      hjust = 0.5
    ),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank()
  )



ggsave(
  file.path(
    fig,
    "offshore-gdp-AJZvsFGZ.pdf"
  ),
  p_ajz,
  width = 10,
  height = 6
)

ggsave(
  file.path(
    fig,
    "offshore-gdp-AJZvsFGZ.png"
  ),
  p_ajz,
  width = 10,
  height = 6
)

# ==============================================================================
# III ---- Offshore Wealth in % of GDP: 2007 vs 2021
# ==============================================================================

countries <- read_countries()


countries2007 <- countries %>%
  filter(
    year == 2007,
    indicator == "total",
    gdp_used > 200 * 1e9,
    !is.na(gdp_used),
    value != 0
  ) %>%
  rename(
    gdp2007 = gdp_used,
    value2007 = value
  )


countries2021 <- countries %>%
  filter(
    year == 2021,
    indicator == "total"
  ) %>%
  rename(
    gdp2021 = gdp_used,
    value2021 = value
  )


comparison_2007_2021 <- countries2021 %>%
  inner_join(
    countries2007 %>%
      select(
        iso3,
        gdp2007,
        value2007
      ),
    by = "iso3"
  ) %>%
  mutate(
    ratio_offshore_GDP2007 =
      value2007 /
      (gdp2007 / 1e9),
    
    ratio_offshore_GDP2021 =
      value2021 /
      (gdp2021 / 1e9),
    
    country = case_when(
      iso3 == "ARE" ~ "UAE",
      iso3 == "GBR" ~ "UK",
      iso3 == "IRN" ~ "Iran",
      iso3 == "KOR" ~ "Korea",
      iso3 == "NLD" ~ "Netherlands",
      iso3 == "RUS" ~ "Russia",
      iso3 == "TWN" ~ "Taiwan",
      iso3 == "USA" ~ "USA",
      iso3 == "VEN" ~ "Venezuela",
      TRUE          ~ country_name
    )
  )


country_order <- comparison_2007_2021 %>%
  arrange(
    ratio_offshore_GDP2007
  ) %>%
  pull(country)


comparison_long <- comparison_2007_2021 %>%
  select(
    country,
    ratio_offshore_GDP2007,
    ratio_offshore_GDP2021
  ) %>%
  pivot_longer(
    cols = starts_with(
      "ratio_offshore"
    ),
    names_to = "year_group",
    values_to = "ratio"
  ) %>%
  mutate(
    country = factor(
      country,
      levels = country_order
    ),
    
    year_group = recode(
      year_group,
      ratio_offshore_GDP2007 =
        "Offshore wealth in 2007",
      ratio_offshore_GDP2021 =
        "Offshore wealth in 2021"
    )
  )


p_comparison <- ggplot(
  comparison_long,
  aes(
    x = country,
    y = ratio,
    fill = year_group
  )
) +
  geom_col(
    position = position_dodge(
      width = 0.8
    ),
    width = 0.7,
    colour = "black",
    linewidth = 0.2
  ) +
  geom_hline(
    yintercept = 0.1164736
  ) +
  geom_hline(
    yintercept = 0.1452904
  ) +
  annotate(
    "text",
    x = Inf,
    y = 0.40,
    label = "World Average in 2007: 11.6%",
    hjust = 1.05,
    size = 3
  ) +
  annotate(
    "text",
    x = Inf,
    y = 0.30,
    label = "World Average in 2021: 14.5%",
    hjust = 1.05,
    size = 3
  ) +
  scale_y_continuous(
    breaks = seq(
      0,
      1.5,
      0.1
    ),
    labels = function(x) paste0(
      round(x * 100),
      "%"
    )
  ) +
  labs(
    x = NULL,
    y = "share of GDP",
    fill = NULL
  ) +
  
  theme_minimal() +
  scale_x_discrete(
    name = "Country"
  ) +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1
    ),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank()
  )


ggsave(
  file.path(
    fig,
    "countries-offshore-gdp-2007-2021.pdf"
  ),
  p_comparison,
  width = 11,
  height = 6
)

ggsave(
  file.path(
    fig,
    "countries-offshore-gdp-2007-2021.png"
  ),
  p_comparison,
  width = 11,
  height = 6
)


# ==============================================================================
# IV ---- Evolution of global offshore wealth, 2001–2021
# ==============================================================================

world_offshore <- read_excel(
  file.path(
    raw,
    "FGZ-raw-data.xlsx"
  ),
  sheet = "T.A1",
  range = "A7:C28"
)


# Use column positions because Excel headings may differ from Stata names.
names(world_offshore)[1:3] <- c(
  "year",
  "world_gdp",
  "offshore_wealth"
)


world_offshore <- world_offshore %>%
  mutate(
    year = as.numeric(year),
    
    offshore_gdp =
      offshore_wealth *
      100 /
      world_gdp
  )


p_world <- ggplot(
  world_offshore,
  aes(
    x = year,
    y = offshore_gdp
  )
) +
  geom_line(
    linewidth = 0.7
  ) +
  geom_point(
    shape = 21,
    fill = "white",
    size = 2.3,
    stroke = 0.7
  ) +
  scale_x_continuous(
    breaks = 2001:2021
  ) +
  scale_y_continuous(
    breaks = seq(
      0,
      16,
      2
    ),
    labels = function(x) paste0(
      x,
      "%"
    )
  ) +
  coord_cartesian(
    ylim = c(
      0,
      17
    )
  ) +
  labs(
    x = NULL,
    y = "% of world GDP"
  ) +
  theme_minimal() +
  
  scale_x_continuous(
    name = "Year",
    breaks = seq(2001, 2021, by = 2),
    limits = c(2000.5, 2021.5)
  ) +
  scale_x_continuous(
    name = "Year",
    breaks = seq(2001, 2021, by = 2),
    limits = c(2000.5, 2021.5)
  ) +
  theme(
    axis.text.x = element_text(
      angle = 0,
      hjust = 0.5
    ),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank()
  )



ggsave(
  file.path(
    fig,
    "world-offshore-gdp-2001-2021.pdf"
  ),
  p_world,
  width = 7,
  height = 5
)

ggsave(
  file.path(
    fig,
    "world-offshore-gdp-2001-2021.png"
  ),
  p_world,
  width = 7,
  height = 5
)


# ==============================================================================
# V ---- Location of global offshore wealth
# ==============================================================================

haven_location <- read_excel(
  file.path(
    raw,
    "FGZ-raw-data.xlsx"
  ),
  sheet = "T.A2",
  range = "A4:G25"
)


# Standardise names by position.
names(haven_location)[1:7] <- c(
  "year",
  "Totaloffshorewealth",
  "Switzerland",
  "TaxhavensotherthanSwitzerland",
  "OfwhichAmericantaxhavens",
  "OfwhichAsiantaxhavens",
  "OfwhichEuropeantaxhavens"
)


haven_location <- haven_location %>%
  mutate(
    year = as.numeric(year),
    
    Switzerland =
      Switzerland * 100,
    
    OfwhichAmericantaxhavens =
      OfwhichAmericantaxhavens * 100,
    
    OfwhichAsiantaxhavens =
      OfwhichAsiantaxhavens * 100,
    
    OfwhichEuropeantaxhavens =
      OfwhichEuropeantaxhavens * 100
  )


haven_location_long <- haven_location %>%
  select(
    year,
    Switzerland,
    OfwhichAmericantaxhavens,
    OfwhichAsiantaxhavens,
    OfwhichEuropeantaxhavens
  ) %>%
  pivot_longer(
    cols = -year,
    names_to = "haven_group",
    values_to = "share"
  ) %>%
  mutate(
    haven_group = recode(
      haven_group,
      Switzerland =
        "Switzerland",
      OfwhichAmericantaxhavens =
        "American tax havens",
      OfwhichAsiantaxhavens =
        "Asian tax havens",
      OfwhichEuropeantaxhavens =
        "Other European tax havens"
    )
  )


p_location <- ggplot(
  haven_location_long,
  aes(
    x = year,
    y = share,
    group = haven_group,
    linetype = haven_group,
    shape = haven_group
  )
) +
  geom_line(
    linewidth = 0.5
  ) +
  geom_point(
    size = 2
  ) +
  scale_x_continuous(
    breaks = 2001:2021
  ) +
  scale_y_continuous(
    breaks = seq(
      0,
      45,
      5
    ),
    labels = function(x) paste0(
      x,
      "%"
    )
  ) +
  labs(
    x = NULL,
    y = "% of the wealth held in all tax havens",
    linetype = NULL,
    shape = NULL
  ) +
  theme_minimal() +
  
  scale_x_continuous(
    name = "Year",
    breaks = seq(2001, 2021, by = 2),
    limits = c(2000.5, 2021.5)
  ) +
  
  theme(
    axis.text.x = element_text(
      angle = 0,
      hjust = 0.5
    ),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank()
  )

ggsave(
  file.path(
    fig,
    "offshore-location-global-wealth.pdf"
  ),
  p_location,
  width = 7,
  height = 5
)

ggsave(
  file.path(
    fig,
    "offshore-location-global-wealth.png"
  ),
  p_location,
  width = 7,
  height = 5
)


# ==============================================================================
# VI ---- Location of offshore wealth as % of world GDP
# ==============================================================================

offshore_haven_groups <- read_excel(
  file.path(
    raw,
    "FGZ-raw-data.xlsx"
  ),
  sheet = "T.A2",
  range = "H4:N26"
)


names(offshore_haven_groups)[1:7] <- c(
  "year",
  "Totaloffshorewealth",
  "Switzerland",
  "TaxhavensotherthanSwitzerland",
  "OfwhichAmericantaxhavens",
  "OfwhichAsiantaxhavens",
  "OfwhichEuropeantaxhavens"
)


offshore_haven_groups <- offshore_haven_groups %>%
  mutate(
    year = as.numeric(year)
  )


world_gdp <- read_excel(
  file.path(
    raw,
    "FGZ-raw-data.xlsx"
  ),
  sheet = "T.A1",
  range = "A7:B29"
)


names(world_gdp)[1:2] <- c(
  "year",
  "world_gdp"
)


world_gdp <- world_gdp %>%
  mutate(
    year = as.numeric(year)
  )


offshore_world_gdp <- full_join(
  world_gdp,
  offshore_haven_groups,
  by = "year"
) %>%
  mutate(
    swiss_havens =
      Switzerland *
      100 /
      world_gdp,
    
    other_european_havens =
      OfwhichEuropeantaxhavens *
      100 /
      world_gdp,
    
    american_havens =
      OfwhichAmericantaxhavens *
      100 /
      world_gdp,
    
    asian_havens =
      OfwhichAsiantaxhavens *
      100 /
      world_gdp
  )


offshore_world_gdp_long <- offshore_world_gdp %>%
  select(
    year,
    swiss_havens,
    american_havens,
    asian_havens,
    other_european_havens
  ) %>%
  pivot_longer(
    cols = -year,
    names_to = "haven_group",
    values_to = "share"
  ) %>%
  mutate(
    haven_group = recode(
      haven_group,
      swiss_havens =
        "Switzerland",
      american_havens =
        "American tax havens",
      asian_havens =
        "Asian tax havens",
      other_european_havens =
        "Other European tax havens"
    )
  )


p_world_location <- ggplot(
  offshore_world_gdp_long,
  aes(
    x = year,
    y = share,
    group = haven_group,
    linetype = haven_group,
    shape = haven_group
  )
) +
  geom_line(
    linewidth = 0.5
  ) +
  geom_point(
    size = 2
  ) +
  scale_x_continuous(
    breaks = 2001:2021
  ) +
  scale_y_continuous(
    breaks = 0:7,
    labels = function(x) paste0(
      x,
      "%"
    )
  ) +
  labs(
    x = NULL,
    y = "% of world GDP",
    linetype = NULL,
    shape = NULL
  ) +
  theme_minimal() +
  
  scale_x_continuous(
    name = "Year",
    breaks = seq(2001, 2021, by = 2),
    limits = c(2000.5, 2021.5)
  ) +
  theme(
    axis.text.x = element_text(
      angle = 0,
      hjust = 0.5
    ),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank()
  )


ggsave(
  file.path(
    fig,
    "offshore_location_world_gdp.pdf"
  ),
  p_world_location,
  width = 7,
  height = 5
)

ggsave(
  file.path(
    fig,
    "offshore_location_world_gdp.png"
  ),
  p_world_location,
  width = 7,
  height = 5
)

# ==============================================================================
# VII ---- Offshore wealth owned by income-country groups
# ==============================================================================

income_data <- read_countries() %>%
  filter(
    indicator == "total",
    year != 2022
  )


# Stata:
#
# gen world_gdp = 0
# forvalues j = 2001/2021 {
#     su gdp if year == `j'
#     replace world_gdp = r(sum) if year == `j'
# }

world_gdp_year <- income_data %>%
  group_by(
    year
  ) %>%
  summarise(
    world_gdp =
      sum(
        gdp_used,
        na.rm = TRUE
      ),
    .groups = "drop"
  )


income_data <- income_data %>%
  left_join(
    world_gdp_year,
    by = "year"
  )


# Stata collapse (sum) gdp value,
# by(year incomelevelname world_gdp)

income_data <- income_data %>%
  group_by(
    year,
    incomelevelname,
    world_gdp
  ) %>%
  summarise(
    gdp =
      sum(
        gdp_used,
        na.rm = TRUE
      ),
    
    value =
      sum(
        value,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  ) %>%
  mutate(
    incomelevelname = case_when(
      incomelevelname ==
        "Upper middle income" ~
        "upper_middle",
      
      incomelevelname ==
        "High income" ~
        "high",
      
      incomelevelname ==
        "Low income" ~
        "low",
      
      incomelevelname ==
        "Lower middle income" ~
        "lower_middle",
      
      incomelevelname ==
        "Unclassified" ~
        "unclassified",
      
      TRUE ~ incomelevelname
    )
  )


# Annual global offshore wealth
ofw_year <- income_data %>%
  group_by(
    year
  ) %>%
  summarise(
    ofw =
      sum(
        value,
        na.rm = TRUE
      ),
    .groups = "drop"
  )


income_data <- income_data %>%
  left_join(
    ofw_year,
    by = "year"
  ) %>%
  mutate(
    sh_ofw_total =
      value *
      100 /
      ofw,
    
    sh_ofw_gdp =
      value *
      100 /
      (world_gdp / 1e9),
    
    sh_world_gdp =
      gdp *
      100 /
      world_gdp
  ) %>%
  select(
    -value,
    -gdp,
    -ofw
  )


# Stata reshape wide
income_wide <- income_data %>%
  pivot_wider(
    id_cols = c(
      year,
      world_gdp
    ),
    names_from = incomelevelname,
    values_from = c(
      sh_ofw_total,
      sh_ofw_gdp,
      sh_world_gdp
    ),
    names_sep = ""
  )


# Ensure all Stata variables exist
needed_income_vars <- c(
  "sh_ofw_totallow",
  "sh_ofw_totallower_middle",
  "sh_ofw_totalupper_middle",
  "sh_ofw_totalunclassified",
  
  "sh_ofw_gdplow",
  "sh_ofw_gdplower_middle",
  "sh_ofw_gdpupper_middle",
  "sh_ofw_gdpunclassified",
  
  "sh_world_gdplow",
  "sh_world_gdplower_middle",
  "sh_world_gdpupper_middle",
  "sh_world_gdpunclassified"
)


for (v in needed_income_vars) {
  
  if (!v %in% names(income_wide)) {
    income_wide[[v]] <- NA_real_
  }
}


# This reproduces Stata's ordinary addition:
# if one component is missing, the result is missing.

income_wide <- income_wide %>%
  mutate(
    sh_ofw_total_low_middle_inc =
      sh_ofw_totallow +
      sh_ofw_totallower_middle +
      sh_ofw_totalupper_middle +
      sh_ofw_totalunclassified,
    
    sh_ofw_gdp_low_middle_inc =
      sh_ofw_gdplow +
      sh_ofw_gdplower_middle +
      sh_ofw_gdpupper_middle +
      sh_ofw_gdpunclassified,
    
    sh_world_gdp_low_middle_inc =
      sh_world_gdplow +
      sh_world_gdplower_middle +
      sh_world_gdpupper_middle +
      sh_world_gdpunclassified
  ) %>%
  select(
    year,
    sh_ofw_gdphigh,
    sh_ofw_totalhigh,
    sh_ofw_gdp_low_middle_inc,
    sh_ofw_total_low_middle_inc,
    sh_world_gdphigh,
    sh_world_gdp_low_middle_inc
  )


# ==============================================================================
# VII.1 ---- Share of total offshore wealth
# ==============================================================================

income_total_long <- income_wide %>%
  select(
    year,
    sh_ofw_totalhigh,
    sh_ofw_total_low_middle_inc
  ) %>%
  pivot_longer(
    cols = -year,
    names_to = "income_group",
    values_to = "share"
  ) %>%
  mutate(
    income_group = recode(
      income_group,
      sh_ofw_totalhigh =
        "High income countries",
      sh_ofw_total_low_middle_inc =
        "Middle- and low-income countries"
    )
  )


p_income_total <- ggplot(
  income_total_long,
  aes(
    x = year,
    y = share,
    group = income_group,
    linetype = income_group,
    shape = income_group
  )
) +
  geom_line(
    linewidth = 0.8
  ) +
  geom_point(
    size = 2
  ) +
  scale_x_continuous(
    breaks = 2001:2021
  ) +
  scale_y_continuous(
    breaks = seq(
      0,
      80,
      20
    ),
    labels = function(x) paste0(
      x,
      "%"
    )
  ) +
  labs(
    x = NULL,
    y = "% of total offshore wealth",
    linetype = NULL,
    shape = NULL
  ) +
  theme_minimal() +
  
  scale_x_continuous(
    name = "Year",
    breaks = seq(2001, 2021, by = 2),
    limits = c(2000.5, 2021.5)
  ) +
  theme(
    axis.text.x = element_text(
      angle = 0,
      hjust = 0.5
    ),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank()
  )



ggsave(
  file.path(
    fig,
    "ofw-owned-income-level-total-ofw.pdf"
  ),
  p_income_total,
  width = 7,
  height = 5
)

ggsave(
  file.path(
    fig,
    "ofw-owned-income-level-total-ofw.png"
  ),
  p_income_total,
  width = 7,
  height = 5
)

# ==============================================================================
# VII.2 ---- Offshore wealth as % of world GDP
# ==============================================================================

income_gdp_long <- income_wide %>%
  select(
    year,
    sh_ofw_gdphigh,
    sh_ofw_gdp_low_middle_inc
  ) %>%
  pivot_longer(
    cols = -year,
    names_to = "income_group",
    values_to = "share"
  ) %>%
  mutate(
    income_group = recode(
      income_group,
      sh_ofw_gdphigh =
        "High income countries",
      sh_ofw_gdp_low_middle_inc =
        "Middle and low income countries"
    )
  )


p_income_gdp <- ggplot(
  income_gdp_long,
  aes(
    x = year,
    y = share,
    group = income_group,
    linetype = income_group,
    shape = income_group
  )
) +
  geom_line(
    linewidth = 0.8
  ) +
  geom_point(
    size = 2
  ) +
  scale_x_continuous(
    breaks = 2000:2021
  ) +
  scale_y_continuous(
    breaks = seq(
      0,
      12,
      2
    ),
    labels = function(x) paste0(
      x,
      "%"
    )
  ) +
  labs(
    x = NULL,
    y = "% of world GDP",
    linetype = NULL,
    shape = NULL
  ) +
  theme_minimal() +
  
  scale_x_continuous(
    name = "Year",
    breaks = seq(2001, 2021, by = 2),
    limits = c(2000.5, 2021.5)
  ) +
  theme(
    axis.text.x = element_text(
      angle = 0,
      hjust = 0.5
    ),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank()
  )



ggsave(
  file.path(
    fig,
    "ofw_owned_incomelevel_gdp.pdf"
  ),
  p_income_gdp,
  width = 7,
  height = 5
)

ggsave(
  file.path(
    fig,
    "ofw_owned_incomelevel_gdp.png"
  ),
  p_income_gdp,
  width = 7,
  height = 5
)

# ==============================================================================
# VII.3 ---- GDP shares by income-country group
# ==============================================================================

income_world_gdp_long <- income_wide %>%
  select(
    year,
    sh_world_gdphigh,
    sh_world_gdp_low_middle_inc
  ) %>%
  pivot_longer(
    cols = -year,
    names_to = "income_group",
    values_to = "share"
  ) %>%
  mutate(
    income_group = recode(
      income_group,
      sh_world_gdphigh =
        "High income countries",
      sh_world_gdp_low_middle_inc =
        "Middle and low income countries"
    )
  )


p_income_world_gdp <- ggplot(
  income_world_gdp_long,
  aes(
    x = year,
    y = share,
    group = income_group,
    linetype = income_group,
    shape = income_group
  )
) +
  geom_line(
    linewidth = 0.8
  ) +
  geom_point(
    size = 2
  ) +
  scale_x_continuous(
    breaks = 2000:2021
  ) +
  scale_y_continuous(
    breaks = seq(
      0,
      100,
      20
    ),
    labels = function(x) paste0(
      x,
      "%"
    )
  ) +
  labs(
    x = NULL,
    y = "% of world GDP",
    linetype = NULL,
    shape = NULL
  ) +
  theme_minimal() +
  
  scale_x_continuous(
    name = "Year",
    breaks = seq(2001, 2021, by = 2),
    limits = c(2000.5, 2021.5)
  ) +
  theme(
    axis.text.x = element_text(
      angle = 0,
      hjust = 0.5
    ),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank()
  )


ggsave(
  file.path(
    fig,
    "share-gdp-income-country-groups.pdf"
  ),
  p_income_world_gdp,
  width = 7,
  height = 5
)

ggsave(
  file.path(
    fig,
    "share-gdp-income-country-groups.png"
  ),
  p_income_world_gdp,
  width = 7,
  height = 5
)

# ==============================================================================
# VIII ---- Finished
# ==============================================================================

cat(
  "\n6c-graph-offshore.R finished.\n",
  "Graphs saved to:\n",
  fig,
  "\n"
)

# --- save plot data
export_plot_data(ajz_long, "offshore-gdp-AJZvsFGZ")

export_plot_data(
  comparison_long,
  "countries-offshore-gdp-2007-2021"
)

export_plot_data(
  world_offshore,
  "world-offshore-gdp-2001-2021"
)

export_plot_data(
  haven_location_long,
  "offshore-location-global-wealth"
)

export_plot_data(
  offshore_world_gdp_long,
  "offshore_location_world_gdp"
)

export_plot_data(
  income_total_long,
  "ofw-owned-income-level-total-ofw"
)

export_plot_data(
  income_gdp_long,
  "ofw_owned_incomelevel_gdp"
)

export_plot_data(
  income_world_gdp_long,
  "share-gdp-income-country-groups"
)

