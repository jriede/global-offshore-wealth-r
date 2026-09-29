# 8a-validation.R
#source(file.path(do, "0a-setup.R"))

library(dplyr)
library(haven)

# --------------------------------------------------
# 1a_import_EWN: data_ewn_update
# --------------------------------------------------

stata <- read_dta(file.path(work2, "data_ewn_update.dta"))
repl <- read_work_data("data_ewn_update")

# 1. Dimensionen
dim(stata)
dim(repl)

# 2. Spaltennamen
setdiff(names(stata), names(repl))
setdiff(names(repl), names(stata))

# 3. Schlüssel prüfen
stata %>%
  count(source, year) %>%
  filter(n > 1)

repl %>%
  count(source, year) %>%
  filter(n > 1)

# 4. Datensätze anhand der Schlüssel vergleichen
comparison <- full_join(
  stata,
  repl,
  by = c("source", "year"),
  suffix = c("_stata", "_r")
)

# 5. Fehlende Schlüssel auf beiden Seiten
anti_join(
  stata, repl,
  by = c("source", "year")
)

anti_join(
  repl, stata,
  by = c("source", "year")
)

# 6. Beispiel: quantitative Abweichung Equity Assets
replication_metrics(
  comparison$aequity_stata,
  comparison$aequity_r
)

