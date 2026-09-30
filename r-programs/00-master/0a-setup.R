# ==============================================================================
# REPL: Global Offshore Wealth Using R, 2001-2021
# jlenke, 2026
#
# This program creates working directories macros necessary to run all programs.
#
#===============================================================================

# TO DO: 
# - libraries werden auch in den nachgelagerten Dateien immer wieder
#   geladen, die könnte man hier auch alle zusammen reinstecken
# - es liegt noch viel debug code herum aus der Tüftelzeit der Übersetzungen,
#   müsste man in Ruhe putzen
# - viele Output Tabellen vor allem in der Validationsphase werden auch in den
#   R Dateien generiert, diese sind noch nciht rdentlich dokumentiert
# - README.md im Repo erweitern, das ist noch etwas schmalbrüstig


# ------------------------------ PATHS -----------------------------------------

# Main directory
root <- "/Users/jule/Library/CloudStorage/Dropbox/UNI/WiWi/BSc/global-offshore-wealth-r"

# Code files path
do <- file.path(root, "r-programs")

# Created data path
work <- file.path(root, "work-data")

# Created data path
work2 <- file.path(root, "../gow01-21/work-data")

# Raw data path
raw <- file.path(root, "../gow01-21/raw-data")

# Figures path
fig <- file.path(root, "figures")

# Tables path
tables <- file.path(root, "tables")


# ------------ SWITCH: use original (dta) or replication (rds) files? ----------
data_mode <- "r"
#data_mode <- "stata"

read_work_data <- function(filename) {
  
  if (data_mode == "stata") {
    path <- file.path(work2, paste0(filename, ".dta"))
    reader <- haven::read_dta
    
  } else if (data_mode == "r") {
    path <- file.path(work, paste0(filename, ".rds"))
    reader <- readRDS
    
  } else {
    stop("data_mode must be either 'stata' or 'r'.")
  }
  
  if (!file.exists(path)) {
    stop("File not found: ", path)
  }
  
  message("Reading: ", path)
  
  reader(path)
}

# ------ berechnung der Abweichungen zwischen der R und der STATA implementation

replication_metrics <- function(original, replicated) {
  
  valid <- !is.na(original) & !is.na(replicated)
  
  original <- original[valid]
  replicated <- replicated[valid]
  
  diff <- replicated - original
  
  relative <- rep(NA_real_, length(diff))
  nonzero <- original != 0
  
  relative[nonzero] <- 100 * diff[nonzero] /
    original[nonzero]
  
  tibble::tibble(
    n = length(diff),
    MAE = mean(abs(diff)),
    MAPE = if (any(nonzero)) {
      mean(abs(relative[nonzero]))
    } else {
      NA_real_
    },
    max_absolute_deviation = max(abs(diff)),
    mean_signed_deviation = mean(diff)
  )
}

# -----
# Untenstehend braucht man nur einmalig wenn man die Zucman Dateien lädt
# Danach löscht das STATA Original die zip files, das ist nicht sinnvoll, falls
# man nochmal von vorne anfangen muss (wie ich *sehr* oft)
# deswegen auskommentiert, folt aber eigenbtlich so der Stata Logik
#------



# ----------------------- EXTRACT ZIPPED DATA FILE -----------------------------

# Set working directory to the Zucman raw-data folder
#setwd(file.path(raw, "Zucman"))

# Unzip data_gravity.zip into the current directory
#zip1 <- file.path(raw, "Zucman", "data_gravity.zip")
#unzip(zip1)

# Delete the zip file after extraction
#file.remove(zip1)

# Set working directory to the Gravity_dta_V202211 folder
#setwd(file.path(raw, "Gravity_dta_V202211"))

# Unzip Gravity_V202211.zip into the current directory
#zip2 <- file.path(raw, "Gravity_dta_V202211", "Gravity_V202211.zip")
#unzip(zip2)

# Delete the zip file after extraction
#file.remove(zip2)

