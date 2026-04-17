# ==============================================================================
# REPL: Global Offshore Wealth, 2001-2021
# jlenke, 2026-04-14
#
# This program creates working directories macros necessary to run all programs.
#
#===============================================================================

# ------------------------------ PATHS -----------------------------------------

# Main directory
root <- "/Users/jule/Library/CloudStorage/Dropbox/UNI/WiWi/BSc/data/global-offshore-wealth-2001-2021"

# Code files path
do_path <- file.path(root, "r-programs")

# Created data path
work <- file.path(root, "work-data")

# Raw data path
raw <- file.path(root, "raw-data")

# Figures path
fig <- file.path(root, "figures")

# Tables path
tables <- file.path(root, "tables")


# ----------------------- EXTRACT ZIPPED DATA FILE -----------------------------

# Set working directory to the Zucman raw-data folder
setwd(file.path(raw, "Zucman"))

# Unzip data_gravity.zip into the current directory, overwriting existing files
zip1 <- file.path(raw, "Zucman", "data_gravity.zip")
unzip(zip1, overwrite = TRUE)

# Delete the zip file after extraction
#file.remove(zip1)

# Set working directory to the Gravity_dta_V202211 folder
setwd(file.path(raw, "Gravity_dta_V202211"))

# Unzip Gravity_V202211.zip into the current directory, overwriting existing files
zip2 <- file.path(raw, "Gravity_dta_V202211", "Gravity_V202211.zip")
unzip(zip2, overwrite = TRUE)

# Delete the zip file after extraction
file.remove(zip2)

