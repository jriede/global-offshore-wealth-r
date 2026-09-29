# 2 full matrices
Zeilen

	

Verarbeitung

91–111

	

Konstruktion von ofc_source

161–174

	

Equity- und Debt-Gravity-Regressionen

179–187

	

Berechnung von eqp und debtp

193–220

	

Normalisierung zu shareeqp und sharedebtp

1088–1113

	

Allokation der Assets von Nicht-CPIS-Ländern

# A1
## E
Spalte E von Tabelle A.1 enthält die ergänzten Portfolio-Assets der Cayman Islands, getrennt nach Equity und Debt. Im R-Skript 3_do_table_A1.R werden dafür augmeqasset und augmdebtasset aus data_full_matrices verwendet, jeweils für source == 377. 
Die Werte werden durch 1.000 geteilt und in Spalte E geschrieben.
Im Stata-Skript wird die Cayman-Korrektur in mehreren Schritten berechnet: zunächst die US-Position anhand der TIC-Daten, anschließend die Anteile der USA und die Verteilung auf andere Zielländer. Besonders relevant sind die Zeilen 249–330 in 2_do_full_matrices.do.


cayman_A1 <- data_full_matrices %>%
  filter(source == 377) %>%
  group_by(year) %>%
  summarise(
    equity_E = sum(augmeqasset, na.rm = TRUE) / 1000,
    debt_E = sum(augmdebtasset, na.rm = TRUE) / 1000,
    .groups = "drop"
  ) %>%
  arrange(year)

print(cayman_A1, n = Inf, digits = 12)

## Spalte D

## G
Bei Spalte G („Other CPIS“) handelt es sich um eine Restgröße. Anders als bei Spalte D oder E können wir deshalb nicht einfach einen einzelnen source-Code aufsummieren.

Die Berechnung in deinem R-Skript lautet:

G
t
s
	​

=
1000
1
	​

	​

i:CPIS
i
	​

=1
∑
	​

(A
it
s,aug
	​

−A
it
s
	​

)−
i∈{377,924}
∑
	​

(A
it
s,aug
	​

−A
it
s
	​

)
	​

,

wobei s Equity oder Debt bezeichnet. Von der gesamten Korrektur für CPIS-meldende Länder werden also die separat ausgewiesenen Korrekturen für die Cayman Islands (377) und China (924) abgezogen.

# P

data_full_matrices <- read_work_data("data_full_matrices")

a1_col_p <- data_full_matrices %>%
  filter(source == 9994) %>%
  group_by(year) %>%
  summarise(
    equity = sum(augmeqasset, na.rm = TRUE) / 1000,
    debt = sum(augmdebtasset, na.rm = TRUE) / 1000,
    .groups = "drop"
  )

print(a1_col_p, n = Inf, digits = 12)

# A tibble: 21 × 3
    year equity  debt
   <dbl>  <dbl> <dbl>
 1  2001   1.79  133.
 2  2002   2.25  167.
 3  2003   2.90  214.
 4  2004   3.54  262.
 5  2005   4.15  307.
 6  2006   4.86  360.
 7  2007   5.59  413.
 8  2008   6.29  465.
 9  2009   6.68  494.
10  2010   7.13  528.
11  2011   7.28  538.
12  2012   7.86  581.
13  2013   8.25  610.
14  2014   8.04  595.
15  2015   7.52  556.
16  2016   7.37  546.
17  2017   7.65  566.
18  2018   7.59  562.
19  2019   7.77  575.
20  2020   8.46  626.
21  2021   8.13  602.
