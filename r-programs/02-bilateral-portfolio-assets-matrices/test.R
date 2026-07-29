tmp <- df %>% filter(ofc_source == 0, ofc_host == 0)

tmp %>%
  summarise(
    n = n(),
    logeqasset_nonmissing = sum(!is.na(logeqasset)),
    complete_cases = sum(complete.cases(
      logeqasset, logdist, gap_lon, comlang_off, col45, industrial,
      loggap_gdp, loggap_gdppc, lat_source, landlocked_source,
      logpop_source, loggdppc_source, year, host
    ))
  )

tmp %>%
  summarise(across(
    c(logeqasset, logdist, gap_lon, comlang_off, col45, industrial,
      loggap_gdp, loggap_gdppc, lat_source, landlocked_source,
      logpop_source, loggdppc_source, year, host),
    ~ sum(is.na(.x))
  )) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_missing") %>%
  arrange(desc(n_missing))

tmp_reg <- tmp %>%
  filter(if_all(
    c(logeqasset, logdist, gap_lon, comlang_off, col45, industrial,
      loggap_gdp, loggap_gdppc, lat_source, landlocked_source,
      logpop_source, loggdppc_source, year, host),
    ~ !is.na(.x)
  ))

nrow(tmp_reg)

mod_eq_bench <- feols(
  logeqasset ~ logdist + gap_lon + comlang_off + col45 + industrial +
    loggap_gdp + loggap_gdppc + lat_source + landlocked_source +
    logpop_source + loggdppc_source | year + host,
  data = tmp_reg
)