pivot_set <- function(set) {
  set %>%
    pivot_longer(
      cols = -Name,
      names_to = c("year", "week"),
      names_sep = "-",
      values_to = "cumulative_cases") %>%
    rename(name = Name)
}

join_set <- function(set) {
  read_csv("raw_data/Predictor Variables.csv") %>%
    clean_names %>% 
    left_join(set)
}

specify_data_type <- function(set) {
  set %>%
    mutate(
      across(
        c(
          level_of_segregation_during_collection,
          slf_site_operational,
          status_of_first_dumpsite,
          status_of_second_dumpsite),
        as_factor),
      across(
        c(
          no_of_mr_fs,
          no_of_brgys_served_by_mrf,
          no_of_operational_slf,
          no_of_lg_us_served_by_slf,
          no_of_brgys,
          population,
          year,
          week),
        as.integer
      ))
}

prepare_set <- function(raw_file, save_file) {
  raw_file %>%
    read_csv %>%
    pivot_set %>%
    join_set %>%
    specify_data_type %>% 
    saveRDS(save_file)
}

