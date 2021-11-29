library(magrittr, include.only = "%>%")

# Read dataset
da_raw <- haven::read_spss(
  "data/banco-suelen-coorte-t1-t2.sav"
)

# Pipeline to clean the data
da_clean <- da_raw %>%
  janitor::clean_names() %>%
  dplyr::mutate(
    dplyr::across(dplyr::starts_with("hcl3"),
      ~ ifelse(.x > 1, NA, .x))
  ) %>%
  dplyr::filter(dplyr::across(dplyr::starts_with("hcl3"), ~ !is.na(.x))) %>%
  dplyr::rename_with(.cols = dplyr::starts_with("hcl3"),
    ~ stringr::str_replace_all(stringr::str_remove_all(
      string = .x, pattern = "_t1"), "hcl3", "va"
    ))

# Split into training and test data
set.seed(666)
da_split <- rsample::initial_split(da_clean, prop = 0.7, strata = bipolar_conferido)
da_train <- rsample::training(da_split)
da_test <- rsample::testing(da_split)

# Export full, train and test datasets
readr::write_rds(da_clean, file = "data/da_clean.rds")
readr::write_rds(da_train, file = "data/da_train.rds")
readr::write_rds(da_test, file = "data/da_test.rds")
