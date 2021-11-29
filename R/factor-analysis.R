library(magrittr, include.only = "%>%")

# Create vector to load data
datasets <- c("clean", "train", "test")

# Create list of datasets and separate into R environment
da_list <- purrr::map(
  datasets, ~ readr::read_rds(paste0("data/da_", .x, ".rds"))
) %>%
  purrr::set_names(datasets) %>%
  list2env(
    datasets,
    envir = .GlobalEnv)

# Create subset with just HCL-32 items
train_hcl <- train %>%
  dplyr::select(
    dplyr::starts_with("va")
  )

test_hcl <- test %>%
  dplyr::select(
    dplyr::starts_with("va")
  )

# KMO and Bartlett's test of sphericity
parameters::check_factorstructure(
  clean %>%
    dplyr::select(dplyr::starts_with("va"))
)

# Exploratory factor analysis
efa <- psych::fa(
  train_hcl,
  nfactors = 2,
  cor = "tet",
  fm = "pa",
  rotate = "geominQ") %>%
  parameters::model_parameters(threshold = 0.3)

# Print output of exploratory factor analysis
efa

# Create confirmatory factor analysis models with lavaan syntax
efa_mod <- efa %>%
  parameters::efa_to_cfa(names = c("Active", "Risk"))

bech_mod <- "
Active =~ va2 + va28 + va11 + va5 + va18 + va4 + va15 + va20 + va10 + va1
Risk =~ va23 + va8 + va9 + va25 + va21 + va31 + va29 + va7 + va27 + va32
"

forty_mod <- "
Active =~ va1 + va4 + va6 + va10 + va13 + va17 + va19 + va20 + va28
Risk =~ va8 + va9 + va14 + va27 + va30 + va31 + va32
"

# Fit the three models
efa_fit <- lavaan::cfa(
  model = two_factor_str,
  data = test_hcl,
  ordered = names(test_hcl),
  estimator = "WLSMV"
  )

bech_fit <- lavaan::cfa(
  model = bech_mod,
  data = test_hcl,
  ordered = names(test_hcl),
  estimator = "WLSMV"
  )

forty_fit <- lavaan::cfa(
  model = forty_mod,
  data = test_hcl,
  ordered = names(test_hcl),
  estimator = "WLSMV"
  )

# Compare model based on fit indices
semTable::compareLavaan(
  models = list(efa_fit, bech_fit, forty_fit),
  fitmeas = c("rmsea", "cfi", "tli", "srmr", "gfi"),
  scaled = TRUE,
  digits = 3
) %>%
  tibble::as_tibble(rownames = "Model") %>%
  dplyr::select(-dchi, -ddf, -npval) %>%
  dplyr::rename(
    RMSEA = rmsea.scaled,
    CFI = cfi.scaled,
    TLI = tli.scaled,
    SRMR = srmr,
    GFI = gfi
    ) %>%
  dplyr::mutate(
    Model = dplyr::case_when(
      Model == "Model3" ~ "Forty et al. (2010)",
      Model == "Model2" ~ "Bech et al. (2011)",
      Model == "Model1" ~ "Exploratory analysis"
    )
  )
