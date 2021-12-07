base <- clean |>
  dplyr::mutate(
    maniahipo_t1 = tidyr::replace_na(maniahipo_t1, 0)
  ) |>
  dplyr::select(
    maniahipo_t1, dplyr::starts_with("va")
  ) |>
  dplyr::mutate(
    dplyr::across(dplyr::everything(), as.factor)
  )

tabela <- tableone::CreateCatTable(
 vars = names(base)[-1],
  strata = names(base)[1],
  data = base
)

tabela_limpa <- print(tabela) |>
  tibble::as_tibble(rownames = "Item") |>
  dplyr::select(-test) |>
  dplyr::rename(
    Control = `0`,
    Experimental = `1`
  ) |>
  dplyr::mutate(
    Experimental = stringr::str_remove(Experimental, "^ +"),
    Experimental = stringr::str_remove(Experimental, " +$"),
    Control = stringr::str_remove(Control, " +$"),
    p = stringr::str_remove(p, "^ +"),
    Item = substr(Item, 1, 4),
    Item = stringr::str_remove_all(Item, " |va")
  )

tabela_limpa
