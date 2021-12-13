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

# Test column plot
tabela_limpa |>
  dplyr::select(Item, Control, Experimental) |>
  dplyr::slice(-1) |>
  dplyr::rename(
    item = Item,
    ct = Control,
    ex = Experimental
  ) |>
  dplyr::mutate(dplyr::across(c(ct, ex),
    ~ as.numeric(stringr::str_trim(stringr::str_remove_all(
      stringr::str_extract(.x, "\\(([^)]+)\\)"),
      "\\(|\\)")
    )))
  ) |>
  tidyr::pivot_longer(
    cols = c(ct, ex),
    names_to = "group",
    values_to = "freq"
  ) |>
  dplyr::mutate(
    freq = freq / 100,
    item = factor(item, levels = 32:1),
    group = ifelse(group == "ct", "Control", "Experimental")) |>
  ggplot2::ggplot(
    ggplot2::aes(y = item, x = freq, fill = group)
  ) +
  ggplot2::geom_col(position = "dodge") +
  ggplot2::scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    n.breaks = 10) +
  ggplot2::scale_fill_viridis_d(name = "") +
  ggplot2::labs(x = "Answers with \"yes\" (%)", y = "HCL-32 item") +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    text = ggplot2::element_text(size = 14))

# Test line plot
tabela_limpa |>
  dplyr::select(Item, Control, Experimental) |>
  dplyr::slice(-1) |>
  dplyr::rename(
    item = Item,
    ct = Control,
    ex = Experimental
  ) |>
  dplyr::mutate(dplyr::across(c(ct, ex),
    ~ as.numeric(stringr::str_trim(stringr::str_remove_all(
      stringr::str_extract(.x, "\\(([^)]+)\\)"),
      "\\(|\\)")
    )))
  ) |>
  tidyr::pivot_longer(
    cols = c(ct, ex),
    names_to = "group",
    values_to = "freq"
  ) |>
  dplyr::mutate(
    item = factor(item, levels = 1:32),
    group = factor(group, levels = c("ct", "ex"), labels = c("Control", "Experimental")),
    freq = freq / 100
  ) |>
  ggplot2::ggplot(
    ggplot2::aes(x = item, y = freq, color = group, group = 1)
  ) +
  ggplot2::geom_point(size = 3) +
  ggplot2::geom_line(alpha = 0.6) +
  ggplot2::labs(x = "HCL-32 item", y = "Answers with \"yes\" (%)") +
  ggplot2::scale_color_discrete(name = "") +
  ggplot2::scale_y_continuous(labels = scales::percent_format()) +
  ggnuplot::theme_gnuplot(base_size = 14) +
  ggplot2::theme(legend.position = c(0.5, 0.20))
