#devtools::load_all()

sbais <- tibble::tibble(
  txt = list.files("data-raw", "^AIS_.*\\.txt$", full.names = T, recursive = T)) %>%
  dplyr::mutate(
    # note use of custom read_ais_txt() function applied to each *.txt file
    data = purrr::map(txt, bbnj::read_ais_txt)) %>%
  dplyr::select(-txt) %>%
  tidyr::unnest(data) %>%
  dplyr::arrange(datetime, name)

readr::write_csv(sbais, "data-raw/sbais.csv")
usethis::use_data(sbais, overwrite = TRUE)
