library(dplyr, warn.conflicts = FALSE)
library(purrr)
library(tidyr)
library(readr)
library(stringr)
library(lubridate)
library(usethis)
devtools::load_all()

sbais <- tibble(
  txt = list.files("data-raw", "^AIS_.*\\.txt$", full.names = T, recursive = T)) %>%
  mutate(
    data = map(txt, read_ais_txt)) %>%
  select(-txt) %>%
  unnest(data) %>%
  arrange(datetime, name)

write_csv(sbais, "data-raw/sbais.csv")
usethis::use_data(sbais, overwrite = TRUE)
