library(xml2)
library(magrittr)
#library(tidyverse)
library(readr)
library(stringr)
library(jsonlite)
library(glue)
library(progress)
library(here)
library(dplyr)

# paths
dir_gdata    <- "/Volumes/GoogleDrive/My Drive/projects/ship-strike/data/ais.sbarc.org/"
url_ftp      <- "https://ais.sbarc.org/logs_delimited/"
pg_connect_R <- here("R/postgres_connect.R")

source(pg_connect_R)

#get all urls with .txt
.get_link <- function(u){
  node  <- xml2::read_html(u)
  hrefs <- xml2::xml_find_all(node, ".//a[not(contains(@href,'../'))]") %>% xml_attr("href")
  urls  <- xml2::url_absolute(hrefs, xml_url(node))
  if(!all(tools::file_ext(urls) == "txt")){
    lapply(urls, .get_link)
  }else {
    return(urls)
  }
}

#get all links from base url
lst_links <- .get_link(url_ftp)

#unlist
links <- unique(unlist(lst_links))

url <- links[1]

download_url <- function(url){
  txt <- str_replace(url, url_ftp, dir_gdata)
  dir.create(dirname(txt), showWarnings = F, recursive = T)
  if (!file.exists(txt)) download.file(url, txt)
}
#walk(b, download_url)

n_links <- length(links)
pb <- progress_bar$new(total = n_links)
for (i in 1:n_links) { # i = 1
  pb$tick()
  Sys.sleep(1 / 100)

  url <- links[i]
  message(glue("{i} of {length(links)}: {url}"))
  download_url(url)

  #if (i == 10) browser()
}

# Next:
#  - load individual txt files into database
d <- shipr::read_ais_txt(txt)

d$src_url <- url

# TODO: check for txt already loaded
d %>% group_by(src_url) %>% summarize(n = n())

# TODO: check for parsing failures -- read_csv(n_max = nLinesOfTxt)
#  - load into database
#  - log files entered
#View(d)
#
#cat(paste(names(d), collapse='","'))

# initial copy
dbplyr::copy_to(
  con, d, "ais_data", overwrite = T, temporary = F,
  indexes = c("datetime","name","ship_type","mmsi","speed","lon","lat","heading"))

# subsequent append
# TODO: check if already there, otherwise skip
dbWriteTable(con, "ais_data", value = d, append=TRUE, row.names=FALSE)

# subsequent querying
tbl_ais <- dbplyr::tbl(con, "ais_data")

d_bupkis <- tbl_ais %>%
  select(datetime, lon, lat) %>%
  filter(name == "bupkis") %>%
  collect()

#read data in
# a_dfs <- rbind_pages(lapply(b[sample(1:length(b),100)], function(i){
#   raw <- readLines(i)
#   Sys.sleep(0.1)
#   str_matrix <- stringi::stri_split_regex(raw, "\\;", simplify = TRUE)
#   as.data.frame(apply(str_matrix, 2, function(j){
#     ifelse(!nchar(j), NA, j)
#   })) %>% mutate(file_name = i)
# }))
