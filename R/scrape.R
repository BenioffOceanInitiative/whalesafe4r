library(xml2)
library(magrittr)
library(tidyverse)
library(readr)
library(jsonlite)

#get all urls with .txt
.get_link <- function(u){
  node <- xml2::read_html(u)
  hrefs <- xml2::xml_find_all(node, ".//a[not(contains(@href,'../'))]") %>% xml_attr("href")
  urls <- xml2::url_absolute(hrefs, xml_url(node))
  if(!all(tools::file_ext(urls) == "txt")){
    lapply(urls, .get_link)
  }else {
    return(urls)
  }
}

#get all links from base url
a <- .get_link("https://ais.sbarc.org/logs_delimited/")

#unlist
b <- unique(unlist(a))

#read data in
a_dfs <- rbind_pages(lapply(b[sample(1:length(b),100)], function(i){
  raw <- readLines(i)
  Sys.sleep(0.1)
  str_matrix <- stringi::stri_split_regex(raw, "\\;", simplify = TRUE)
  as.data.frame(apply(str_matrix, 2, function(j){
    ifelse(!nchar(j), NA, j)
  })) %>% mutate(file_name = i)
}))
