library(tidyverse)
library(DBI)
library(bigrquery)
library(knitr)
library(kableExtra)
library(tidyverse)
library(leaflet)
library(RColorBrewer)
library(maptools)
library(sf)
library(rgeos)
library(dplyr)
library(RPostgreSQL)
library(RPostgres)
library(dbplyr)
library(lubridate)
library(units)
library(data.table)
library(parallel)
library(ggplot2)
library(here)

# Crawl https://ais.sbarc.org/logs_delimited/ for URLs
source('~/github/whalesafe4r/R/crawlers.R')

# Read ais.txt URL into ais_data df and read ais_data df into ais_segs df
source('~/github/whalesafe4r/R/readers.R')

# 1) Loop through ais.txt URLs to return ais_data
# 2) Create ais_segments from ais_data
# 3) Intersect ais_segments and vsr_zones table in the database to create vsr_segments table in the database
source('~/github/whalesafe4r/R/update_ais.R')

# Utility functions for crawler, date and spatial functions, etc...
source('~/github/whalesafe4r/R/utils.R')

# Create summary statistics for merged vsr_sements/ihs_data df
source('~/github/whalesafe4r/R/seg_stats.R')

#devtools::install_github("BenioffOceanInitiative/whalesafe4r")

# MUST HAVE CORRECT CREDENTIALS JSON TO RUN UPDATE!
bq_cred_path = '/Users/seangoral/bq_api_test/venv/Benioff Ocean Initiative-454f666d1896.json'

#' BigQuery Connect
#'
#' @return bigquery dataset connection
#' @export 
#'
bq_connect <- function(){
  con <-  dbConnect(
            bigquery(), 
            project = 'benioff-ocean-initiative',
            dataset = "sbarc_ais", 
            billing = "benioff-ocean-initiative",
            use_legacy_sql = FALSE) 
            # specify we are using Standard SQL
  return(con)
}

# bq_connect()

#' Get Last URL in BigQuery
#'
#' @return URL value
#' @export
#'
get_last_read_url_bq <- function(){
  sql<- "
SELECT MAX(url) AS last_read_url FROM `sbarc_ais.ais_data` WHERE DATE(datetime) >= DATE_SUB(CURRENT_DATE(), INTERVAL 4 MONTH) AND datetime = (SELECT MAX(datetime) FROM `sbarc_ais.ais_data` WHERE DATE(datetime) >= DATE_SUB(CURRENT_DATE(), INTERVAL 4 MONTH));"
  
  last_read_url <- dbGetQuery(bq_connect(), sql) %>% .$last_read_url
  
  return(last_read_url)
}

#' Update SBARC BigQuery AIS Data Table
#'
#' @param bq_cred_path 
#'
#' @return
#' @export
update_sbarc_bq <- function(bq_cred_path = NULL){
  # Authenticate w/ BQ project
  print("Authenticating")
  bigrquery::bq_auth(path = bq_cred_path)
  # set my project ID and dataset name
  project_id <- 'benioff-ocean-initiative'
  dataset_name <- 'sbarc_ais'
  # Get last read URL from BQ table
  print("Getting last read URL from BQ")
  get_last_read_url_bq()
  # Get new links from SBARC
  new_links <- get_ais_urls(get_last_read_url_bq())
  # Get New data from New links
  new_ais_data <- get_ais_data(links = new_links)
  # Setup BQ table for new data load
  sbarc = bq_table(project = project_id, 
                   dataset = dataset_name, 
                   table = 'ais_data')
  # Load new_ais_data df into BigQuery
  print("Loading new AIS Data")
  bq_table_upload(x = sbarc, 
                  values = new_ais_data, 
                  create_disposition='CREATE_IF_NEEDED', 
                  write_disposition='WRITE_APPEND')
}

update_sbarc_bq(bq_cred_path = bq_cred_path)

# get_last_read_url_bq()
# 
# new_links <- get_ais_urls(get_last_read_url_bq())

# new_links <- get_ais_urls("https://ais.sbarc.org/logs_delimited/2020/200107/AIS_SBARC_200107-11.txt")

# new_links
# 
# new_ais_data <- get_ais_data(links = new_links)
