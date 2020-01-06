
#' Connect to database
#'
#' @param yml yaml metadata for connecting. should contain parameters for: host, database, user, port, password
#'
#' @return database connection object
#' @export
db_connect <- function(db_yml = here::here("s4w_amazon_rds.yml")){

  # library(yaml)
  # library(dplyr)
  # library(here)
  # library(glue)
  # library(RPostgres)
  # library(dbplyr)

  #message(glue::glue("db_yml: {db_yml}"))

  # copy amazon_rds.yml into
  # https://drive.google.com/open?id=1eddyoeFO5bslUakzireH1NFh8UsGBfEY
  # loc_usr <- Sys.info()[['user']]
  # dir_gdata <- case_when(
  #   loc_usr == "bbest" ~ "/Volumes/GoogleDrive/My Drive/projects/ship-strike/data",
  #   loc_usr == "mvisalli" ~ "TODO",
  #   loc_usr == "seang" ~ "/Volumes/GoogleDrive/My Drive/ship-strike/data",
  #   TRUE ~ here("cache"))
  # message(glue("loc_usr: {loc_usr}\ndir_gdata: {dir_gdata}"))

  # create database ----
  # psql -h database-1.cbh6z8ln2pdp.us-west-2.rds.amazonaws.com -U postgres
  #
  # postgres=> CREATE DATABASE ships4whales;
  # CREATE DATABASE
  # postgres=> CREATE EXTENSION postgis;
  # ERROR:  extension "postgis" already exists
  # postgres=> CREATE EXTENSION postgis_topology;
  # ERROR:  extension "postgis_topology" already exists
  # postgres=> CREATE EXTENSION pgrouting;

  # connect ----
  #db_yml <- file.path(dir_gdata, "amazon_rds.yml")
  db <- yaml::yaml.load_file(db_yml)

  con <- DBI::dbConnect(
    RPostgres::Postgres(),
    dbname   = db$database,
    host     = db$host,
    port     = db$port,
    user     = db$user,
    password = db$password,
    sslmode  = 'require')

  # dbListTables(con)

  # dbGetQuery(con, 'select count(*) from ais_data')

  # ais <- tbl(con, "ais_data")
  # ais_ships <- ais %>%
  #   group_by(name) %>%
  #   summarize(
  #     n = n(),
  #     date_beg = min(datetime, na.rm=T),
  #     date_end = max(datetime, na.rm=T)) %>%
  #   collect()
  # 2019-12-04:
  #   nrow(ais_ships)  #      4,823
  #   sum(ais_ships$n) # 51,271,029

  con
}




