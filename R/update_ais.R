
#' Update AIS data and Spatial Features data in postgres database.  TODO: HANDLE ERRORS, WARNINGS, EMPTY TXT FILE URLs
#'
#' @return new_ais_data data.frame
#' @importFrom RPostgreSQL
#' @importFrom dplyr select
#' @importFrom lubridate as_datetime
#' @importFrom parallel mclapply detectCores
#' @export
#'
#' @examples
#' new_ais_data = update_ais_data(con = con, new_links = new_links)
#'
update_ais_data <- function(con, links){
# Initiate connection ----
  #con = db_connect()

# Loop through new_links using urls2df() ----
  new_ais_data <-  parallel::mclapply(links, function(url){
    df <-  tryCatch(urls2df(path = url), error=function(e) NULL)
    assign(url, df)
  }, mc.cores = detectCores())

  # Row bind "new_segs_data" ----
  new_ais_data = data.table::rbindlist(new_ais_data)

# append ais_data table in database with new_ais_data ----
  dbWriteTable(con, name = 'test_ais_data', value = new_ais_data, append=TRUE)

# Close database connection ----
  #dbDisconnect(con)

  return(new_ais_data)
}


#' update_segments_data()
#'
#' @param ais_data
#'
#' @return new_segs_data "sf" "data.table" "data.frame"
#' @export
#'
#' @examples
#' new_segs_data = update_segments_data(con = con, ais_data = new_ais_data)

update_segments_data <- function(con, ais_data){
# Create connection ----
  #con = db_connect()
# Run new_ais_data through ais2segments() function ----
  new_segs_data <- ais2segments(ais_data)
# Write ais_segs_data to 'ais_segments' table in database ----
  dbWriteTable(con, name = 'test_ais_segments', value = new_segs_data, append=T)

# Disconnect from database
  #dbDisconnect(con)

  return(new_segs_data)
}

# NEEDS TO JUST PERFORM INTERSECTION WITH VSR_ZONES BASED ON DATETIME FOR JUST NEW_SEGS_DATA, AND APPEND TABLE VERSUS DROPPING AND RECREATING...
#' update_vsr_segments(con)
update_vsr_segments <- function(con){
# initiate db connection ----
  #con=db_connect()
# get list of tables in database
  database_tables_list = db_list_tables(con)
# If vsr_segments is in the database, remove table ----
  if ('vsr_segments' %in% database_tables_list){
  dbRemoveTable(con, 'vsr_segments')}
# Execute table create sql to get new vsr_segments table
  dbExecute(con,
            "CREATE TABLE vsr_segments AS
            SELECT
            s.name, s.mmsi, s.speed,
            s.seg_mins, s.seg_km,
            s.seg_kmhr, s.seg_knots, s.speed_diff,
            s.year, s.beg_dt, s.end_dt,
            s.beg_lon, s.beg_lat,
            s.end_lon, s.end_lat, z.gid,
            CASE
            WHEN
            ST_CoveredBy(s.geometry, z.geom)
            THEN s.geometry
            ELSE
            ST_Multi(
            ST_Intersection(s.geometry, z.geom)
            ) END AS geometry
            FROM ais_segments AS s
            INNER JOIN vsr_zones AS z
            ON ST_Intersects(s.geometry, z.geom)
            WHERE
            s.datetime::date <= z.date_end AND
            s.datetime >= z.date_beg;")

  #dbDisconnect(con)
}


# query="SELECT s.name, s.mmsi, s.speed,
#                           s.seg_mins, s.seg_km,
# s.seg_kmhr, s.seg_knots, s.speed_diff,
# s.year, s.beg_dt, s.end_dt,
# s.beg_lon, s.beg_lat,
# s.end_lon, s.end_lat, z.gid,
# CASE
# WHEN
# ST_CoveredBy(s.geometry, z.geom)
# THEN s.geometry
# ELSE
# ST_Multi(
# ST_Intersection(s.geometry, z.geom)
# ) END AS geometry
# FROM new_segs_data AS s
# INNER JOIN vsr_zones AS z
# ON ST_Intersects(s.geometry, z.geom)
# WHERE
# s.datetime::date <= z.date_end AND
# s.datetime >= z.date_beg;"
#
# vsr_segs=sqldf(query,connection=con)
