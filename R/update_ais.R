
#' Update AIS data and Spatial Features data in postgres database.  TODO: HANDLE ERRORS, WARNINGS, EMPTY TXT FILE URLs
#'
#' @return new_ais_data data.frame
#' @importFrom RPostgreSQL dbDisconnect
#' @importFrom dplyr select
#' @importFrom lubridate as_datetime
#' @importFrom parallel mclapply detectCores
#'
#' @examples
#' new_ais_data = update_ais_data(con = con, links = links)
#'
#' @export

update_ais_data <- function(links=NULL){

# Loop through new_links using urls2df() ----
  new_ais_data <-  parallel::mclapply(links, function(url){
    df <-  tryCatch(urls2df(path = url), error=function(e) NULL)
    assign(url, df)
  }, mc.cores = detectCores())

  # Row bind "new_segs_data" ----
  new_ais_data = data.table::rbindlist(new_ais_data)

  return(new_ais_data)
}


#' update_segments_data()
#'
#' @param ais_data
#'
#' @return new_segs_data "sf" "data.table" "data.frame"
#' @description Creates database connection, and then produces new ais segments data frame, which is written to a temporary table. Next, a date specific geom intersection is applied to the temporary table. Finally the temporary table with the intersecting data is inserted into the vsr_segments database table and the connection is closed.
#' 
#' @examples
#' new_segs_data = update_segments_data(con = con, ais_data = new_ais_data)
#' @export

update_segments_data <- function(ais_data=NULL){
# Initiate connection for temporary table session
  con = db_connect()
# Run new_ais_data through ais2segments() function ----
  new_segs_data <- ais2segments(new_ais_data)
# Write new_segs_data to temporary table, "temp_ais_segments"
  dbWriteTable(conn = con,
               name = "temp_ais_segments",
               value = new_segs_data,
               temporary = TRUE)
# 'vsr_query' creates temporary "temp_vsr_segments" table which intersects vsr_zones based on date ranges
  vsr_query = ("CREATE TEMPORARY TABLE temp_vsr_segments AS
           SELECT
           s.name, s.mmsi, s.speed,
           s.seg_mins, s.seg_km,
           s.seg_kmhr, s.seg_knots, s.speed_diff,
           s.beg_dt, s.end_dt,
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
           FROM temp_ais_segments AS s
           INNER JOIN vsr_zones AS z
           ON ST_Intersects(s.geometry, z.geom)
           WHERE
           s.datetime::date <= z.date_end AND
           s.datetime >= z.date_beg;")
# Execute vsr_query  
  dbExecute(conn = con, vsr_query)
# Insert temp_vsr_segments into vsr_segments table
  dbExecute(conn = con, 
          "INSERT INTO vsr_segments_test
          SELECT * FROM temp_vsr_segments;")
  # Disconnect from temporary table connection
  dbDisconnect(conn = con)
  
  return(new_segs_data)
}

#' # NEEDS TO JUST PERFORM INTERSECTION WITH VSR_ZONES BASED ON DATETIME FOR JUST NEW_SEGS_DATA, AND APPEND TABLE VERSUS DROPPING AND RECREATING...
#' 
#' #' Update VSR Segments data table in database (long way...)
#' #'
#' #' @param con Formal class PqConnection
#' #'
#' #' @importFrom dplyr db_list_tables
#' #' @importFrom RSQLite dbExecute dbGetQuery dbRemoveTable
#' #' @importFrom RPostgreSQL dbDisconnect
#' #'
#' #' @examples
#' #' update_vsr_segments(con)
#' #' @export
#' update_vsr_segments <- function(con=NULL){
#' # SQL Date and Geometry intersect query
#'   # Creates "vsr_segments_temp" table which will be inserted into "vsr_segments" table.
#'   query = ("CREATE TEMPORARY TABLE temp_vsr_segments AS
#'             SELECT
#'             s.name, s.mmsi, s.speed,
#'             s.seg_mins, s.seg_km,
#'             s.seg_kmhr, s.seg_knots, s.speed_diff,
#'             s.beg_dt, s.end_dt,
#'             s.beg_lon, s.beg_lat,
#'             s.end_lon, s.end_lat, z.gid,
#'             CASE
#'             WHEN
#'             ST_CoveredBy(s.geometry, z.geom)
#'             THEN s.geometry
#'             ELSE
#'             ST_Multi(
#'             ST_Intersection(s.geometry, z.geom)
#'             ) END AS geometry
#'             FROM ais_segments_temp AS s
#'             INNER JOIN vsr_zones AS z
#'             ON ST_Intersects(s.geometry, z.geom)
#'             WHERE
#'             s.datetime::date <= z.date_end AND
#'             s.datetime >= z.date_beg;")
#'   
#'   # get list of tables in database
#'   database_tables_list = db_list_tables(con)
#'   
#'   if ('vsr_segments_temp' %!in% database_tables_list){
#'     
#'     dbExecute(con, query)
#'     
#'     # dbExecute(con, "CREATE INDEX
#'     #           vsr_segments_geom_index
#'     #           ON vsr_segments
#'     #           USING GIST (geometry);")
#'     # 
#'     # dbExecute(con, "CREATE INDEX dt_idx
#'     #           ON vsr_segments (beg_dt, end_dt);")
#'     # 
#'     # dbExecute(con, "CREATE INDEX vsr_mmsi_idx
#'     #           ON vsr_segments (mmsi);")
#'   }
#'   
#'   else if ('vsr_segments_temp' %in% database_tables_list){
#'     
#'     dbRemoveTable(con, "vsr_segments_temp")
#'     
#'     dbExecute(con, query)
#'   }
#'   
#' else {
#'   
#'   print(NULL)
#'   
#'   }
#' }
#' 
