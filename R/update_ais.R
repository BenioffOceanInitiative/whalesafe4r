#' Update AIS Data
#' 
#' Use urls2df() to loop through links to create AIS dataframe  
#' @description Uses urls2df() function to loop through ais.txt URLs list created by get_ais_urls() function. Creates ais_data data.frame which will write to the database.
#' @return nais_data points data.frame
#' @importFrom RPostgreSQL dbDisconnect dbWriteTable
#' @importFrom parallel mclapply detectCores
#' @importFrom data.table rbindlist
#'
#' @examples
#' new_ais_data = update_ais_data(links = links)
#'
#' @export

update_ais_data <- function(links=NULL){
#initiate database connection
  con = db_connect()
# Loop through new_links using urls2df() 
  ais_data <-  mclapply(links, function(url){
    df <-  tryCatch(urls2df(path = url), error=function(e) NULL)
    assign(url, df)
  }, mc.cores = detectCores())

  # Row bind "ais_data" 
  ais_data = rbindlist(ais_data)

  dbWriteTable(conn = con,
               name = "test_ais_data",
               value = ais_data,
               append = TRUE)
  
  dbDisconnect(conn = con)
  
  return(ais_data)
}


#' Update Segments Data
#'
#' @param ais_data
#'
#' @return segs_data "sf" "data.table" "data.frame"
#' @importFrom RPostgreSQL dbDisconnect dbWriteTable
#' @description Creates database connection, produces new ais segments data frame from ais_data, and writes it to the "ais_segments" database table.
#' 
#' @examples
#'   new_segs_data = update_segments_data(ais_data = new_ais_data_test)
#' 
#' @export

update_segments_data <- function(ais_data=NULL){
# Initiate connection for temporary table session
  con = db_connect()
# Run new_ais_data through ais2segments() function ----
  segs_data = ais2segments(ais_data)
  
  segs_data$year = as.integer(segs_data$year)
  
  dbWriteTable(conn = con, 
               name = "test_ais_segments", 
               value = segs_data, 
               append = TRUE)
  
  dbDisconnect(conn = con)
  
  return(segs_data)
}

#' Get IHS Data
#'
#' @importFrom RPostgreSQL dbGetQuery dbDisconnect
#' 
#' @return ihs_data data.frame
#' @examples 
#' ihs_data = get_ihs_data()
#' @export

get_ihs_data <- function(){
  con = db_connect()
  ihs_data = dbGetQuery(con, "select * from ihs_data;")
  dbDisconnect(con)
  
  return(ihs_data)
}

#' Update VSR Segments 
#'
#' @param con Formal class PqConnection
#'
#' @importFrom RSQLite dbExecute 
#' @importFrom RPostgreSQL dbDisconnect
#' @importFrom sf dbWriteTable
#' @importFrom dplyr left_join
#' @examples
#' update_vsr_segments(segs_data = new_segs_data)
#' @export
update_vsr_segments <- function(segs_data=NULL){
  # Initiate database connection
  con = db_connect()
  # Write 'segs_data' to a temporary table called "temp_ais_segments"
  dbWriteTable(conn = con,
               name = "temp_ais_segments",
               value = segs_data,
               temporary = TRUE)
  # SQL'vsr_query' creates temporary "temp_vsr_segments" table which intersects vsr_zones based on date ranges ----
  vsr_query = ("CREATE TEMPORARY TABLE 
               temp_vsr_segments AS
               SELECT
               s.name, s.datetime, s.ship_type, s.mmsi,
               s.speed, s.lon, s.lat, s.heading, s.url, 
               s.date_modified, s.beg_dt, s.end_dt, 
               s.beg_lon, s.beg_lat, s.end_lon, s.end_lat, 
               s.seg_mins, s.seg_km, s.seg_kmhr, 
               s.seg_knots, s.seg_new, s.speed_diff, 
               s.seg_lt10_rep , s.seg_lt10_calc,
               z.vsr_category,
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
  # Execute vsr_query on "temp_ais_segments" to create "temp_vsr_segments" temporary table
  dbExecute(conn = con, vsr_query)
  # snag temp_vsr_segments just for fun ----
  temp_vsr_segments = dbGetQuery(conn = con, "SELECT * FROM temp_vsr_segments;")
  
  # Left join query to join vsr_segments data and ihs_data
  join_query = "CREATE TEMPORARY TABLE 
                temp_vsr_ihs_segs AS
                SELECT temp_vsr_segments.*, ihs_data.*
                FROM temp_vsr_segments 
                LEFT JOIN ihs_data ON 
                temp_vsr_segments.mmsi = ihs_data.mmsi_ihs"
  
  dbExecute(conn = con, join_query)
   # Insert temporary table "temp_vsr_ihs_segs" into "vsr_segments' table.
  dbExecute(conn = con, 
            "INSERT INTO vsr_segments
            SELECT * FROM temp_vsr_ihs_segs;")
  
  # Disconnect from temporary table connection
  dbDisconnect(conn = con)

}

get_vsr_ihs_segs_data <- function(){

con = db_connect()

vsr_ihs_segs_query= "SELECT * FROM vsr_segments 
                    WHERE 'operator' IS NOT NULL;"
vsr_ihs_segs_data = dbGetQuery(con, vsr_ihs_segs_query)

dbDisconnect(con)

return(vsr_ihs_segs_data)
}

system.time({
vsr_ihs_segs_data <- get_vsr_ihs_segs_data()
})
