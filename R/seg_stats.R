 con=db_connect()
#
# vsr_segs <- sf::st_read(dsn = con, EWKB = TRUE, query =
#   # [PostGIS — Getting intersections the faster way](https://postgis.net/2014/03/14/tip_intersection_faster/)
#   "SELECT s.name,
#   s.beg_dt, s.beg_lon, s.beg_lat,
#   s.end_dt, s.end_lon, s.end_lat,
#   z.gid
#   , CASE
#   WHEN
#   ST_CoveredBy(s.geometry, z.geom)
#   THEN s.geometry
#   ELSE
#   ST_Multi(
#   ST_Intersection(s.geometry, z.geom)
#   ) END AS geometry
#   FROM ais_segments AS s
#   INNER JOIN vsr_zones AS z
#   ON ST_Intersects(s.geometry, z.geom)
#   WHERE
#   s.datetime::date <= z.date_end AND
#   s.datetime >= z.date_beg;")
#
 # vsr_segs <- sf::st_read(dsn = con, EWKB = TRUE, query ="select * from vsr_segments;")

# vsr_segs <- dbGetQuery(con, "select * from vsr_segments;") %>%
#   select(-geometry)
# vsr_segs = tbl(con,"vsr_segments") %>%
#   select(-geometry)
# ihs_data = tbl(con, "ihs_data")
#
# vsr_segs_ihs = merge(vsr_segs, ihs_data, by="mmsi")


#' Join VSR segments with IHS Ownership Data
#'
#' @param data 'vsr_ais_segments' (table from the postgres database)
#'
#' @return vsr_segs_ihs (merged dataframe with VSR segments and ihs data)
#' @importFrom RpostgreSQL database connect
#' @export
#'
#' @examples
 .merge_ihs_vsr <- function(){
   # Connect to DB
   con=db_connect()
   # Get vsr_gegments data from database
   vsr_segs = tbl(con,"vsr_segments") %>%
     select(-geometry)
   # Get IHS data from database
   ihs_data = tbl(con, "ihs_data")
   #   %>% filter(gt>=300)
   # Merge/inner join vsr_segments and IHS data to get only segments with complete records...
   vsr_segs_ihs = merge(vsr_segs, ihs_data, by="mmsi")
   #vsr_segs_2019 = vsr_segs %>% filter(year==2019)
   # set the data frame as data table
   vsr_segs_ihs = setDT(vsr_segs_ihs)
   # Disconnect from DB
   dbDisconnect(con)

 return(vsr_segs_ihs)
 }

#' Ship Cooperation Statistics Function
#'
#' @param data 'vsr_ais_segments' (table from the postgres database)
#'
#' @return 'ship_stats' (ship statistics dataframe)
#' @importFrom data.table for data frame summary stats and ordering
#' @export
#'
#' @examples
#' ship_stats = ship_statistics(data = vsr_segs_ihs)
#'
#' ship_stats_1 = ship_statistics()

ship_statistics <- function(data=NULL,...){
  if (length(data)) {
    vsr_segs_ihs = data
  } else vsr_segs_ihs = .merge_ihs_vsr()
  # merge IHS data with vsr_segments data to get operator data----
  # vsr_segs_ihs = .merge_ihs_vsr()
  # Produce ship_stata data.table grouped by mmsi ----
  ship_stats = vsr_segs_ihs[, list(
    `compliance score (reported speed)` = (sum(seg_km [speed<=10])/sum(seg_km))*100,
    `compliance score (calculated speed)` = (sum(seg_km [seg_knots<=10])/sum(seg_km))*100,
    `total distance (km)` = sum(seg_km),
    `total distance (nautcal miles)` = sum(seg_km*0.539957),
    #`average distance` = mean(seg_km),
    `distance under 10 knots` = sum(seg_km [speed<=10]),
    `distance over 10 knots` = sum(seg_km [speed>=10])),
    by=list(mmsi)]
  # Assign letter grades for 'cooperation' ----
  ship_stats$grade = cut(ship_stats$`compliance score (reported speed)`,
                      breaks = c(0, 60, 70, 80, 90, 99, 100),
                      labels = c("F", "D", "C", "B", "A", "A+"),
                      right = FALSE,
                      include.lowest = TRUE)
  # order ship_stats data.table ----
  ship_stats = ship_stats <- ship_stats[order(-grade,mmsi)]
  # set options...
  options(scipen=999, digits=3)

  return(ship_stats)
}


#' Operator Cooperation Statistics Function
#'
#' @param data 'vsr_ais_segments' (table from the postgres database)
#'
#' @return 'operator_stats' (summary statistics for each "operator")
#' @importFrom data.table for data frame summary stats and ordering
#' @export
#'
#' @examples
#' operator_stats = operator_statistics(data = vsr_segs_ihs)
#' operator_stats_1 = operator_statistics()

operator_statistics <- function(data=NULL,...) {
  # if vsr_segments data is given, use it, else use .merge_ihs_vsr function to generate data from the database
  if (length(data)) {
    vsr_segs_ihs = data
  } else vsr_segs_ihs = .merge_ihs_vsr()

  # Produce ship_stata data.table grouped by mmsi ----
  operator_stats = vsr_segs_ihs[, list(
    `compliance score (reported speed)` = (sum(seg_km [speed<=10])/sum(seg_km))*100,
    `compliance score (calculated speed)` = (sum(seg_km [seg_knots<=10])/sum(seg_km))*100,
    `total distance (km)` = sum(seg_km),
    `total distance (nautcal miles)` = sum(seg_km*0.539957),
    #`average distance` = mean(seg_km),
    `distance under 10 knots` = sum(seg_km [speed<=10]),
    `distance over 10 knots` = sum(seg_km [speed>=10])),
    by=list(operator)]
  # Assign letter grades
  operator_stats$grade = cut(operator_stats$`compliance score (reported speed)`,
                         breaks = c(0, 60, 70, 80, 90, 99, 100),
                         labels = c("F", "D", "C", "B", "A", "A+"),
                         right = FALSE,
                         include.lowest = TRUE)
  # order by best grades and furthest travelled
  operator_stats = operator_stats <- operator_stats[order(-grade, -`total distance (km)`)]
  # set options...
  options(scipen=999, digits=3)

  return(operator_stats)
}
