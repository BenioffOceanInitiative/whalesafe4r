# con=db_connect()
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
# vsr_segs <- sf::st_read(dsn = con, EWKB = TRUE, query ="select * from vsr_ais_segments;")


#' Ship Compiance Statistics Function
#'
#' @param data 'vsr_ais_segments' (table from the postgres database)
#'
#' @return 'ship_stats' (ship statistics dataframe)
#' @importFrom data.table for data frame summary stats and ordering
#' @export
#'
#' @examples
#' df = ship_statistics(data = vsr_segs)

ship_statistics <- function(data=NULL){

  ship_stats = setDT(data)  # set the data frame as data table

  ship_stats = ship_stats[, list(
    `compliance score (reported speed)` = (sum(seg_km [seg_lt10_rep==TRUE])/sum(seg_km))*100,
    `compliance score (calculated speed)` = (sum(seg_km [seg_lt10_calc==TRUE])/sum(seg_km))*100,
    `total distance (km)` = sum(seg_km),
    `total distance (nautcal miles)` = sum(seg_km*0.539957),
    #`average distance` = mean(seg_km),
    `distance under 10 knots` = sum(seg_km [seg_lt10_rep==TRUE]),
    `distance over 10 knots` = sum(seg_km [seg_lt10_rep==FALSE]),
    good_segs_count = sum(seg_lt10_rep==TRUE),
    bad_segs_count = sum(seg_lt10_rep==FALSE)),
    by=list(name)]

  ship_stats = ship_stats <- ship_stats[order(-`compliance score (reported speed)`, -`total distance (nautcal miles)`)]

  options(scipen=999, digits=3)


  return(ship_stats)
}
