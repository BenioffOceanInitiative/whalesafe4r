# con=db_connect()

#' Join VSR segments with IHS Ownership Data
#'
#' @param data 'vsr_ais_segments' (table from the postgres database)
#'
#' @return vsr_segs_ihs (merged dataframe with VSR segments and ihs data)
#' @importFrom RpostgreSQL database connect
#' @export
#'
#' @examples
#'  vsr_segs_ihs = .merge_ihs_vsr()
#'  
#'     user  system elapsed 
#'     21.183   2.853  44.379

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
   # Create 'date' column 
   vsr_segs_ihs$date = as.Date(format(vsr_segs_ihs$beg_dt,"%Y-%m-%d"))
   # set the data frame as data table
   # vsr_segs_ihs = setDT(vsr_segs_ihs)
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
#' ship_stats_2018 = ship_statistics(data = vsr_segs_ihs, yr = 2018)
#' 
#' ship_stats_2019 = ship_statistics(data = vsr_segs_ihs, yr = 2019)
#'
#' ship_stats_1 = ship_statistics()

ship_statistics <- function(data=NULL, yr=NULL,...){
  
  if (length(data)) {
    vsr_segs_ihs = data
  } else vsr_segs_ihs = .merge_ihs_vsr()
  
  # Filter data by yr (year) input
  vsr_segs_ihs = vsr_segs_ihs %>% 
    filter(vsr_segs_ihs$year == yr) 
  # Set data.frame to data.table 
  vsr_segs_ihs = data.table::setDT(vsr_segs_ihs)
  # Produce ship_stata data.table grouped by mmsi, name and operator ----
  ship_stats = vsr_segs_ihs[, list(
    #datetime = beg_dt,
    `compliance score (reported speed)` = (sum(seg_km [speed<=10])/sum(seg_km))*100,
    `compliance score (calculated speed)` = (sum(seg_km [seg_knots<=10])/sum(seg_km))*100,
    `total distance (km)` = sum(seg_km),
    `total distance (nautcal miles)` = sum(seg_km*0.539957),
    #`average distance` = mean(seg_km),
    `distance (nautcal miles) under 10 knots` = sum(seg_km [speed<=10]*0.539957),
    `distance (nautcal miles) over 10 knots` = sum(seg_km [speed>=10]*0.539957),
    number_of_distinct_trips = length(unique(date)),
    avg_daily_speed = mean(speed),
    gt = unique(gt)),
    by=list(mmsi, name, operator, date)]
  # Assign letter grades for 'cooperation' ----
  ship_stats$grade = cut(ship_stats$`compliance score (reported speed)`,
                      breaks = c(0, 60, 70, 80, 90, 99, 100),
                      labels = c("F", "D", "C", "B", "A", "A+"),
                      right = FALSE,
                      include.lowest = TRUE)
  # order ship_stats data.table ----
  ship_stats = ship_stats <- ship_stats[order(-grade, -`total distance (nautcal miles)`)]
  # set options...
  options(scipen=999, digits=3)

  # con = db_connect()
  #
  # dbWriteTable(con, "ship_stats", value = ship_stats, overwrite = TRUE)
  #
  # dbDisconnect(con)

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
#'  operator_stats_2018 = operator_statistics(data = vsr_segs_ihs, yr=2018)
#'    operator_stats_2019 = operator_statistics(data = vsr_segs_ihs, yr=2019, tonnage=300)
#'  
#'  operator_stats_scratch_2018 = operator_statistics(yr=2018)
#'  operator_stats_scratch_2019 = operator_statistics(yr=2019)


operator_statistics <- function(data=NULL,yr=NULL,tonnage=NULL,...) {
  # if vsr_segments data is given, use it, else use .merge_ihs_vsr function to generate data from the database
  if (length(data)) {
    vsr_segs_ihs = data
  } else vsr_segs_ihs = .merge_ihs_vsr()

  vsr_segs_ihs = vsr_segs_ihs %>% 
    filter(vsr_segs_ihs$year == yr, vsr_segs_ihs$gt >= tonnage)
  
  vsr_segs_ihs = data.table::setDT(vsr_segs_ihs)
  # Produce ship_stata data.table grouped by operator ----
  operator_stats = vsr_segs_ihs[, list(
    `compliance score (reported speed)` = (sum(seg_km [speed<=10])/sum(seg_km))*100,
    `compliance score (calculated speed)` = (sum(seg_km [seg_knots<=10])/sum(seg_km))*100,
    `total distance (km)` = sum(seg_km),
    `total distance (nautcal miles)` = sum(seg_km*0.539957),
    #`average distance` = mean(seg_km),
    `distance (nautcal miles) under 10 knots` = sum(seg_km [speed<=10]*0.539957),
    `distance (nautcal miles) over 10 knots` = sum(seg_km [speed>=10]*0.539957),
    number_of_distinct_trips = length(unique(date)), 
    number_of_distinct_mmsi = length(unique(mmsi)),
    number_of_distinct_names = length(unique(name))),
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
  options(scipen=999, digits=2)

  # con = db_connect()
  #
  # dbWriteTable(con, "operator_stats", value = operator_stats, overwrite = TRUE)
  #
  # dbDisconnect(con)

  return(operator_stats)
}
