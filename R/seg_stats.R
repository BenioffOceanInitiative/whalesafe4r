
#' Join VSR segments with IHS Ownership Data 
#'
#' @param data 'vsr_ais_segments' (table from the postgres database)
#'
#' @return vsr_segs_ihs (merged dataframe with VSR segments and ihs data)
#' @importFrom dplyr select tbl 
#' @importFrom RPostgreSQL dbDisconnect
#' @export
#'
#' @examples
 vsr_segs_ihs = merge_ihs_vsr()
#'  

merge_ihs_vsr <- function(){
   # Connect to DB
   con = db_connect()
  
   # Get vsr_gegments data from database
   vsr_segs = tbl(con,"vsr_segments") %>%
     select(-geometry)
   # Get IHS data from database
   ihs_data = tbl(con, "ihs_data")

    # Merge/inner join vsr_segments and IHS data to get only segments with complete records...
   vsr_segs_ihs = merge(vsr_segs, ihs_data, by="mmsi")
   # Create 'date' column 
   vsr_segs_ihs$date = as.Date(format(vsr_segs_ihs$beg_dt,"%Y-%m-%d"))
   
   return(vsr_segs_ihs)

   # set the data frame as data table
   # vsr_segs_ihs = setDT(vsr_segs_ihs)
   # Disconnect from DB
   dbDisconnect(con)
 }

#' Ship Cooperation Statistics Function ----
#'
#' @param data 'vsr_ais_segments' (table from the postgres database)
#'
#' @return 'ship_stats' (ship statistics dataframe)
#' @importFrom data.table setDT
#' @export
#'
#' @examples
#' 
#' ship_stats = ship_statistics(data=vsr_segs_ihs ,date_start = '2018-01-01', date_end = '2019-11-14', tonnage = 300)
#'
#' ship_stats = ship_statistics(date_start = '2018-01-01',date_end = '2019-08-01', tonnage=300)

ship_statistics <- function(data=NULL, date_start=NA, date_end=NA, tonnage=NA,...) {
  
  if (length(data)==0 & is.na(date_start) & is.na(date_end)){
    vsr_segs_ihs = merge_ihs_vsr()
  }
  else if (length(data)==0 & is.na(date_start)){
    vsr_segs_ihs = merge_ihs_vsr()
    vsr_segs_ihs = vsr_segs_ihs %>% 
      filter(date <= as.Date(date_end))
  }
  else if (length(data)==0 & is.na(date_end)){
    vsr_segs_ihs = merge_ihs_vsr()
    vsr_segs_ihs = vsr_segs_ihs %>% 
      filter(date >= as.Date(date_start))
  }
  else if (length(data)==0){
    vsr_segs_ihs = merge_ihs_vsr()
    vsr_segs_ihs = vsr_segs_ihs %>%
      filter(date >= as.Date(date_start)
             & date <= as.Date(date_end))
  }
  else if (is.na(date_start) & is.na(date_end)){
    vsr_segs_ihs = data
  } 
  else if (is.na(date_start)){
    vsr_segs_ihs = data
    vsr_segs_ihs = vsr_segs_ihs %>% 
      filter(date <= as.Date(date_end))
  } 
  else if (is.na(date_end)){
    vsr_segs_ihs = data
    vsr_segs_ihs = vsr_segs_ihs %>% 
      filter(date >= as.Date(date_start))
  } 
  else {
    vsr_segs_ihs = data
    vsr_segs_ihs = vsr_segs_ihs %>%
      filter(date >= as.Date(date_start)
             & date <= as.Date(date_end))
  }
  
  if (is.na(tonnage)){
    vsr_segs_ihs = vsr_segs_ihs
  }
  else{
    vsr_segs_ihs = vsr_segs_ihs %>% 
      filter(gt>=tonnage)
  }
  # Set data.frame to data.table 
  vsr_segs_ihs = setDT(vsr_segs_ihs)
  # Produce ship_stata data.table grouped by mmsi, name and operator ----
  ship_stats = vsr_segs_ihs[, list(
    #datetime = beg_dt,
    `compliance score (reported speed)` = as.numeric((sum(seg_km [speed<=10])/sum(seg_km))*100),
    `compliance score (calculated speed)` = as.numeric((sum(seg_km [seg_knots<=10])/sum(seg_km))*100),
    `total distance (km)` = as.numeric(sum(seg_km)),
    `average speed (knots)` = as.numeric(mean(speed)),
    `average speed calculated (knots)` = as.numeric(mean(seg_knots)),
    `total distance (nautcal miles)` = as.numeric(sum(seg_km*0.539957)),
    #`average distance` = mean(seg_km),
    `distance (nautcal miles) over 10 knots` = as.numeric(sum(seg_km [speed>=10]*0.539957)),
    `distance (nautcal miles) 0-10 knots` = as.numeric(sum(seg_km [speed<=10]*0.539957)),
    `distance (nautcal miles) 10-12 knots` = as.numeric(sum(seg_km [speed>10 & speed<=12]*0.539957)),
    `distance (nautcal miles) 12-15 knots` = as.numeric(sum(seg_km [speed>12 & speed<=15]*0.539957)),
    `distance (nautcal miles) over 15 knots` = as.numeric(sum(seg_km [speed>15]*0.539957)),
    number_of_distinct_trips = length(unique(date)),
    # mean_daily_speed = mean(speed),
    # mean_daily_speed_over_12 = if_else(mean(speed) > 12, 1, 0),
    gt = unique(gt),
    `noaa compliance score (reported speed)` = as.numeric((sum(seg_km [speed<=10 & mean(speed)<=12])/sum(seg_km)*100)),
    `noaa compliance score (calculated speed)` = as.numeric((sum(seg_km [seg_knots<=10 & mean(seg_knots)<=12])/sum(seg_km))*100)),
    by=list(mmsi, name, operator)]
  # Assign letter grades for 'cooperation' ----
  ship_stats$grade = cut(ship_stats$`compliance score (reported speed)`,
                      breaks = c(0, 60, 70, 80, 90, 99, 100),
                      labels = c("F", "D", "C", "B", "A", "A+"),
                      right = FALSE,
                      include.lowest = TRUE)
  
  ship_stats$noaa_grade = cut(ship_stats$`noaa compliance score (reported speed)`,
                         breaks = c(0, 10, 25, 50, 75, 100),
                         labels = c("F", "D", "C", "B", "A"),
                         right = FALSE,
                         include.lowest = TRUE)
  # order ship_stats data.table ----
  ship_stats = ship_stats <- ship_stats[order(-grade, -`total distance (nautcal miles)`)]
  # set options...
  options(scipen=999, digits=3)

  # con = db_connect()
  #
  # dbWriteTable(con, "ship_stats_2018_2019", value = ship_stats, overwrite = TRUE)
  #
  # dbDisconnect(con)

  return(ship_stats)
}


#' Operator Cooperation Statistics Function ----
#'
#' @param data 'vsr_ais_segments' (table from the postgres database)
#'
#' @return 'operator_stats' (summary statistics for each "operator")
#' @importFrom data.table setDT
#' @export
#' 
#' @examples
#' 
#' operator_stats = operator_statistics(data=vsr_segs_ihs ,date_start = '2018-02-01', date_end = '2019-11-01', tonnage = 300)
#' 
#' operator_stats_scratch = operator_statistics(date_start = '2018-01-01', date_end = '2019-12-31', tonnage=300)


operator_statistics <- function(data=NULL, date_start=NA, date_end=NA, tonnage=NA,...) {

  if (length(data)==0 & is.na(date_start) & is.na(date_end)){
    vsr_segs_ihs = merge_ihs_vsr()
  }
  else if (length(data)==0 & is.na(date_start)){
    vsr_segs_ihs = merge_ihs_vsr()
      vsr_segs_ihs = vsr_segs_ihs %>% 
        filter(date <= as.Date(date_end))
  }
  else if (length(data)==0 & is.na(date_end)){
    vsr_segs_ihs = merge_ihs_vsr()
      vsr_segs_ihs = vsr_segs_ihs %>% 
        filter(date >= as.Date(date_start))
  }
  else if (length(data)==0){
    vsr_segs_ihs = merge_ihs_vsr()
      vsr_segs_ihs = vsr_segs_ihs %>%
        filter(date >= as.Date(date_start)
             & date <= as.Date(date_end))
  }
  else if (is.na(date_start) & is.na(date_end)){
    vsr_segs_ihs = data
    } 
  else if (is.na(date_start)){
    vsr_segs_ihs = data
      vsr_segs_ihs = vsr_segs_ihs %>% 
        filter(date <= as.Date(date_end))
  } 
  else if (is.na(date_end)){
    vsr_segs_ihs = data
      vsr_segs_ihs = vsr_segs_ihs %>% 
        filter(date >= as.Date(date_start))
  } 
  else {
    vsr_segs_ihs = data
      vsr_segs_ihs = vsr_segs_ihs %>%
        filter(date >= as.Date(date_start)
           & date <= as.Date(date_end))
  }
  
  if (is.na(tonnage)){
    vsr_segs_ihs = vsr_segs_ihs
  }
  else{
    vsr_segs_ihs = vsr_segs_ihs %>% 
      filter(gt>=tonnage)
  }
  
  vsr_segs_ihs = setDT(vsr_segs_ihs)
  # Produce ship_stata data.table grouped by operator ----
  operator_stats = vsr_segs_ihs[, list(
    `compliance score (reported speed)` = (sum(seg_km [speed<=10])/sum(seg_km))*100,
    `compliance score (calculated speed)` = (sum(seg_km [seg_knots<=10])/sum(seg_km))*100,
    `total distance (km)` = sum(seg_km),
    `total distance (nautcal miles)` = sum(seg_km*0.539957),
    #`average distance` = mean(seg_km),
    `distance (nautcal miles) over 10 knots` = sum(seg_km [speed>=10]*0.539957),
    `distance (nautcal miles) 0-10 knots` = sum(seg_km [speed<=10]*0.539957),
    `distance (nautcal miles) 10-12 knots` = sum(seg_km [speed>10 & speed<=12]*0.539957),
    `distance (nautcal miles) 12-15 knots` = sum(seg_km [speed>12 & speed<=15]*0.539957),
    `distance (nautcal miles) over 15 knots` = sum(seg_km [speed>15]*0.539957),
    number_of_distinct_trips = length(unique(date)), 
    number_of_distinct_mmsi = length(unique(mmsi)),
    number_of_distinct_names = length(unique(name))),
    by=list(operator)]
  # Assign letter grades ----
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
