
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
#'  ship_stats = ship_statistics(data=vsr_ihs_data ,date_start = '2018-01-01', date_end = '2019-11-14', tonnage = 300)
#' 
#' ship_statistics from scratch, takes a bit longer
#' ship_stats = ship_statistics(date_start = '2018-01-01',date_end = '2019-11-14', tonnage=300)

ship_statistics <- function(data=NULL, date_start=NA, date_end=NA, tonnage=NA,...) {
  
  if (length(data)==0 & is.na(date_start) & is.na(date_end)){
    vsr_ihs_data = get_vsr_ihs_data()
  }
  else if (length(data)==0 & is.na(date_start)){
    vsr_ihs_data = get_vsr_ihs_data()
    vsr_ihs_data = vsr_ihs_data %>% 
      filter(date <= as.Date(date_end))
  }
  else if (length(data)==0 & is.na(date_end)){
    vsr_ihs_data = get_vsr_ihs_data()
    vsr_ihs_data = vsr_ihs_data %>% 
      filter(date >= as.Date(date_start))
  }
  else if (length(data)==0){
    vsr_ihs_data = get_vsr_ihs_data()
    vsr_ihs_data = vsr_ihs_data %>%
      filter(date >= as.Date(date_start)
             & date <= as.Date(date_end))
  }
  else if (is.na(date_start) & is.na(date_end)){
    vsr_ihs_data = data
  } 
  else if (is.na(date_start)){
    vsr_ihs_data = data
    vsr_ihs_data = vsr_ihs_data %>% 
      filter(date <= as.Date(date_end))
  } 
  else if (is.na(date_end)){
    vsr_ihs_data = data
    vsr_ihs_data = vsr_ihs_data %>% 
      filter(date >= as.Date(date_start))
  } 
  else {
    vsr_ihs_data = data
    vsr_ihs_data = vsr_ihs_data %>%
      filter(date >= as.Date(date_start)
             & date <= as.Date(date_end))
  }
  
  if (is.na(tonnage)){
    vsr_ihs_data = vsr_ihs_data
  }
  else{
    vsr_ihs_data = vsr_ihs_data %>% 
      filter(gt>=tonnage)
  }
  # Set data.frame to data.table 
  vsr_ihs_data = setDT(vsr_ihs_data)
  # Produce ship_stata data.table grouped by mmsi, name and operator ----
  ship_stats = vsr_ihs_data[, list(
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
    ship_type = unique(ship_type),
    ship_type_ihs = unique(shiptype),
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
#' operator_stats = operator_statistics(data=vsr_ihs_data ,date_start = '2018-02-01', date_end = '2019-12-31', tonnage = 300)
#' 
#' Operator statistics from scratch, takes a bit longer
#' operator_stats_scratch = operator_statistics(date_start = '2018-01-01', date_end = '2019-12-31', tonnage=300)


operator_statistics <- function(data=NULL, date_start=NA, date_end=NA, tonnage=NA,...) {

  if (length(data)==0 & is.na(date_start) & is.na(date_end)){
    vsr_ihs_data = get_vsr_ihs_data()
  }
  else if (length(data)==0 & is.na(date_start)){
    vsr_ihs_data = get_vsr_ihs_data()
      vsr_ihs_data = vsr_ihs_data %>% 
        filter(date <= as.Date(date_end))
  }
  else if (length(data)==0 & is.na(date_end)){
    vsr_ihs_data = get_vsr_ihs_data()
      vsr_ihs_data = vsr_ihs_data %>% 
        filter(date >= as.Date(date_start))
  }
  else if (length(data)==0){
    vsr_ihs_data = get_vsr_ihs_data()
      vsr_ihs_data = vsr_ihs_data %>%
        filter(date >= as.Date(date_start)
             & date <= as.Date(date_end))
  }
  else if (is.na(date_start) & is.na(date_end)){
    vsr_ihs_data = data
    } 
  else if (is.na(date_start)){
    vsr_ihs_data = data
      vsr_ihs_data = vsr_ihs_data %>% 
        filter(date <= as.Date(date_end))
  } 
  else if (is.na(date_end)){
    vsr_ihs_data = data
      vsr_ihs_data = vsr_ihs_data %>% 
        filter(date >= as.Date(date_start))
  } 
  else {
    vsr_ihs_data = data
      vsr_ihs_data = vsr_ihs_data %>%
        filter(date >= as.Date(date_start)
           & date <= as.Date(date_end))
  }
  
  if (is.na(tonnage)){
    vsr_ihs_data = vsr_ihs_data
  }
  else{
    vsr_ihs_data = vsr_ihs_data %>% 
      filter(gt>=tonnage)
  }
  
  vsr_ihs_data = setDT(vsr_ihs_data)
  # Produce ship_stata data.table grouped by operator ----
  operator_stats = vsr_ihs_data[, list(
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
    number_of_distinct_names = length(unique(name)),
    `noaa compliance score (reported speed)` = as.numeric((sum(seg_km [speed<=10 & mean(speed)<=12])/sum(seg_km)*100)),
    `noaa compliance score (calculated speed)` = as.numeric((sum(seg_km [seg_knots<=10 & mean(seg_knots)<=12])/sum(seg_km))*100)),
    by=list(operator)]
  # Assign letter grades ----
  operator_stats$grade = cut(operator_stats$`compliance score (reported speed)`,
                         breaks = c(0, 60, 70, 80, 90, 99, 100),
                         labels = c("F", "D", "C", "B", "A", "A+"),
                         right = FALSE,
                         include.lowest = TRUE)
  
  operator_stats$noaa_grade = cut(operator_stats$`noaa compliance score (reported speed)`,
                              breaks = c(0, 10, 25, 50, 75, 100),
                              labels = c("F", "D", "C", "B", "A"),
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


