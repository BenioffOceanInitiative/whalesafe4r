source('~/github/s4wr/R/logfile_funs.R')

# date handlers -----------------------------------------------------------

#' Parse and build the Ymd from the url/filename
date.from_filename <- function(fname){
  str <- stringi::stri_extract_all_regex(basename(fname), "[0-9]+(?=\\-)") %>% unlist
  strptime(as.numeric(sprintf("20%s", str)), "%Y%m%d") %>% as.character()
}

#' Reformat the timestamp for fractionals
date.as_frac <- function(t){
  if(!grepl("\\.", t, perl = TRUE)){
    stringi::stri_replace_last_regex(t, "\\:", ".")
  }else {
    return(t)
  }
}

#' Build the datestring object
date.build <- function(ymd = NULL, ts = NULL){
  options(digits.secs = 12)
  if(!grepl("\\.", ts, perl = TRUE)){
    ts <- date.as_frac(ts)
  }
  pat <- sprintf("%s %s", ymd, ts)
  as.POSIXct(pat, "%Y-%m-%d %H:%M:%OS", tz = "UTC")
}

# Spatial functions -----------------------------------------------------------

get_length_km <- function(segment){
  # seg <- p$segment[2]
  if (is.na(segment)) return(NA)

  st_length(segment) %>%
    set_units("km") %>%
    drop_units()
}

get_segment <- function(p1, p2, crs=4326){

  if (any(is.na(p1), is.na(p2))) return(NA)

  st_combine(c(p1, p2)) %>%
    st_cast("LINESTRING") %>%
    st_set_crs(crs)
}

# Reader functions -----------------------------------------------------------
#read text file, update log_df

#' Read AIS text files from URL
#'
#' @param path
#' @param log_df
#' @param logfile_path
#' @param assign_back
#' @param ...
#'
#' @return df data.frame of data. TODO: describe all the rules and expectations
#'   based on AIS_SBARC_*.txt with possibility for alternate file formats and
#'   filtering for class A position reports, ie Message ID 1,2,3 per
#'   https://www.navcen.uscg.gov/?pageName=AISMessages.
#' @export
#'
#' @examples
#'
#' df = whale.read("https://ais.sbarc.org/logs_delimited/2019/190101/AIS_SBARC_190101-00.txt")

whale.read <- function(path = NULL, log_df = NULL, logfile_path = NULL, assign_back = TRUE, ...){
  raw <- utils::read.delim(path, sep = ";", header = FALSE, quote = "")
  # clean and parse the df
  df <- dplyr::filter(raw, V6 %in% 1:3) %>%
    mutate(datetime = date.build(ymd = date.from_filename(path), ts = date.as_frac(V1))) %>%
    select(datetime, name = 2, ship_type = 3, mmsi = 8, speed = 11, lon = 13, lat = 14, heading = 16)
  #set up numeric columns
  cols.num = c("speed","lon","lat")
  df[cols.num] = sapply(df[cols.num], as.numeric)
  #update logfile
  logfile.update(log_df = log_df, url = path, is_read = TRUE, logfile_path = logfile_path, assign_back = assign_back)

  df <- df %>%
    filter(lon >= -123.5, lon <= -116.9) %>%
    filter(lat >= 32.0, lat <= 35.5)
  return(df)
}


#' whale.reader()
#' Read AIS text files into data.frame from URL and updates log_df data.frame
#'
#' @param path
#' @param log_df
#' @param logfile_path
#' @param assign_back
#' @param ...
#'
#' @returndf data.frame of data. TODO: describe all the rules and expectations
#'   based on AIS_SBARC_*.txt with possibility for alternate file formats and
#'   filtering for class A position reports, ie Message ID 1,2,3 per
#'   https://www.navcen.uscg.gov/?pageName=AISMessages.
#' @export
#'
#' @examples
#' df = whale.reader("https://ais.sbarc.org/logs_delimited/2019/190101/AIS_SBARC_190101-00.txt")
#'

whale.reader <- function(path = NULL, log_df = NULL, logfile_path = NULL, assign_back = TRUE, ...){
  raw <- read.csv(path, stringsAsFactors = F, sep = ";", header = FALSE, quote = "")
  # clean and parse the df
  df <- dplyr::filter(raw, V6 %in% 1:3) %>%
    mutate(datetime = date.build(ymd = date.from_filename(path), ts = date.as_frac(V1))) %>%
    select(datetime, name = 2, ship_type = 3, mmsi = 8, speed = 11, lon = 13, lat = 14, heading = 16)
  df$date = format(as.Date(df$datetime,format="%Y:%m:%d %H:%M:%S"),"%Y-%m-%d")
  df$date = lubridate::as_date(df$datetime)
  #set specific columns as numeric
  cols.num = c("mmsi","speed","lon","lat","heading")
  df[cols.num] = sapply(df[cols.num], as.numeric)
  #update log_df with files being read by function
  logfile.update(log_df = log_df, url = path, is_read = TRUE, logfile_path = logfile_path, assign_back = assign_back)
  # filter lon and lat within certain area
  df <- df %>%
    filter(lon >= -123.5, lon <= -116.9) %>%
    filter(lat >= 32.0, lat <= 35.5)

  return(df)
}


#' Spatial Reader function -----------------------------------------------------------
#' Read ais.txt data from URL path and return spatial data.frame (still tweaking)
#' @param path URL for AIS.txt file
#'
#' @return data.frame with segments/linestrings with distance (km), calculated speed, and reported speed
#' @export
#'
#' @examples
#' testy=shippy_lines("https://ais.sbarc.org/logs_delimited/2019/190101/AIS_SBARC_190101-00.txt")
#'

shippy_lines <- function(path=NULL){
  # library(dplyr)
  # library(tidyr)
  # devtools::load_all()

  # path = "https://ais.sbarc.org/logs_delimited/2019/190101/AIS_SBARC_190101-00.txt"
  d <- whale.reader(path) %>%
    arrange(name, datetime) %>%
    # filter lon and lat to area just slightly larger than NOAA's 2019 VOLUNTARY WHALE ADVISORY VESSEL SPEED REDUCTION ZONE (largest zone thus far)
    filter(
      lon >= -121.15 , lon <= -117.15,
      lat >=   33.175, lat <=   34.355) %>%
    # collapse data by ship name
    group_by(name) %>%
    nest()

  # convert to points and add segment fields to the data list column
  d2pts <- function(df){
    df %>%
      # sort by datetime
      arrange(datetime) %>%
      # FILTER to only one point per MINUTE to reduce weird speeds,
      # TODO: check that does not FILTER A LOT OF SHIP NAMES
      filter(!duplicated(round_date(datetime, unit="minute"))) %>%
      # convert to sf points tibble
      st_as_sf(coords = c("lon", "lat"), crs=4326, remove = F) %>%
      mutate(
        # get segment based on previous point
        speed     = lag(speed),
        beg_dt    = lag(datetime),
        end_dt    = datetime,
        beg_lon   = lag(lon),
        beg_lat   = lag(lat),
        end_lon   = lon,
        end_lat   = lat,
        seg       = map2(lag(geometry), geometry, get_segment),
        seg_mins  = (datetime - lag(datetime)) %>% as.double(units = "mins"),
        seg_km    = map_dbl(seg, get_length_km),
        seg_kmhr  = seg_km / (seg_mins / 60),
        seg_knots = seg_kmhr * 0.539957,
        seg_new   = if_else(is.na(seg_mins) | seg_mins > 60, 1, 0),
        # Reported "speed" - (calculated speed) "seg_knots"
        speed_diff    = seg_knots - speed,
        seg_lt10_rep  = if_else(speed <= 10, TRUE, FALSE),
        seg_lt10_calc = if_else(seg_knots <= 10, TRUE, FALSE))
  }

  d <- d %>%
    mutate(
      data = map(data, d2pts))


  # filter points and construct segments as lines
  pts2lns <- function(d){

    # setup lines
    d %>%
      # filter points longer than 60 km, negative in time or speed
      filter(seg_km <=60, seg_mins >=0, speed >0) %>%
      filter(seg_new == 0) %>%
      # construct lines
      mutate(
        geometry = map(seg, 1) %>% st_as_sfc(crs=4326)) %>%
      st_set_geometry("geometry") %>%
      # add year
      mutate(
        year = lubridate::year(datetime))
  }

  d <- d %>%
    mutate(
      data = map(data, pts2lns))

  # combine ship name with data and bind across tibbles
  d <- d %>%
    # filter ships without lines
    # TODO: investigate why some nrow == 0, eg INDEPENDENCE (row 9) in example path
    mutate(
      nrow = map_int(data, nrow)) %>%
    filter(nrow > 0) %>%
    select(-nrow) %>%
    # combine ship name with rest of data
    mutate(
      data = map2(
        name, data,
        function(x, y)
          add_column(y, name = x, .before = 1))) %>%
    ungroup() %>%
    select(data)
  d <- do.call(rbind, d$data)

  d
}


#test whale.read() function

# #system.time({
# df = whale.reader("https://ais.sbarc.org/logs_delimited/2019/190101/AIS_SBARC_190101-00.txt")
# #})
#
# #test shippy_lines() function
#
# #system.time({
# testy=shippy_lines("https://ais.sbarc.org/logs_delimited/2019/190101/AIS_SBARC_190101-00.txt")
# #})
#
# m=leaflet(testy) %>%
#   addTiles() %>%
#   addPolylines()
# m
#
#  length(unique(df$name))
# # # 37 ship names
#  length(unique(testy$name))
# #unfiltered rounded datetime: 26, slower but retains most data
# # filtered rounded datetime(seconds): 22, faster, but may be losing important data...
# # filtered rounded datetime(minutes): 2, sketchy...

.whale <- function(){

  seq_table <- data.frame(s = seq(1, nrow(log_df), 1200)) %>%
    mutate(e = c(lead(s, 1, NULL) - 1, nrow(log_df)))

  lapply(1:nrow(seq_table), function(i){

    d <- seq_table[i, ]
    # file name
    rds_path <- sprintf("inst/data/whale_data_urls_%s_through_%s.rds", d$s, d$e)

    files_needed <- log_df[seq(d$s, d$e), 'url']

    o <- lapply(files_needed, function(j){
      whale.read(path = j, log_df = log_df, assign_back = TRUE)
    })

    saveRDS(rbind_pages(o), rds_path)
  })
}

