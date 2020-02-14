
# Check file size from url function ----
file_size_check <- function(path){
    response = httr::HEAD(path)
    file_size=as.numeric(httr::headers(response)[["Content-Length"]])
    return(file_size)
}

# Date from url handlers ----

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

# Spatial functions ----

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

# AIS URL Reader function ----

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
#' df = urls2df("https://ais.sbarc.org/logs_delimited/2019/190101/AIS_SBARC_190101-00.txt")
#' 
#' Empty file returns small dummy dataframe with date, filename,and system time
#' df_oops = urls2df("https://ais.sbarc.org/logs_delimited/2019/191217/AIS_SBARC_191217-00.txt")
#'

urls2df <- function(path = NULL){
    if (file_size_check(path)==0) {
      df = data.frame("datetime" = as.Date(date.from_filename(path)), "name" = as.character(c('oops','oops')), "ship_type" = as.integer(c(0,0)), "mmsi" = as.numeric(c(000000000, 000000000)), "speed" = as.numeric(c(0,0)), "lon" = as.numeric(c(0,0)), "lat" = as.numeric(c(0,0)), "heading" = as.numeric(c(0,0)), url = path, date_modified = lubridate::now(tzone = "America/Los_Angeles"))
    }
  else {
    raw <- read.csv(path, stringsAsFactors = F, sep = ";", header = FALSE, quote = "")
  
  # raw <- read.csv(path, stringsAsFactors = F, sep = ";", header = FALSE, quote = "")
  # clean and parse the df
  df <- dplyr::filter(raw, V6 %in% 1:3) %>%
        mutate(datetime = date.build(ymd = date.from_filename(path), ts = date.as_frac(V1))) %>%
        select(datetime, name = 2, ship_type = 3, mmsi = 8, speed = 11, lon = 13, lat = 14, heading = 16)
  df1 <- df %>%
        mutate(url = path,
               date_modified = lubridate::now(tzone = "America/Los_Angeles"))

  #set specific columns as numeric
  cols.num = c("mmsi","speed","lon","lat","heading")
  df[cols.num] = sapply(df[cols.num], as.numeric)

  # filter lon and lat within reasonable area on interest
  df <- df %>%
    filter(lon >= -124, lon <= -114) %>%
    filter(lat >= 31.0, lat <= 36)
}
  return(df)
}


# Spatial Reader function ----

#' Read ais.txt data from URL path and return spatial data.frame (still tweaking)
#' @param path URL for AIS.txt file
#'
#' @return data.frame with segments/linestrings with distance (km), calculated speed, and reported speed
#' @export
#'
#' @examples
#' create segments from dataframe (data=df)
#' segs=ais2segments(data = df)
#' Empty files returns empty df with date, filename, and timestamp
#' segs_oops = ais2segments(data = df_oops)
#'

ais2segments <- function(data=NULL){

  if (nrow(data)<=300){
    d <- data
    d <- d %>%
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
    return(d)
  }
  
  else {
    d <- data
   

  # library(dplyr)
  # library(tidyr)
  # devtools::load_all()

  d <- d %>%
    arrange(name, datetime) %>%
    # collapse data by ship name
    group_by(name) %>%
    nest()

  # convert to points and add segment fields to the data list column
  d2pts2lns <- function(df){
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
        seg_lt10_calc = if_else(seg_knots <= 10, TRUE, FALSE)) %>%
      filter(seg_km <=60, seg_mins >=0, speed >0) %>%
      filter(seg_new == 0) %>%
      # construct lines
      mutate(
        geometry = map(seg, 1) %>% st_as_sfc(crs=4326)) %>%
      st_set_geometry("geometry") %>%
      # add year
      mutate(
        year = format(as.Date(datetime, format="%Y/%m/%d"),"%Y")) %>%
      select(-seg)
  }

  n_cores = detectCores()

  d <- d %>%
    mutate(
      data = parallel::mclapply(data, d2pts2lns, mc.cores = 8))

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
  # data.table::rbindlist seemingly faster than do.call... ~30 seconds/day
  # d <- do.call(rbind, d$data)
  d <- sf::st_as_sf(data.table::rbindlist(d$data))

  return(d)
  }
}
