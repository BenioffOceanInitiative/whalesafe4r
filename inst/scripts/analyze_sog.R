library(tidyverse)
library(sf)
library(lubridate)
library(units)
#library(ggspatial)
library(leaflet)

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


df_postgres <- dbGetQuery(con, "SELECT datetime, name, mmsi, speed, lon, lat from all_ship_data WHERE datetime >= '2019-11-12'")

df=df_postgres[order(df_postgres$name,df_postgres$datetime),]
df$speed <- as.numeric(df$speed)

pts <- df %>%
  # convert to sf points tibble
  st_as_sf(coords = c("lon", "lat"), crs=4326) %>%
  # sort by datetime
  # filter to only one point per minute to reduce weird speeds
  filter(!duplicated(round_date(datetime, unit="minute"))) %>%
  mutate(
    # get segment based on previous point
    seg      = map2(lag(geometry), geometry, get_segment),
    seg_mins = (datetime - lag(datetime)) %>% as.double(units = "mins"),
    seg_km   = map_dbl(seg, get_length_km),
    seg_kmhr = seg_km / (seg_mins / 60),
    seg_new  = if_else(is.na(seg_mins) | seg_mins > 60, 1, 0))


# setup lines
lns <- pts %>%
  filter(seg_km <=30, seg_mins >=0, speed >0) %>%
  filter(seg_new == 0) %>%
  mutate(
    seg_geom = map(seg, 1) %>% st_as_sfc(crs=4326)) %>%
  st_set_geometry("seg_geom")

#leaflet map for km/hr calculated by distance/time
pal <- leaflet::colorNumeric(palette="Spectral", lns$seg_kmhr, reverse=T)

pal1 <- leaflet::colorNumeric(palette="Spectral", lns$speed, reverse=T)

m = leaflet::leaflet(lns) %>%
  leaflet::addProviderTiles(providers$Esri.OceanBasemap) %>%
  leaflet::addPolylines(
    color = ~pal1(speed),
    label = ~sprintf("%0.03f km/hr on %s", speed, name), group="sog") %>%
  leaflet::addLegend(
    pal = pal1, values = ~speed, title = "Speed (km/hr)") %>%
  leaflet::addPolylines(
    color = ~pal(seg_kmhr),
    label = ~sprintf("%0.03f km/hr on %s", seg_kmhr, name),group="speed_calc") %>%
  addLayersControl(
  overlayGroups = c("sog", "speed_calc"),
  options = layersControlOptions(collapsed = F))
m

whale.reader <- function(path = NULL){
  raw <- read.csv(path, stringsAsFactors = F, sep = ";", header = FALSE, quote = "")
  # clean and parse the df
  df <- dplyr::filter(raw, V6 %in% 1:3) %>%
    mutate(datetime = date.build(ymd = date.from_filename(path), ts = date.as_frac(V1))) %>%
    select(datetime, name = 2, ship_type = 3, mmsi = 8, speed = 11, lon = 13, lat = 14, heading = 16)
  cols.num = c("mmsi","speed","lon","lat","heading")
  df[cols.num] = sapply(df[cols.num], as.numeric)
  #df$geom = st_as_sf(coords = c("lon", "lat"), crs=4326)

  return(df)
}

#df= whale.reader("https://ais.sbarc.org/logs_delimited/2018/180101/AIS_SBARC_180101-00.txt")

shippy_lines <- function(path=NULL){
  
  whale.reader(path)

  #order df
  df=df[order(df$name,df$datetime),]
  df$speed <- as.numeric(df$speed)
  
  pts <- df %>%
    # convert to sf points tibble
    st_as_sf(coords = c("lon", "lat"), crs=4326) %>%
    # sort by datetime
  # FILTER to only one point per MINUTE to reduce weird speeds, BUT FILTERS A LOT OF SHIP NAMES
    #filter(!duplicated(round_date(datetime, unit="minute"))) %>%
 # FILTER to only one point per SECOND to reduce weird speeds, BUT STILL FILTERS OUT SOME SHIPS
    #filter(!duplicated(round_date(datetime, unit="second"))) %>%
    mutate(
      # get segment based on previous point
      seg      = map2(lag(geometry), geometry, get_segment),
      seg_mins = (datetime - lag(datetime)) %>% as.double(units = "mins"),
      seg_km   = map_dbl(seg, get_length_km),
      seg_kmhr = seg_km / (seg_mins / 60),
      seg_new  = if_else(is.na(seg_mins) | seg_mins > 60, 1, 0),
      speed_diff = seg_kmhr - speed)
  
  # setup lines
  lns <- pts %>%
    #segments can't be longer than 60 km, negative in time or speed
    filter(seg_km <=60, seg_mins >=0, speed >0) %>%
    filter(seg_new == 0) %>%
    mutate(
      seg_geom = map(seg, 1) %>% st_as_sfc(crs=4326)) %>%
    st_set_geometry("seg_geom")
  
  return(lns)
}

#system.time({
testy=shippy_lines(test)
#})

length(unique(df$name))
# 113 ship names
length(unique(pts$name))
#unfiltered rounded datetime: 113
# filtered rounded datetime(seconds): 63
# filtered rounded datetime(minutes): 6
length(unique(lns$name))
#unfiltered rounded datetime: 79
# filtered rounded datetime(seconds): 43
# filtered rounded datetime(minutes): 3
