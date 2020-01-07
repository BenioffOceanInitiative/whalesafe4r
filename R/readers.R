

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

#Spatial functions
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


#read text file, update log_df
whale.read <- function(path = NULL, log_df = NULL, logfile_path = NULL, assign_back = TRUE, ...){
  raw <- utils::read.delim(path, sep = ";", header = FALSE, quote = "")
  # clean and parse the df
  df <- dplyr::filter(raw, V6 %in% 1:3) %>%
    mutate(datetime = date.build(ymd = date.from_filename(path), ts = date.as_frac(V1))) %>%
    select(datetime, name = 2, ship_type = 3, mmsi = 8, speed = 11, lon = 13, lat = 14, heading = 16) #%>%
    #st_as_sf(coords = c("lon", "lat"), crs=4326)
  #set up numeric columns
  cols.num = c("mmsi","speed","lon","lat")
  df[cols.num] = sapply(df[cols.num], as.numeric)
  #update logfile
  logfile.update(log_df = log_df, url = path, is_read = TRUE, logfile_path = logfile_path, assign_back = assign_back)
  return(df)
}
#test whale.read function
d = whale.read("https://ais.sbarc.org/logs_delimited/2019/190101/AIS_SBARC_190101-00.txt")

#read ais.txt data and return linestring df (still tweaking)
ship_lines <- function(path = NULL){
  raw <- utils::read.delim(path, sep = ";", header = FALSE, quote = "")
  # clean and parse the df
  df <- dplyr::filter(raw, V6 %in% 1:3) %>%
    mutate(datetime = date.build(ymd = date.from_filename(path), ts = date.as_frac(V1))) %>%
    select(datetime, name = 2, ship_type = 3, mmsi = 8, speed = 11, lon = 13, lat = 14, heading = 16) #%>%
  #st_as_sf(coords = c("lon", "lat"), crs=4326)
  #set up numeric columns

  cols.num = c("mmsi","speed","lon","lat")
  df[cols.num] = sapply(df[cols.num], as.numeric)

  df=df[order(df$name, df$datetime),]

  df <- df %>%
    # convert to sf points tibble
     sf::st_as_sf(coords = c("lon", "lat"), crs=4326) %>%
    # # filter to only one point per minute to reduce weird speeds
      dplyr::filter(!duplicated(round_date(datetime, unit="minute"))) %>%
    mutate(
      # get segment based on previous point
      seg = map2(lag(geometry), geometry, get_segment),
      seg_mins = (datetime - lag(datetime)) %>% as.double(units = "mins"),
      seg_km   = map_dbl(seg, get_length_km),#giving warning
      #calculated speed in kilometers/hour
      seg_kmhr = seg_km / (seg_mins / 60),
      #calculated speed in knots
      seg_knots = seg_kmhr * 0.539957,
      #tells whether segment is 'new' based on being greater than 60 mins.
      seg_new  = if_else(is.na(seg_mins) | seg_mins > 60, 1, 0),
      #apply speed over ground (SOG) to next segment
      seg_sog = speed)
    df$seg_sog = as.numeric(df$seg_sog)
  df = df %>%
      dplyr::filter(seg_sog < 100) %>%
      dplyr::filter(seg_new == 0) %>%
    mutate(
      seg_geom = map(seg, 1) %>%
        st_as_sfc(crs=4326)) %>%
    st_set_geometry("seg_geom")

  return(df)
}

e = ship_lines("https://ais.sbarc.org/logs_delimited/2019/190101/AIS_SBARC_190101-00.txt")


#for (i in 1:n_links) { # i = 1
 # pb$tick()
  #Sys.sleep(1 / 100)

#  url <- b[i]
 # message(glue("{i} of {length(b)}: {url}"))
  #df = whale.read(path = url, log_df = log_df, assign_back = TRUE)
  #dbWriteTable(conn = con, name = 'ais_data_test', value = df,append=T)
#}

#
#whale_df <- new.env()
#
#o <- parallel::mclapply(tst, function(i){
#  df <- whale.read(path = i, log_df = log_df, assign_back = TRUE)
#  assign(i, df, envir = whale_df)
#  dbWriteTable(conn = con, name = 'ais_data_test1', value = df,append=T)
#}, mc.cores = 6)
#.whale(x = log_df$url[321:1200])
#
#
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

#read.csv reader....
whale.reader <- function(path = NULL, log_df = NULL, logfile_path = NULL, assign_back = TRUE, ...){
  raw <- read.csv(path, stringsAsFactors = F, sep = ";", header = FALSE, quote = "")
  # clean and parse the df
  df <- dplyr::filter(raw, V6 %in% 1:3) %>%
    mutate(datetime = date.build(ymd = date.from_filename(path), ts = date.as_frac(V1))) %>%
    select(datetime, name = 2, ship_type = 3, mmsi = 8, speed = 11, lon = 13, lat = 14, heading = 16)
  cols.num = c("mmsi","speed","lon","lat")
  df[cols.num] = sapply(df[cols.num], as.numeric)
    #df$geom = st_as_sf(coords = c("lon", "lat"), crs=4326)
    logfile.update(log_df = log_df, url = path, is_read = TRUE, logfile_path = logfile_path, assign_back = assign_back)

  return(df)
}
