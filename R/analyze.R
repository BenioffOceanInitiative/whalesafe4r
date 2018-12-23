#' Get length in km (internal function)
#'
#' @param segment
#'
#' @return units in km
#' @importFrom sf st_length
#' @importFrom units set_units drop_units
#' @keywords internal
#'
#' @examples
get_length_km <- function(segment){
  # seg <- p$segment[2]
  if (is.na(segment)) return(NA)

  st_length(segment) %>%
    set_units("km") %>%
    drop_units()
}

#' Create segment from 2 points (internal function)
#'
#' @param p1 sf first point
#' @param p2 sf second point
#' @param crs coordinate reference system (default 4326)
#'
#' @return linestring
#' @importFrom sf st_combine st_cast st_set_crs
#' @keywords internal
#'
#' @examples
get_segment <- function(p1, p2, crs=4326){

  if (any(is.na(p1), is.na(p2))) return(NA)

  st_combine(c(p1, p2)) %>%
    st_cast("LINESTRING") %>%
    st_set_crs(crs)
}

#' Create line segments with speed for specific ship from AIS data
#'
#' @param data AIS data.frame
#' @param ship_name name of ship in AIS data.frame
#'
#' @return data.frame with sf line segments and speed (km/hr) per segment
#' @importFrom sf st_as_sf st_as_sfc st_set_geometry read_sf write_sf
#' @importFrom dplyr filter mutate arrange lag if_else select
#' @importFrom lubridate round_date
#' @importFrom purrr map map2 map_dbl
#' @export
#'
#' @examples
get_ship_segments <- function(data, ship_name, dir_data="data"){

  lns_rds <- file.path(dir_data, "ship", paste0(ship_name, "_segments.rds"))
  if (file.exists(lns_rds)){
    message(paste("get_ship_segments(): reading from", lns_rds))
    lns <- read_rds(lns_rds)
    return(lns)
  }
  if (!dir.exists(dirname(lns_rds))) dir.create(dirname(lns_rds), recursive=T)

  pts <- data %>%
    # filter to single vessel
    filter(name == ship_name) %>%
    # convert to sf points tibble
    st_as_sf(coords = c("lon", "lat"), crs=4326) %>%
    # sort by datetime
    arrange(datetime) %>%
    # filter to only one point per minute to reduce weird speeds
    filter(!duplicated(round_date(datetime, unit="minute"))) %>%
    mutate(
      # get segment based on previous point
      seg      = map2(lag(geometry), geometry, get_segment),
      seg_mins = (datetime - lag(datetime)) %>% as.double(units = "mins"),
      seg_km   = map_dbl(seg, get_length_km),
      seg_kmhr = seg_km / (seg_mins / 60),
      seg_new  = if_else(is.na(seg_mins) | seg_mins > 60, 1, 0),
      seg_num  = cumsum(seg_new))

  # setup lines
  lns <- pts %>%
    filter(seg_new == 0) %>%
    mutate(
      seg_geom = map(seg, 1) %>% st_as_sfc(crs=4326)) %>%
    st_set_geometry("seg_geom") %>%
    select(-seg, -geometry) %>%
    rename(geometry = seg_geom)

  # write lns
  message(paste("get_ship_segments(): writing to", lns_rds))
  write_rds(lns, lns_rds)

  # return lns
  lns
}


#' Get breakdown of distance and time above/below speed limit
#'
#' @param segs data.frame with sf line segments and speed (km/hr) per segment,
#'   as returned by \code{\link{get_ship_segments}}
#' @param limit_knots speed limit in knots
#' @param ship_name used for storing rds data
#' @param dir_data directory to cache data
#'
#' @return data.frame summarizing distance and speed, and sf line segments geometry
#' @importFrom dplyr filter mutate summarize
#' @export
#'
#' @examples
get_ship_limits <- function(segs, limit_knots = 10, ship_name, dir_data="data"){

  lims_rds <- file.path(dir_data, "ship", paste0(ship_name, "_limits.rds"))
  if (file.exists(lims_rds)){
    message(paste("get_ship_limits(): reading from", lims_rds))
    lims <- read_rds(lims_rds)
    return(lims)
  }
  if (!dir.exists(dirname(lims_rds))) dir.create(dirname(lims_rds), recursive=T)

  limit_kph   <- limit_knots * 1.852

  lims <- segs %>%
    filter(seg_kmhr > limit_kph) %>%
    summarize(
      label = paste(">", limit_knots, "knots"),
      min   = sum(seg_mins), # TODO: seg_mins -> seg_min
      km    = sum(seg_km)) %>%
    rbind(
      segs %>%
        filter(seg_kmhr <= limit_kph) %>%
        summarize(
          label = paste("<=", limit_knots, "knots"),
          min   = sum(seg_mins), # TODO: seg_mins -> seg_min
          km    = sum(seg_km))) %>%
    mutate(
      pct_dist = km / sum(km) * 100,
      pct_time = min / sum(min) * 100)

  # write lims
  message(paste("get_ship_limits(): writing to", lims_rds))
  write_rds(lims, lims_rds)

  # return lims
  lims
}
