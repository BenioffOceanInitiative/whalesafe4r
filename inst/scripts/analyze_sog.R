library(tidyverse)
library(sf)
library(lubridate)
library(units)
#library(map)

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

pts <- sbais %>%
  # filter to single vessel
  #group_by(name) %>%
  # convert to sf points tibble
  st_as_sf(coords = c("lon", "lat"), crs=4326) %>%
  # sort by datetime
  arrange(datetime) %>%
  # filter to only one point per minute to reduce weird speeds
  filter(!duplicated(round_date(datetime, unit="minute"))) %>%
  mutate(
    # get segment based on previous point
    seg      = map2(lag(geometry), geometry, get_segment),
    #seg_km   = map_dbl(seg, get_length_km), #giving warning
    seg_sog = lag(speed),
    seg_new  = if_else(is.na(seg_sog) | seg_sog > 50, 1, 0),
  )

# setup lines
lns <- pts %>%
  filter(!is.na(seg_sog)) %>%
  filter(seg_new == 0) %>%
  filter(seg_km <=)
  group_by(name) %>%
  mutate(
    seg_geom = map(seg, 1) %>% st_as_sfc(crs=4326)) %>%
  st_set_geometry("seg_geom") %>%
  select(-seg, -geometry) %>%
  rename(geometry = seg_geom)

ca = map_data("usa")

g = ggplot(lns) +
  annotation_map(map_data("world"))+
  geom_sf(aes(color = seg_sog)) +
  coord_sf(xlim = c(-132, -110), ylim = c(37, 20))


g

