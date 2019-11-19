library(tidyverse)
library(sf)
library(lubridate)
library(units)
library(ggspatial)
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
    seg_mins = (datetime - lag(datetime)) %>% as.double(units = "mins"),
    seg_km   = map_dbl(seg, get_length_km),#giving warning
    seg_kmhr = seg_km / (seg_mins / 60),
    #apply speed over ground to next segment
    seg_sog = lag(speed),
    #if speed over ground is NA or greaterthan 50, return 1, otherwise 0
    seg_new  = if_else(is.na(seg_sog) | seg_sog > 50, 1, 0))

# setup lines
lns <- pts %>%
  filter(seg_km <=100) %>%
  filter(!is.na(seg_sog)) %>%
  filter(seg_new == 0) %>%
  group_by(name) %>%
  mutate(
    seg_geom = map(seg, 1) %>% st_as_sfc(crs=4326)) %>%
  st_set_geometry("seg_geom") %>%
  select(-seg, -geometry) %>%
  rename(geometry = seg_geom)

#map to see linestring errors
#ggplot map...
g = ggplot(lns) +
  annotation_map_tile(zoom = 7) +
  geom_sf(aes(color = seg_sog)) +
  coord_sf(xlim = c(-132, -110), ylim = c(37, 20))

g

#leaflet map for S.O.G.
pal <- leaflet::colorNumeric("Spectral", lns$seg_sog, reverse=T)

data("providers", package="leaflet")

leaflet::leaflet(lns) %>%
  leaflet::addProviderTiles(providers$Esri.OceanBasemap) %>%
  leaflet::addPolylines(
    color = ~pal(seg_sog),
    label = ~sprintf("%0.03f km/hr on %s", seg_sog, datetime)) %>%
  leaflet::addLegend(
    pal = pal, values = ~seg_sog, title = "Speed (km/hr)")

#leaflet map for km/hr calculated by distance/time
pal1 <- leaflet::colorNumeric("Spectral", lns$seg_km, reverse=T)

leaflet::leaflet(lns) %>%
  leaflet::addProviderTiles(providers$Esri.OceanBasemap) %>%
  leaflet::addPolylines(
    color = ~pal1(seg_km),
    label = ~sprintf("%0.03f km/hr on %s", seg_km, datetime)) %>%
  leaflet::addLegend(
    pal = pal1, values = ~seg_km, title = "Speed (km/hr)") # , labFormat = labelFormat())

