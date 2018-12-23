
#' Interactive map of ship segments
#'
#' @param segs data.frame with sf line segments and speed (km/hr) per segment,
#'   as returned by \code{\link{get_ship_segments}}
#'
#' @return leaflet map of ship segments, color coded by speed (km/hr)
#' @importFrom leaflet leaflet colorNumeric addProviderTiles addPolylines addLegend
#' @export
#'
#' @examples
map_ship_segments <- function(segs){
  #library(leaflet)
  #library(dplyr)

  pal <- leaflet::colorNumeric("Spectral", segs$seg_kmhr, reverse=T)

  data("providers", package="leaflet")

  leaflet::leaflet(segs) %>%
    leaflet::addProviderTiles(providers$Esri.OceanBasemap) %>%
    leaflet::addPolylines(
      color = ~pal(seg_kmhr),
      label = ~sprintf("%0.03f km/hr on %s", seg_kmhr, datetime)) %>%
    leaflet::addLegend(
      pal = pal, values = ~seg_kmhr, title = "Speed (km/hr)") # , labFormat = labelFormat())
}

#' Interactive map of ship limits
#'
#' @param lims data.frame with sf line segments and speed (km/hr) per segment,
#'   as returned by \code{\link{get_ship_limits}}
#'
#' @return leaflet map of ship segments, color coded by speed (km/hr)
#' @importFrom leaflet leaflet colorNumeric addProviderTiles addPolylines addLegend
#' @importFrom sf st_is_empty
#' @export
#'
#' @examples
map_ship_limits <- function(lims){

  lims$label <- factor(lims$label)

  pal <- leaflet::colorFactor(c("green", "red"), lims$label)

  lims <- lims %>%
    mutate(
      geom_empty = st_is_empty(geometry)) %>%
    filter(!geom_empty)

  data("providers", package="leaflet")

  leaflet::leaflet(lims) %>%
    leaflet::addProviderTiles(providers$Esri.OceanBasemap) %>%
    leaflet::addPolylines(
      color = ~pal(label),
      label = ~label) %>%
    leaflet::addLegend(
      pal = pal, values = ~label, title = "Speed Limit") # , labFormat = labelFormat())
}

#' Show gauge of ship performance
#'
#' @param pct_dist_ok percent distance <= speed limit
#'
#' @return htmlwidget of gauge
#' @export
#'
#' @examples
show_ship_gauge <- function(pct_dist_ok, round=1){
  flexdashboard::gauge(
    round(pct_dist_ok, round), min = 0, max = 100, symbol = '%',
    flexdashboard::gaugeSectors(
      success = c(80, 100),
      warning = c(40, 79.999),
      danger = c(0, 39.999)))
}

#' Show value box of ship performance
#'
#' @param pct_dist_ok percent distance <= speed limit
#'
#' @return html of valueBox
#' @export
#'
#' @examples
show_ship_box <- function(pct_dist_ok, round=1){
  flexdashboard::valueBox(
    round(pct_dist_ok, round),
    icon = "fa-ship",
    color = dplyr::case_when(
      pct_dist_ok >= 80                    ~ "success",
      pct_dist_ok < 80 & pct_dist_ok >= 40 ~ "warning",
      TRUE                                 ~ "danger"))
}
