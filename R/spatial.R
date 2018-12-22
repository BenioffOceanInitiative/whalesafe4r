#' Get length in km
#'
#' @param segment
#'
#' @return units in km
#' @export
#'
#' @examples
get_length_km <- function(segment){
  # seg <- p$segment[2]
  if (is.na(segment)) return(NA)

  st_length(segment) %>%
    set_units("km") %>%
    drop_units()
}

#' Create segment from 2 points
#'
#' @param p1 sf first point
#' @param p2 sf second point
#' @param crs coordinate reference system (default 4326)
#'
#' @return linestring
#' @export
#'
#' @examples
create_segment <- function(p1, p2, crs=4326){

  if (any(is.na(p1), is.na(p2))) return(NA)

  st_combine(c(p1, p2)) %>%
    st_cast("LINESTRING") %>%
    st_set_crs(crs)
}
