#' Read AIS text file
#'
#' @param txt path to text file
#'
#' @return data.frame of data
#' @export
#'
#' @examples
read_ais_txt <- function(txt){
  #f <- 'data-raw/2018-06-01_2018-06-07/AIS_SBARC_180601-09.txt'

  # get date from name of file
  str_date <- str_replace(
    basename(txt),
    "AIS_SBARC_([0-9]{2})([0-9]{2})([0-9]{2})-[0-9]+\\.txt",
    "20\\1-\\2-\\3")

  #spec_delim("data-raw/2018-06-01_2018-06-07/AIS_SBARC_180607-23.txt", ";", col_names = F)
  ais_col_types <- cols(
    X1 = col_character(),
    X2 = col_character(),
    X3 = col_character(),
    X4 = col_double(),
    X5 = col_double(),
    X6 = col_character(),
    X7 = col_character(),
    X8 = col_character(),
    X9 = col_character(),
    X10 = col_double(),
    X11 = col_double(),
    X12 = col_double(),
    X13 = col_double(),
    X14 = col_double(),
    X15 = col_double(),
    X16 = col_double(),
    X17 = col_character(),
    X18 = col_double(),
    X19 = col_double(),
    X20 = col_double(),
    X21 = col_double(),
    X22 = col_double(),
    X23 = col_double(),
    X24 = col_double(),
    X25 = col_double()
  )

  d <- read_delim(txt, ";", col_names = sprintf("X%d", 1:25), col_types = ais_col_types) %>%
    # filter for class A position reports, ie Message ID 1,2,3 per https://www.navcen.uscg.gov/?pageName=AISMessages
    filter(X6 %in% c(1,2,3)) %>%
    mutate(
      # make seconds fractional
      str_time = str_replace(
        X1,
        "([0-9]{2}):([0-9]{2}):([0-9]{2}):([0-9]+)",
        "\\1:\\2:\\3.\\4"),
      # parse into datetime
      datetime = paste(str_date, str_time) %>%
        as_datetime(format = "%Y-%m-%d %H:%M:%OS", tz="UTC") %>%
        as.POSIXct()) %>%
    # rename and extract columns of interest
    select(
      datetime,
      name = X2, ship_type = X3, mmsi = X8, speed = X11,
      lon = X13, lat = X14, heading = X16)
  d
}
