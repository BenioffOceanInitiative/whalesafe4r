

seg_filter <- function(){
  # Intiate connection
  con = db_connect()
  # Get Vessel Speed Reduction table from S4W Database
  vsr_zones = sf::st_read(dsn = con, EWKB = TRUE, query = "select * from vsr_zones;")
    #dbGetQuery(con, "select * from vsr_zones;")
    #st_as_sfc.pq_geometry(vsr_zones$geom)
  # Order VSR zones by category?
  vsr_zones <- vsr_zones[order(vsr_zones$vsr_category),]

  # Setup VSR date range "tbl_df"     "tbl"        "data.frame"
  vsr_dates <- tibble::tribble(
    ~vsr_category, ~vsr_id,       ~start,         ~end,
    "2017_v1",         "1", "2017/06/01", "2017/06/30",
    "2017_v2",         "2", "2017/07/01", "2017/12/07",
    "2017_v3",         "3", "2017/12/08", "2018/02/28",
    "2018",            "4", "2018/06/04", "2018/12/31",
    "2019",            "5", "2019/05/15", "2019/11/15"
  ) %>% mutate_at(vars(start, end), as.Date, format = "%Y/%m/%d")

  #Regular left join doesnt work
  # st_test=left_join(SF_DF, left = TRUE, df,
  #   by = c(
  #     "date" >= "date_beg",
  #     "date" >= "date_end"
  #   ))

  # Left join, fuzzy style... to get sf df with column specifying which, if any, vsr_zone daterange that segment is within...
  sf_df_filtered = fuzzy_left_join(
    SF_DF, vsr_dates,
    by = c(
      "date" = "start",
      "date" = "end"
    ),
    match_fun = list(`>=`, `<=`)
  )
  # Remove segments that are NOT within vsr_zone date ranges
  sf_df_filtered = sf_df_filtered[!is.na(sf_df_filtered$vsr_id),]



  # Add indicator coulmn whether the segment is within the VSR polygon
  sf_df_filtered$indicator <- st_within(sf_df_filtered, vsr_zones$geom[3]) %>% lengths > 0

  # Filter only segments within VSR polygon
  sf_df_filtered = sf_df_filtered %>%
    filter(indicator==TRUE)


  # Speed bins

  return(SF_DF_filtered)
}

# a=sqldf("select * from 'sf_df_filtered'")
# DBI::dbGetQuery(con, )

# leaflet(sf_df_filtered) %>%
#   addTiles() %>%
#   addPolylines()

# if (sf_df_filtered$vsr_id ==1){
#   sf_df_filtered$indicator <- st_within(sf_df_filtered, vsr_zones$geom[1]) %>% lengths > 0
# }
