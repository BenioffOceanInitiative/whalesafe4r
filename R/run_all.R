source('~/github/s4wr/R/db.R')
#source('~/github/s4wr/R/logfile_funs.R')
source('~/github/s4wr/R/crawlers.R')
source('~/github/s4wr/R/readers.R')
source('~/github/s4wr/R/utils.R')


# Initiate connection ----
# Must be on UCSB IP address for connection to work
con = db_connect()

# Get last read URL from ais_data table ----
last_read_url = dbGetQuery(con, "SELECT MAX(url) AS last_read_url FROM ais_data;") %>% .$last_read_url
# Get list of new links by giving get_ais_urls() the last_read_url----
new_links = get_ais_urls(last_read_url)

# Get new_ais_data for global environment while also writing it to the DB----
new_ais_data = update_ais_data(con = con, new_links = new_links)

# Get new_segs_data for global environment while also writing it to the DB ----
new_segs_data = update_segments_data(con = con, ais_data = new_ais_data)

# Update vsr_segments table in the database ----
update_vsr_segments(con = con)

# Disconnect from database ----
dbDisconnect(con)
