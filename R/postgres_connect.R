# install.packages("RPostgreSQL")
require("RPostgreSQL")
install.packages("RPostgreSQL")
library(RPostgreSQL)

driver <- dbDriver("PostgreSQL")
# creates a connection to the postgres database

con <- dbConnect(driver,
                 dbname = "mydb",
                 host = "yew.clem64jg8sf4.us-west-1.rds.amazonaws.com",
                 port = 5432,
                 user = "master",
                 password = "Kook1991")

sql_command = "CREATE TABLE ais_data (
            X1 TIMESTAMPZ,
            X2 double precision,
            X3 double precision,
            X4 double precision,
            X5 double precision,
            X6 double precision,
            X7 double precision,
            X8 double precision,
            X9 double precision,
            X10 double precision,
            X11 double precision,
            X12 double precision,
            X13 double precision,
            X14 double precision,
            X15 double precision,
            X16 double precision,
            X17 double precision,
            X18 double precision,
            X19 double precision,
            X20 double precision,
            X21 double precision,
            X22 double precision,
            X23 double precision,
            X24 double precision,
            X25 double precision
            );"

dbGetQuery(con, sql_command)

hours.list = c(0:23)
hours.list_1 = sprintf('%02d', hours.list)

ais_text = lapply(paste0('https://ais.sbarc.org/logs_delimited/2019/190102/AIS_SBARC_190102-', hours.list_1,'.txt'),
           function(url){
             url %>%
               read_delim(";", col_names = sprintf("x%d", 1:25), col_types = ais_col_types)

           }
  )
  DF = do.call(rbind.data.frame, ais_text)



dbWriteTable(con, "ais_data", value = DF, append=TRUE, row.names=FALSE)




