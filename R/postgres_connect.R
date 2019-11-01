# install.packages("RPostgreSQL")
# require("RPostgreSQL") # install.packages("RPostgreSQL")
library(RPostgreSQL)
library(dplyr)
library(dbplyr)

# creates a connection to the postgres database
driver <- dbDriver("PostgreSQL")

# echo "secret" > ~/private/boi_ship-strike_rds-free.txt
pw_file <- "~/private/boi_ship-strike_rds-free.txt"
pw <- readLines(pw_file)

con <- dbConnect(
  driver,
  dbname = "mydb",
  host = "yew.clem64jg8sf4.us-west-1.rds.amazonaws.com",
  port = 5432,
  user = "master",
  password = pw)
