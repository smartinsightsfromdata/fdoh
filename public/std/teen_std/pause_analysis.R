library(maptools)
library(rgdal)

#################
## Setwd
if ( Sys.info()["sysname"] == "Linux" ){
  setwd("/home/joebrew/Documents/")
} else {
  setwd("C:/Users/BrewJR/Documents/")
}

#################
## Read in STD data (not stored in this directory)
std <- read.csv("C:/Users/BrewJR/Desktop/std_private/alachua_2009-2014.csv")

# Format dates
std$date <- as.Date(std$date_added, format = "%Y-%m-%d")

# Subset to keep only relevant columns
std <- std[,c("zip", "disease", "disease_name", "age_group", "sex", "race", "ethnicity",
              "pregnant", "msm")]

#################
## Read in Alachua zip code shapefiles
setwd("~/fdoh/public/gis/alachuazipcodes")
zip <- readOGR(".", layer="ACDPS_zipcode")

# Create a funtion for getting zip code counts
