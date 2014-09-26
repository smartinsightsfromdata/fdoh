library(maptools)
library(rgdal)
library(RColorBrewer)
library(classInt)

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
              "pregnant", "msm", "date")]

#################
## Read in Alachua zip code shapefiles
setwd("~/fdoh/public/gis/alachuazipcodes")
zip <- readOGR(".", layer="ACDPS_zipcode")
setwd("~/fdoh/public/gis/zip")
zip2 <- readOGR(".", layer = "zipbnd_2012") # this has 2010 pop
setwd("~/fdoh/public/fl_zcta")
fl <- readOGR(".", layer="tl_2010_12_zcta510")

setwd("~/fdoh/public/census")
pop <- read.csv("florida_details.csv", skip = 1)
names(pop) <- gsub("[.]", "", names(pop))

# Cases by zip code
zip$cases <- NA
for (i in unique(zip$ZIP)){
  zip$cases[which(zip$ZIP == i)] <- 
    nrow(std[which(std$zip == i),])
}

# Cases by zip code divided by population
zip$casesPer <-zip$cases / (zip$POP1996) * 100000 / 5.75

# Plotting function 
ZipFun <- function(var, color, percent = FALSE){
  plotvar <- var
  nclr <- 8
  plotclr <- brewer.pal(nclr, color)
  class <- classIntervals(plotvar, nclr, style = "quantile", dataPrecision=0) #use "equal" instead
  #class <- classIntervals(0:100, nclr, style="equal")
  colcode <- findColours(class, plotclr)
  if(percent){
    legcode <- paste0(gsub(",", " - ", gsub("[[]|[]]|[)]", "", names(attr(colcode, "table")))), "%")
    } else {
      legcode <- paste0(gsub(",", " - ", gsub("[[]|[]]|[)]", "", names(attr(colcode, "table")))))
    }
  plot(zip, border=NA, col=colcode)
  legend("left", # position
         legend = legcode, #names(attr(colcode, "table")), 
         fill = attr(colcode, "palette"), 
         cex = 0.6, 
         border=NA,
         bty = "n")
}

ZipFun(zip$casesPer, "Greens")


zip$cases

# Create a funtion for getting zip code counts
MapFun <- function(var, df=NULL){
  
  if(is.null(df)){
    df <- std
  } else {
    df <- df
  }
  
  zip@data[var] <- NA
  for (i in unique(zip@data$ZIP)){
    zip@data[var][which(zip@data$ZIP == i)] <-
      nrow(df[which(df[,var] == i),])
  } 
}
