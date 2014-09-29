library(maptools)
library(rgdal)
library(RColorBrewer)
library(classInt)
library(dplyr)

#################
## Setwd
if ( Sys.info()["sysname"] == "Linux" ){
  setwd("/home/joebrew/")
} else {
  setwd("C:/Users/BrewJR/")
}

mywd <- getwd()

#################
## Read in STD data (not stored in this directory)
std <- read.csv("Desktop/std_private/alachua_2009-2014.csv")

# Convert to local dataframe
std <- tbl_df(std)

# Format dates
std <-
  std %>%
  mutate(date = as.Date(date_added, 
                        format = "%Y-%m-%d"))

# Subset to keep only relevant columns
std <- std %>%
  select(zip, disease, disease_name, age_group, sex, race, ethnicity, pregnant, msm, date)

# Create a month / year column
std <- std %>%
  mutate(month = format(date, "%m"),
         year = format(date, "%Y"))

# number of cases by month by race
ts <- 
  std %>% 
  group_by(month, year, race) %>%
  summarise(cases = n()) %>%
  arrange( year, month,race)

# Define function for overlapping plots
BarFun <- function(df = ts){
  # asian
  a <-
    ts %>%
    filter(race == "Asian")
  # white
  w <-
    ts %>%
    filter(race == "White")
  # black
  b <-
    ts %>%
    filter(race == "Black/African American")
  
  mymax <- max(df$cases, na.rm = T)
  
  barplot(a$cases, ylim=c(0,mymax))
  Sys.sleep(3)
  barplot(w$cases, add = T, col = adjustcolor("blue", alpha.f=0.5))
  Sys.sleep(3)
  barplot(b$cases, add = T, col = adjustcolor("red", alpha.f=0.5))
}

BarFun(ts)
legend(x= "topright",
       fill = c("grey",
               adjustcolor("blue", alpha.f = 0.5),
               adjustcolor("red", alpha.f = 0.5)),
       legend = c("Asian", "White", "Black"))


## Read in Alachua zip code shapefiles
setwd("Documents/fdoh/public/gis/")
zip <- readOGR("alachuazipcodes", layer="ACDPS_zipcode")
fl <- readOGR("zip", layer = "zipbnd_2012") # this has 2010 pop

plot(fl[which(fl$PO_NAME %in% zip$PO_NAME),])

## Read in population data for florida zip codes
setwd(paste0(mywd,"/Documents/fdoh/public/census"))
pop <- read.csv("florida_details.csv", skip = 1)
names(pop) <- gsub("[.]", "", names(pop))
pop <- tbl_df(pop)

# Keep only the age variables relevant to this analysis
pop <- pop %>% 
  select(matches(paste(15:24, collapse = "|")))

# Reformat all columns in pop as Numeric
pop <- pop %>% 
  filter(AreaName == "Alachua County")


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
