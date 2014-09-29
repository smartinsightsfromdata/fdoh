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
         year = as.numeric(format(date, "%Y")))

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

# Transform the projections of both
zip2 <- spTransform(zip, CRS("+init=epsg:4326"))
fl2 <- spTransform(fl, CRS("+init=epsg:4326"))

# Subset fl to keep only those zips in Alachua
fl2 <- fl2[which(fl2$PO_NAME %in% zip$PO_NAME),]

# Visualize to confirm that they're good
plot(zip2, col = "white", border = "green")
plot(fl2, col = adjustcolor("red", alpha.f=0.2), border = "red", add = TRUE)

# Rename
zip <- zip2
fl <- fl2
rm(zip2)
rm(fl2)

# Merge together
zip@data <- merge(zip,
           fl,
           by = "ZIP",
           all.x = TRUE,
           all.y = FALSE)

# Make zip$zip numeric
zip$zip <- as.numeric(as.character(zip$ZIP))

## Get cases by zip code
# Chlamydia
zip_df <- 
  std %>% 
  filter(disease_name == "Chlamydia") %>%
  group_by(zip) %>%
  summarise(chlamydia = n()) %>%
  arrange(desc(zip))

# Gonorrhea
x <- 
  std %>% 
  filter(disease_name == "Gonorrhea") %>%
  group_by(zip) %>%
  summarise(gonorrhea = n()) %>%
  arrange(desc(zip))
zip_df <- merge(zip_df, x, all.x = TRUE, all.y = FALSE)

# Syphilis
x <- 
  std %>% 
  filter(disease_name == "Syphilis") %>%
  group_by(zip) %>%
  summarise(syphilis = n()) %>%
  arrange(desc(zip)) 
zip_df <- merge(zip_df, x, all.x = TRUE, all.y = FALSE)

# Rejoin the data from zip_df to the spatial zip
zip@data <- merge(zip@data,
                  zip_df,
                  by = "zip",
                  all.x = TRUE,
                  all.y = FALSE)

## Read in population data for florida zip codes
setwd(paste0(mywd,"/Documents/fdoh/public/census"))
pop <- read.csv("florida_details.csv", skip = 1)
names(pop) <- gsub("[.]", "", names(pop))
pop <- tbl_df(pop)

# Get the age distribution for Alachua County
x <- pop %>% 
  filter(RaceEthnicity == "Total" &
           AreaName == "Alachua County") %>%
  select(TotalUnder1Year:Total110YearsandOlder)
names(x)[c(1:2, 
           101:103)] <- c("Total0Years", "Total1Years",
                         "Total100-104Years", "Total105-109Years", "Total110+Years")


# Fake Alachua data
y <- as.numeric(x[1,])
y[19:31] <- jitter(rep(2700,13), factor = 3)
bp <- barplot(y,
              border = adjustcolor("black", alpha.f = 0.3),
              las = 3,
              cex.names = 0.5,
              xlab = NA,
              ylab = NA,
              main = "Age distribution of normal counties",
              ylim = c(-103.880, 10388.000))
mtext("Age", side=1, line=0,
      col = adjustcolor("darkred", alpha.f = 0.6))
text(x = bp[,1][seq(1,101,10)],
     y = 500,
     labels = seq(0,100,10),
     cex = 0.8,
     col = adjustcolor("darkred", alpha.f = 0.6))

# Real Alachua data
bp <- barplot(as.numeric(x[1,]),
        border = adjustcolor("black", alpha.f = 0.3),
        names.arg = NA, #gsub("Total|Years|Year|Under|Over", "", names(x)), 
        las = 3,
        cex.names = 0.5,
        xlab = NA,
        ylab = NA,
        main = "Age distribution of Alachua residents")
mtext("Age", side=1, line=0,
      col = adjustcolor("darkred", alpha.f = 0.6))
text(x = bp[,1][seq(1,101,10)],
     y = 500,
     labels = seq(0,100,10),
     cex = 0.8,
     col = adjustcolor("darkred", alpha.f = 0.6))

# Get the numbers of each age
ages <- data.frame(t(x))
names(ages) <- c("n")
ages$group <- row.names(ages)
ages$group <- as.numeric(substr(gsub("Total|Years|-|[+]", "", ages$group), 1,3))

# What percentage of Alachuans are 15-19
n1519 <- sum(ages$n[which(ages$group >=15 & ages$group <= 19)])
p1519 <- n1519 / sum(ages$n)

# What percentage of Alachuans are 20-24
n2024 <- sum(ages$n[which(ages$group >=20 & ages$group <= 24)])
p2024 <- n2024 / sum(ages$n)

# What percentage of Alachuans are 15-24
n1524 <- sum(ages$n[which(ages$group >=15 & ages$group <= 24)])
p1524 <- n1524 / sum(ages$n)


# Make lots of relevant columns in zip

# (first, populate columns with NA's)
# for (j in unique(sort(std$year))){
#   for (k in c("Chlamydia", "Syphilis", "Gonorrhea")){
#     zip@data[,paste0(k, j)] <- NA
#   }
# }

# then populate with actual numbers (all age groups)
for (i in zip$zip){
  for (j in unique(sort(std$year))){
    for (k in c("Chlamydia", "Syphilis", "Gonorrhea")){
      zip@data[which(zip$zip == i),paste0(k, j, "1524")] <- 
        (nrow(std[which(std$zip == i &
                         std$year %in% j &
                         std$disease_name == k),]) /
        zip$POP2010[which(zip$zip == i)] *
        p1524) * 100000
    }
  }
}

# And add columns for age groups (1519)
for (i in zip$zip){
  for (j in unique(sort(std$year))){
    for (k in c("Chlamydia", "Syphilis", "Gonorrhea")){
      zip@data[which(zip$zip == i),paste0(k, j, "1519")] <- 
        (nrow(std[which(std$zip == i &
                         std$year %in% j &
                         std$disease_name == k),]) /
        zip$POP2010[which(zip$zip == i)] *
        p1519) * 100000
        
    }
  }
}

# And add columns for age groups (2024)
for (i in zip$zip){
  for (j in unique(sort(std$year))){
    for (k in c("Chlamydia", "Syphilis", "Gonorrhea")){
      zip@data[which(zip$zip == i),paste0(k, j, "2024")] <- 
        (nrow(std[which(std$zip == i &
                          std$year %in% j &
                          std$disease_name == k),]) /
           zip$POP2010[which(zip$zip == i)] *
           p2024) * 100000
      
    }
  }
}

# And add columns for all STDS combined (all age groups)
for (i in zip$zip){
  for (j in unique(sort(std$year))){
      zip@data[which(zip$zip == i),paste0("std", j, "1524")] <- 
        (nrow(std[which(std$zip == i &
                          std$year %in% j ),]) /
           zip$POP2010[which(zip$zip == i)] *
           p1524) * 100000
    }
  }



# Write function to plot choropleth map
MapFun <- function(disease = "std",
                   year = "all", 
                   min_age = 15, 
                   max_age = 24, 
                   color = "Blues"){
  plotvar <- zip@data[,paste0(disease, year, min_age, max_age)]
  nclr <- 8
  plotclr <- brewer.pal(nclr, color)
  class <- classIntervals(plotvar, nclr, style = "quantile", dataPrecision=0) #use "equal" instead
  #class <- classIntervals(0:100, nclr, style="equal")
  colcode <- findColours(class, plotclr)
  legcode <- paste0(gsub(",", " - ", gsub("[[]|[]]|[)]", "", names(attr(colcode, "table")))))
  plot(zip, border="darkgrey", col=colcode)
  legend("left", # position
         legend = legcode, #names(attr(colcode, "table")), 
         fill = attr(colcode, "palette"), 
         cex = 0.6, 
         border=NA,
         bty = "n")
}

MapFun(disease = "std", 
       year = 2013,
       min_age = 15, 
       max_age = 24, 
       color = "Blues")