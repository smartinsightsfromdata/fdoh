###
# SET WDS
###
if ( Sys.info()["sysname"] == "Linux" ){
  public <- "/home/joebrew/Documents/fdoh/public/lbw"
  private <- "/media/joebrew/JB/fdoh/private/vital_stats"
} else if(Sys.info()["user"] == "BrewJR" ){
  public <- "C:/Users/BrewJR/Documents/fdoh/public/lbw"
  private <- "E:/fdoh/private/vital_stats"
} else {
   stop("Don't work on this computer!")
}

########
# Read in private data
########
setwd(private)
#source("lbw_read_and_clean.R) #only necessary once

###
# LOAD 2000-2014 DATA
###
load("births_2000-2014.RData")

# Get rid of NA and * counties
births <- births[which(births$county != "*" &
                         !is.na(births$county)),]

# Fix St Johns / St Lucie
births$county[which(births$county == "ST. JOHNS")] <- "ST JOHNS"
births$county[which(births$county == "SAINT JOHNS")] <- "ST JOHNS"
births$county[which(births$county == "SAINT LUCIE")] <- "ST LUCIE"

# Fix Dade
births$county[which(births$county == "DADE")] <- "MIAMI-DADE"

# Fix unknown
births$county[which(births$county == "UNKNOWN")] <- NA

###
# LOAD FLORIDA ZIP CODE SHAPEFILE
###
setwd(public)
library(rgdal)
fl <- readOGR("florida_zip", "zip")
fl$name <- toupper(fl$NAME)

# since merging messes with order, assign and id and keep in this order
fl$id <- 1:nrow(fl)

# test some stuff with the map
fl$color <- "blue"
fl$color[which(fl$name == "ALACHUA")] <- "red"
fl$color[which(fl$name == "MARION")] <- "green"
fl$color[which(fl$name == "MIAMI-DADE")] <- "orange"
plot(fl, col = fl$color)

###
# READ IN INCOME AND STD RATES
###
# read in the std rate data
std <- read.csv("std.csv", stringsAsFactors = FALSE)

# compare to see if names are perfect matches
table(fl@data$name == std$NAME) # they're not all correct

# get the closest match for each county

std$name <- NA 
for (i in 1:nrow(std)){
  # see how close (in character changes) each name in fl is to those in STD
  m <- adist(std$NAME[i],
             fl$name)
  # get the one with the least differences
  ind <- which.min(m)
  # get the name from std
  best <- fl$name[ind][1]
  # assign to fl
  std$name[i] <- best
}

# fix the duplicated lake issue
std$name[which(std$NAME == "DADE")] <- "MIAMI-DADE" 

# Merge the two datasets together
fl@data <- merge(x = fl@data,
                 y = std,
                 by = "name",
                 all.x = TRUE,
                 all.y = FALSE)

# test out to make sure there was no reordering
fl$color <- "blue"
fl$color[which(fl$name == "ALACHUA")] <- "red"
fl$color[which(fl$name == "MARION")] <- "green"
fl$color[which(fl$name == "MIAMI-DADE")] <- "orange"
plot(fl, col = fl$color)


##########
# READ IN FLORIDA DATA ON LOW BIRTH WEIGHT, POPULATION CHARACTERISTICS
########## 
setwd(public)
dat <- read.csv("lbw.csv")
dat$lbw_births <- NULL
dat$live_births <- NULL
# Make a column called percent_black - this should be the percentage of each county's residents which are black
dat$percent_black <- (dat$black/dat$total_pop)*100

# Make a column called percent_white - this should be the percentage of each county's residents which are white
dat$percent_white<- (dat$white/dat$total_pop)*100

# Make a column called percent_other - this should be the percentage of each county's residents which are white
dat$percent_other <- (dat$other/dat$total_pop)*100

# get the closest match for each county
dat$county <- as.character(toupper(dat$county))
dat$name <- NA # create empty vector where we'll put our names
for (i in 1:nrow(dat)){
  # see how close (in character changes) each name in fl is to those in STD
  m <- adist(dat$county[i],
             fl$name)
  # get the one with the least differences
  ind <- which.min(m)
  # get the name from std
  best <- fl$name[ind][1]
  # assign to fl
  dat$name[i] <- best
}
# check matches and then merge
cbind(dat$name, dat$county)
fl@data <- merge(x = fl@data,
                 y = dat,
                 by = "name",
                 all.x = TRUE,
                 all.y = FALSE)


# Get lbw stats for each county
library(dplyr)

# Total births
x <- births %>%
  group_by(county) %>%
  summarise(total_births = n())
total_births <- x$total_births

#(create a dataframe to stick all my aggregations into)
counties <- data.frame(x)

# Low weight births
x <- births %>%
  group_by(county) %>%
  filter(weight < 2500) %>%
  summarise(lbw_births = n())
counties <- merge(x = counties,
                  y = x,
                  by = "county",
                  all.x = TRUE,
                  all.y = FALSE)

# Total births by year
for (i in 2000:2014){
  x <- births %>%
    group_by(county) %>%
    filter(year == i) %>%
    summarise(total_births = n())
  x[,paste0("total_births", i)] <- x$total_births
  x$total_births <- NULL
  counties <- merge(x = counties,
                    y = x,
                    by = "county",
                    all.x = TRUE,
                    all.y = FALSE)
  
}

# LBW births by year
for (i in 2000:2014){
  x <- births %>%
    group_by(county) %>%
    filter(year == i,
           weight < 2500) %>%
    summarise(lbw_births = n())
  
  x[,paste0("lbw_births", i)] <- x$lbw_births
  x$lbw_births <- NULL
  
  counties <- merge(x = counties,
                    y = x,
                    by = "county",
                    all.x = TRUE,
                    all.y = FALSE)
}

# LBW rate by year
for (i in 2000:2014){
  counties[,paste0("lbw_rate", i)] <-
    counties[,paste0("lbw_births", i)] /
    counties[,paste0("total_births", i)]
}

### 
# Bring counties data into fl
###
counties$county <- as.character(counties$county)
counties$name <- NA
for (i in 1:nrow(counties)){
  # see how close (in character changes) each name in fl is to those in STD
  m <- adist(counties$county[i],
             fl$name)
  # get the one with the least differences
  ind <- which.min(m)[1]
  # get the name from std
  best <- fl$name[ind]
  # assign to county
  counties$name[i] <- best
}

# Check the names match
cbind(as.character(counties$name), as.character(counties$county))
# Remove the uknown row
counties <- counties[which(!is.na(counties$county)),]
# Check on reps
length(unique(counties$name)) == length(counties$name)

# Merge the two datasets together
fl@data <- merge(x = fl@data,
                 y = counties,
                 by = "name",
                 all.x = TRUE,
                 all.y = FALSE)


##########
##################################
# ADD THOMPSON / CLARK CATEGORIES
##################################

births$cat <- ifelse(births$black==FALSE &
                          births$married  &
                          births$ed >= 3,
                        1,
                        ifelse(births$black==FALSE &
                                 births$married  &
                                 births$ed < 3,
                               2,
                               ifelse(births$black==FALSE &
                                        births$married ==FALSE &
                                        births$ed >= 3,
                                      3,
                                      ifelse(births$black==FALSE &
                                               births$married ==FALSE &
                                               births$ed < 3,
                                             4,
                                             ifelse(births$black &
                                                      births$married  &
                                                      births$ed >= 3,
                                                    5,
                                                    ifelse(births$black &
                                                             births$married  &
                                                             births$ed < 3,
                                                           6,
                                                           ifelse(births$black &
                                                                    births$married ==FALSE &
                                                                    births$ed >= 3,
                                                                  7,
                                                                  ifelse(births$black &
                                                                           births$married ==FALSE &
                                                                           births$ed < 3,
                                                                         8,
                                                                         9))))))))


##########################################
# PREPARATION FINISHED
##########################################

#####
# LBW RATE BY CAT
dat <- data.frame("cat" = 1:8)

# total births
x <- births %>%
  group_by(cat) %>%
  summarise(total_births = n())
x <- x[which(!is.na(x$cat)),]
dat$total_births <- x$total_births

# lbw_births
x <- births %>%
  filter(weight < 2500) %>%
  group_by(cat) %>%
  summarise(lbw_births = n())
x <- x[which(!is.na(x$cat)),]
dat$lbw_births <- x$lbw_births

# lbw_rate
dat$lbw_rate <- dat$lbw_births / dat$total_births

### PLOTS
my_colors <- colorRampPalette(c("lightblue", "darkorange"))(8)
barplot(dat$total_births/sum(dat$total_births)*100, 
        names.arg = dat$cat,
        col = my_colors,
        ylab = "Percent",
        border = FALSE)
title(main = "Births by category")

my_colors <- colorRampPalette(c("lightblue", "darkorange"))(8)
barplot(dat$lbw_rate*100, 
        names.arg = dat$cat,
        col = my_colors,
        ylab = "%",
        border = FALSE)
title(main = "LBW rate by category")

###
# Time series chart
###
ts <- data.frame("year" = 2000:2014)
for (i in ts$year){
  ts$lbw_rate[which(ts$year == i)] <-
    sum(counties[,paste0("lbw_births",i)], na.rm = TRUE) / 
  sum(counties[,paste0("total_births",i)], na.rm = TRUE)
}

# plot state
plot(ts$year,
     ts$lbw_rate,
     ylim = c(0.06,0.14),
     xlab = "Year",
     ylab = "LBW rate",
     type = "l",
     col = adjustcolor("black", alpha.f = 0.7),
     lwd = 3)
points(ts$year,
     ts$lbw_rate,
     pch = 16)

# add lines for counties
library(splines)
#my_colors <- adjustcolor(rainbow(nrow(counties)), alpha.f = 0.6)
my_colors <- colorRampPalette(c("grey", "black"))(nrow(counties))
my_colors <- adjustcolor(my_colors, alpha.f = 0.6)
for (i in 1:nrow(counties)){
  xspline(x = 2000:2014,
        y = counties[i,
                     names(counties[grepl("lbw_rate", names(counties))])],
        border = my_colors[i], 
        shape = 1)
}

# thick line for alachua
xspline(x = 2000:2014,
        y = counties[which(counties$county == "ALACHUA"),
                     names(counties[grepl("lbw_rate", names(counties))])],
        border = "red", 
        shape = 1, lwd = 2)

##################
# MAPPING
##################
# Create boundary shapefile of just florida
library(maptools)
library(RColorBrewer)
library(classInt)
boundary <- unionSpatialPolygons(fl, rep(1, length(fl@polygons)))

# Code for compass rose 
#(from http://r-sig-geo.2731867.n2.nabble.com/How-to-diplasy-a-compass-rose-on-a-map-td4509034.html)
compassRose<-function(x,y,rot=0,cex=1) { 
  oldcex<-par(cex=cex) 
  mheight<-strheight("M") 
  xylim<-par("usr") 
  plotdim<-par("pin") 
  xmult<-(xylim[2]-xylim[1])/(xylim[4]-xylim[3])*plotdim[2]/plotdim[1] 
  point.angles<-seq(0,7*pi/4,by=pi/4)+pi*rot/180 
  crspans<-rep(c(mheight*3,mheight/2),4) 
  xpoints<-cos(point.angles)*crspans*xmult+x 
  ypoints<-sin(point.angles)*crspans+y 
  polygon(xpoints,ypoints) 
  txtxpoints<-cos(point.angles[c(1,3,5,7)])*1.33*crspans[1]*xmult+x 
  txtypoints<-sin(point.angles[c(1,3,5,7)])*1.33*crspans[1]+y 
  text(txtxpoints,txtypoints,c("E","N","W","S")) 
  par(oldcex) 
} 

MapFun <- function(var,
                   color1 = "lightblue",
                   color2 = "darkorange",
                   legend_round = 2,
                   fixed_scale = NULL,
                   lwd = 0.2,
                   border = TRUE){
  my_colors <- colorRampPalette(c(color1, color2))(10)
  if(!is.null(fixed_scale)){
    if(length(fixed_scale) != 10){stop("Fixed scale must be of length 10")}
    my_quantiles <- fixed_scale
  } else{
    my_quantiles <- quantile(var, na.rm = TRUE, probs = seq(0,1, length = 10))
  }
  my_values <- vector(mode = "numeric", length = length(var))
  for (i in 1:length(var)){
    diffs <- (var[i] - as.numeric(my_quantiles))^2
    best <- which.min(diffs)[1]
    my_values[i] <- best
  }
  map_colors <- my_colors[my_values]
  plot(fl, col = map_colors, border = border, lwd = lwd)
  plot(boundary, add = TRUE)
  legend_colors <- colorRampPalette(my_colors)(25)
  legend("center", # position
         legend = c(min(round(my_quantiles, digits = legend_round)),
                    rep(NA, 11),
                    median(round(my_quantiles, digits = legend_round)),
                    rep(NA, 11),
                    max(round(my_quantiles, digits = legend_round))),
         fill = legend_colors,
         cex = 0.75,
         y.intersp = 0.5,
         border=NA,
         bty = "n")
  compassRose(x = -80.5 ,y = 30,rot=0,cex=0.25)
}

setwd(paste0(public, "/images"))

# LBW rate
png(filename = paste0("2013lbw", ".png"),
    width = 1000,
    height = 650)
MapFun(var = fl@data$lbw_rate2013*100, legend_round = 0)
title(main = "2013 LBW rate")
dev.off()

# INCOME
png(filename = paste0("income", ".png"),
    width = 1000,
    height = 650)
MapFun(var = fl@data$INCOME,
       color1 = "darkorange",
       color2 = "lightblue",
       legend_round = -3)
title(main = "Median household income")
dev.off()

# RACE
png(filename = paste0("race", ".png"),
    width = 1000,
    height = 650)
MapFun(var = fl@data$percent_black,
       legend_round = 0)
title(main = "Percent African American")
dev.off()

setwd(paste0(public, "/images"))

for (i in 2001:2014){
  png(filename = paste0("map", i, ".png"),
      width = 750,
      height = 500)
  
  MapFun(var = fl@data[,paste0("lbw_rate", i)]*100, 
         legend_round = 0,
         fixed_scale = as.numeric(quantile(counties$lbw_rate2013, 
                                           probs = seq(0,1,length = 10)))*100)

  title(main = paste0(i, " LBW rate"))
  
  dev.off()
}

################
# MAP OF ALACHUA
################
setwd(public)
zip <- readOGR("alachuazipcodes", "ACDPS_zipcode")

#####################33
# GOOGLE VIS
###########################
# suppressPackageStartupMessages(library(googleVis))
# x <- gvisBubbleChart(data = fl@data,
#                      idvar = "name",
#                      xvar = "percent_black",
#                      yvar = "lbw_rate2013")
# plot(x)
###
# EXPLORE A BIT
###

### barplot lbw 2013 by county
my_colors <- colorRampPalette(c("lightblue", "darkorange"))(nrow(counties))
x <- counties[order(counties$lbw_rate2013),]
mybp <- barplot(x$lbw_rate2013,
        names.arg = x$county,
        las = 3,
        cex.names = 0.3,
        col = my_colors)
text(x = mybp[which(x$county == "ALACHUA"),1],
     y = .09,
     labels = "Alachua")
title(main = "2013 LBW rate by county")

###
# scatterplot

# Income
x <- fl@data[order(fl@data$lbw_rate2013),]
plot(x = x$INCOME,
     y = x$lbw_rate2013,
     xlab = "Income",
     ylab = "LBW rate",
     col = adjustcolor(my_colors, alpha.f = 0.4),
     pch = 16,
     cex = ((x$total_births)^(1/4))/3)
points(x = x$INCOME,
     y = x$lbw_rate2013,
    col = adjustcolor(my_colors, alpha.f = 1),
    cex = ((x$total_births)^(1/4))/3)

lox <- x$INCOME
loy <- x$lbw_rate2013
lw1 <- loess(loy ~ lox, span=0.5,
             weights = x$total_births)
j <- order(lox)
lines(lox[j],lw1$fitted[j],col="red", lty=1)

# Race
x <- fl@data[order(fl@data$lbw_rate2013),]
plot(x = x$percent_black,
     y = x$lbw_rate2013,
     xlab = "Percent African American",
     ylab = "LBW rate",
     col = adjustcolor(my_colors, alpha.f = 0.4),
     pch = 16,
     cex = ((x$total_births)^(1/4))/3)
points(x = x$percent_black,
       y = x$lbw_rate2013,
       col = adjustcolor(my_colors, alpha.f = 1),
       cex = ((x$total_births)^(1/4))/3)

lox <- x$percent_black
loy <- x$lbw_rate2013
lw1 <- loess(loy ~ lox, span=1,
             weights = x$total_births)
j <- order(lox)
lines(lox[j],lw1$fitted[j],col="red", lty=1)



library(dplyr)
x <- births %>%
  group_by(age) %>%
  summarise(n = median(weight, na.rm = TRUE))
x <- x[which(x$age > 12 & x$age <=50),]
plot(x$age, x$n)
boxplot(births$weight ~ births$age)

library(hexbin)
HexFun <- function(x, y, xlab = NA, ylab = NA, main = NA){
  bin<-hexbin(x, y, xbins=50) 
  plot(bin, main=main,
       xlab = xlab,
       ylab = ylab)
}
HexFun(x = births$age,
       y = births$weight,
       xlab = "Mother's age",
       ylab = "Birth weight (grams)",
       main = "Age and weight")


# Histogram of LBW
hist(births$weight / 453.6,
     breaks = 30,
     xlab = "Weight (pounds)",
     freq = FALSE,
     las = 1,
     main = "Distribution of weight at birth",
     col = "grey",
     border = "darkgrey")
abline(v = 2500 / 453.6,
       col = adjustcolor("darkred", alpha.f = 0.6),
       lwd = 2)
abline(v = 1500 / 453.6,
       col = adjustcolor("purple", alpha.f = 0.6),
       lwd = 2)
legend("topright",
       lty = 1,
       lwd = 2,
       col = adjustcolor(c("darkred", "purple"), alpha.f = 0.6),
       legend = c("low birth weight", "very low birth weight"),
       cex = 0.7,
       bty= "n")

hist(births$weight[which(births$white)] / 453.6,
     breaks = 30,
     xlab = "Weight (pounds)",
     freq = FALSE,
     las = 1,
     main = "Distribution of weight at birth",
     col = "grey",
     border = "darkgrey")
abline(v = 2500 / 453.6,
       col = adjustcolor("darkred", alpha.f = 0.6),
       lwd = 2)
abline(v = 1500 / 453.6,
       col = adjustcolor("purple", alpha.f = 0.6),
       lwd = 2)
legend("topright",
       lty = 1,
       lwd = 2,
       col = adjustcolor(c("darkred", "purple"), alpha.f = 0.6),
       legend = c("low birth weight", "very low birth weight"),
       cex = 0.7,
       bty= "n")

x <- births[which(births$age > 12 &
                    births$age <= 40),]
x <- x %>%
  group_by(age) %>%
  summarise(weight = median(weight, na.rm = TRUE))

plot(x$age, x$weight, type = "l",
     xlab = "Age",
     ylab = "Weight (grams)",
     main = "Relationship between mother's age and birth weight",
     lty = 1,
     lwd = 2,
     col = adjustcolor("black", alpha.f = 0.6),
     las = 3,
     ylim = c(3000, 3400))
legend("topleft",
       lty = 1,
       col = adjustcolor(c(
         "black",
         "darkred",
         "purple",
         "darkgreen",
         "darkorange"),
         alpha.f = 0.6),
       legend = c("All",
                  "black",
                  "white",
                  "'high-risk' white",
                  "'high-risk' black",
                  "tobacco use during pregnancy"),
       bty = "n",
       border = NA,
       cex = 0.8)


x <- births[which(births$age > 12 &
                    births$age <= 40 &
                    births$black),]
x <- x %>%
  group_by(age) %>%
  summarise(weight = median(weight, na.rm = TRUE))

lines(x$age, x$weight, type = "l",
      col = adjustcolor("darkred", alpha.f = 0.6))

x <- births[which(births$age > 12 &
                    births$age <= 40 &
                    births$white),]
x <- x %>%
  group_by(age) %>%
  summarise(weight = median(weight, na.rm = TRUE))

lines(x$age, x$weight, type = "l",
      col = adjustcolor("purple", alpha.f = 0.6))

x <- births[which(births$age > 12 &
                    births$age <= 40 &
                    births$cat == 4),]
x <- x %>%
  group_by(age) %>%
  summarise(weight = median(weight, na.rm = TRUE))

lines(x$age, x$weight, type = "l",
      col = adjustcolor("darkgreen", alpha.f = 0.6))

x <- births[which(births$age > 12 &
                    births$age <= 40 &
                    births$cat == 8),]
x <- x %>%
  group_by(age) %>%
  summarise(weight = median(weight, na.rm = TRUE))

lines(x$age, x$weight, type = "l",
      col = adjustcolor("darkorange", alpha.f = 0.6))



x <- births[which(births$age > 12 &
                    births$age <= 40 &
                    births$tobacco),]
x <- x %>%
  group_by(age) %>%
  summarise(weight = median(weight, na.rm = TRUE))

lines(x$age, x$weight, type = "l",
      col = adjustcolor("grey", alpha.f = 0.6))


