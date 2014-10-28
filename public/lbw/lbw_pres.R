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
# temp <- list.files(pattern="*.csv") # get all csvs
# temp <- temp[which(grepl("births", temp))] # only births
# temp <- temp[21:35] # get only years since 2000
# for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i])) #read in all csvs
# all_births <- list(
#   births2000.csv,
#   births2001.csv,
#   births2002.csv,
#   births2003.csv,
#   births2004.csv,
#   births2005.csv,
#   births2006.csv,
#   births2007.csv,
#   births2008.csv,
#   births2009.csv,
#   births2010.csv,
#   births2011.csv,
#   births2012.csv,
#   births2013.csv,
#   births2014.csv)
# births <- data.frame(do.call(rbind, all_births))
# 
# rm(temp,
#    births2000.csv,
#    births2001.csv,
#    births2002.csv,
#    births2003.csv,
#    births2004.csv,
#    births2005.csv,
#    births2006.csv,
#    births2007.csv,
#    births2008.csv,
#    births2009.csv,
#    births2010.csv,
#    births2011.csv,
#    births2012.csv,
#    births2013.csv,
#    births2014.csv,
#    all_births,
#    i)
# births <- births[,c("EVENT_YEAR",
#                     "DATE_OF_BIRTH",
#                     "MOTHER_RES_ZIP",
#                     "MOTHER_AGE",
#                     "MOTHER_RES_COUNTY",
#                     "MOTHER_EDCODE",
#                     "MOTHER_MARRIED",
#                     "BIRTH_WEIGHT_GRAMS",
#                     "MOTHER_WIC_YESNO",
#                     "PRINCIPAL_SRCPAY_CODE",
#                     "MOTHER_RACE_BLACK",
#                     "MOTHER_RACE_WHITE",
#                     "TOBACCO_USE_YESNO")]
# names(births) <- c("year",
#                    "dob",
#                    "zip",
#                    "age",
#                    "county",
#                    "ed",
#                    "married",
#                    "weight",
#                    "wic",
#                    "pay",
#                    "black",
#                    "white",
#                    "tobacco")
# 
# births$dob <- as.Date(births$dob, format = "%Y-%m-%d")
# births$zip[which(births$zip == 99999)] <- NA
# births$age[which(births$age >= 70)] <- NA
# births$ed[which(births$ed == 9)] <- NA
# births$weight[which(births$weight > 6000)] <- NA
# births$married[which(births$married %in% c("U", "W"))] <- NA
# births$married <- births$married == "Y"
# births$wic[which(births$wic == "U")] <- NA
# births$wic <- births$wic == "Y"
# births$pay <- ifelse(births$pay == 1, "Medicaid",
#                      ifelse(births$pay == 2, "Private",
#                             ifelse(births$pay == 3, "Self-pay",
#                                    NA)))
# births$medicaid <- births$pay == "Medicaid"
# births$pay <- NULL
# births$black <- births$black == "Y"
# births$white <- births$white == "Y"
# births$tobacco <- births$tobacco == "Y"
# save.image("births_2000-2014.RData")

###
# LOAD 2000-2014 DATA
###
load("births_2000-2014.RData")

###
# LOAD FLORIDA ZIP CODE SHAPEFILE
###
setwd(public)
library(rgdal)
fl <- readOGR("florida_zip", "zip")

###
# READ IN INCOME AND STD RATES
###
# read in the std rate data
std <- read.csv("std.csv", stringsAsFactors = FALSE)

# compare to see if names are perfect matches
fl$NAME <- toupper(fl$NAME)
table(fl@data$NAME == std$NAME) # they're not all correct

# get the closest match for each county

fl$name <- NA # create empty vector where we'll put our names
std$name <- std$NAME # we'll use lower cases for the matches
for (i in 1:nrow(fl)){
  # see how close (in character changes) each name in fl is to those in STD
  m <- adist(fl$NAME[i],
             std$NAME)
  # get the one with the least differences
  ind <- which.min(m)
  # get the name from std
  best <- std$NAME[ind]
  # assign to fl
  fl$name[i] <- best
}

# Merge the two datasets together
fl@data <- merge(x = fl@data,
                 y = std,
                 by = "name",
                 all.x = TRUE,
                 all.y = FALSE)


###
# EXPLORE A BIT
###
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

