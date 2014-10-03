library(Hmisc)
library(RColorBrewer)
library(car)

# Set wd
setwd("C:/Users/BrewJR/Documents/tobacco")

# Read in data
comb <- read.csv("comb1.csv")

# Read in names of questions
comb_names <- read.csv("comb1.csv", header = FALSE)
comb_names <- comb_names[1,]

###################
# SIMPASYM FUNCTION FOR CALCULATIN C.I. OF PROPORTIONS
###################
simpasym <- function(n, p, z=1.96, cc=TRUE){
  out <- list()
  if(cc){
    out$lb <- p - z*sqrt((p*(1-p))/n) - 0.5/n
    out$ub <- p + z*sqrt((p*(1-p))/n) + 0.5/n
  } else {
    out$lb <- p - z*sqrt((p*(1-p))/n)
    out$ub <- p + z*sqrt((p*(1-p))/n)
  }
  out
}

###################
# WRITE FUNCTION FOR PLOTTING BARS
###################
BarFun <- function(var, recode_var = NULL){
  
  # Ensure it's treated as a character
  var <- as.character(var)
  
  # Replace 77,88,99 with "Unknown"
  var[which(var %in% c(77,88,99))] <- "Unknown"
  
  # If recoding vector is supplied, use it
  if(is.null(recode_var)){
    var <- Recode(var,
                  "'1' = 'Yes';
                '2' = 'No'")
  } else {
    var <- Recode(var,
                  recode_var)    
  }
  
  # Make var a factor and relevel so "unknown" comes first
  var <- factor(var)
  var <- relevel(var, ref = "Unknown")
    
  # Make a table
  var_table <- table(var)
  var_names <- names(var_table)
  var_vals <- as.numeric(var_table)
  var_prop <- prop.table(var_table) * 100
  
  # Assing positions based on relative value compared to others
  var_pos <- ifelse(var_prop < 0.5 * max(var_prop),
                    3, 1)
  
  # Calculate confidence intervals
  ci <- simpasym(n=sum(var_table), 
                 p=var_prop / 100, 
                 z=1.96, cc=TRUE) 
  lb <- ci$lb * sum(var_table)
  ub <- ci$ub * sum(var_table)
  
  # Create color vector (first is always red if there are unknowns)
  if( var_table["Unknown"] > 0 ){
    my_colors <- c("Red", colorRampPalette(c("darkblue", "darkgreen"))(length(levels(var)) -1))
  } else {
    my_colors <- colorRampPalette(c("darkblue", "darkgreen"))(length(levels(var)))
  }
  my_colors <- adjustcolor(my_colors, alpha.f = 0.6)
  
  # Make barplot
  bp <- barplot(var_table,
                ylim = c(0, max(ub)*1.2),
                col = my_colors,
                border = "darkgrey")
  
  # Add point estimate text
#   text(x = bp[,1],
#        y = var_vals,
#        labels = paste0(round(var_prop, digits = 1), "%"),
#        pos = var_pos)
  
# Add error bars
  errbar(x=bp[,1],
         y=var_table,
         yplus=ub,
         yminus=lb,
         add=TRUE,
         type="n",
         errbar.col=adjustcolor("darkred", alpha.f=0.6),
         lwd=2)
  
  # Add text of point estimate with c.i.'s
  text(x=bp[,1], 
       y= ifelse(var_pos == 1, 
                 var_table - (.15*max(var_table)),
                 var_table + (.15*max(var_table))),
       labels=paste0(" ", round(var_prop, digits=1),
                     "%",
                     "\n(",
                     round(ci$lb*100, digits=1),
                     "%",
                     "-",
                     round(ci$ub*100, digits=1),
                     "%",
                     ")"
       ),
       cex=2/length(var_table),
       col=adjustcolor("black", alpha.f=0.7))
  
}

par(mfrow=c(1,1))
BarFun(comb[,7])
title(main = comb_names[,7])

par(mfrow=c(1,2))
BarFun(comb[which(comb[,5] == 1),8])
title(main = "< 50 employees", cex.main = 0.8)
BarFun(comb[which(comb[,5] == 2),8])
title(main = "50 + employees", cex.main = 0.8)
title(main = "Does your employeer provide health insurance?",
      outer = TRUE,
      line = -1)

###############
# GEOCODING
###############

# Having already geocoded, just read in locations
locations <- read.csv("locations.csv")

# merge with comb
comb <- cbind(comb, locations)

# THE BELOW ONLY NEEDS TO BE RUN ONCE:
# places <- paste0(comb$Name.of.Employer,
#                 ", ", 
#                 comb$County.Reporting,
#                 " ", "county",
#                 ", ",
#                 "Florida")

# library(RCurl)
# library(RJSONIO)
# library(plyr)
# 
# url <- function(address, return.call = "json", sensor = "false") {
#   root <- "http://maps.google.com/maps/api/geocode/"
#   u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
#   return(URLencode(u))
# }
# 
# geoCode <- function(address,verbose=FALSE) {
#   if(verbose) cat(address,"\n")
#   u <- url(address)
#   doc <- getURL(u)
#   x <- fromJSON(doc,simplify = FALSE)
#   if(x$status=="OK") {
#     lat <- x$results[[1]]$geometry$location$lat
#     lng <- x$results[[1]]$geometry$location$lng
#     location_type <- x$results[[1]]$geometry$location_type
#     formatted_address <- x$results[[1]]$formatted_address
#     return(c(lat, lng, location_type, formatted_address))
#   } else {
#     return(c(NA,NA,NA, NA))
#   }
# }
# 
# 
# locations <- ldply(places, function(x) geoCode(x))
# names(locations) <- c("lat","lon","location_type", "forAddress")
# 
# #write.csv(locations, "locations.csv")

###############
# MAP FUNCTION
###############
par(mfrow = c(1,1))
par(mar=c(1,1,1,1))
library(maps)
mymap <- map("county", "fl",
             fill = TRUE,
             col = "grey",
             border = "darkgrey")

size_col <- ifelse(comb[,5] == 1, "blue", 
                   ifelse(comb[,5] == 2, "red",
                          "darkgreen"))
size_pch <- ifelse(comb[,5] == 1, 16,
                   ifelse(comb[,5] == 2, 15,
                          17))

# points(comb$lon, comb$lat, col = adjustcolor("black", alpha.f = 0.6),
#        cex = 0.5)
points(comb$lon, comb$lat, 
       col = adjustcolor(size_col, alpha.f = 0.2),
       pch = size_pch,
       cex = 0.5)

legend(x = "left",
       pch = c(16,15,17),
       col = adjustcolor(c("blue", "red", "darkgreen"), alpha.f = 0.4),
       legend = c("Small (<50)", "Large (50+)", "Unknown"),
       bty = "n",
       cex = 0.75,
       title = "Business size")




##########
# MAP FUNCTION
##########
TractFun <- function(var, color){
  plotvar <- var
  nclr <- 5
  plotclr <- brewer.pal(nclr, color)
  class <- classIntervals(plotvar, nclr, style = "quantile", dataPrecision=0) #use "equal" instead
  #class <- classIntervals(0:100, nclr, style="equal")
  colcode <- findColours(class, plotclr)
  legcode <- paste0(gsub(",", " - ", gsub("[[]|[]]|[)]", "", names(attr(colcode, "table")))), "%")
  plot(tract2, border=NA, col=colcode)
  legend("left", # position
         legend = legcode, #names(attr(colcode, "table")), 
         fill = attr(colcode, "palette"), 
         cex = 0.6, 
         border=NA,
         bty = "n")
}