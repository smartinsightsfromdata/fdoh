library(gstat)
library(geoR)

disease <- "wnv"

SurfaceFun <- function(disease){
  temp <- master[,which(grepl(disease ,colnames(master)) & 
                          grepl("site", colnames(master)))]
  temp <- as.data.frame(cbind(date=master$date, temp))
  temp <- temp[which(temp$date == 
                       max(temp$date[which(is.na(temp[,paste0(disease, "site1")]) == FALSE)])),]
  temp <- as.numeric(temp[-1])
  
  a <- data.frame("x" = loc$lon,
                  "y" = loc$lat,
                  "z" = temp)
  b <- as.geodata(a)
  
  # Predict one point
  prediction <- ksline(b, cov.model="exp", cov.pars=c(10,3.33), nugget=0,
                       locations=c(-82.15,29.323))
  
  # Predict multiple points
   x <- seq(-83, -82, by=0.01)
   y <- seq(29,30, by=0.01)
   xv <- rep(x,14)
   yv <- rep(y, each=14)
   in_mat <- as.matrix(cbind(xv,yv))

  # 
  q <- ksline(b, cov.model="exp",cov.pars=c(10,3.33), nugget=0,
              locations=in_mat)
  q$predict
  image(q, val=q$predict)
  image(q)
  
  
  # Construct matrix in alternative way
#    x.range <- as.integer(range(a[,1]))
#    y.range <- as.integer(range(a[,2]))
#    x=seq(from=x.range[1], to=x.range[2], by=1)
#    y=seq(from=y.range[1], to=y.range[2], by=1)
#    length(x)
#    length(y)
#    xv <- rep(x,length(y))
#    yv <- rep(y, each=length(x))
#    in_mat <- as.matrix(cbind(xv,yv))
#    plot(in_mat)
  
  
  

  
  
  
  
  mymap <- map("county", "florida", 
      fill=TRUE, 
      col=c(alachuacol2,countycols2),
      xlim=c(-82.75,-82), 
      ylim=c(29.4,30),
      border=NA, 
      add=F)
  title(main=toupper(disease), cex.main=0.8)
  
  #ADD TRAP POINTS
  points(loc$long, loc$lat,
         pch=16,
         col=adjustcolor("darkred", alpha.f=0.6),
         cex=temp^(1/4))
  
  legend(x="bottomleft",
         pch=16,
         col=adjustcolor("grey", alpha.f=0.6),
         pt.cex=c(10, 50, 200)^(1/4),
         legend=c(10,50,200), bty="n")
}