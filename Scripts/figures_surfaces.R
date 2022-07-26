#using fields package to visualize hare performance across nutritional space

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


#read in results
trials <- readRDS("Output/data/trialresultscleaned.rds")
day <- readRDS("Output/data/dailyresultscleaned.rds")


#example of quiltplot
data( ozone2)
fitozone <- Tps(ozone2$lon.lat, ozone2$y[16,], scale.type = "range")
surface(fitozone)
image.plot(fitozone)

quilt.plot( ozone2$lon.lat, ozone2$y[16,])



# surface plots for weight change digestion -------------------------------



fitweight <- Tps(trials[, .(NDF_in_bw, CP_in_bw)], trials$Weight_change, scale.type = "range")
surface(fitweight)



fitdigCP <- Tps(day[, .(NDF_in_bw, CP_in_bw)], day$CP_dig, scale.type = "range")
surface(fitdigCP)

fitdigNDF <- Tps(day[, .(NDF_in_bw, CP_in_bw)], day$NDF_dig, scale.type = "range")
surface(fitdigNDF)

fitdigADF <- Tps(day[, .(NDF_in_bw, CP_in_bw)], day$ADF_dig, scale.type = "range")
surface(fitdigADF)

plot(fitdigCP)

ggplot(fitdigCP)+
  geom_raster(aes(x = x, y = y, fill = z), interpolate = FALSE)
  


savePlot("Output/figures/CPsurface.jpeg", CPdig, type = "jpg")
