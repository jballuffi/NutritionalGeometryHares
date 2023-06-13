#using fields package to visualize hare performance across nutritional space

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in results
trials <- readRDS("Output/data/trialresultscleaned.rds")
day <- readRDS("Output/data/dailyresultscleaned.rds")



# learning some plot methods ----------------------------------------------

#example of quiltplot
data( ozone2)
fitozone <- Tps(ozone2$lon.lat, ozone2$y[16,], scale.type = "range")
surface(fitozone)
image.plot(fitozone)

quilt.plot( ozone2$lon.lat, ozone2$y[16,])



# Protein and DMI  -------------------------------

#not accounting for digestibility
fitDMI <- Tps(trials[, .(DMI_bw, DMI_CP_bw)], trials$Weight_change, scale.type = "range")
surface(fitDMI)

trials2 <- trials[!is.na(DMDI)]

#yes accounting for digestibilty 
fitDMD <- Tps(trials2[, .(DMDI, DPI)], trials2$Weight_change, scale.type = "range")
surface(fitDMD)



# Protein and NDF ---------------------------------------------------------

#not accounting for digestibility
fitCP <- Tps(trials[, .(DMI_NDF_bw, DMI_CP_bw)], trials$Weight_change, scale.type = "range")
surface(fitCP)

#yes accounting for digestibility
fitDP <- Tps(trials2[, .(DNDFI, DPI)], trials2$Weight_change, scale.type = "range")
surface(fitDP)


