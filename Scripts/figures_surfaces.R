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

#yes accounting for digestibilty 
fitDMD <- Tps(trials[, .(DMDI, DPI)], trials$Weight_change, scale.type = "range")
surface(fitDMD)



# Protein and NDF ---------------------------------------------------------

#not accounting for digestibility
fitCP <- Tps(trials[, .(DMI_NDF_bw, DMI_CP_bw)], trials$Weight_change, scale.type = "range")
surface(fitCP)

#yes accounting for digestibility
fitDP <- Tps(trials[, .(DNDFI, DPI)], trials$Weight_change, scale.type = "range")
surface(fitDP)



#NDF and CP intake effect on dry matter digestibility
# from just diet A to diet B
trials2 <- trials[Diet == "A" | Diet == "B"]
dmd <- Tps(trials2[, .(DMI_ADL_bw, DMI_CP_bw)], trials2$DMD, scale.type = "range")
surface(dmd)
# any kind of fibre works. A very balanced relationship between protein and fibre