#using fields package to visualize hare performance across nutritional space

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


#read in results
trials <- readRDS("Output/trialresultscleaned.rds")
days <- readRDS("Output/dailyresultscleaned.rds")


fitweight <- Tps(trials[, .(NDF_in_bw, CP_in_bw)], trials$Weight_change, scale.type = "range")
surface(fitweight)

fitdigCP <- Tps(day[, .(NDF_in_bw, CP_in_bw)], day$CP_dig, scale.type = "range")
surface(fitdigCP)

fitdigNDF <- Tps(day[, .(NDF_in_bw, CP_in_bw)], day$NDF_dig, scale.type = "range")
surface(fitdigNDF)

fitdigADF <- Tps(day[, .(NDF_in_bw, CP_in_bw)], day$ADF_dig, scale.type = "range")
surface(fitdigADF)
