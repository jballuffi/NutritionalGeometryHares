
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


#read in results
trials <- readRDS("Output/data/trialresultscleaned.rds")
day<- readRDS("Output/data/dailyresultscleaned.rds")


trials[, CP2 := CP_in_bw^2]
trials[, NDF2 := NDF_in_bw^2]

summary(lm(Weight_change ~ CP_in_bw + NDF_in_bw, data = trials))

summary(lm(Weight_change ~ CP_in_bw + CP2, data = trials))


plot(Weight_change ~ CP_in_bw, data = trials)

#Weight change

WC <- lm(trials$Weight_change ~ trials$CP_in_bw*trials$NDF_in_bw)

summary(WC)
