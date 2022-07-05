#script that calculates final responses by feeding trial, merged with temp data

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in cleaned daily feeding trial results
day <- readRDS("Output/data/dailyresultscleaned.rds")


#run the trialavg function (in R folder) by ID and Diet (trial is extra, same as diet)
#trials is a spreadsheet with results for whole trials
trials <- day[, trialavg(.SD), by = c("ID", "Diet", "Trial")]

#calculate weight change per day for each trial (% change/day)
trials[, Weight_change := (((Weight_end - Weight_start)/Weight_start)*100)/3]

#save trial format of results
saveRDS(trials, "Output/data/trialresultscleaned.rds")
