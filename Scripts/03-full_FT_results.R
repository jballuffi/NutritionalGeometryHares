#script that calculates final responses by feeding trial, merged with temp data

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in cleaned daily feeding trial results
day <- readRDS("Output/dailyresultscleaned.rds")



#average results by feeding trial



#calculate weight change per day for the entire trial
trials[, Weight_change := (((Weight_end - Weight_start)/Weight_start)*100)/3]

#calculate average intake rate (IR) for entire trial per kg of body weight
trials[, IR_trial := ((D1 + D2 + D3)/(Weight_start/1000))/3] 

#subset data to just be those intake rates and overall weight change
SC <- trials[, .(Diet, ID, Trial, Enclosure, Date_start, Date_end, D1, D2, D3, IR_trial, Weight_change)]



