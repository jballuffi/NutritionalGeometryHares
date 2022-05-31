#script that merges and cleans all feeding trial results

library(data.table)
library(lubridate)
library(ggplot2)

#read in temp data
temp <- fread("Input/temperatures_SW_2022.csv")

#read in cleaned daily feeding trial results
day <- readRDS("Output/dailyresultscleaned.rds")

#average results by feeding trial



#calculate weight change per day for the entire trial
trials[, Weight_change := (((Weight_end - Weight_start)/Weight_start)*100)/3]

#calculate average intake rate (IR) for entire trial per kg of body weight
trials[, IR_trial := ((D1 + D2 + D3)/(Weight_start/1000))/3] 

#subset data to just be those intake rates and overall weight change
SC <- trials[, .(Diet, ID, Trial, Enclosure, Date_start, Date_end, D1, D2, D3, IR_trial, Weight_change)]




#create a daily start and end time for feeding trials
SC[, Time_start := "10:00:00"][, Time_end := "10:00:00"]

#create a datetime for feeding trial starts and ends
SC[, DateTime_start := as_datetime(paste0(Date_start, " ", Time_start))]
SC[, DateTime_end := as_datetime(paste0(Date_end, " ", Time_end))]



# merge temperature data --------------------------------------------------

#merge date and time into a datetime
temp[, DateTime := as_datetime(paste0(Date, " ", Time, " ", TimeStamp))]

#cut for only dates in which feeding trials occurred
temp <- temp[Date > '2022-02-08' & Date < '2022-03-15']

#plot that shows temp over time for the whole study period
ggplot(temp)+
  geom_line(aes(y = Temp, x = DateTime))+
  labs(x = "Date", y = "Temperature (C)")+
  theme_minimal()

#function that calculates mean temperature between start and end date-times of feeding trials
tempcalc <- function(start, end) {
  avgtemp <- temp[DateTime > start & DateTime < end, mean(Temp)]
}

#run the tempcalc function by feeding trial (i.e., ID and trial number)
SC[, Temp := tempcalc(start = DateTime_start, end = DateTime_end), by = .(ID, Trial)]


