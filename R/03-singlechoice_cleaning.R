#script that merges and cleans all feeding trial results

library(data.table)
library(lubridate)
library(ggplot2)

#read in feeding trial data
SC <- fread("Input/Results_singlechoice.csv")
SC <- SC[!is.na(Trial)] #remove space holders

#read in the nutritional compositions of each diet
diets <- fread("Input/Diet_compositions.csv")

#read in temp data
temp <- fread("Input/temperatures_SW_2022.csv")

#merge date and time into a datetime
temp[, DateTime := as_datetime(paste0(Date, " ", Time, " ", TimeStamp))]

#cut for only dates in which feeding trials occurred
temp <- temp[Date > '2022-02-08' & Date < '2022-03-15']

#plot that shows temp over time for the whole study period
ggplot(temp)+
  geom_line(aes(y = Temp, x = DateTime))+
  labs(x = "Date", y = "Temperature (C)")+
  theme_minimal()

#create a daily start and end time for feeding trials
SC[, Time_start := "10:00:00"][, Time_end := "10:00:00"]

#create a datetime for feeding trial starts and ends
SC[, DateTime_start := as_datetime(paste0(Date_start, " ", Time_start))]
SC[, DateTime_end := as_datetime(paste0(Date_end, " ", Time_end))]

#function that calculates mean temperature between start and end date-times of feeding trials
tempcalc <- function(start, end) {
  avgtemp <- temp[DateTime > start & DateTime < end, mean(Temp)]
}

#run the tempcalc function by feeding trial (i.e., ID and trial number)
SC[, Temp := tempcalc(start = DateTime_start, end = DateTime_end), by = .(ID, Trial)]


#Calculate intake rates and weight loss
SC[, D1 := D1offer_wet - D1end_wet] #day 1 of consumption
SC[, D2 := D2offer_wet - D2end_wet] #day 2 of consumption
SC[, D3 := D3offer_wet - D3end_wet] #day 3 of consumption
SC[, Consumed := ((D1 + D2 + D3)/(Weight_start/1000))/3] #consumption by kg bodyweight
SC[, Weight_change := (((Weight_end - Weight_start)/Weight_start)*100)/3]



#merge feeding results with diet compositions by diet
SCdiets <- merge(SC, diets, by = "Diet", all.x = TRUE)

SCdiets <- SCdiets[!is.na(Consumed)]

#calculate the intake of protein (g/kg body mass/3 days) by diet 
SCdiets[, Consumed_CP := Consumed*(Protein/100)]
#calculate the intake of fibre (g/kg body mass/3 days) by diet
SCdiets[, Consumed_NDF := Consumed*(NDF/100)]




SCmeans <- SCdiets[, .(mean(Consumed), sd(Consumed), mean(Weight_change), sd(Weight_change)), by = Diet]
names(SCmeans) <- c("Diet", "Consumed_mean", "Consumed_SD", "Weight_mean", "Weight_SD")

Macromeans <- SCdiets[, .(mean(Consumed_CP), sd(Consumed_CP), mean(Consumed_NDF), sd(Consumed_NDF)), by = Diet]
names(Macromeans) <- c("Diet", "CP_mean", "CP_SD", "NDF_mean", "NDF_SD")



