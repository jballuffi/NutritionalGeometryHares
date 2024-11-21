

temp2022 <- fread("Input/temperatures_SW_2022.csv")
temp2023 <- fread("Input/temperature_SW_2023.csv")


#old code for pulling temperature logger data

# Merge in daily temperatures ---------------------------------------------

#create a daily start and end time for feeding trials
DT[, Time_start := "11:00:00"][, Time_end := "11:00:00"]

#create a datetime for feeding trial starts and ends
DT[, DayTime_start := lubridate::as_datetime(paste0(Date-1, " ", Time_start))]
DT[, DayTime_end := lubridate::as_datetime(paste0(Date, " ", Time_end))]

#get date columns from both years into the same format 
temp2022[, Date := lubridate::ymd(Date)]
temp2023[, Date := lubridate::mdy(Date)]

#rbind both years of temperature data
temp <- rbind(temp2022, temp2023)

#in temp data, merge date and time into a datetime
temp[, DateTime := lubridate::as_datetime(paste0(Date, " ", Time, " ", TimeStamp))]

#run the tempcalc function (in R folder) by day and ID
DT[, Temp := tempcalc(start = DayTime_start, end = DayTime_end), by = .(ID, Trial, Day)]

