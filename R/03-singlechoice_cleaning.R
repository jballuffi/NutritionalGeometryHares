#script that merges and cleans all feeding trial results

library(data.table)
library(lubridate)
library(ggplot2)



# read in data ------------------------------------------------------------

#read in feeding trial data
trials <- fread("Input/Results_singlechoice.csv")

#read in the nutritional compositions of each diet
diets <- fread("Input/Diet_compositions.csv")
avgDietDM <- mean(diets$DM, na.rm =TRUE) #cal avg DM for all diets
diets[is.na(DM), DM := avgDietDM] #one diet is missing DM, replace with avg for now

#read in temp data
temp <- fread("Input/temperatures_SW_2022.csv")

#read in daily dry matter measures
DM <- fread("Input/Daily_DryMatter.csv")
avgSampleDM <- mean(DM$DM, na.rm =TRUE) #calculate avg dry matter for all samples

#read in any food remainder data (leftover food that fell and mixed in with feces)
rem <- fread("Input/Daily_food_remainders.csv")

#read in fecal response data
feces <- fread("Input/Results_feces.csv")



# Melt feeding trial data -------------------------------------------------

#make lists of columns for the starting food masses and the ending food masses 
offercols <- grep("offer_wet", names(trials),value = TRUE)
endcols <- grep("end_wet", names(trials),value = TRUE)

#melt into one day of feeding trial per row
mtrials <- melt(trials, measure.vars = offercols, variable.name = "DayOffer", value.name = "OfferWet" )
mtrials <- melt(mtrials, measure.vars = endcols, variable.name = "DayEnd", value.name = "EndWet")

#cut out the words within the DayOffer and DayEnd columns
mtrials[, DayOffer := gsub("offer_wet", "", DayOffer)]
mtrials[, DayEnd := gsub("end_wet", "", DayEnd)]

#create a new column that shows whether or not DayOffer and DayEnd are the same
mtrials$test <- ifelse(mtrials$DayOffer == mtrials$DayEnd, "equal", "not equal")

#subset data to only include cases where DayOffer and Dayend are he same
DT <- mtrials[test == "equal"]

#clean up useless columns, rename day offer to just show day
#D1 is results after 24 hours, D2 is after 48, D3 is after 72
DT[, test := NULL][, DayEnd := NULL]
setnames(DT, "DayOffer", "Day")



# create date column and unique sample IDs ------------------------

#create a date for each day of the feeding trials based on the "day" column
DT[Day == "D1", Date := Date_start + 1][Day == "D2", Date := Date_start + 2][Day == "D3", Date := Date_start + 3]

#paste enclosure and date together to create a 'sample id' that can be merged with lab results
DT[, Sample := paste0(Enclosure, "_", Date)]
DT[, Sample := gsub("2022", "22", Sample)]



# merge with dry matter and food remainder data --------------------------------

#make just a DM table
DM <- DM[, .(Sample, DM)]

#make just a total remainder table (this is food that fell and mixed with poop)
rem <- rem[, .(Sample, Total_DM)]

#make just a diet DM table
dietDM <- diets[, .(Diet, DM)]

#merge feeding data (wet weights) with DM data
DT <- merge(DT, DM, by = "Sample", all.x = TRUE)

#any lines with missing DM get the average
DT[is.na(DM), DM := avgSampleDM]

#merge in the mass of food that fell in with poop
DT <- merge(DT, rem, by = "Sample", all.x = TRUE)
setnames(DT, "Total_DM", "Rem_DM") #rename to be specific about remainders
DT[is.na(Rem_DM), Rem_DM := 0] #fill in cases where no food was dumped




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




# merge daily dry matter measures -----------------------------------------










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



