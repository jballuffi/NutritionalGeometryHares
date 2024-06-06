#script that merges and cleans all feeding trial results

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


# read in data ------------------------------------------------------------

#read in feeding trial data
trials <- fread("Input/Results_singlechoice.csv")

#read in the nutritional compositions of each diet
diets <- fread("Input/Diet_compositions.csv")

#read in prepped diet compositions
dietcomp <- fread("Output/data/dietcompositions.rds")

#read in daily dry matter measures
DM <- fread("Input/Daily_DryMatter.csv")

#read in amounts and DMs of spilled food (leftover food that fell and mixed in with feces)
spill <- fread("Input/Daily_food_remainders.csv")

#read in fecal response data
feces <- fread("Input/Results_feces.csv")

#read in temp data
temp2022 <- fread("Input/temperatures_SW_2022.csv")
temp2023 <- fread("Input/temperature_SW_2023.csv")



# Melt feeding trial data into individual days-------------------------------------------------

#make lists of columns for the starting food masses and the ending food masses 
offercols <- grep("offer_wet", names(trials), value = TRUE)
endcols <- grep("end_wet", names(trials), value = TRUE)

#melt into one day of feeding trial per row
mtrials <- melt(trials, measure.vars = offercols, variable.name = "DayOffer", value.name = "OfferWet" )
mtrials <- melt(mtrials, measure.vars = endcols, variable.name = "DayEnd", value.name = "EndWet")

#cut out the words within the DayOffer and DayEnd columns
mtrials[, DayOffer := gsub("offer_wet", "", DayOffer)]
mtrials[, DayEnd := gsub("end_wet", "", DayEnd)]

#create a new column that shows whether or not DayOffer and DayEnd are the same
mtrials$test <- ifelse(mtrials$DayOffer == mtrials$DayEnd, "equal", "not equal")
mtrials[, test := ifelse(DayOffer == DayEnd, 'equal', 'not equal')]

#subset data to only include cases where DayOffer and Dayend are he same
DT <- mtrials[test == "equal"]

#clean up useless columns, rename day offer to just show day
#D1 is results after 24 hours, D2 is after 48, D3 is after 72
DT[, test := NULL][, DayEnd := NULL]
setnames(DT, "DayOffer", "Day")



# create date column and unique sample IDs ------------------------

DT[, Date_start := lubridate::mdy(Date_start)]
DT[, Date_end := lubridate::mdy(Date_end)]


#create a date for each day of the feeding trials based on the "day" column
DT[Day == "D1", Date := Date_start + 1][Day == "D2", Date := Date_start + 2][Day == "D3", Date := Date_start + 3]

#paste enclosure and date together to create a 'sample id' that can be merged with lab results
DT[, Sample := paste0(Enclosure, "_", substring(Date, 3))] #remove the first two characters from year

#create winter col 
DT[, Winter := year(Date)]



# merge with daily dry matter and diet compositions --------------------------------

#remove DM from dietcomp. Calculated by winter
dietcomp[, DM_diet := NULL]

#remove excess columns in daily DM table
DM <- DM[, .(Sample, DM)]

#pull diet DMs by winter
dietDM <- diets[, mean(DM), by = .(Winter, Sample)]
names(dietDM) <- c ("Winter", "Diet", "DM_diet")

#merge feeding data (wet weights) with daily DM data
DT <- merge(DT, DM, by = "Sample", all.x = TRUE)

#merge diet compositions into datasheet
DT <- merge(DT, dietcomp, by = "Diet", all.x = TRUE)

#merge diet DM into datasheet
DT <- merge(DT, dietDM, by = c("Winter", "Diet"), all.x = TRUE)

#create mean DMs by winter
meanDM <- DT[,  mean(DM, na.rm = TRUE), Winter]

DT[is.na(DM) & Winter == 2022, DM := meanDM[Winter == 2022, V1]]
DT[is.na(DM) & Winter == 2023, DM := meanDM[Winter == 2023, V1]]


# merge in spilled food data --------------------------------------------
#this is food that got knocked out of dishes and fell in with poop
#has a mass and a unique DM

#remove excess columns from remainder/spilled food table
spill <- spill[, .(Sample, Total_DM)]
setnames(spill, "Total_DM", "DM_spilled")

#merge in the mass of spilled food with full datasheet
DT <- merge(DT, spill, by = "Sample", all.x = TRUE)
DT[is.na(DM_spilled), DM_spilled := 0] #fill NAs with 0 for times where no food was spilled



# merge in fecal data ------------------------------------------------------

#pull diets from sample IDs in main data
IDdiet <- DT[, .(Sample, Diet)]

#merge diets with feces
feces <- merge(IDdiet, feces, all = TRUE)

#average compositions by diet. This is for filling in gaps in data for now
avg <- feces[, .(mNDF = mean(NDF_F, na.rm = TRUE),
                 mADF = mean(ADF_F, na.rm = TRUE),
                 mADL = mean(ADL_F, na.rm = TRUE), 
                 mCP = mean(CP_F, na.rm = TRUE),
                 mC = mean(C_F, na.rm = TRUE)), Diet]

#merge averages with full feces data
feces <- merge(feces, avg, by = "Diet", all.x = TRUE)

#replace NA's with the averages
feces[is.na(NDF_F), NDF_F := mNDF][is.na(ADF_F), ADF_F := mADF][is.na(ADL_F), ADL_F := mADL]
feces[is.na(CP_F), CP_F := mCP][is.na(C_F), C_F := mC]
mDM <- feces[, mean(DM, na.rm = TRUE)]
feces[is.na(DM), DM := mDM]

#calculate excretion rates for each nutrient (g/day)
feces[, DMF := Total_dried*DM] #total dry matter
feces[, DMF_NDF := DMF*(NDF_F/100)] #total NDF on DM basis (g)
feces[, DMF_ADF := DMF*(ADF_F/100)] #total ADF on DM basis (g)
feces[, DMF_ADL := DMF*(ADL_F/100)] #total ADF on DM basis (g)
feces[, DMF_CP := DMF*(CP_F/100)] #total CP on DM basis (g)

#cut the fecal data down to just fecal output columns
fecaloutput <- feces[, .(Sample, 
                         CP_F, NDF_F, ADF_F, ADL_F,#fecal compositions
                         DMF, DMF_NDF, DMF_ADF, DMF_ADL, DMF_CP)]  #fecal compositions

#merge intake rates and DMs with fecal output data
DT <- merge(DT, fecaloutput, by = "Sample", all.x = TRUE)



# Calculate dry matter intake with and without weight --------------------------------------

#calculate start and end food weights in terms of dry matter
DT[, DM_offer := OfferWet*DM_diet]
DT[, DM_end := (EndWet*DM/100) + DM_spilled] #end weight adds in the dry matter of spilled food

#calculate dry matter intake
DT[, DMI := DM_offer - DM_end]

#calculate dry matter intake of each currency
DT[, DMI_CP := DMI*CP_diet]   #dry matter intake protein (g)
DT[, DMI_NDF := DMI*NDF_diet] #dry matter intake of NDF (g)
DT[, DMI_ADF := DMI*ADF_diet] #dry matter intake of ADF (g)
DT[, DMI_ADL := DMI*ADL_diet] #dry matter intake of ADL (g)
DT[, DMI_C := DMI*C_diet]     #dry matter intake of carbon (g)
DT[, DMI_energy := DMI*Energy_diet/1000] #intake of energy (kj)

#convert weights from g to kg
DT[, Weight_start := Weight_start/1000]
DT[, Weight_end := Weight_end/1000]

#calculate dry matter intakes by kg^.75
DT[, DMI_bw := DMI/(Weight_start^.75)]
DT[, DMI_CP_bw := DMI_CP/(Weight_start^.75)]
DT[, DMI_NDF_bw := DMI_NDF/(Weight_start^.75)]
DT[, DMI_ADF_bw := DMI_ADF/(Weight_start^.75)]
DT[, DMI_ADL_bw := DMI_ADL/(Weight_start^.75)]
DT[, DMI_C_bw := DMI_C/(Weight_start^.75)]
DT[, DMI_energy_bw := DMI_energy/(Weight_start^.75)]



# Calculate digestibility -----------------------------------------------

DT[, DMD := (DMI-DMF)/DMI] #dry matter digestibility (%)
DT[, DP := (DMI_CP - DMF_CP)/DMI_CP] #digestible protein (%)
DT[, DNDF := (DMI_NDF - DMF_NDF)/DMI_NDF] #digestible NDF (%)
DT[, DADF := (DMI_ADF - DMF_ADF)/DMI_ADF] #digestible ADF (%)
DT[, DADL := (DMI_ADL - DMF_ADL)/DMI_ADL] #digestible ADL (%)
DT[, DE := DMD*Energy_diet/1000] #digestible energy (kj/g)


# Calculate digestibility intake ------------------------------------------

#these intake rates are on a kg^.75 basis
DT[, DMDI := DMD*DMI_bw]        #digestible dry matter intake (g/kg.75)
DT[, DPI := DP*DMI_CP_bw]       #digestible protein intake (g/kg.75)
DT[, DNDFI := DNDF*DMI_NDF_bw]  #digestible NDF intake (g/kg.75)
DT[, DADFI := DADF*DMI_ADF_bw]  #digestible ADF intake (g/kg.75)
DT[, DEI := DE*DMI_bw] #digestible energy intake (kj/kg.75)



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



# create simplified datasheet for daily results --------------------------------------

#cut out a datasheet of just key feeding trial info and results
Dailyresults <- DT[, c("Diet", "Sample", "ID", "Trial", "Day", "Date_start", "Date_end", "Date", #info
                       "CP_diet", "NDF_diet", "ADF_diet", "ADL_diet", #diet compositions
                       "DMI", "DMI_CP", "DMI_NDF", "DMI_ADF", "DMI_ADL", #dry matter intakes (g/day)
                       "Weight_start", "Weight_end", #weights (g)
                       "DMI_bw", "DMI_CP_bw", "DMI_NDF_bw", "DMI_ADF_bw", "DMI_ADL_bw", #dry matter intake by weight (g/kg^.75/day)
                       "DMF", "DMF_CP", "DMF_NDF", "DMF_ADF", "DMF_ADL", #fecal outputs (g/day) 
                       "CP_F", "NDF_F", "ADF_F", "ADL_F", #fecal compositions (%)
                       "DMD", "DP", "DNDF", "DADF", "DADL",  #digestibility (%)
                       "DMDI", "DPI", "DNDFI", "DADFI", #digestible intake rate (g/kg^.75/day)
                       "Temp"
)] 


#remove two random duplicates. 
Dailyresults <- Dailyresults[!duplicated(Sample)==TRUE]


#cut out three samples with weirdly negative NDF digestion
#Dailyresults <- Dailyresults[!DNDF < -.10]

    ## ^^^ look into this in the lab


# create trial results ----------------------------------------------------

#run the trialavg function (in R folder) by ID and Diet (trial is extra, same as diet)
#trials is a spreadsheet with results for whole trials
trials <- Dailyresults[, trialavg(.SD), by = c("ID", "Diet", "Trial")]

#calculate weight change per day for each trial (% change/day)
trials[, Weight_change := (((Weight_end - Weight_start)/Weight_start)*100)/3]



# save results ------------------------------------------------------------

#save daily format of results
saveRDS(Dailyresults, "Output/data/dailyresultscleaned.rds")
#save trial format of results
saveRDS(trials, "Output/data/trialresultscleaned.rds")

