#script that merges and cleans all feeding trial results

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


# read in data ------------------------------------------------------------

#read in feeding trial data
trials <- fread("Input/Results_singlechoice.csv")

#read in the nutritional compositions of each diet
diets <- fread("Input/Diet_compositions.csv")

#read in daily dry matter measures
DM <- fread("Input/Daily_DryMatter.csv")
DM[, DM := DM/100]
avgSampleDM <- mean(DM$DM, na.rm =TRUE) #calculate avg dry matter for all samples

#read in amounts and DMs of spilled food (leftover food that fell and mixed in with feces)
spill <- fread("Input/Daily_food_remainders.csv")

#read in fecal response data
feces <- fread("Input/Results_feces.csv")

#read in temp data
temp <- fread("Input/temperatures_SW_2022.csv")



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

#create a date for each day of the feeding trials based on the "day" column
DT[Day == "D1", Date := Date_start + 1][Day == "D2", Date := Date_start + 2][Day == "D3", Date := Date_start + 3]

#paste enclosure and date together to create a 'sample id' that can be merged with lab results
DT[, Sample := paste0(Enclosure, "_", substring(Date, 3))] #remove the first two characters from year



# merge with daily dry matter and diet compositions --------------------------------

#remove excess columns in daily DM table
DM <- DM[, .(Sample, DM)]

#make just a diet DM table
dietDM <- diets[, .(mean(DM, na.rm = TRUE), mean(CP_diet/100, na.rm = TRUE), mean(NDF_diet/100, na.rm = TRUE), mean(ADF_diet/100, na.rm = TRUE), mean(ADL_diet/100, na.rm = TRUE), mean(C_diet/100, na.rm = TRUE)), Sample]
names(dietDM) <- c("Diet", "DM_diet", "CP_diet", "NDF_diet", "ADF_diet", "ADL_diet", "C_diet") #C isnt predicted it was measured after (not for paper)

#merge feeding data (wet weights) with daily DM data
DT <- merge(DT, DM, by = "Sample", all.x = TRUE)

#any lines with missing DM get the average DM
DT[is.na(DM), DM := avgSampleDM]

#merge diet DM into datasheet
DT <- merge(DT, dietDM, by = "Diet", all.x = TRUE)


# merge in spilled food data --------------------------------------------
#this is food that got knocked out of dishes and fell in with poop
#has a mass and a unique DM

#remove excess columns from remainder/spilled food table
spill <- spill[, .(Sample, Total_DM)]
setnames(spill, "Total_DM", "DM_spilled")

#merge in the mass of spilled food with full datasheet
DT <- merge(DT, spill, by = "Sample", all.x = TRUE)
DT[is.na(DM_spilled), DM_spilled := 0] #fill in cases where no food was dumped


# merge in fecal data ------------------------------------------------------

#calculate fecal outputs
feces[, DMF := Total_dried*DM] #total dry matter
feces[, DMF_NDF := DMF*(NDF_F/100)] #total NDF on DM basis (g)
feces[, DMF_ADF := DMF*(ADF_F/100)] #total ADF on DM basis (g)
feces[, DMF_ADL := DMF*(ADL_F/100)] #total ADF on DM basis (g)
feces[, DMF_CP := DMF*(CP_F/100)] #total CP on DM basis (g)
feces[, DMF_C := DMF*(C_F/100)] #total CP on DM basis (g)

#cut the fecal data down to just fecal output columns
fecaloutput <- feces[, .(Sample, DMF, DMF_NDF, DMF_ADF, DMF_ADL, DMF_CP, DMF_C)]

#merge intake rates and DMs with fecal output data
DT <- merge(DT, fecaloutput, by = "Sample", all.x = TRUE)


# Calculate intake measures  --------------------------------------

#calculate start and end food weights in terms of dry matter
DT[, DM_offer := OfferWet*DM_diet]
DT[, DM_end := (EndWet*DM) + DM_spilled] #end weight adds in the dry matter of spilled food

#calculate dry matter intake
DT[, DMI := DM_offer - DM_end]

#calculate dry matter intake of each currency
DT[, DMI_CP := DMI*CP_diet]
DT[, DMI_NDF := DMI*NDF_diet]
DT[, DMI_ADF := DMI*ADF_diet]
DT[, DMI_ADL := DMI*ADL_diet]
DT[, DMI_C := DMI*C_diet]

#convert weights from g to kg
DT[, Weight_start := Weight_start/1000]
DT[, Weight_end := Weight_end/1000]


# Calculate digestibility -----------------------------------------------

#four 
DT[, DP := (DMI_CP - DMF_CP)/DMI_CP]
DT[, DNDF := (DMI_NDF - DMF_NDF)/DMI_NDF]
DT[, DADF := (DMI_ADF - DMF_ADF)/DMI_ADF]
DT[, DADL := (DMI_ADL - DMF_ADL)/DMI_ADL]
DT[, DC := (DMI_C - DMF_C)/DMI_C]


# Merge in daily temperatures ---------------------------------------------

#create a daily start and end time for feeding trials
DT[, Time_start := "10:00:00"][, Time_end := "10:00:00"]

#create a datetime for feeding trial starts and ends
DT[, DayTime_start := as_datetime(paste0(Date-1, " ", Time_start))]
DT[, DayTime_end := as_datetime(paste0(Date, " ", Time_end))]

#in temp data, merge date and time into a datetime
temp[, DateTime := as_datetime(paste0(Date, " ", Time, " ", TimeStamp))]

#run the tempcalc function (in R folder) by day and ID
DT[, Temp := tempcalc(start = DayTime_start, end = DayTime_end), by = .(ID, Trial, Day)]



# create final, simplified datasheet --------------------------------------

#cut out a datasheet of just key feeding trial info and results
Dailyresults <- DT[, .(Diet, Sample, ID, Trial, Day, Date_start, Date_end, Date, #info
                   CP_diet, NDF_diet, ADF_diet, ADL_diet, C_diet, #diet compositions (%)
                   DMI, DMI_CP, DMI_NDF, DMI_NDF, DMI_ADF, DMI_ADL, DMI_C, # dry matter intakes (g)
                   Weight_start, Weight_end, #weight change
                   DMF, DMF_CP, DMF_NDF, DMF_ADF, DMF_ADL, DMF_C, #dry matter fecal outputs
                   DP, DNDF, DADF, DADL, DC, #digestibility (%)
                   Temp
                   )] 

#cut out three samples with weirdly negative NDF digestion
Dailyresults <- Dailyresults[!DNDF < -.10]


saveRDS(Dailyresults, "Output/data/dailyresultscleaned.rds")
