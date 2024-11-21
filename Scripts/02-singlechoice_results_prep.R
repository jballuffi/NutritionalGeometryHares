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
                 mCP = mean(CP_F, na.rm = TRUE)), Diet]

#merge averages with full feces data
feces <- merge(feces, avg, by = "Diet", all.x = TRUE)

#replace NA's with the averages
feces[is.na(NDF_F), NDF_F := mNDF]
feces[is.na(CP_F), CP_F := mCP]
mDM <- feces[, mean(DM, na.rm = TRUE)]
feces[is.na(DM), DM := mDM]

#calculate output rates for each nutrient (g/day)
feces[, DMO := Total_dried*DM] #total dry matter output
feces[, NDFO := DMO*(NDF_F/100)] #total NDF output on DM basis (g)
feces[, CPO := DMO*(CP_F/100)] #total CP output on DM basis (g)

#cut the fecal data down to just fecal output columns
fecaloutput <- feces[, .(Sample, 
                         CP_F, NDF_F,#fecal compositions
                         DMO, NDFO, CPO)]  #fecal outputs

#merge intake rates and DMs with fecal output data
DT <- merge(DT, fecaloutput, by = "Sample", all.x = TRUE)



# Calculate dry matter intake with and without weight --------------------------------------

#calculate start and end food weights in terms of dry matter
DT[, DM_offer := OfferWet*DM_diet]
DT[, DM_end := (EndWet*DM/100) + DM_spilled] #end weight adds in the dry matter of spilled food

#calculate dry matter intake
DT[, DMI := DM_offer - DM_end]

#calculate dry matter intake of each currency
DT[, CPI := DMI*CP_diet]   #crude protein intake (g DM)
DT[, NDFI := DMI*NDF_diet] #neutral detergent fibre intake (g DM)
DT[, GEI := DMI*GE_diet] #gross energy intake (kj)

#convert weights from g to kg
DT[, Weight_start := Weight_start/1000]
DT[, Weight_end := Weight_end/1000]

#calculate dry matter intakes by kg^.75 (bw  stands for bw)
DT[, DMI_bw := DMI/(Weight_start^.75)]
DT[, CPI_bw := CPI/(Weight_start^.75)]
DT[, NDFI_bw := NDFI/(Weight_start^.75)]
DT[, GEI_bw := GEI/(Weight_start^.75)]



# Calculate digestibility -----------------------------------------------

#this DOES NOT use by weight intake rates, just the g/day

DT[, DMD := (DMI-DMO)/DMI] #dry matter digestibility (%)
DT[, CPD := (CPI - CPO)/CPI] #crude protein digestibility (%)
DT[, NDFD := (NDFI - NDFO)/NDFI] #NDF digestibility (%)
DT[, GED := DMD*GE_diet] #gross energy digestibility (kj/g)

#calculate total digestible protein in diets
DT[, DP_diet := CP_diet*CPD] ###WHAT IS THIS CALCULATION?

#caclulate digestibility by diet (use in table 1)
#dry matter digestibility, gross energy digestibility, crude protein digestibility
diet_digest <- DT[, .(DMD_diet = mean(DMD), GED_diet = mean(GED), CPD_diet = mean(DP_diet)), Diet]

###ISSUE WITH USING THE ABOVE TO MAKE DIGESTIBLE LINES> I THNK THE PROBLEM IS DP_diet

# Calculate digestibility intake ------------------------------------------

#these intake rates are on a kg^.75 basis
DT[, DMDI := DMD*DMI_bw]        #digestible dry matter intake (g/kg.75)
DT[, DPI := CPD*CPI_bw]       #digestible protein intake (g/kg.75)
DT[, DNDFI := NDFD*NDFI_bw]  #digestible NDF intake (g/kg.75)
DT[, DEI := GED*DMI_bw] #digestible energy intake (kj/kg.75)




# create simplified datasheet for daily results --------------------------------------

#cut out a datasheet of just key feeding trial info and results
Dailyresults <- DT[, c("Diet", "Sample", "ID", "Trial", "Day", "Date_start", "Date_end", "Date", #info
                       "CP_diet", "NDF_diet", "ADF_diet", "ADL_diet", "GE_diet", #diet compositions
                       "DMI", "CPI", "NDFI", "GEI", #dry matter intakes (g/day)
                       "Weight_start", "Weight_end", #weights (g)
                       "DMI_bw", "CPI_bw", "NDFI_bw", "GEI_bw", #dry matter intake by weight (g/kg^.75/day)
                       "DMO", "CPO", "NDFO", #fecal outputs (g/day) 
                       "CP_F", "NDF_F", #fecal compositions (%)
                       "DMD", "CPD", "NDFD", "GED",  #digestibility (%)
                       "DMDI", "DPI", "DNDFI", "DEI" #digestible intake rate (g/kg^.75/day)
                       
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
#save average digestibilities
saveRDS(diet_digest, "Output/data/diet_digestibilities.rds")

