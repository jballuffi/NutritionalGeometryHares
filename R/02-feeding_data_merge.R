#script that merges and cleans all feeding trial results

library(data.table)
library(lubridate)
library(ggplot2)



# read in data ------------------------------------------------------------

#read in feeding trial data
trials <- fread("Input/Results_singlechoice.csv")

#read in the nutritional compositions of each diet
diets <- fread("Input/Diet_compositions.csv")
diets[is.na(DM), DM := mean(diets$DM, na.rm =TRUE)] #one diet is missing DM, replace with avg for now

#read in daily dry matter measures
DM <- fread("Input/Daily_DryMatter.csv")
DM[, DM := DM/100]
avgSampleDM <- mean(DM$DM, na.rm =TRUE) #calculate avg dry matter for all samples

#read in amounts and DMs of spilled food (leftover food that fell and mixed in with feces)
spill <- fread("Input/Daily_food_remainders.csv")

#read in fecal response data
feces <- fread("Input/Results_feces.csv")



# Melt feeding trial data into individual days-------------------------------------------------

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
DT[, Sample := paste0(Enclosure, "_", Date)]
DT[, Sample := gsub("2022", "22", Sample)]



# merge with daily dry matter --------------------------------

#make just a DM table
DM <- DM[, .(Sample, DM)]

#make just a diet DM table
dietDM <- diets[, .(Diet, DM)]
setnames(dietDM, "DM", "DietDM") 

#merge feeding data (wet weights) with DM data
DT <- merge(DT, DM, by = "Sample", all.x = TRUE)

#any lines with missing DM get the average DM
DT[is.na(DM), DM := avgSampleDM]



# merge in spilled food data --------------------------------------------

#food that got knocked out of dishes and fell in with poop

#make just a total remainder/spilled table
spill <- spill[, .(Sample, Total_DM)]

#merge in the mass of spilled food
DT <- merge(DT, spill, by = "Sample", all.x = TRUE)
setnames(DT, "Total_DM", "Spilled_DM") #rename to be specific to spilled food
DT[is.na(Spilled_DM), Spilled_DM := 0] #fill in cases where no food was dumped

#calculate start DM based on diet DM
DT <- merge(DT, dietDM, by = "Diet", all.x = TRUE)



# merge in fecal data ------------------------------------------------------

#calculate fecal outputs
feces[, Total_out := Total_dried*DM] #total dry matter
feces[, NDF_out := Total_out*NDF_DM/100] #total NDF on DM basis
feces[, ADF_out := Total_out*ADF_DM/100] #total ADF on DM basis
feces[, CP_out := Total_out*CP_DM/100] #total CP on DM basis

#cut the fecal data down to just fecal output columns
fecaloutput <- feces[, .(Sample, Total_out, NDF_out, ADF_out, CP_out)]

#merge intake rates and DMs with fecal output data
DT <- merge(DT, fecaloutput, by = "Sample", all.x = TRUE)


# Calculate intake measures  --------------------------------------

#cut diet compositions to just be DM
dietDM <- diets[, .(Diet, CP_DM_pred/100, NDF_DM_pred/100, ADF_DM_pred/100, ADL_DM_pred/100)]
names(dietDM) <- c("Diet", "CP_diet", "NDF_diet", "ADF_diet", "ADL_diet")

#merge DT with diet compositions in terms of DM 
DT <- merge(DT, dietDM, by = "Diet", all.x = TRUE)

#calculate start and end food weights in terms of dry matter
DT[, OfferDM := OfferWet*DietDM]
DT[, EndDM := (EndWet*DM) + Spilled_DM] #end weight adds in the dry matter of spilled food

#calculate daily itake rate in DM
DT[, Intake := OfferDM - EndDM]

#calculate intake rates of each nutrient
DT[, CP_in := Intake*CP_diet]
DT[, NDF_in := Intake*NDF_diet]
DT[, ADF_in := Intake*ADF_diet]
DT[, ADL_in := Intake*ADL_diet]



# create final, simplified datasheet --------------------------------------

#cut out a datasheet of just key feeding trial info and results
Dailyresults <- DT[, .(Diet, Sample, ID, Trial, Date_start, Date_end, Day, #info
                   Intake, CP_in, NDF_in, ADF_in, ADL_in, #intakes
                   Weight_start, Weight_end, #weight change
                   Total_out, CP_out, NDF_out, ADF_out)] #fecal outputs


saveRDS(Dailyresults, "Output/dailyresultscleaned.rds")


# #calculate the intake of protein (g/kg body mass/3 days) by diet 
# SCdiets[, Consumed_CP := Consumed*(Protein/100)]
# #calculate the intake of fibre (g/kg body mass/3 days) by diet
# SCdiets[, Consumed_NDF := Consumed*(NDF/100)]
# 
# SCmeans <- SCdiets[, .(mean(Consumed), sd(Consumed), mean(Weight_change), sd(Weight_change)), by = Diet]
# names(SCmeans) <- c("Diet", "Consumed_mean", "Consumed_SD", "Weight_mean", "Weight_SD")
# 
# Macromeans <- SCdiets[, .(mean(Consumed_CP), sd(Consumed_CP), mean(Consumed_NDF), sd(Consumed_NDF)), by = Diet]
# names(Macromeans) <- c("Diet", "CP_mean", "CP_SD", "NDF_mean", "NDF_SD")
