#script to prep data from naiive multi-choice trials


#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


# Read in data ------------------------------------------------------------


#read in habituation feeding trials results
MC <- fread("Input/Results_multichoice.csv")

#read in habituation dry matters
DM <- fread("Input/Habituation_DryMatter.csv")

#read in the nutritional compositions of each diet
diets <- fread("Input/Diet_compositions.csv")


# merge data together -----------------------------------------------------

#cut diet compositions to just be DM
dietDM <- diets[, .(mean(DM, na.rm = TRUE), mean(CP_DM/100, na.rm = TRUE), mean(NDF_DM/100, na.rm = TRUE), mean(ADF_DM/100, na.rm = TRUE), mean(ADL_DM/100, na.rm = TRUE), mean(C_DM/100, na.rm = TRUE)), Sample]
names(dietDM) <- c("Diet", "DM_diet", "CP_diet", "NDF_diet", "ADF_diet", "ADL_diet", "C_diet") #C isnt predicted it was measured after (not for paper)

#subset DM data
DM <- DM[, .(Sample, DM)]

#split sample col into date and diet
DM[, Diet := tstrsplit(Sample, " ", keep = 3)]
DM[, Date := tstrsplit(Sample, " ", keep = 2)]
DM[, Date := ymd(Date)]
DM[, Enclosure := tstrsplit(Sample, " ", keep = 1)]

#set date in results
MC[, Date := ymd(End_date)]

#merge results with DM
DT <- merge(MC, DM, by = c("Enclosure", "Date", "Diet"), all.x = TRUE)

#merge results with diet compositions
DT <- merge(DT, dietDM, by = "Diet", all.x = TRUE)

#missing DM gets mean DM
avgSampleDM <- mean(DT$DM, na.rm =TRUE)
DT[is.na(DM), DM := avgSampleDM]


# Intake rates in DM ------------------------------------------------------------

DT[, OfferDM := Offer_wet*DM_diet]
DT[, EndDM := End_wet*(DM/100)]

#calculate daily intake rate in DM
DT[, Intake := OfferDM - EndDM]
#if intake is neg make zero 
DT[Intake < 0, Intake := 0]

#calculate intake rates of each nutrient
DT[, CP_in := Intake*CP_diet]
DT[, NDF_in := Intake*NDF_diet]
DT[, ADF_in := Intake*ADF_diet]
DT[, ADL_in := Intake*ADL_diet]
DT[, C_in := Intake*C_diet]



# DM intakes by body weight ----------------------------------------------------


#calculate intake rates by weight
DT[, Weight_start := Weight_start/1000]

DT[, Intake_bw := Intake/Weight_start]
DT[, CP_in_bw := CP_in/Weight_start]
DT[, NDF_in_bw := NDF_in/Weight_start]
DT[, ADF_in_bw := ADF_in/Weight_start]
DT[, ADL_in_bw := ADF_in/Weight_start]
DT[, C_in_bw:= C_in/Weight_start]




# Sum nutrient intakes by individual------------------------------------------------------------

#calculate total protein and fibre consumed from all diets in one day
totals <- DT[, .(sum(Intake_bw), sum(CP_in_bw), sum(NDF_in_bw)), by = ID]

names(totals) <- c("ID","Intake", "CP", "NDF")



saveRDS(totals, "Output/data/multichoicesums.rds")
saveRDS(DT, "Output/data/multichoiceresults.rds")

