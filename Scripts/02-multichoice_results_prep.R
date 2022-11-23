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
dietDM <- diets[, .(mean(DM, na.rm = TRUE), mean(CP_diet/100, na.rm = TRUE), mean(NDF_diet/100, na.rm = TRUE), mean(ADF_diet/100, na.rm = TRUE), mean(ADL_diet/100, na.rm = TRUE), mean(C_diet/100, na.rm = TRUE)), Sample]
names(dietDM) <- c("Diet", "DM_diet", "CP_diet", "NDF_diet", "ADF_diet", "ADL_diet", "C_diet" )

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
DT[, DMI := OfferDM - EndDM]
#if intake is neg make zero 
DT[DMI < 0, DMI := 0]

#calculate intake rates of each nutrient
DT[, DMI_CP := DMI*CP_diet]
DT[, DMI_NDF := DMI*NDF_diet]
DT[, DMI_ADF := DMI*ADF_diet]
DT[, DMI_ADL := DMI*ADL_diet]
DT[, DMI_C := DMI*C_diet]


# DM intakes by body weight ----------------------------------------------------


#calculate intake rates by weight
DT[, Weight_start := Weight_start/1000]

#calculate dry matter intakes by kg^.75
DT[, DMI_bw := DMI/(Weight_start^.75)]
DT[, DMI_CP_bw := DMI_CP/(Weight_start^.75)]
DT[, DMI_NDF_bw := DMI_NDF/(Weight_start^.75)]
DT[, DMI_ADF_bw := DMI_ADF/(Weight_start^.75)]
DT[, DMI_ADF_bw := DMI_ADL/(Weight_start^.75)]
DT[, DMI_C_bw := DMI_C/(Weight_start^.75)]




# Sum nutrient intakes by individual------------------------------------------------------------

#calculate total protein and fibre consumed from all diets in one day
totals <- DT[, .(sum(DMI_bw), sum(DMI_CP_bw), sum(DMI_NDF_bw)), by = ID]
names(totals) <- c("ID","DMI_bw", "DMI_CP_bw", "DMI_NDF_bw")



saveRDS(totals, "Output/data/multichoicesums.rds")
saveRDS(DT, "Output/data/multichoiceresults.rds")

