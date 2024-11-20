#script to prep data from naiive multi-choice trials


#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


# Read in data ------------------------------------------------------------


#read in habituation feeding trials results
MC <- fread("Input/Results_multichoice.csv")

#read in habituation dry matters
DM <- fread("Input/Habituation_DryMatter.csv")

#read in the nutritional compositions of each diet
dietcomp <- fread("Output/data/dietcompositions.rds")

diets <- fread("Input/Diet_compositions.csv")

diet_digest <- readRDS("Output/data/diet_digestibilities.rds")



# merge data together -----------------------------------------------------

#remove diet DM column from dietcomp. Will be calculated for each winter
dietcomp[, DM_diet := NULL]

#average diet DM by winter
dietDM <- diets[, .(DM_diet = mean(DM)), by = .(Winter, Sample)]
setnames(dietDM, "Sample", "Diet")

#subset DM data
DM <- DM[, .(Sample, DM)]

#split sample col into date and diet
DM[, Diet := tstrsplit(Sample, " ", keep = 3)]
DM[, Date := tstrsplit(Sample, " ", keep = 2)]
DM[, Date := ymd(Date)]
DM[, Enclosure := tstrsplit(Sample, " ", keep = 1)]

#set date to be end date, this will match the dates in the habituation dry matter sheet
MC[, Date := mdy(End_date)]

#create winter col
MC[, Winter := year(Date)]

#merge results with DM
DT <- merge(MC, DM, by = c("Enclosure", "Date", "Diet"), all.x = TRUE)

#merge results with diet compositions
DT <- merge(DT, dietcomp, by = "Diet", all.x = TRUE)

DT <- merge(DT, dietDM, by = c("Diet", "Winter"), all.x = TRUE)

DT <- merge(DT, diet_digest, by = "Diet", all.x = TRUE)

#missing DM gets mean DM
avgSampleDM <- mean(DT$DM, na.rm =TRUE)
DT[is.na(DM), DM := avgSampleDM]



# Intake rates in DM ------------------------------------------------------------

#convert start and end food weights in DM
DT[, OfferDM := Offer_wet*DM_diet]
DT[, EndDM := End_wet*(DM/100)]

#calculate daily intake rate in DM
DT[, DMI := OfferDM - EndDM]
#if intake is neg make zero 
DT[DMI < 0, DMI := 0]

#calculate intake rates of each nutrient
DT[, DMI_CP := DMI*CP_diet]
DT[, DMI_NDF := DMI*NDF_diet]
DT[, DMI_GE := DMI*Energy_diet]



# DM intakes by body weight ----------------------------------------------------

#calculate intake rates by weight
DT[, Weight_start := Weight_start/1000]

#calculate dry matter intakes by kg^.75
DT[, DMI_bw := DMI/(Weight_start^.75)]
DT[, DMI_CP_bw := DMI_CP/(Weight_start^.75)]
DT[, DMI_NDF_bw := DMI_NDF/(Weight_start^.75)]
DT[, DMI_GE_bw := DMI_GE/(Weight_start^.75)]



# Digestible intake rates by weight ---------------------------------------

DT[, DMDI := DMD_diet*DMI_bw]        #digestible dry matter intake (g/kg.75)
DT[, DPI := CPD_diet*DMI_CP_bw]       #digestible protein intake (g/kg.75)
DT[, DEI := GED_diet*DMI_bw]              #digestible energy intake (kj/kg.75)


# remove food strikers ----------------------------------------------------

totals <- DT[, .(DMI_bw = sum(DMI_bw)), by = ID] 
strikers <- totals[DMI_bw < 20, ID]
DT <- DT[!ID %in% strikers]



saveRDS(DT, "Output/data/multichoiceresults.rds")

