# script to make rails with energy and digestible parts included

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in diet compositions and daily results
diets <- fread("Output/data/dietcompositions.rds")
days <- readRDS("Output/data/dailyresultscleaned.rds")
MC <- readRDS("Output/data/multichoiceresults.rds")



# prep data ---------------------------------------------------------------

#calculate the mean digestible energy, protein digestibility, and NDF digestibility for each diet
dig <- days[, .(DE_diet = mean(DE), DP = mean(DP), DNDF = mean(DNDF)), Diet]

#merge these means in with diet compositions
diets <- merge(diets, dig, by = "Diet")

#calculate digestible protein in each diet
diets[, DP_diet := CP_diet*DP]

#calculate digestible NDF in each diet
diets[, DNDF_diet := NDF_diet*DNDF]

#convert from j to kj
diets[, Energy_diet := Energy_diet/1000]



# create rails ------------------------------------------------------------

#create list of intake rates and nutritional values for each diet
dietintakes<- list(
  
  #diet A
  data.table(IR = seq(1, 300, by = 1), 
             CP = diets[Diet == "A", return(CP_diet)], #crude protein
             NDF = diets[Diet == "A", return(NDF_diet)], #NDF
             CE = diets[Diet == "A", return(Energy_diet)], #crude energy
             DP = diets[Diet == "A", return(DP_diet)], #digestible protein
             DNDF = diets[Diet == "A", return(DNDF_diet)], #digestible NDF
             DE = diets[Diet == "A", return(DE_diet)], #digestible energy
             Diet = "A"),
  #diet B
  data.table(IR = seq(1, 300, by = 1), 
             CP = diets[Diet == "B", return(CP_diet)], #crude protein
             NDF = diets[Diet == "B", return(NDF_diet)], #NDF
             CE = diets[Diet == "B", return(Energy_diet)], #crude energy
             DP = diets[Diet == "B", return(DP_diet)], #digestible protein
             DNDF = diets[Diet == "B", return(DNDF_diet)], #digestible NDF
             DE = diets[Diet == "B", return(DE_diet)], #digestible energy
             Diet = "B"),
  #diet c
  data.table(IR = seq(1, 300, by = 1), 
             CP = diets[Diet == "C", return(CP_diet)], #crude protein
             NDF = diets[Diet == "C", return(NDF_diet)], #NDF
             CE = diets[Diet == "C", return(Energy_diet)], #crude energy
             DP = diets[Diet == "C", return(DP_diet)], #digestible protein
             DNDF = diets[Diet == "C", return(DNDF_diet)], #digestible NDF
             DE = diets[Diet == "C", return(DE_diet)], #digestible energy
             Diet = "C"),
  #diet D
  data.table(IR = seq(1, 300, by = 1), 
             CP = diets[Diet == "D", return(CP_diet)], #crude protein
             NDF = diets[Diet == "D", return(NDF_diet)], #NDF
             CE = diets[Diet == "D", return(Energy_diet)], #crude energy
             DP = diets[Diet == "D", return(DP_diet)], #digestible protein
             DNDF = diets[Diet == "D", return(DNDF_diet)], #digestible NDF
             DE = diets[Diet == "D", return(DE_diet)], #digestible energy
             Diet = "D")
)

#combine all diet intake rates into one table
dietrails <- rbindlist(dietintakes)

#calculate the intake rates of CP and NDF
dietrails[, c("CP_IR", "NDF_IR", "CE_IR", "DP_IR", "DNDF_IR", "DE_IR") := .(IR*CP, IR*NDF, IR*CE, IR*DP, IR*DNDF, IR*DE)]

#create columnn indicating that this is a diet
dietrails[, Type := "Diet"]



# Add digestibility to multi-choice ----------------------------------------

MC <- merge(dig, MC, by = "Diet", all.y = TRUE)

#these intake rates are on a kg^.75 basis
MC[, DPI := DP*DMI_CP_bw]       #digestible protein intake (g/kg.75)
MC[, DNDFI := DNDF*DMI_NDF_bw]  #digestible NDF intake (g/kg.75)
MC[, DEI := DE_diet*DMI_bw] #digestible energy intake (kj/kg.75)



# Sum nutrient intakes in multi-choice trials by individual------------------------------------------------------------

#calculate total protein and fibre consumed from all diets in one day
totals <- MC[, .(DMI_bw = sum(DMI_bw), 
                 DMI_CP_bw = sum(DMI_CP_bw), 
                 DMI_NDF_bw = sum(DMI_NDF_bw),
                 DMI_energy_bw = sum(DMI_energy_bw),
                 DMI_DP_bw = sum(DPI),
                 DMI_DNDF_bw = sum(DNDFI),
                 DMI_DEI_bw = sum(DEI)), by = ID]



# save --------------------------------------------------------------------

fwrite(dietrails, "Output/data/dietdigestionrails.rds")
saveRDS(totals, "Output/data/multichoicesums.rds")
