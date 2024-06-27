# script to make rails with energy and digestible parts included

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in diet compositions and daily results
diets <- fread("Output/data/dietcompositions.rds")
days <- readRDS("Output/data/dailyresultscleaned.rds")


# prep data ---------------------------------------------------------------

#calculate the mean digestible energy, protein digestibility, and NDF digestibility for each diet
dig <- days[, .(DE_diet = mean(DE), DP = mean(DP), DNDF = mean(DNDF)), Diet]

#merge these means in with diet compositions
diets <- merge(diets, dig, by = "Diet")

#calculate digestible protein in each diet
diets[, DP_diet := CP_diet*DP]

#calculate digestible NDF in each diet
diets[, DNDF_diet := NDF_diet*DNDF]



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


# save --------------------------------------------------------------------

fwrite(dietrails, "Output/data/dietdigestionrails.rds")
