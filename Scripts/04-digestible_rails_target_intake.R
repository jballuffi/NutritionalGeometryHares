# script to make rails with energy and digestible parts included

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in diet compositions and daily results
diet <- fread("Output/data/dietcompositions.rds")
dietdigest <- readRDS("Output/data/diet_digestibilities.rds")


# prep data ---------------------------------------------------------------

#merge
diets <- merge(diet, dietdigest, by = "Diet")


# create short rails ------------------------------------------------------------

#these rails only go up to 120 g of intake. To be used for multichoice figures

#create list of intake rates and nutritional values for each diet
dietintakesshort <- list(
  
  #diet A
  data.table(IR = seq(1, 120, by = 1), 
             CP = diets[Diet == "A", return(CP_diet)], #crude protein
             NDF = diets[Diet == "A", return(NDF_diet)], #NDF
             GE = diets[Diet == "A", return(GE_diet)], #crude energy
             DP = diets[Diet == "A", return(DP_diet)], #digestible protein
             DE = diets[Diet == "A", return(GED_diet)], #digestible energy
             Diet = "A"),
  #diet B
  data.table(IR = seq(1, 120, by = 1), 
             CP = diets[Diet == "B", return(CP_diet)], #crude protein
             NDF = diets[Diet == "B", return(NDF_diet)], #NDF
             GE = diets[Diet == "B", return(GE_diet)], #crude energy
             DP = diets[Diet == "B", return(DP_diet)], #digestible protein
             DE = diets[Diet == "B", return(GED_diet)], #digestible energy
             Diet = "B"),
  #diet c
  data.table(IR = seq(1, 120, by = 1), 
             CP = diets[Diet == "C", return(CP_diet)], #crude protein
             NDF = diets[Diet == "C", return(NDF_diet)], #NDF
             GE = diets[Diet == "C", return(GE_diet)], #crude energy
             DP = diets[Diet == "C", return(DP_diet)], #digestible protein
             DE = diets[Diet == "C", return(GED_diet)], #digestible energy
             Diet = "C"),
  #diet D
  data.table(IR = seq(1, 120, by = 1), 
             CP = diets[Diet == "D", return(CP_diet)], #crude protein
             NDF = diets[Diet == "D", return(NDF_diet)], #NDF
             GE = diets[Diet == "D", return(GE_diet)], #crude energy
             DP = diets[Diet == "D", return(DP_diet)], #digestible protein
             DE = diets[Diet == "D", return(GED_diet)], #digestible energy
             Diet = "D")
)

#combine all diet intake rates into one table
railsshort <- rbindlist(dietintakesshort)

#calculate the intake rates of CP and NDF
railsshort[, c("CP_IR", "NDF_IR", "GE_IR", "DP_IR", "DE_IR") := .(IR*CP, IR*NDF, IR*GE, IR*DP, IR*DE)]

#create columnn indicating that this is a diet
railsshort[, Type := "Diet"]



# create short rails ------------------------------------------------------------

#these rails go up to 300 g intake. For single choice heat maps

#create list of intake rates and nutritional values for each diet
dietintakeslong <- list(
  
  #diet A
  data.table(IR = seq(1, 300, by = 1), 
             CP = diets[Diet == "A", return(CP_diet)], #crude protein
             NDF = diets[Diet == "A", return(NDF_diet)], #NDF
             GE = diets[Diet == "A", return(GE_diet)], #crude energy
             DP = diets[Diet == "A", return(DP_diet)], #digestible protein
             DE = diets[Diet == "A", return(GED_diet)], #digestible energy
             Diet = "A"),
  #diet B
  data.table(IR = seq(1, 300, by = 1), 
             CP = diets[Diet == "B", return(CP_diet)], #crude protein
             NDF = diets[Diet == "B", return(NDF_diet)], #NDF
             GE = diets[Diet == "B", return(GE_diet)], #crude energy
             DP = diets[Diet == "B", return(DP_diet)], #digestible protein
             DE = diets[Diet == "B", return(GED_diet)], #digestible energy
             Diet = "B"),
  #diet c
  data.table(IR = seq(1, 300, by = 1), 
             CP = diets[Diet == "C", return(CP_diet)], #crude protein
             NDF = diets[Diet == "C", return(NDF_diet)], #NDF
             GE = diets[Diet == "C", return(GE_diet)], #crude energy
             DP = diets[Diet == "C", return(DP_diet)], #digestible protein
             DE = diets[Diet == "C", return(GED_diet)], #digestible energy
             Diet = "C"),
  #diet D
  data.table(IR = seq(1, 300, by = 1), 
             CP = diets[Diet == "D", return(CP_diet)], #crude protein
             NDF = diets[Diet == "D", return(NDF_diet)], #NDF
             GE = diets[Diet == "D", return(GE_diet)], #crude energy
             DP = diets[Diet == "D", return(DP_diet)], #digestible protein
             DE = diets[Diet == "D", return(GED_diet)], #digestible energy
             Diet = "D")
)

#combine all diet intake rates into one table
railslong <- rbindlist(dietintakeslong)

#calculate the intake rates of CP and NDF
railslong[, c("CP_IR", "NDF_IR", "GE_IR", "DP_IR", "DE_IR") := .(IR*CP, IR*NDF, IR*GE, IR*DP, IR*DE)]

#create columnn indicating that this is a diet
railslong[, Type := "Diet"]



# save --------------------------------------------------------------------

fwrite(railsshort, "Output/data/digestibilerails_short.rds")
fwrite(railslong, "Output/data/digestibilerails_long.rds")

