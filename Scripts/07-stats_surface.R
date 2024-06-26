
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


#read in results
trials <- readRDS("Output/data/trialresultscleaned.rds")



# model weight change ~ crude NDF -------------------------------------

bodyCNDF <- gam(Weight_change ~ s(DMI_NDF_bw, DMI_CP_bw), data = trials)

#save summary of model
sumCNDF <- summary(bodyCNDF)
  
#make p-table, and indicate model, this is for intercept stuff
sumpCNDF <- as.data.table(round((sumCNDF$p.table), 4))
sumpCNDF[, Model := "Crude NDF"]
sumpCNDF[, Response := "Weight change"]
sumpCNDF[, `Dev. Explained` := round(sumCNDF$dev.expl, 2)]
  
#make s-table, and indicate model, this is for variable stuff
sumsCNDF <- as.data.table(round((sumCNDF$s.table), 4))
sumsCNDF[, Model := "Crude NDF"]
sumsCNDF[, Response := "Weight change"]



# model weight change ~ crude energy --------------------

bodyCE <- gam(Weight_change ~ s(DMI_energy_bw, DMI_CP_bw), data = trials)

#save summary of model
sumCE <- summary(bodyCE)

#make p-table, and indicate model, this is for intercept stuff
sumpCE <- as.data.table(round((sumCE$p.table), 4))
sumpCE[, Model := "Crude energy"]
sumpCE[, Response := "Weight change"]
sumpCE[, `Dev. Explained` := round(sumCE$dev.expl, 2)]

#make s-table, and indicate model, this is for variable stuff
sumsCE <- as.data.table(round((sumCE$s.table), 4))
sumsCE[, Model := "Crude energy"]
sumsCE[, Response := "Weight change"]



# model weight change ~ digestible NDF -----------------------------------

bodyDNDF <- gam(Weight_change ~ s(DNDFI, DPI), data = trials)

#save summary of model
sumDNDF <- summary(bodyDNDF)

#make p-table, and indicate model, this is for intercept stuff
sumpDNDF <- as.data.table(round((sumDNDF$p.table), 4))
sumpDNDF[, Model := "Digestible NDF"]
sumpDNDF[, Response := "Weight change"]
sumpDNDF[, `Dev. Explained` := round(sumDNDF$dev.expl, 2)]

#make s-table, and indicate model, this is for variable stuff
sumsDNDF <- as.data.table(round((sumDNDF$s.table), 4))
sumsDNDF[, Model := "Digestible NDF"]
sumsDNDF[, Response := "Weight change"]



# model weight change ~ digestible energy -----------------------------------

bodyDE <- gam(Weight_change ~ s(DEI, DPI), data = trials)

#save summary of model
sumDE <- summary(bodyDE)

#make p-table, and indicate model, this is for intercept stuff
sumpDE <- as.data.table(round((sumDE$p.table), 4))
sumpDE[, Model := "Digestible energy"]
sumpDE[, Response := "Weight change"]
sumpDE[, `Dev. Explained` := round(sumDE$dev.expl, 2)]

#make s-table, and indicate model, this is for variable stuff
sumsDE <- as.data.table(round((sumDE$s.table), 4))
sumsDE[, Model := "Digestible energy"]
sumsDE[, Response := "Weight change"]



# Merge output tables -----------------------------------------------------

#bind all p tables
allsump <- rbind(sumpCNDF, sumpCE, sumpDNDF, sumpDE)
names(allsump) <- c("Estimate/edf", "SE/Ref.df", "t/F", "p", "Model", "Response", "Dev. Explained")
allsump[, Parameter := "Intercept"]

#bind all s tables
allsums <- rbind(sumsCNDF, sumsCE, sumsDNDF, sumsDE)
names(allsums) <- c("Estimate/edf", "SE/Ref.df", "t/F", "p", "Model", "Response")
allsums[, Parameter := "s"]

#bind p and s tables together
summarytable <- rbind(allsump, allsums, fill = TRUE)
setorder(summarytable, Response, Model)
setcolorder(summarytable, c("Response", "Model", "Parameter", "Dev. Explained", "Estimate/edf", "SE/Ref.df", "t/F", "p"))

#round the table to 2 decimal places
summarytable <- summarytable %>% mutate_if(is.numeric, round, digits = 2)



# Save table --------------------------------------------------------------

write.csv(summarytable, "Output/stats/GAMoutputs.csv")

        