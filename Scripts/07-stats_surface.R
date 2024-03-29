
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


#read in results
trials <- readRDS("Output/data/trialresultscleaned.rds")
day <- readRDS("Output/data/dailyresultscleaned.rds")



# model for non-digestible intake on weight change -------------------------------------

bodyND <- gam(Weight_change ~ s(DMI_NDF_bw, DMI_CP_bw), data = trials)

#save summary of model
sum <- summary(bodyND)
  
#make p-table, and indicate model, this is for intercept stuff
sump <- as.data.table(round((sum$p.table), 4))
sump[, Model := "Crude macronutrient"]
sump[, Response := "Weight change"]
sump[, `Dev. Explained` := round(sum$dev.expl, 2)]
  
#make s-table, and indicate model, this is for variable stuff
sums <- as.data.table(round((sum$s.table), 4))
sums[, Model := "Crude macronutrient"]
sums[, Response := "Weight change"]
 

# model for digestible intake on weight change -----------------------------------

bodyD <- gam(Weight_change ~ s(DNDFI, DPI), data = trials)

#save summary of model
sum2 <- summary(bodyD)

#make p-table, and indicate model, this is for intercept stuff
sum2p <- as.data.table(round((sum2$p.table), 4))
sum2p[, Model := "Digestible macronutrient"]
sum2p[, Response := "Weight change"]
sum2p[, `Dev. Explained` := round(sum2$dev.expl, 2)]

#make s-table, and indicate model, this is for variable stuff
sum2s <- as.data.table(round((sum2$s.table), 4))
sum2s[, Model := "Digestible macronutrient"]
sum2s[, Response := "Weight change"]

#  model for intake on DMD -----------------------------------------------

DMD <- gam(DMD ~ s(DMI_NDF_bw, DMI_CP_bw), data = day)

#save summary of model
sum3 <- summary(DMD)

#make p-table, intercept stuff
sum3p <- as.data.table(round((sum3$p.table), 4))
sum3p[, Model := "Crude macronutrient"]
sum3p[, Response := "DMD"]
sum3p[, `Dev. Explained` := round(sum3$dev.expl, 2)]

#make s-table, intercept stuff
sum3s <- as.data.table(round((sum3$s.table), 4))
sum3s[, Model := "Crude macronutrient"]
sum3s[, Response := "DMD"]



# Merge output tables -----------------------------------------------------

#bind all p tables
allsump <- rbind(sump, sum2p, sum3p)
names(allsump) <- c("Estimate/edf", "SE/Ref.df", "t/F", "p", "Model", "Response", "Dev. Explained")
allsump[, Parameter := "Intercept"]

#bind all s tables
allsums <- rbind(sums, sum2s, sum3s)
names(allsums) <- c("Estimate/edf", "SE/Ref.df", "t/F", "p", "Model", "Response")
allsums[, Parameter := "s(DMI_NDF x DMI_protein)"]

#bind p and s tables together
summarytable <- rbind(allsump, allsums, fill = TRUE)
setorder(summarytable, Response, Model)
setcolorder(summarytable, c("Response", "Model", "Parameter", "Dev. Explained", "Estimate/edf", "SE/Ref.df", "t/F", "p"))

#round the table to 2 decimal places
summarytable <- summarytable %>% mutate_if(is.numeric, round, digits = 2)



# Save table --------------------------------------------------------------

write.csv(summarytable, "Output/stats/GAMoutputs.csv")

        