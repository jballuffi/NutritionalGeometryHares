
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


#read in results
trials <- readRDS("Output/data/trialresultscleaned.rds")
day<- readRDS("Output/data/dailyresultscleaned.rds")



# model for non-digestible intake on weight change -------------------------------------

bodyND <- gam(Weight_change ~ s(DMI_NDF_bw, DMI_CP_bw), data = trials)

#save summary of model
sum <- summary(bodyND)
  
#make p-table, and indicate model, this is for intercept stuff
sump <- as.data.table(round((sum$p.table), 4))
sump[, Model := "Crude macronutrient intake"]
sump[, `Dev. Explained` := round(sum$dev.expl, 2)]
  
#make s-table, and indicate model, this is for variable stuff
sums <- as.data.table(round((sum$s.table), 4))
sums[, Model := "Crude macronutrient intake"]

 

# model for digestible intake on weight change -----------------------------------

bodyD <- gam(Weight_change ~ s(DNDFI, DPI), data = trials)

#save summary of model
sum2 <- summary(bodyD)

#make p-table, and indicate model, this is for intercept stuff
sum2p <- as.data.table(round((sum2$p.table), 4))
sum2p[, Model := "Digestible macro-nutrient intake"]
sum2p[, `Dev. Explained` := round(sum2$dev.expl, 2)]

#make s-table, and indicate model, this is for variable stuff
sum2s <- as.data.table(round((sum2$s.table), 4))
sum2s[, Model := "Digestible macro-nutrient intake"]


#  model for intake on DMD -----------------------------------------------

DMD <- gam(DMD ~ s(DMI_NDF_bw, DMI_CP_bw), data = day)

#save summary of model
sum3 <- summary(DMD)

#make p-table, intercept stuff
sum3p <- as.data.table(round((sum3$p.table), 4))
sum3p[, Model := "DMD"]
sum3p[, `Dev. Explained` := round(sum3$dev.expl, 2)]

#make s-table, intercept stuff
sum3s <- as.data.table(round((sum3$s.table), 4))
sum3s[, Model := "DMD"]



# Merge output tables -----------------------------------------------------

#bind all p tables
allsump <- rbind(sump, sum2p, sum3p)
names(allsump) <- c("Estimate/edf", "SE/Ref.df", "t/F", "p", "Model", "Dev. Explained")
allsump[, Parameter := "Intercept"]

#bind all s tables
allsums <- rbind(sums, sum2s, sum3s)
names(allsums) <- c("Estimate/edf", "SE/Ref.df", "t/F", "p", "Model")
allsums[, Parameter := "s(DMI_NDF, DMI_protein)"]

#bind p and s tables together
summarytable <- rbind(allsump, allsums, fill = TRUE)
setorder(summarytable, Model)
setcolorder(summarytable, c("Model", "Parameter", "Dev. Explained", "Estimate/edf", "SE/Ref.df", "t/F", "p"))



# Save table --------------------------------------------------------------

write.csv(summarytable, "Output/GAMoutputs.csv")




# Figures -----------------------------------------------------------

#regular NDF intake
ggplot(trials)+
  geom_point(aes(x = DMI_NDF_bw, y = Weight_change))+
  theme_minimal()

#digestible NDF intake
ggplot(trials)+
  geom_point(aes(x = DNDFI, y = Weight_change))+
  theme_minimal()

#regular protein intake
ggplot(trials)+
  geom_point(aes(x = DMI_CP_bw, y = Weight_change))+
  theme_minimal()

#digestible protein intake
ggplot(trials)+
  geom_point(aes(x = DPI, y = Weight_change))+
  theme_minimal()

#protein intake to DMD
ggplot(day)+
  geom_point(aes(x = DMI_CP_bw, y = DMD))+
  theme_minimal()

#NDF intake to DMD
ggplot(day)+
  geom_point(aes(x = DMI_NDF_bw, y = DMD))+
  theme_minimal()


        