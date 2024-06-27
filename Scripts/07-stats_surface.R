
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)
library(tidymv)


#read in results
trials <- readRDS("Output/data/trialresultscleaned.rds")

#nutritional rails
rails <- fread("Output/data/dietdigestionrails.rds")


# model weight change ~ crude NDF -------------------------------------

#bodyCNDF <- gam(Weight_change ~ s(DMI_NDF_bw, DMI_CP_bw), data = trials)
bodyCNDF <- gam(Weight_change ~ s(DMI_NDF_bw) + s(DMI_CP_bw) + s(DMI_NDF_bw, DMI_CP_bw), data = trials)

#save summary of model
sumCNDF <- summary(bodyCNDF)
  
#make p-table, and indicate model, this is for intercept stuff
sumpCNDF <- as.data.table(round((sumCNDF$p.table), 4))
sumpCNDF[, Model := "Crude NDF"]
sumpCNDF[, `Dev. Explained` := round(sumCNDF$dev.expl, 2)]
  
#make s-table, and indicate model, this is for variable stuff
sumsCNDF <- as.data.table(round((sumCNDF$s.table), 4))
sumsCNDF[, Model := "Crude NDF"]



# model weight change ~ crude energy --------------------

#bodyCE <- gam(Weight_change ~ s(DMI_energy_bw, DMI_CP_bw), data = trials)
bodyCE <- gam(Weight_change ~ s(DMI_energy_bw) + s(DMI_CP_bw) + s(DMI_energy_bw, DMI_CP_bw), data = trials)

#save summary of model
sumCE <- summary(bodyCE)

#make p-table, and indicate model, this is for intercept stuff
sumpCE <- as.data.table(round((sumCE$p.table), 4))
sumpCE[, Model := "Crude energy"]
sumpCE[, `Dev. Explained` := round(sumCE$dev.expl, 2)]

#make s-table, and indicate model, this is for variable stuff
sumsCE <- as.data.table(round((sumCE$s.table), 4))
sumsCE[, Model := "Crude energy"]



# model weight change ~ digestible NDF -----------------------------------

#bodyDNDF <- gam(Weight_change ~ s(DNDFI, DPI), data = trials)
bodyDNDF <- gam(Weight_change ~ s(DNDFI) + s(DPI) + s(DNDFI, DPI), data = trials)

#save summary of model
sumDNDF <- summary(bodyDNDF)

#make p-table, and indicate model, this is for intercept stuff
sumpDNDF <- as.data.table(round((sumDNDF$p.table), 4))
sumpDNDF[, Model := "Digestible NDF"]
sumpDNDF[, `Dev. Explained` := round(sumDNDF$dev.expl, 2)]

#make s-table, and indicate model, this is for variable stuff
sumsDNDF <- as.data.table(round((sumDNDF$s.table), 4))
sumsDNDF[, Model := "Digestible NDF"]



# model weight change ~ digestible energy -----------------------------------

#bodyDE <- gam(Weight_change ~ s(DEI, DPI), data = trials)
bodyDE <- gam(Weight_change ~ s(DEI) + s(DPI) + s(DEI, DPI), data = trials)

#save summary of model
sumDE <- summary(bodyDE)

#make p-table, and indicate model, this is for intercept stuff
sumpDE <- as.data.table(round((sumDE$p.table), 4))
sumpDE[, Model := "Digestible energy"]
sumpDE[, `Dev. Explained` := round(sumDE$dev.expl, 2)]

#make s-table, and indicate model, this is for variable stuff
sumsDE <- as.data.table(round((sumDE$s.table), 4))
sumsDE[, Model := "Digestible energy"]



# Merge output tables -----------------------------------------------------

# #bind all p tables
# allsump <- rbind(sumpCNDF, sumpCE, sumpDNDF, sumpDE)
# names(allsump) <- c("Estimate/edf", "SE/Ref.df", "t/F", "p", "Model", "Response", "Dev. Explained")
# allsump[, Parameter := "Intercept"]

#bind all s tables
allsums <- rbind(sumsCNDF, sumsCE, sumsDNDF, sumsDE)

# #bind p and s tables together
# summarytable <- rbind(allsump, allsums, fill = TRUE)
# setorder(summarytable, Response, Model)

setcolorder(allsums, c("Model", "edf", "Ref.df", "F", "p-value"))

#round the table to 2 decimal places
summarytable <- allsums %>% mutate_if(is.numeric, round, digits = 3)



# make predictive datasheets of each model --------------------------------

CNDF <- predict_gam(bodyCNDF)
CE <- predict_gam(bodyCE)
DNDF <- predict_gam(bodyDNDF)
DE <- predict_gam(bodyDE)



# figures of curved lines -------------------------------------------------

ggplot(CNDF)+
  geom_smooth(aes(x = DMI_CP_bw, y = fit))

ggplot(CNDF)+
  geom_smooth(aes(x = DMI_NDF_bw, y = fit))

ggplot(CE)+
  geom_smooth(aes(x = DMI_CP_bw, y = fit))

ggplot(DE)+
  geom_smooth(aes(x = DPI, y = fit))



# visualizing GAMs ---------------------

ggplot(CNDF)+
  geom_raster(aes(x = DMI_NDF_bw, y = DMI_CP_bw, z = fit, fill = fit), data = CNDF)+
  geom_contour(aes(x = DMI_NDF_bw, y = DMI_CP_bw, z = fit), bins = 5, colour = "white", data = CNDF)+
  geom_line(aes(y = CP_IR, x = NDF_IR, group = Diet), data = rails, size = .8)+
  scale_fill_continuous(name = "Weight change (%/day)", type = "viridis")+
  xlim(min(CNDF$DMI_NDF_bw), max(CNDF$DMI_NDF_bw))+
  ylim(min(CNDF$DMI_CP_bw), max(CNDF$DMI_CP_bw))+
  theme_minimal()

ggplot(CE)+
  geom_raster(aes(x = DMI_energy_bw, y = DMI_CP_bw, z = fit, fill = fit))+
  geom_contour(aes(x = DMI_energy_bw, y = DMI_CP_bw, z = fit), bins = 5, colour = "white")+
  scale_fill_continuous(name = "Weight change (%/day)", type = "viridis")+
  theme_minimal()

ggplot(DNDF)+
  geom_raster(aes(x = DNDFI, y = DPI, z = fit, fill = fit))+
  geom_contour(aes(x = DNDFI, y = DPI, z = fit), bins = 5, colour = "white")+
  scale_fill_continuous(name = "Weight change (%/day)", type = "viridis")+
  theme_minimal()





ggplot(DE)+
  geom_raster(aes(x = DEI, y = DPI, z = fit, fill = fit))+
  geom_contour(aes(x = DEI, y = DPI, z = fit), bins = 5, colour = "white")+
  scale_fill_continuous(name = "Weight change (%/day)", type = "viridis")+
  theme_minimal()



# lisa figures --------------------------------------------------------------------

(DEintake <-
   ggplot(trials)+
   geom_point(aes(x = DEI, y = Weight_change))+
   geom_smooth(aes(x = DEI, y = Weight_change), method = "gam")+
   geom_abline(intercept = 0, slope = 0, linetype = 2)+
   ylab("Weight change (%/day)")+
   xlab(expression(DE~intake~(kj/kg^0.75/day)))+
   labs(title = "B")+
   themerails)

(DPintake <-
    ggplot(trials)+
    geom_point(aes(x = DPI, y = Weight_change))+
    geom_smooth(aes(x = DPI, y = Weight_change), method = "gam")+
    geom_abline(intercept = 0, slope = 0, linetype = 2)+
    ylab("Weight change (%/day)")+
    xlab(expression(DP~intake~(g/kg^0.75/day)))+
    labs(title = "B")+
    themerails)

weight <- ggarrange(DEintake, DPintake,  ncol = 1, nrow =2)



# Save table --------------------------------------------------------------

write.csv(summarytable, "Output/stats/GAMoutputs.csv")

        