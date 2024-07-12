  
  #source the R folder to load any packages and functions
  lapply(dir('R', '*.R', full.names = TRUE), source)
  library(tidymv)
  
  
  #read in results
  trials <- readRDS("Output/data/trialresultscleaned.rds")
  
  #nutritional rails
  rails <- fread("Output/data/dietdigestionrails.rds")
  
  #target intake rates
  MCsums <- readRDS("Output/data/multichoicesums.rds")


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

#get target intake for each food component
targets <- MCsums[, .(meanCPI = mean(DMI_CP_bw), meanNDFI = mean(DMI_NDF_bw), meanCEI = mean(DMI_energy_bw),
                      meanDPI = mean(DMI_DP_bw), meanDNDFI = mean(DMI_DNDF_bw), meanDEI = mean(DMI_DEI_bw))]

dietlines <- c("A" = "solid", "B" = "longdash", "C" = "dotdash", "D" = "dotted")

(a <- ggplot()+
  geom_raster(aes(x = DMI_NDF_bw, y = DMI_CP_bw, z = fit, fill = fit), data = CNDF)+
  geom_contour(aes(x = DMI_NDF_bw, y = DMI_CP_bw, z = fit), bins = 5, colour = "grey90", data = CNDF)+
  scale_fill_continuous(name = "%/day", type = "viridis")+
  geom_line(aes(x = NDF_IR, y = CP_IR, group = Diet, linetype = Diet), size = .8, data = rails)+
  scale_linetype_manual(values = dietlines, guide = NULL)+
  geom_point(aes(x = meanNDFI, y = meanCPI), data = targets)+
  xlim(min(CNDF$DMI_NDF_bw), max(CNDF$DMI_NDF_bw))+
  ylim(min(CNDF$DMI_CP_bw), max(CNDF$DMI_CP_bw))+
  xlab(expression(NDF~intake~(g/kg^0.75/day)))+
  ylab(expression(CP~intake~(g/kg^0.75/day)))+
  labs(title = "A) Crude NDF and protein")+
  themerails)

(b <- ggplot()+
  geom_raster(aes(x = DMI_energy_bw, y = DMI_CP_bw, z = fit, fill = fit), data = CE)+
  geom_contour(aes(x = DMI_energy_bw, y = DMI_CP_bw, z = fit), bins = 5, colour = "grey90", data = CE)+
  scale_fill_continuous(name = "%/day", type = "viridis")+
  geom_line(aes(x = CE_IR, y = CP_IR, group = Diet, linetype = Diet), size = .8, data = rails)+
  scale_linetype_manual(values = dietlines, guide = NULL)+
  geom_point(aes(x = meanCEI, y = meanCPI), data = targets)+
  xlim(min(CE$DMI_energy_bw), max(CE$DMI_energy_bw))+
  ylim(min(CE$DMI_CP_bw), max(CE$DMI_CP_bw))+
  xlab(expression(CE~intake~(kj/kg^0.75/day)))+
  ylab(expression(CP~intake~(g/kg^0.75/day)))+
  labs(title = "B) Crude energy and protein")+
  themerails)

c <- ggplot()+
  geom_raster(aes(x = DNDFI, y = DPI, z = fit, fill = fit), data = DNDF)+
  geom_contour(aes(x = DNDFI, y = DPI, z = fit), bins = 5, colour = "grey90", data = DNDF)+
  scale_fill_continuous(name = "%/day", type = "viridis")+
  geom_line(aes(x = DNDF_IR, y = DP_IR, group = Diet, linetype = Diet), size = .8, data = rails, )+
  scale_linetype_manual(values = dietlines, guide = NULL)+
  geom_point(aes(x = meanDNDFI, y = meanDPI), data = targets)+
  xlim(min(DNDF$DNDFI), max(DNDF$DNDFI))+
  ylim(min(DNDF$DPI), max(DNDF$DPI))+
  xlab(expression(DNDF~intake~(g/kg^0.75/day)))+
  ylab(expression(DP~intake~(g/kg^0.75/day)))+
  labs(title = "C) Digestible NDF and protein")+
  themerails

d <- ggplot()+
  geom_raster(aes(x = DEI, y = DPI, z = fit, fill = fit), data = DE)+
  geom_contour(aes(x = DEI, y = DPI, z = fit), bins = 5, colour = "white", data = DE)+
  scale_fill_continuous(name = "%/day", type = "viridis")+
  geom_line(aes(x = DE_IR, y = DP_IR, group = Diet, linetype = Diet), size = .8, data = rails)+
  scale_linetype_manual(values = dietlines)+
  geom_point(aes(x = meanDEI, y = meanDPI), data = targets)+
  xlim(min(DE$DEI), max(DE$DEI))+
  ylim(min(DE$DPI), max(DE$DPI))+
  xlab(expression(DE~intake~(kj/kg^0.75/day)))+
  ylab(expression(DP~intake~(g/kg^0.75/day)))+
  labs(title = "D) Digestible energy and protein")+
  themerails

surfaceplot <- ggarrange(a, b, c, d, ncol = 2, nrow = 2)



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



# Save --------------------------------------------------------------

write.csv(summarytable, "Output/stats/GAMoutputs.csv")

ggsave("Output/figures/weightchangedigestible.jpeg", weight, width = 5, height = 8, unit = "in")

ggsave("Output/figures/surfaceplots.jpeg", surfaceplot, width = 12, height = 9, unit = "in")
        