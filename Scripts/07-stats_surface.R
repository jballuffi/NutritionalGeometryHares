  
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)
library(tidymv)
  
  
#read in results
trials <- readRDS("Output/data/trialresultscleaned.rds")
  
#nutritional rails
rails <- fread("Output/data/dietdigestionrails.rds")
  
#target intake rates
MCsums <- readRDS("Output/data/multichoicesums.rds")



# model weight change ~ crude energy --------------------

#bodyCE <- gam(Weight_change ~ s(DMI_energy_bw, DMI_CP_bw), data = trials)
bodyCE <- gam(Weight_change ~ s(DMI_energy_bw) + s(DMI_CP_bw) + s(DMI_energy_bw, DMI_CP_bw), data = trials)

#save summary of model
sumCE <- summary(bodyCE)

#make p-table, and indicate model, this is for intercept stuff
sumpCE <- as.data.table(round((sumCE$p.table), 4))
sumpCE[, Model := "Crude energy"]
sumpCE[, `Dev. Explained` := round(sumCE$dev.expl, 1)]

#make s-table, and indicate model, this is for variable stuff
sumsCE <- as.data.table(round((sumCE$s.table), 4))
sumsCE[, Model := "Crude energy"]



# model weight change ~ digestible energy -----------------------------------

#bodyDE <- gam(Weight_change ~ s(DEI, DPI), data = trials)
bodyDE <- gam(Weight_change ~ s(DEI) + s(DPI) + s(DEI, DPI), data = trials)

#save summary of model
sumDE <- summary(bodyDE)

#make p-table, and indicate model, this is for intercept stuff
sumpDE <- as.data.table(round((sumDE$p.table), 4))
sumpDE[, Model := "Digestible energy"]
sumpDE[, `Dev. Explained` := round(sumDE$dev.expl, 1)]

#make s-table, and indicate model, this is for variable stuff
sumsDE <- as.data.table(round((sumDE$s.table), 4))
sumsDE[, Model := "Digestible energy"]



# Merge output tables -----------------------------------------------------

# #bind all p tables
# allsump <- rbind(sumpCNDF, sumpCE, sumpDNDF, sumpDE)
# names(allsump) <- c("Estimate/edf", "SE/Ref.df", "t/F", "p", "Model", "Response", "Dev. Explained")
# allsump[, Parameter := "Intercept"]

#bind all s tables
allsums <- rbind(sumsCE, sumsDE)

setcolorder(allsums, c("Model", "edf", "Ref.df", "F", "p-value"))

#round the table to 2 decimal places
summarytable <- allsums %>% mutate_if(is.numeric, round, digits = 2)



# make predictive datasheets of each model --------------------------------


CE <- predict_gam(bodyCE)
DE <- predict_gam(bodyDE)



# visualizing GAMs ---------------------

#get target intake for each food component
targets <- MCsums[, .(meanCPI = mean(DMI_CP_bw), meanCEI = mean(DMI_energy_bw),
                      meanDPI = mean(DMI_DP_bw),  meanDEI = mean(DMI_DEI_bw))]

dietlines <- c("A" = "solid", "B" = "longdash", "C" = "dotdash", "D" = "dotted")

(a <- ggplot()+
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
  labs(title = "A) Crude energy and protein")+
  themerails)

(b <- ggplot()+
  geom_raster(aes(x = DEI, y = DPI, z = fit, fill = fit), data = DE)+
  geom_contour(aes(x = DEI, y = DPI, z = fit), bins = 5, colour = "white", data = DE)+
  scale_fill_continuous(name = "%/day", type = "viridis", guide = NULL)+
  geom_line(aes(x = DE_IR, y = DP_IR, group = Diet, linetype = Diet), size = .8, data = rails)+
  scale_linetype_manual(values = dietlines)+
  geom_point(aes(x = meanDEI, y = meanDPI), data = targets)+
  xlim(min(DE$DEI), max(DE$DEI))+
  ylim(min(DE$DPI), max(DE$DPI))+
  xlab(expression(DE~intake~(kj/kg^0.75/day)))+
  ylab(expression(DP~intake~(g/kg^0.75/day)))+
  labs(title = "B) Digestible energy and protein")+
  themerails)

surfaceplot <- ggarrange(a, b, ncol = 1, nrow = 2)



# Linear relationship between DE and weight change --------------------------------------------------------------------

#run linear model and make prediction table
lmDE <- lm(Weight_change ~ DEI, trials)
effs_lmDE <- as.data.table(ggpredict(lmDE, terms = c("DEI")))

#create rounded predictions
effs_lmDE[, predicted_round := round(predicted, digits = 1)]

#get predicted Energy intake for body maintanence
reqDE <- effs_lmDE[predicted_round == 0.0, return(as.numeric(x))]


(DEintake <-
    ggplot()+
    geom_point(aes(x = DEI, y = Weight_change), data = trials)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), colour = "grey80", alpha = .3, data = effs_lmDE)+
    geom_line(aes(x = x, y = predicted), linewidth = 1, data = effs_lmDE)+
    geom_abline(intercept = 0, slope = 0, linetype = 2)+
    ylab("Weight change (%/day)")+
    xlab(expression(DE~intake~(kj/kg^0.75/day)))+
    labs(title = "A")+
    themerails)



# Linear relationship beween DP and weight change -------------------------

#run linear model and make prediction table
lmDP <- lm(Weight_change ~ DPI, trials)
effs_lmDP <- as.data.table(ggpredict(lmDP, terms = c("DPI")))

#create rounded predictions
effs_lmDP[, predicted_round := round(predicted, digits = 1)]

#get predicted Energy intake for body maintanence
reqDP <- effs_lmDP[predicted_round == 0.2, return(as.numeric(x))]



(DPintake <-
    ggplot()+
    geom_point(aes(x = DPI, y = Weight_change), data = trials)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), colour = "grey80", alpha = .3, data = effs_lmDP)+
    geom_line(aes(x = x, y = predicted), linewidth = 1, data = effs_lmDP)+
    geom_abline(intercept = 0, slope = 0, linetype = 2)+
    ylab("Weight change (%/day)")+
    xlab(expression(DP~intake~(g/kg^0.75/day)))+
    labs(title = "B")+
    themerails)

weight <- ggarrange(DEintake, DPintake,  ncol = 1, nrow =2)



# CE and CP ---------------------------------------------------------------

lmCP <- lm(Weight_change ~ DMI_CP_bw, trials)

lmCE <- lm(Weight_change ~ DMI_energy_bw, trials)



# Save --------------------------------------------------------------

write.csv(summarytable, "Output/stats/GAMoutputs.csv")

ggsave("Output/figures/weightchangedigestible.jpeg", weight, width = 5, height = 8, unit = "in")

ggsave("Output/figures/surfaceplots.jpeg", surfaceplot, width = 6, height = 10, unit = "in")
        