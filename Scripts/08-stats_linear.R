
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in results
trials <- readRDS("Output/data/trialresultscleaned.rds")




# Linear relationships between intake and weight change --------------------------------------------------------------------

###    MODEL FOR GROSS ENERGY

#make model
lmGE <- lm(Weight_change ~ GEI_bw, trials)

#get prediction table
effs_lmGE <- as.data.table(ggpredict(lmGE, terms = c("GEI_bw")))

#create rounded predictions
effs_lmGE[, predicted_round := round(predicted, digits = 1)]

#get predicted Energy intake for body maintanence
reqGE <- effs_lmGE[predicted_round == 0.0, return(as.numeric(x))]


###    MODEL FOR DIGESTIBLE ENERGY 

#make model
lmDE <- lm(Weight_change ~ DEI, trials)

#get prediction table 
effs_lmDE <- as.data.table(ggpredict(lmDE, terms = c("DEI")))

#create rounded predictions
effs_lmDE[, predicted_round := round(predicted, digits = 1)]

#get predicted Energy intake for body maintanence
reqDE <- effs_lmDE[predicted_round == 0.0, return(as.numeric(x))]


###   MODEL FOR CRUDE PROTEIN

#make model
lmCP <- lm(Weight_change ~ CPI_bw, trials)

#get prediction table
effs_lmCP <- as.data.table(ggpredict(lmCP, terms = c("CPI_bw")))

#create rounded predictions
effs_lmCP[, predicted_round := round(predicted, digits = 1)]

#get predicted Energy intake for body maintanence
reqCP <- effs_lmCP[predicted_round == 0.2, return(as.numeric(x))]


###   MODEL FOR DIGESTIBLE PROTEIN

#make model
lmDP <- lm(Weight_change ~ DPI, trials)

#get prediction table
effs_lmDP <- as.data.table(ggpredict(lmDP, terms = c("DPI")))

#create rounded predictions
effs_lmDP[, predicted_round := round(predicted, digits = 1)]

#get predicted Energy intake for body maintanence
reqDP <- effs_lmDP[predicted_round == 0.2, return(as.numeric(x))]




# Figures for linear regressions ------------------------------------------

(GEintake <-
   ggplot()+
   geom_point(aes(x = GEI_bw, y = Weight_change), shape = 1, data = trials)+
   geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), colour = "grey80", alpha = .3, data = effs_lmGE)+
   geom_line(aes(x = x, y = predicted), linewidth = 1, data = effs_lmGE)+
   geom_abline(intercept = 0, slope = 0, linetype = 2)+
   ylab("Weight change (%/day)")+
   xlab(expression(GE~intake~(kJ/kg^0.75/day)))+
   labs(title = "A) Gross Energy")+
   themerails)

(DEintake <-
    ggplot()+
    geom_point(aes(x = DEI, y = Weight_change), shape = 1, data = trials)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), colour = "grey80", alpha = .3, data = effs_lmDE)+
    geom_line(aes(x = x, y = predicted), linewidth = 1, data = effs_lmDE)+
    geom_abline(intercept = 0, slope = 0, linetype = 2)+
    ylab("Weight change (%/day)")+
    xlab(expression(DE~intake~(kJ/kg^0.75/day)))+
    labs(title = "B) Digestible Energy")+
    themerails)

(CPintake <-
    ggplot()+
    geom_point(aes(x = CPI_bw, y = Weight_change), shape = 1, data = trials)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), colour = "grey80", alpha = .3, data = effs_lmCP)+
    geom_line(aes(x = x, y = predicted), linewidth = 1, data = effs_lmCP)+
    geom_abline(intercept = 0, slope = 0, linetype = 2)+
    ylab("Weight change (%/day)")+
    xlab(expression(CP~intake~(g/kg^0.75/day)))+
    labs(title = "C) Crude Protein")+
    themerails)

(DPintake <-
    ggplot()+
    geom_point(aes(x = DPI, y = Weight_change), shape = 1, data = trials)+
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), colour = "grey80", alpha = .3, data = effs_lmDP)+
    geom_line(aes(x = x, y = predicted), linewidth = 1, data = effs_lmDP)+
    geom_abline(intercept = 0, slope = 0, linetype = 2)+
    ylab("Weight change (%/day)")+
    xlab(expression(DCP~intake~(g/kg^0.75/day)))+
    labs(title = "D) Digestible Protein")+
    themerails)

weight <- ggarrange(GEintake, DEintake, CPintake, DPintake,  ncol = 2, nrow =2)



# Save --------------------------------------------------------------

ggsave("Output/figures/weightchangelinear.jpeg", weight, width = 8, height = 7, unit = "in")
