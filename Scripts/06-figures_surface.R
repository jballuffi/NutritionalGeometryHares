#using fields package to visualize hare performance across nutritional space

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in results
trials <- readRDS("Output/data/trialresultscleaned.rds")
day <- readRDS("Output/data/dailyresultscleaned.rds")



# figure for digestible intake rate and weight change ---------------------

(DEintake <- 
   ggplot(trials)+
   geom_point(aes(x = DEI, y = Weight_change))+
   geom_smooth(aes(x = DEI, y = Weight_change), method = "lm")+
   geom_abline(intercept = 0, slope = 0, linetype = 2)+
   ylab("Weight change (%/day)")+
   xlab(expression(DE~intake~(kj/kg^0.75/day)))+
   labs(title = "A")+
   themerails)

(DPintake <-
    ggplot(trials)+
    geom_point(aes(x = DPI, y = Weight_change))+
    geom_smooth(aes(x = DPI, y = Weight_change), method = "lm")+
    geom_abline(intercept = 0, slope = 0, linetype = 2)+
    ylab("Weight change (%/day)")+
    xlab(expression(DP~intake~(g/kg^0.75/day)))+
    labs(title = "B")+
    themerails)

weight <- ggarrange(DEintake, DPintake,  ncol = 1, nrow =2)



# fecal protein and weight change -----------------------------------------

FP <- lm(Weight_change ~ CP_F, data = trials)

ggplot(trials)+
  geom_point(aes(x = CP_F, y = Weight_change), size = 2, shape = 1)+
  geom_abline(intercept = 0, slope = 0, linetype = 2, size = .8)+
  geom_abline(intercept = -1.6, slope = 0.15, linetype = 1, color = "blue3", size = .8)+
  #geom_smooth(aes(x = CP_F, y = Weight_change), method = "lm")+
  #geom_abline(intercept = -1.62, slope = 0.15)
  themerails



# surface plots -----------------------------------------------------------

#not accounting for digestibility
fitCP <- Tps(trials[, .(DMI_NDF_bw, DMI_CP_bw)], trials$Weight_change, scale.type = "range")
surface(fitCP, x = "NDF intake (g DM/kg^0.75/day)", 
        y = "Protein intake (g DM/kg^0.75/day)", main = "Weight change (%/day)")

#yes accounting for digestibility
fitDP <- Tps(trials[, .(DMDI, DPI)], trials$Weight_change, scale.type = "range")
surface(fitDP, x = "DMD intake (g DM/kg^0.75/day)", 
        y = "Digestible protein intake (g DM/kg^0.75/day)", main = "Weight change (%/day)")

#yes accounting for digestibility
fitDP <- Tps(trials[, .(DEI, DPI)], trials$Weight_change, scale.type = "range")
surface(fitDP, x = "DE intake (g DM/kg^0.75/day)", 
        y = "Digestible protein intake (g DM/kg^0.75/day)", main = "Weight change (%/day)")



#NDF and CP intake effect on dry matter digestibility
dmd <- Tps(day[, .(DMI_NDF_bw, DMI_CP_bw)], day$DMD, scale.type = "range")
surface(dmd, x = "NDF intake (g DM/kg^0.75/day)", 
        y = "Protein intake (g DM/kg^0.75/day)", main = "DMD (%)")

#NDF and CP intake on protein digestibility
dp <- Tps(day[, .(DMI_NDF_bw, DMI_CP_bw)], day$DP, scale.type = "range")
surface(dp, x = "NDF intake (g DM/kg^0.75/day)", 
        y = "Protein intake (g DM/kg^0.75/day)", main = "DP (%)")



# save ' ------------------------------------------------------------------

ggsave("Output/figures/weightchangedigestible.jpeg", weight, width = 5, height = 8, unit = "in")
