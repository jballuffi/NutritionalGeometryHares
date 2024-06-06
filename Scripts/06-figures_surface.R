#using fields package to visualize hare performance across nutritional space

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in results
trials <- readRDS("Output/data/trialresultscleaned.rds")
day <- readRDS("Output/data/dailyresultscleaned.rds")



# figure for digestible intake rate and weight change ---------------------

(DMDintake <- 
   ggplot(trials)+
   geom_point(aes(x = DMDI, y = Weight_change))+
   geom_smooth(aes(x = DMDI, y = Weight_change), method = "lm")+
   geom_abline(intercept = 0, slope = 0, linetype = 2)+
   ylab("Weight change (%/day)")+
   xlab(expression(DMD~intake~(g/kg^0.75/day)))+
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

weight <- ggarrange(DMDintake, DPintake,  ncol = 1, nrow =2)



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
