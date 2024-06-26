#using fields package to visualize hare performance across nutritional space

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in results
trials <- readRDS("Output/data/trialresultscleaned.rds")
day <- readRDS("Output/data/dailyresultscleaned.rds")



# fecal protein and weight change -----------------------------------------

FP <- lm(Weight_change ~ CP_F, data = trials)

ggplot(trials)+
  geom_point(aes(x = CP_F, y = Weight_change), size = 2, shape = 1)+
  geom_abline(intercept = 0, slope = 0, linetype = 2, size = .8)+
  geom_abline(intercept = -1.6, slope = 0.15, linetype = 1, color = "blue3", size = .8)+
  #geom_smooth(aes(x = CP_F, y = Weight_change), method = "lm")+
  #geom_abline(intercept = -1.62, slope = 0.15)
  themerails



# surface plots for weight change response -----------------------------------------------------------

#weight change ~ NDF and CP
fitNDF <- Tps(trials[, .(DMI_NDF_bw, DMI_CP_bw)], trials$Weight_change, scale.type = "range")
surface(fitNDF, x = "NDF intake (g DM/kg^0.75/day)", 
        y = "Protein intake (g DM/kg^0.75/day)", main = "Weight change (%/day)")

#weight change ~ DE and DPI
fitCE <- Tps(trials[, .(DMI_energy_bw, DMI_CP_bw)], trials$Weight_change, scale.type = "range")
surface(fitCE, x = "Crude Energy intake (kj/kg^0.75/day)", 
        y = "Protein intake (g DM/kg^0.75/day)", main = "Weight change (%/day)")

#weight change ~ NDF and CP
fitDNDF <- Tps(trials[, .(DNDFI, DPI)], trials$Weight_change, scale.type = "range")
surface(fitDNDF, x = "Digestible NDF intake (g DM/kg^0.75/day)", 
        y = "Digestible Protein intake (g DM/kg^0.75/day)", main = "Weight change (%/day)")

#weight change ~ DE and DPI
fitDE <- Tps(trials[, .(DEI, DPI)], trials$Weight_change, scale.type = "range")
surface(fitDE, x = "Digestible Energy intake (kj DM/kg^0.75/day)", 
        y = "Digestible protein intake (g DM/kg^0.75/day)", main = "Weight change (%/day)")



# surface plots for digestibilities ---------------------------------------

#NDF and CP intake effect on digestible energy
de <- Tps(day[, .(DMI_NDF_bw, DMI_CP_bw)], day$DE, scale.type = "range")
surface(de, x = "NDF intake (g DM/kg^0.75/day)", 
        y = "Protein intake (g DM/kg^0.75/day)", main = "DE (%)")

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
