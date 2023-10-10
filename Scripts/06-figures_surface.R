#using fields package to visualize hare performance across nutritional space

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in results
trials <- readRDS("Output/data/trialresultscleaned.rds")
day <- readRDS("Output/data/dailyresultscleaned.rds")




#not accounting for digestibility
fitCP <- Tps(trials[, .(DMI_NDF_bw, DMI_CP_bw)], trials$Weight_change, scale.type = "range")
surface(fitCP, x = "NDF intake (g DM/kg^0.75/day)", 
        y = "Protein intake (g DM/kg^0.75/day)", main = "Weight change (%/day)")

#yes accounting for digestibility
fitDP <- Tps(trials[, .(DNDFI, DPI)], trials$Weight_change, scale.type = "range")
surface(fitDP, x = "Digestible NDF intake (g DM/kg^0.75/day)", 
        y = "Digestible protein intake (g DM/kg^0.75/day)", main = "Weight change (%/day)")

#NDF and CP intake effect on dry matter digestibility
dmd <- Tps(day[, .(DMI_NDF_bw, DMI_CP_bw)], day$DMD, scale.type = "range")
surface(dmd, x = "NDF intake (g DM/kg^0.75/day)", 
        y = "Protein intake (g DM/kg^0.75/day)", main = "DMD (%)")

#NDF and CP intake on protein digestibility
dp <- Tps(day[, .(DMI_NDF_bw, DMI_CP_bw)], day$DP, scale.type = "range")
surface(dp, x = "NDF intake (g DM/kg^0.75/day)", 
        y = "Protein intake (g DM/kg^0.75/day)", main = "DP (%)")

