# script to collect digestability results

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in daily feeding trial results
days <- readRDS("Output/data/dailyresultscleaned.rds")

days[, Weight_start := Weight_start/1000]





# Nutrient intake and excretion rates ----------------------------------------------


ggplot(days)+
  geom_point(aes(x = CP_in_bw, y = CP_out/Weight_start, color = Diet))+
  labs(x = "Protein Intake (g DM/day)", y = "Protein Excretion (g DM/day")+
  themerails

ggplot(days)+
  geom_point(aes(x = NDF_in_bw, y = NDF_out/Weight_start, color = Diet))+
  labs(x = "NDF Intake (g DM/day)", y = "NDF Excretion (g DM/day")+
  themerails

ggplot(days)+
  geom_point(aes(x = ADF_in, ADF_out, color = Diet))+
  labs(x = "ADF Intake (g DM/day", y = "ADF Extretion (g DM/day)")+
  themerails

ggplot(days)+
  geom_point(aes(x = C_in, C_out, color = Diet))+
  labs(x = "C Intake (g DM/day", y = "C Extretion (g DM/day)")+
  themerails


