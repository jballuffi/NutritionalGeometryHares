# script to collect digestability results

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in daily feeding trial results
days <- readRDS("Output/data/dailyresultscleaned.rds")

days[, Weight_start := Weight_start/1000]





# Nutrient intake and excretion rates ----------------------------------------------

total <- 
  ggplot(days)+
  geom_point(aes(x = Intake, y = Total_out, color = Diet))+
  labs(x = "Intake Rate (g DM/day)", y = "Excretion Rate (g DM/day")+
  themepoints

protein <-
  ggplot(days)+
  geom_point(aes(x = CP_in, y = CP_out, color = Diet))+
  labs(x = "Protein Intake (g DM/day)", y = "Protein Excretion (g DM/day")+
  themepoints

NDF <- 
  ggplot(days)+
  geom_point(aes(x = NDF_in_bw, y = NDF_out, color = Diet))+
  labs(x = "NDF Intake (g DM/day)", y = "NDF Excretion (g DM/day")+
  themepoints

ADF <- 
  ggplot(days)+
  geom_point(aes(x = ADF_in, ADF_out, color = Diet))+
  labs(x = "ADF Intake (g DM/day", y = "ADF Excretion (g DM/day)")+
  themepoints

fullfig <- ggarrange(total, protein, NDF, ADF, ncol = 2, nrow = 2)

ggsave("Output/figures/excretionrates.jpg", fullfig, width = 7, height = 6, units = "in")

