# script to collect digestability results

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in daily feeding trial results
days <- readRDS("Output/data/dailyresultscleaned.rds")

days[, Weight_start := Weight_start/1000]





# Nutrient intake and excretion rates (quantity) ----------------------------------------------

(total <- 
  ggplot(days)+
  geom_point(aes(x = DMI, y = DMF, color = Diet))+
  labs(x = "Dry Matter Intake (g DM/day)", y = "Dry Matter Excretion (g DM/day")+
  themepoints)

(protein <-
  ggplot(days)+
  geom_point(aes(x = DMI_CP, y = DMF_CP, color = Diet))+
  labs(x = "Protein Intake (g DM/day)", y = "Protein Excretion (g DM/day")+
  themepoints)

(NDF <- 
  ggplot(days)+
  geom_point(aes(x = DMI_NDF, y = DMF_NDF, color = Diet))+
  labs(x = "NDF Intake (g DM/day)", y = "NDF Excretion (g DM/day")+
  themepoints)

(ADF <- 
  ggplot(days)+
  geom_point(aes(x = DMI_ADF, y = DMF_ADF, color = Diet))+
  labs(x = "ADF Intake (g DM/day", y = "ADF Excretion (g DM/day)")+
  themepoints)

fullfig <- ggarrange(total, protein, NDF, ADF, ncol = 2, nrow = 2)




# nutrient intake and excretion compositions (%) --------------------------


(Proteincomp <- 
  ggplot(days)+
  geom_jitter(aes(x = CP_diet*100, y = CP_F), width = .5)+
  geom_smooth(aes(x = CP_diet*100, y = CP_F))+
  labs(x = "Diet crude protein (%)", y = "Fecal crude protein (%)")+
  themerails)

(NDFcomp <-
  ggplot(days)+
  geom_jitter(aes(x = NDF_diet*100, y = NDF_F), width = .5)+
  geom_smooth(aes(x = NDF_diet*100, y = NDF_F))+
  labs(x = "Diet NDF (%)", y = "Fecal NDF (%)")+
  themerails)

comps <- ggarrange(Proteincomp, NDFcomp, ncol = 1, nrow = 2)


# save figures ------------------------------------------------------------


ggsave("Output/figures/excretionrates.jpg", fullfig, width = 7, height = 6, units = "in")
ggsave("Output/figures/fecalcompositions.jpg", comps, width = 3.5, height = 6, units = "in")
