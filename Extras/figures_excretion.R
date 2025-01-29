# script to collect digestability results

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in daily feeding trial results
days <- readRDS("Output/data/dailyresultscleaned.rds")

trials <- readRDS("Output/data/trialresultscleaned.rds")





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

(ADL <- 
    ggplot(days)+
    geom_point(aes(x = DMI_ADL, y = DMF_ADL, color = Diet))+
    labs(x = "ADL Intake (g DM/day", y = "ADL Excretion (g DM/day)")+
    themepoints)


fullfig <- ggarrange(total, protein, NDF, ADF, ncol = 2, nrow = 2)




# nutrient intake and excretion compositions (%) --------------------------



Proteinintake <-  ggplot(days)+
  geom_jitter(aes(x = CP_diet*100, y = CP_F), shape = 1, width = .5)+
  geom_smooth(aes(x = CP_diet*100, y = CP_F), color = "black")+
  labs(x = "Diet crude protein (%)", y = "Fecal crude protein (%)")+
  themerails

ggplot(trials)+
    geom_jitter(aes(x = CP_diet*100, y = Weight_change), width = .5)+
    geom_smooth(aes(x = CP_diet*100, y = Weight_change))+
    labs(x = "Diet crude protein (%)", y = "Weight change (%)")+
    themerails

Proteincomp <- ggplot(trials)+
  geom_abline(intercept = 0, slope = 0, linetype = 2)+
  geom_jitter(aes(x = CP_F, y = Weight_change), width = .5, shape = 1)+
  geom_smooth(aes(x = CP_F, y = Weight_change), method = "lm", color = "black")+
  labs(x = "Fecal crude protein (%)", y = "Weight change (%)")+
  themerails

# save figures ------------------------------------------------------------http://127.0.0.1:30311/graphics/63aa41fe-b6f4-4ee6-a023-5358019a1a6f.png


ggsave("Output/figures/excretionrates.jpg", fullfig, width = 7, height = 6, units = "in")
ggsave("Output/figures/fecalprotein.jpg", Proteincomp, width = 6, height = 5, units = "in")
ggsave("Output/figures/fecalprotein_intake.jpg", Proteinintake, width = 6, height = 5, units = "in")
