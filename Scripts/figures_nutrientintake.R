
#script for rail plots

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in data for diet nutritional rails
rails <- fread("Output/data/dietrails.rds")

#read in single choice results
trials <- readRDS("Output/data/trialresultscleaned.rds")
days <- readRDS("Output/data/dailyresultscleaned.rds")



# weight change in response to crude matter intake ----------------------------

(DMintake <- 
  ggplot(trials)+
  geom_point(aes(x = DMI_bw, y = Weight_change, color = Diet), size = 1.75)+
  labs(y = "Weight change (%/Day)", x = "Dry Matter Intake (g/kg^0.75/day)")+
  #scale_x_continuous("\n Protein Intake (g DM/k^0.75/day) \n", n.breaks =5) +
  themerails)

(proteinintake <-
   ggplot(trials)+
   geom_point(aes(x = DMI_CP_bw, y = Weight_change, color = Diet), size = 1.75)+
   labs(y = "Weight change (%/Day)", x = "Protein intake (g DM/kg^0.75/day)")+
   #scale_x_continuous("\n Protein Intake (g DM/k^0.75/day) \n", n.breaks =5) +
   themerails)

(NDFintake <-
    ggplot(trials)+
    geom_point(aes(x = DMI_NDF_bw, y = Weight_change, color = Diet), size = 1.75)+
    labs(y = "Weight change (%/Day)", x = "NDF intake (g DM/kg^0.75/day)")+
    themerails)



# Weight change in response to digestible matter intake -------------------


(DMDintake <- 
   ggplot(trials)+
   geom_point(aes(x = DMDI, y = Weight_change, color = Diet), size = 1.75)+
   labs(y = "Weight change (%/Day)", x = "Dry Matter Digestibility Intake (g/kg^0.75/day)")+
   #scale_x_continuous("\n Protein Intake (g DM/k^0.75/day) \n", n.breaks =5) +
   themerails)

(digproteinintake <-
    ggplot(trials)+
    geom_point(aes(x = DPI, y = Weight_change, color = Diet), size = 1.75)+
    labs(y = "Weight change (%/Day)", x = "Digestible Protein intake (g DM/kg^0.75/day)")+
    #scale_x_continuous("\n Protein Intake (g DM/k^0.75/day) \n", n.breaks =5) +
    themerails)

(digNDFintake <-
    ggplot(trials)+
    geom_point(aes(x = DNDFI, y = Weight_change, color = Diet), size = 1.75,)+
    labs(y = "Weight change (%/Day)", x = "Digestible NDF Intake (g DM/kg^0.75/day)")+
    themerails)



# Digestable intake vs. regular intake ------------------------------------

ggplot(days)+
  geom_point(aes(x = DMI_bw, y = DMDI, color = Diet), size = 1.75)+
  labs(x = "Dry matter intake (g/kg^0.75/day)", y = "Dry matter digestibility intake (g/kg^0.75/day)")+
  themerails

ggplot(days)+
  geom_point(aes(x = DMI_CP_bw, y = DPI, color = Diet), size = 1.75)+
  labs(y = "Digestible protein intake (g/kg^0.75/day", x = "Protein intake (g/kg^0.75/day")+
  themerails

ggplot(days)+
  geom_point(aes(x = DMI_NDF_bw, y = DNDFI, color = Diet), size = 1.75)+
  labs(y = "Digestible NDF intake (g/kg^0.75/day", x = "NDF intake (g/kg^0.75/day")+
  themerails




ggsave("Output/figures/proteinintake.jpeg", proteinintake, width = 5, height = 3, unit = "in")
ggsave("Output/figures/NDFintake.jpeg", NDFintake, width = 5, height = 3, unit = "in")
