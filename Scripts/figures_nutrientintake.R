
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
  geom_point(aes(x = DMI_bw, y = Weight_change), size = 1.75)+
  geom_smooth(aes(x = DMI_bw, y = Weight_change), method = "lm")+
  labs(y = "Weight change (%/Day)", x = "DMI (g/kg^0.75/day)")+
  #scale_x_continuous("\n Protein Intake (g DM/k^0.75/day) \n", n.breaks =5) +
  themerails)

(CPintake <-
   ggplot(trials)+
   geom_point(aes(x = DMI_CP_bw, y = Weight_change), size = 1.75)+
   geom_smooth(aes(x = DMI_CP_bw, y = Weight_change), method = "lm")+
   labs(y = "Weight change (%/Day)", x = "CP Intake (g DM/kg^0.75/day)")+
   #scale_x_continuous("\n Protein Intake (g DM/k^0.75/day) \n", n.breaks =5) +
   themerails)

(NDFintake <-
    ggplot(trials)+
    geom_point(aes(x = DMI_NDF_bw, y = Weight_change), size = 1.75)+
    geom_smooth(aes(x = DMI_NDF_bw, y = Weight_change), method = "lm")+
    labs(y = "Weight change (%/Day)", x = "NDF intake (g DM/kg^0.75/day)")+
    themerails)

nondig <- ggarrange(DMintake, CPintake, NDFintake, nrow = 1, ncol = 3)

# Weight change in response to digestible matter intake -------------------


(DMDintake <- 
   ggplot(trials)+
   geom_point(aes(x = DMDI, y = Weight_change), size = 1.75)+
   geom_smooth(aes(x = DMDI, y = Weight_change), method = "lm")+
   labs(y = "Weight change (%/Day)", x = "DMD intake (g/kg^0.75/day)")+
   #scale_x_continuous("\n Protein Intake (g DM/k^0.75/day) \n", n.breaks =5) +
   themerails)

(DPintake <-
    ggplot(trials)+
    geom_point(aes(x = DPI, y = Weight_change), size = 1.75)+
    geom_smooth(aes(x = DPI, y = Weight_change), method = "lm")+
    labs(y = "Weight change (%/Day)", x = "DP intake (g DM/kg^0.75/day)")+
    #scale_x_continuous("\n Protein Intake (g DM/k^0.75/day) \n", n.breaks =5) +
    themerails)

(DNDFintake <-
    ggplot(trials)+
    geom_point(aes(x = DNDFI, y = Weight_change), size = 1.75,)+
    geom_smooth(aes(x = DNDFI, y = Weight_change), method = "lm")+
    labs(y = "Weight change (%/Day)", x = "DNDF intake (g DM/kg^0.75/day)")+
    themerails)

dig <- ggarrange(DMDintake, DPintake, DNDFintake, ncol = 3, nrow = 1)


#single panel
ggsave("Output/figures/proteinintake.jpeg", proteinintake, width = 5, height = 3, unit = "in")
ggsave("Output/figures/NDFintake.jpeg", NDFintake, width = 5, height = 3, unit = "in")

#full panel figs
ggsave("Output/figures/nondigintake.jpeg", nondig, width = 9.5, height = 3, unit = "in")
ggsave("Output/figures/digintake.jpeg", dig, width = 9.5, height = 3, unit = "in")
