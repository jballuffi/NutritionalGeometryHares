
#script for rail plots

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in data for diet nutritional rails
rails <- fread("Output/data/dietrails.rds")

#read in single choice results
trials <- readRDS("Output/data/trialresultscleaned.rds")
day <- readRDS("Output/data/dailyresultscleaned.rds")



# weight change in response to crude matter intake ----------------------------

(DMintake <- 
  ggplot(trials)+
  geom_point(aes(x = DMI_bw, y = Weight_change))+
  geom_smooth(aes(x = DMI_bw, y = Weight_change), method = "lm")+
  ylab("Weight change (%/day)")+
  xlab(expression(DM~intake~(g/kg^0.75/day)))+
  #scale_x_continuous("\n Protein Intake (g DM/k^0.75/day) \n", n.breaks =5) +
  themerails)

(CPintake <-
   ggplot(trials)+
   geom_point(aes(x = DMI_CP_bw, y = Weight_change))+
   geom_smooth(aes(x = DMI_CP_bw, y = Weight_change), method = "lm")+
   ylab("")+
   xlab(expression(CP~Intake~(gDM/kg^0.75/day)))+
   #scale_x_continuous("\n Protein Intake (g DM/k^0.75/day) \n", n.breaks =5) +
   themerails)

(NDFintake <-
    ggplot(trials)+
    geom_point(aes(x = DMI_NDF_bw, y = Weight_change))+
    geom_smooth(aes(x = DMI_NDF_bw, y = Weight_change), method = "lm")+
    ylab("")+
    xlab(expression(NDF~intake~(gDM/kg^0.75/day)))+
    themerails)

nondig <- ggarrange(DMintake, CPintake, NDFintake, nrow = 1, ncol = 3)




# DMD in response to nutrient intake --------------------------------------

ggplot(day)+
  geom_point(aes(x = DMI_CP_bw, y = DMD))+
  geom_smooth(aes(x = DMI_CP_bw, y = DMD), method = "lm")+
  ylab("DMD (%)")+
  xlab(expression(Protein~intake~(g/kg^0.75/day)))+
  themerails

ggplot(day)+
  geom_point(aes(x = DMI_NDF_bw, y = DMD))+
  geom_smooth(aes(x = DMI_NDF_bw, y = DMD), method = "lm")+
  ylab("DMD (%)")+
  xlab(expression(NDF~intake~(g/kg^0.75/day)))+
  themerails


#single panel
ggsave("Output/figures/proteinintake.jpeg", CPintake, width = 5, height = 3, unit = "in")
ggsave("Output/figures/NDFintake.jpeg", NDFintake, width = 5, height = 3, unit = "in")

#full panel figs
ggsave("Output/figures/nondigintake.jpeg", nondig, width = 9.5, height = 3, unit = "in")
ggsave("Output/figures/digintake.jpeg", dig, width = 9.5, height = 3, unit = "in")
