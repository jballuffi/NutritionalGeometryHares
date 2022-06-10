#Script to plot basic intake and weight change results

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in cleaned feeding trial data
trials <- readRDS("Output/data/trialresultscleaned.rds")

#calculate mean intakes and weight change
FTmeans <- trials[, .(mean(Intake_bw), sd(Intake_bw), mean(Weight_change), sd(Weight_change)), by = Diet]
names(FTmeans) <-  c("Diet", "Intake_mean", "Intake_SD", "Weight_mean", "Weight_SD")



# intake rate and weight change in response to diet -----------------------

(ConsumptionRates<-
  ggplot(FTmeans)+
  geom_bar(aes(y = Intake_mean, x = Diet), width = .75, stat = "identity", fill = "grey70")+
  geom_errorbar(aes(x = Diet, ymax = Intake_mean + Intake_SD, ymin = Intake_mean - Intake_SD), width = .2, color = "grey30")+
  labs(y = "Total Consumption (g DM/kg/day)", x = "")+
  themerails+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()))

(WeightChange<-
  ggplot(trials)+
  geom_boxplot(aes(x = Diet, y = Weight_change), outlier.shape = NA, width = .75)+
  geom_jitter(aes(x = Diet, y = Weight_change), shape = 1, size = 2, width = .25)+
  labs(y = "Weight change (%/Day)")+
  themerails)

IntakeWeight <- ggarrange(ConsumptionRates, WeightChange, nrow = 2, ncol = 1)





# wieght change in response to nutrient intake ----------------------------


(proteinintake <-
  ggplot(trials)+
  geom_point(aes(x = CP_in_bw, y = Weight_change), size = 1.75, color = "grey20")+
  labs(y = "Weight change (%/Day)", x = "Protein intake (g DM/kg/day)")+
  themerails)

(NDFintake <-
    ggplot(trials)+
    geom_point(aes(x = NDF_in_bw, y = Weight_change), size = 1.75, color = "grey20")+
    labs(y = "Weight change (%/Day)", x = "NDF intake (g DM/kg/day)")+
    themerails)



#save plots
ggsave("Output/figures/intakeandweightchange.jpeg", IntakeWeight, width = 4, height = 7, unit = "in")
ggsave("Output/figures/proteinintake.jpeg", proteinintake, width = 5, height = 3, unit = "in")
ggsave("Output/figures/NDFintake.jpeg", NDFintake, width = 5, height = 3, unit = "in")
