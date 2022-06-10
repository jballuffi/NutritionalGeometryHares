#Script to plot basic intake and weight change results

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in cleaned feeding trial data
trials <- readRDS("Output/data/trialresultscleaned.rds")
day <- readRDS("Output/data/dailyresultscleaned.rds")

#calculate mean intakes and weight change
Intakemeans <- day[, .(mean(Intake_bw), sd(Intake_bw)), by = Diet]
names(Intakemeans) <-  c("Diet", "Intake_mean", "Intake_SD")



# intake rate and weight change in response to diet -----------------------

(ConsumptionRates<-
  ggplot(Intakemeans)+
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


# CP and NDF digestion in response to diet --------------------------------

(CPdigestion<-
   ggplot(day)+
   geom_boxplot(aes(x = Diet, y = CP_dig*100), outlier.shape = NA, width = .75)+
   labs(y = "Protein Digested (%)", x ="")+
   themerails+
   theme(axis.text.x = element_blank(),
         axis.ticks.x = element_blank()))

(NDFdigestion<-
    ggplot(day)+
    geom_boxplot(aes(x = Diet, y = NDF_dig*100), outlier.shape = NA, width = .75)+
    labs(y = "NDF Digested (%)", x = "")+
    themerails+
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank()))

(ADFdigestion<-
    ggplot(day)+
    geom_boxplot(aes(x = Diet, y = ADF_dig*100), outlier.shape = NA, width = .75)+
    labs(y = "ADF Digested (%)")+
    themerails)


digestion <- ggarrange(CPdigestion, NDFdigestion, ADFdigestion, ncol = 1, nrow = 3)


#save plots
ggsave("Output/figures/intakeandweightchange.jpeg", IntakeWeight, width = 4, height = 7, unit = "in")
ggsave("Output/figures/ntrientdigestion.jpeg", digestion, width = 3, height = 7, unit = "in")
