#Script to plot basic intake and weight change results

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in cleaned feeding trial data
trials <- readRDS("Output/data/trialresultscleaned.rds")


FTmeans <- trials[, .(mean(Intake_bw), sd(Intake_bw), mean(Weight_change), sd(Weight_change)), by = Diet]
names(FTmeans) <-  c("Diet", "Intake_mean", "Intake_SD", "Weight_mean", "Weight_SD")


(ConsumptionRates<-
  ggplot(FTmeans)+
  geom_bar(aes(y = Intake_mean, x = Diet), width = .5, stat = "identity", fill = "grey70")+
  geom_errorbar(aes(x = Diet, ymax = Intake_mean + Intake_SD, ymin = Intake_mean - Intake_SD), width = .2, color = "grey30")+
  labs(y = "Total Consumption (g DM/kg/day)")+
  themerails)


(WeightChange<-
  ggplot(trials)+
  geom_boxplot(aes(x = Diet, y = Weight_change), outlier.shape = NA, width = .75)+
  geom_jitter(aes(x = Diet, y = Weight_change), shape = 1, size = 2, width = .3)+
  labs(y = "Weight change (%/Day)")+
  themerails)


(proteinintake <-
  ggplot(trials)+
  geom_point(aes(x = CP_in_bw, y = Weight_change, color = Diet), size = 2)+
  labs(y = "Weight change (%/Day)", x = "Protein intake (g DM/kg/day)")+
  themerails)

#exploratory plots with temp data 
ggplot(trials)+
  geom_point(aes(x = Temp, y = Weight_change, color = Diet))+
  theme_minimal()

ggplot(trials)+
  geom_point(aes(x = Temp, y = Intake_bw, color = Diet))+
  theme_minimal()




#save plots
ggsave("Output/figures/consumptionbarplot.jpeg", ConsumptionRates, width = 4, height = 3, unit = "in")
ggsave("Output/figures/weightboxplot.jpeg", WeightChange, width = 4, height = 3, unit = "in")
ggsave("Output/figures/proteinintake.jpeg", proteinintake, width = 5, height = 3, unit = "in")
