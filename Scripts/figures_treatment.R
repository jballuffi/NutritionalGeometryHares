#Script to plot basic intake and weight change results

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in cleaned feeding trial data
trials <- readRDS("Output/data/trialresultscleaned.rds")
day <- readRDS("Output/data/dailyresultscleaned.rds")

#read in data for diet nutritional rails
rails <- fread("Output/data/dietrails.rds")

#read in multichoice trial sums
sums <- readRDS("Output/data/multichoicesums.rds")


# intake rate figures -----------------------


#calculate mean intakes and weight change
Intakemeans <- day[, .(mean(Intake_bw), sd(Intake_bw)), by = Diet]
names(Intakemeans) <-  c("Diet", "Intake_mean", "Intake_SD")

#bar graph by treatment
(IntakeBar<-
  ggplot(Intakemeans)+
  geom_bar(aes(y = Intake_mean, x = Diet), width = .75, stat = "identity", fill = "grey70")+
  geom_errorbar(aes(x = Diet, ymax = Intake_mean + Intake_SD, ymin = Intake_mean - Intake_SD), width = .2, color = "grey30")+
  labs(y = "Total Consumption (g DM/kg/day)", x = "")+
  themerails+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()))


#calculate mean intake rates by diet
meanday <- day[, .(mean(CP_in_bw), sd(CP_in_bw), mean(NDF_in_bw), sd(CP_in_bw)), Diet]
names(meanday) <- c("Diet", "CP", "CPsd", "NDF", "NDFsd")

#rail plot showing intake (rule of compromise)
(IntakeRails <-
    ggplot(rails)+
    geom_line(aes(y = CP_IR, x = NDF_IR, group = Diet))+
    geom_point(aes(x = mean(NDF), y = mean(CP)), shape = 12, size = 3, data = sums)+
    geom_point(aes(x = NDF, y = CP), size = 3, data = meanday)+
    geom_errorbar(aes(x = NDF, y = CP, ymin = CP - CPsd, ymax = CP + CPsd), width = .5, data = meanday)+
    geom_errorbar(aes(x = NDF, y = CP,xmin = NDF - NDFsd, xmax = NDF + NDFsd), width = .5, data = meanday)+
    labs(y = "CP Intake (g DM/day)", x = "NDF Intake (g DM/day)")+
    themerails)

IntakeWeight <- ggarrange(IntakeBar, IntakeRails, nrow = 2, ncol = 1)



# weight change by treatment ----------------------------------------------

(WeightChange<-
   ggplot(trials)+
   geom_boxplot(aes(x = Diet, y = Weight_change), outlier.shape = NA, width = .75)+
   geom_jitter(aes(x = Diet, y = Weight_change), shape = 1, size = 2, width = .25)+
   labs(y = "Weight change (%/Day)")+
   themerails)



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
ggsave("Output/figures/intakebarandrail.jpeg", IntakeWeight, width = 4, height = 7, unit = "in")
ggsave("Output/figures/weightchangebar.jpeg", WeightChange, width = 4, height = 4, unit = "in")
ggsave("Output/figures/nutrientdigestion.jpeg", digestion, width = 3, height = 7, unit = "in")
