#Script to plot basic intake and weight change results

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in cleaned feeding trial data
trials <- readRDS("Output/data/trialresultscleaned.rds")
day <- readRDS("Output/data/dailyresultscleaned.rds")

#read in data for diet nutritional rails
rails <- fread("Output/data/dietrails.rds")

#read in full multichoice results
MC <- readRDS("Output/data/multichoiceresults.rds")

#read in multichoice trial sums
sums <- readRDS("Output/data/multichoicesums.rds")




# intake rate figures (4 panel) -----------------------


#calculate mean intakes in mutlichoice trials
Multimeans <- MC[, .(mean(DMI_bw), sd(DMI_bw)), by = Diet]
names(Multimeans) <-  c("Diet", "Intake_mean", "Intake_SD")

#bar graph
(Mbar<-
    ggplot(Multimeans)+
    geom_bar(aes(y = Intake_mean, x = Diet), width = .75, stat = "identity", fill = "grey70")+
    geom_errorbar(aes(x = Diet, ymax = Intake_mean + Intake_SD, ymin = Intake_mean - Intake_SD), width = .2, color = "grey30")+
    ylab(expression(Daily~intake~(gDM/kg^0.75/day)))+
    xlab(expression(Diet))+
    ggtitle("Multi-choice trials", subtitle = "A")+
    themerails+
    theme(axis.ticks.x = element_blank()))

#rail plot for target intake according to naiive multi choice trials
(Mrail <-
    ggplot(rails)+
    geom_line(aes(y = CP_IR, x = NDF_IR, group = Diet))+
    geom_point(aes(x = DMI_NDF_bw, y = DMI_CP_bw), size = 2, data = sums)+
    geom_point(aes(x = mean(DMI_NDF_bw), y = mean(DMI_CP_bw)), shape = 12, size = 3, data = sums)+
    ylab(expression(Protein~intake~(gDM/kg^0.75/day)))+
    xlab(expression(NDF~intake~(gDM/kg^0.75/day)))+
    ggtitle(" ", subtitle = "B")+
    themerails)


#calculate mean intakes in single choice trials
Singlemeans <- day[, .(mean(DMI_bw), sd(DMI_bw), mean(DMI_CP_bw), sd(DMI_CP_bw), mean(DMI_NDF_bw), sd(DMI_NDF_bw)), by = Diet]
names(Singlemeans) <-  c("Diet", "DMI_mean", "DMI_sd", "CP", "CPsd", "NDF", "NDFsd")

#bar graph by treatment
(Sbar<-
  ggplot(Singlemeans)+
  geom_bar(aes(y = DMI_mean, x = Diet), width = .75, stat = "identity", fill = "grey70")+
  geom_errorbar(aes(x = Diet, ymax = DMI_mean + DMI_sd, ymin = DMI_mean - DMI_sd), width = .2, color = "grey30")+
  ylab(expression(Daily~intake~(gDM/kg^0.75/day)))+
  xlab(expression(Diet))+
  ggtitle("Single-choice trials", subtitle = "C")+
  themerails)

#rail plot showing intake (rule of compromise)
(Srail <-
    ggplot(rails)+
    geom_line(aes(y = CP_IR, x = NDF_IR, group = Diet))+
    geom_point(aes(x = mean(DMI_NDF_bw), y = mean(DMI_CP_bw)), shape = 12, size = 3, data = sums)+
    geom_point(aes(x = NDF, y = CP), size = 3, data = Singlemeans)+
    geom_errorbar(aes(x = NDF, y = CP, ymin = CP - CPsd, ymax = CP + CPsd), width = .5, data = Singlemeans)+
    geom_errorbar(aes(x = NDF, y = CP,xmin = NDF - NDFsd, xmax = NDF + NDFsd), width = .5, data = Singlemeans)+
    ylab(expression(Protein~intake~(gDM/kg^0.75/day)))+
    xlab(expression(NDF~intake~(gDM/kg^0.75/day)))+
    ggtitle(" ", subtitle = "D")+
    themerails)



Intake <- ggarrange(Mbar, Sbar, Mrail, Srail, nrow = 2, ncol = 2)



# weight change by treatment ----------------------------------------------

#weight change by diet
(WeightChange<-
   ggplot(trials)+
   geom_boxplot(aes(x = Diet, y = Weight_change), outlier.shape = NA, width = .75)+
   geom_jitter(aes(x = Diet, y = Weight_change), shape = 1, size = 2, width = .25)+
   labs(y = "Weight change (%/Day)")+
   themerails)


#weight change by trial
ggplot(trials)+
  geom_boxplot(aes(x = as.factor(Trial), y = Weight_change))+
  labs(x = "Trial #", y = "Weight change (%/Day)")



# Digestibility by diet ---------------------------------------------------

#subset to just digestibility columns
dig <- day[, .(Diet, DMD, DP, DNDF, DADF, DADL)]

#melt columns to have nutrient as a new variable
digmelt <- melt(dig, measure.vars = c("DMD", "DP", "DNDF", "DADF", "DADL"), 
                variable.name = "nutrient", 
                value.name = "digestibility")

#re-order the nutrients for facet wrap
digmelt[, nutrient := factor(nutrient, levels = c("DMD", "DP", "DNDF", "DADF", "DADL"))]

#remove stuff that's not possible
digmelt <- digmelt[!digestibility < -0.2]

#ggplot digestibility against diet
(dietdigest <- 
    ggplot(digmelt)+
    geom_boxplot(aes(x = Diet, y = digestibility*100))+
    labs(y = "Apparent digestability (%)", x = "Diet")+
    facet_wrap(~nutrient, nrow = 3, ncol = 3)+
    themerails+
    theme(strip.background = element_blank()))





# Rail plot with DP and DMD ------------------------------------------------------

#need to make rails with DMD and DP etc



#save plots
ggsave("Output/figures/intakebarandrail.jpeg", Intake, width = 8, height = 8, unit = "in")
ggsave("Output/figures/weightchangebar.jpeg", WeightChange, width = 4, height = 4, unit = "in")
ggsave("Output/figures/dietdigestion.jpeg", dietdigest, width = 5, height = 10 )
