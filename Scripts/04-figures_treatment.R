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
sums <- sums[DMI_bw > 30] #remove food strikers

# intake rate figures -----------------------


#calculate mean intakes and weight change
Intakemeans <- day[, .(mean(DMI_bw), sd(DMI_bw)), by = Diet]
names(Intakemeans) <-  c("Diet", "DMI_mean", "DMI_sd")

#bar graph by treatment
(IntakeBar<-
  ggplot(Intakemeans)+
  geom_bar(aes(y = DMI_mean, x = Diet), width = .75, stat = "identity", fill = "grey70")+
  geom_errorbar(aes(x = Diet, ymax = DMI_mean + DMI_sd, ymin = DMI_mean - DMI_sd), width = .2, color = "grey30")+
  labs(y = "Total Consumption (g DM/kg^0.75/day)", x = "Diet", title = "A")+
  themerails)


#calculate mean intake rates by diet
meanday <- day[, .(mean(DMI_CP_bw), sd(DMI_CP_bw), mean(DMI_NDF_bw), sd(DMI_NDF_bw)), Diet]
names(meanday) <- c("Diet", "CP", "CPsd", "NDF", "NDFsd")

#rail plot showing intake (rule of compromise)
(IntakeRails <-
    ggplot(rails)+
    geom_line(aes(y = CP_IR, x = NDF_IR, group = Diet))+
    geom_point(aes(x = mean(DMI_NDF_bw), y = mean(DMI_CP_bw)), shape = 12, size = 3, data = sums)+
    geom_point(aes(x = NDF, y = CP), size = 3, data = meanday)+
    geom_errorbar(aes(x = NDF, y = CP, ymin = CP - CPsd, ymax = CP + CPsd), width = .5, data = meanday)+
    geom_errorbar(aes(x = NDF, y = CP,xmin = NDF - NDFsd, xmax = NDF + NDFsd), width = .5, data = meanday)+
    labs(y = "CP Intake (g DM/day)", x = "NDF Intake (g DM/day)", title = "B")+
    themerails)

Intake <- ggarrange(IntakeBar, IntakeRails, nrow = 2, ncol = 1)



# weight change by treatment ----------------------------------------------

(WeightChange<-
   ggplot(trials)+
   geom_boxplot(aes(x = Diet, y = Weight_change), outlier.shape = NA, width = .75)+
   geom_jitter(aes(x = Diet, y = Weight_change), shape = 1, size = 2, width = .25)+
   labs(y = "Weight change (%/Day)")+
   themerails)


#weight change by trial
ggplot(trials)+geom_boxplot(aes(x = as.factor(Trial), y = Weight_change))



# Digestibility by diet ---------------------------------------------------


#subset to just digestibility columns
dig <- day[, .(Diet, DMD, DP, DNDF, DADF)]

#melt columns to have nutrient as a new variable
digmelt <- melt(dig, measure.vars = c("DMD", "DP", "DNDF", "DADF"), 
                variable.name = "nutrient", 
                value.name = "digestibility")

#re-order the nutrients for facet wrap
digmelt[, nutrient := factor(nutrient, levels = c("DMD", "DP", "DNDF", "DADF"))]

digmelt <- digmelt[!digestibility < -0.2]

#ggplot digestibility against diet
(dietdigest <- 
    ggplot(digmelt)+
    geom_boxplot(aes(x = Diet, y = digestibility*100))+
    labs(y = "Digestability", x = "Diet")+
    facet_wrap(~nutrient, nrow = 2, ncol = 2)+
    themepoints+
    theme(strip.background = element_blank()))


# Just PD and PD intake by diet ----------------------------------------------

#protein digestability
(ProteinDig<-
   ggplot(day)+
   geom_boxplot(aes(x = Diet, y = DP*100), width = .75)+
   #geom_jitter(aes(x = Diet, y = DP*100), shape = 1, size = 2, width = .25)+
   labs(y = "Protein Digestibility (%)")+
   themerails)

#protein intake
(ProteinIntake<-
    ggplot(day)+
    geom_boxplot(aes(x = Diet, y = DPI), width = .75)+
    #geom_jitter(aes(x = Diet, y = DPI), shape = 1, size = 2, width = .25)+
    labs(y = "Digestible Protein Intake (g/kg^0.75/day")+
    themerails)

#pull protein figs together
Protein <- ggarrange(ProteinDig, ProteinIntake, nrow = 1, ncol = 2)



# Just DMD by diet --------------------------------------------------------

(DMDdig<-
   ggplot(day)+
   geom_boxplot(aes(x = Diet, y = DMD*100),  width = .75)+
   #geom_jitter(aes(x = Diet, y = DMD*100), shape = 1, size = 2, width = .25)+
   labs(y = "Dry Matter Digestibility (%)")+
   themerails)

(DMDIntake<-
    ggplot(day)+
    geom_boxplot(aes(x = Diet, y = DMDI), width = .75)+
    #geom_jitter(aes(x = Diet, y = DMDI), shape = 1, size = 2, width = .25)+
    labs(y = "DMD Intake (g/kg^0.75/day)")+
    themerails)

#pull protein figs together
DMD <- ggarrange(DMDdig, DMDIntake, nrow = 1, ncol = 2)




# Rail plot with DP and DMD ------------------------------------------------------

#need to make rails with DMD and DP etc



#save plots
ggsave("Output/figures/intakebarandrail.jpeg", Intake, width = 4, height = 7, unit = "in")
ggsave("Output/figures/weightchangebar.jpeg", WeightChange, width = 4, height = 4, unit = "in")
ggsave("Output/figures/dietdigestion.jpeg", dietdigest, width = 7.5, height = 6 )
ggsave("Output/figures/proteindigestibility.jpeg", Protein, width = 8, height = 4, unit = "in")
ggsave("Output/figures/drymatterdigestibility.jpeg", DMD, width = 8, height = 4, unit = "in")
