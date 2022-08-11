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
  labs(y = "Total Consumption (g DM/kg/day)", x = "Diet", title = "A")+
  themerails)


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


# Just protein digestibility for presentation ----------------------------------------------

(ProteinDig<-
   ggplot(day)+
   geom_boxplot(aes(x = Diet, y = CP_dig*100), outlier.shape = NA, width = .75)+
   geom_jitter(aes(x = Diet, y = CP_dig*100), shape = 1, size = 2, width = .25)+
   labs(y = "Protein Digestibility (%)")+
   themerails)


# Digestability by diet ---------------------------------------------------


#subset to just digestability columns
dig <- day[, .(Diet, CP_dig, NDF_dig, ADF_dig)]

#melt columns to have nutrient as a new variable
digmelt <- melt(dig, measure.vars = c("CP_dig", "NDF_dig", "ADF_dig"), 
                variable.name = "nutrient", 
                value.name = "digestability")

#remove the "_dig" from the nutrient values (for label in facet wrap)
digmelt[, nutrient := gsub("_dig", "", nutrient)]

#re-order the nutrients for facet wrap
digmelt[, nutrient := factor(nutrient, levels = c("CP", "NDF", "ADF"))]

#ggplot digestability against diet
(dietdigest <- 
    ggplot(digmelt)+
    geom_boxplot(aes(x = Diet, y = digestability))+
    labs(y = "Digestability", x = "Diet")+
    facet_wrap(~nutrient, nrow = 1, ncol = 3)+
    themepoints+
    theme(strip.background = element_blank()))




#save plots
ggsave("Output/figures/intakebarandrail.jpeg", Intake, width = 4, height = 7, unit = "in")
ggsave("Output/figures/weightchangebar.jpeg", WeightChange, width = 4, height = 4, unit = "in")
ggsave("Output/figures/dietdigestion.jpeg", dietdigest, width = 7.5, height = 3 )
ggsave("Output/figures/proteindigestibility.jpeg", ProteinDig, width = 4, height = 4, unit = "in")
