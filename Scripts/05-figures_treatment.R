#Script to plot basic intake and weight change results

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in cleaned feeding trial data
trials <- readRDS("Output/data/trialresultscleaned.rds")
day <- readRDS("Output/data/dailyresultscleaned.rds")

#read in data for diet nutritional rails
rails <- fread("Output/data/dietdigestibilityrails.rds")

#read in full multichoice results
MC <- readRDS("Output/data/multichoiceresults.rds")

#read in multichoice trial sums
sums <- readRDS("Output/data/multichoicesums.rds")



# intake rate figures (4 panel) -----------------------

#calculate mean intakes in mutlichoice trials
Multimeans <- MC[, .(mean(DMI_bw), sd(DMI_bw)/sqrt(.N)), by = Diet]
names(Multimeans) <-  c("Diet", "Intake_mean", "Intake_SD")

#make position for diet labels. Take max intakes so that labels are placed at the end of rails
dietlabs <- rails[, .(max_CP = max(CP_IR), max_GE = max(GE_IR), max_NDF = max(NDF_IR)), Diet]


#bar graph
(Mbar<-
    ggplot(Multimeans)+
    geom_bar(aes(y = Intake_mean, x = Diet), width = .75, stat = "identity", fill = "grey70")+
    geom_errorbar(aes(x = Diet, ymax = Intake_mean + Intake_SD, ymin = Intake_mean - Intake_SD), width = .2, color = "grey30")+
    geom_text(aes(x = 1, y = 13, label = "B"), family = "serif")+
    geom_text(aes(x = 2, y = 29, label = "A"), family = "serif")+
    ylab(expression(Daily~intake~(gDM/kg^0.75/day)))+
    xlab(expression(Diet))+
    ggtitle("Multi-choice", subtitle = "A")+
    themerails+
    theme(axis.ticks.x = element_blank()))

#rail plot for target intake according to naiive multi choice trials
(MGrail <-
    ggplot()+
    geom_line(aes(x = GE_IR, y = CP_IR, group = Diet), data = rails)+
    geom_point(aes(x = GEI_bw, y = CPI_bw), shape = 1, size = 2, data = sums)+
    geom_point(aes(x = mean(GEI_bw), y = mean(CPI_bw)), shape = 12, size = 3, data = sums)+
    geom_text(aes(x = max_GE + 50, y = max_CP, label = Diet), family = "serif", data = dietlabs)+
    ylab(expression(CP~intake~(gDM/kg^0.75/day)))+
    xlab(expression(GE~intake~(kJ/kg^0.75/day)))+
    ggtitle("Multi-choice", subtitle = "B")+
    themerails)

(MDrail <-
    ggplot()+
    geom_line(aes(x = DE_IR, y = DP_IR, group = Diet), data = rails)+
    geom_point(aes(x = DEI_bw, y = DPI_bw), shape = 1, size = 2, data = sums)+
    geom_point(aes(x = mean(DEI_bw), y = mean(DPI_bw)), shape = 12, size = 3, data = sums)+
    #geom_text(aes(x = max_CE + 50, y = max_CP, label = Diet), family = "serif", data = dietlabs)+
    ylab(expression(DP~intake~(gDM/kg^0.75/day)))+
    xlab(expression(DE~intake~(kJ/kg^0.75/day)))+
    ggtitle("Multi-choice", subtitle = "B")+
    themerails)




#calculate mean intakes in single choice trials
Singlemeans <- day[, .(mean(DMI_bw), sd(DMI_bw)/(sqrt(.N)), mean(DMI_CP_bw), sd(DMI_CP_bw)/(sqrt(.N)),
                       mean(DMI_energy_bw), sd(DMI_energy_bw)/(sqrt(.N)) ), by = Diet]
names(Singlemeans) <-  c("Diet", "DMI_mean", "DMI_sd", "CP", "CPsd", "CE", "CEsd")

#bar graph by treatment
(Sbar <-
    ggplot(Singlemeans)+
    geom_bar(aes(y = DMI_mean, x = Diet), width = .75, stat = "identity", fill = "grey70")+
    geom_errorbar(aes(x = Diet, ymax = DMI_mean + DMI_sd, ymin = DMI_mean - DMI_sd), width = .2, color = "grey30")+
    geom_text(aes(x = 1, y = 121, label = "B, C, D"), family = "serif")+
    geom_text(aes(x = 2, y = 105, label = "A"), family = "serif")+
    geom_text(aes(x = 3, y = 97, label = "A"), family = "serif")+
    geom_text(aes(x = 4, y = 100, label = "A"), family = "serif")+
    ylab(expression(Daily~intake~(gDM/kg^0.75/day)))+
    xlab(expression(Diet))+
    ggtitle("No-choice", subtitle = "C")+
    themerails)

#rail plot showing intake (rule of compromise)
(Srail <-
    ggplot()+
    geom_line(aes(y = CP_IR, x = CE_IR, group = Diet), data = rails)+
    geom_point(aes(x = mean(DMI_energy_bw), y = mean(DMI_CP_bw)), shape = 12, size = 3, data = sums)+
    geom_point(aes(x = CE, y = CP), size = 2, shape = 1, data = Singlemeans)+
    geom_errorbar(aes(x = CE, y = CP, ymin = CP - CPsd, ymax = CP + CPsd), width = .5, data = Singlemeans)+
    geom_errorbar(aes(x = CE, y = CP,xmin = CE - CEsd, xmax = CE + CEsd), width = .5, data = Singlemeans)+
    geom_text(aes(x = max_CE + 50, y = max_CP, label = Diet), family = "serif", data = dietlabs)+
    ylab(expression(CP~intake~(gDM/kg^0.75/day)))+
    xlab(expression(CE~intake~(kJ/kg^0.75/day)))+
    ggtitle("Single-choice", subtitle = "D")+
    themerails)



Intake <- ggarrange(Mbar, Mrail, Sbar, Srail, nrow = 2, ncol = 2)



# weight change by treatment ----------------------------------------------

#weight change by diet
(WeightChange<-
   ggplot(trials)+
   geom_boxplot(aes(x = Diet, y = Weight_change), outlier.shape = NA, width = .75)+
   geom_jitter(aes(x = Diet, y = Weight_change), shape = 1, size = 2, width = .25)+
   geom_text(aes(x = 1, y = .8, label = "B, C, D"), family = "serif")+
   geom_text(aes(x = 2, y = 1.4, label = "A"), family = "serif")+
   geom_text(aes(x = 3, y = 1.6, label = "A"), family = "serif")+
   geom_text(aes(x = 4, y = 1.4, label = "A"), family = "serif")+
   labs(y = "Weight change (%/Day)")+
   ylim(-2.5, 1.7)+
   themerails)



# Digestibility by diet ---------------------------------------------------

#dry matter digestibility in response to diet
(DMDplot <- 
    ggplot(day)+
    geom_boxplot(aes(x = Diet, y = DMD*100), outlier.shape = NA)+
    geom_text(aes(x = 1, y = 50, label = "B, C, D"), family = "serif")+
    geom_text(aes(x = 2, y = 70, label = "A, D"), family = "serif")+
    geom_text(aes(x = 3, y = 70, label = "A"), family = "serif")+
    geom_text(aes(x = 4, y = 70, label = "A"), family = "serif")+
    labs(y = "Apparent digestability (%)", x = "", title = "A) Dry matter")+
    themerails+
    theme(strip.background = element_blank()))

#protein digestibility in response to diet
(DPplot <- 
    ggplot(day)+
    geom_boxplot(aes(x = Diet, y = CPD*100), outlier.shape = NA)+
    geom_text(aes(x = 1, y = 70, label = "B, C, D"), family = "serif")+
    geom_text(aes(x = 2, y = 81, label = "A, C, D"), family = "serif")+
    geom_text(aes(x = 3, y = 85, label = "A, B, D"), family = "serif")+
    geom_text(aes(x = 4, y = 93, label = "A, B, C"), family = "serif")+
    labs(y = "Apparent digestability (%)", x = "Diet", title = "B) Protein")+
    themerails+
    theme(strip.background = element_blank()))

dietdigest <- ggarrange(DMDplot, DPplot, ncol = 1, nrow = 2)


#save plots
ggsave("Output/figures/intakebarandrail.jpeg", Intake, width = 8, height = 8, unit = "in")
ggsave("Output/figures/weightchangebar.jpeg", WeightChange, width = 4, height = 4, unit = "in")
ggsave("Output/figures/dietdigestion.jpeg", dietdigest, width = 5, height = 8 )
