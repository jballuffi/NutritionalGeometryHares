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



# figures for multi-choice -----------------------

#calculate mean intakes rates for bar plot
Multimeans <- MC[, .(Intake_mean = mean(DMI_bw), Intake_SD = sd(DMI_bw)/sqrt(.N)), by = Diet]

#positions for diet labels. Take max intakes so that labels are placed at the end of rails
dietlabs <- rails[, .(max_CP = max(CP_IR), 
                      max_GE = max(GE_IR), 
                      max_NDF = max(NDF_IR),
                      max_DP = max(DP_IR), 
                      max_DE = max(DE_IR)), Diet]

#bar plot
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

#rail plot for crude/gross intake rates
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

#rail plot for digestible intake rates
(MDrail <-
    ggplot()+
    geom_line(aes(x = DE_IR, y = DP_IR, group = Diet), data = rails)+
    geom_point(aes(x = DEI_bw, y = DPI_bw), shape = 1, size = 2, data = sums)+
    geom_point(aes(x = mean(DEI_bw), y = mean(DPI_bw)), shape = 12, size = 3, data = sums)+
    geom_text(aes(x = max_DE + 50, y = max_DP, label = Diet), family = "serif", data = dietlabs)+
    ylab(expression(DP~intake~(gDM/kg^0.75/day)))+
    xlab(expression(DE~intake~(kJ/kg^0.75/day)))+
    ggtitle("Multi-choice", subtitle = "B")+
    themerails)




# figures for no-choice or single-choice trials ---------------------------
  
#calculate mean intakes rates for both bar plots and rail plots
Singlemeans <- day[, .(DMI_mean = mean(DMI_bw),
                       DMI_sd = sd(DMI_bw)/(sqrt(.N)),
                       CPI_mean = mean(CPI_bw), 
                       CPI_sd = sd(CPI_bw)/(sqrt(.N)),
                       GEI_mean = mean(GEI_bw), 
                       GEI_sd = sd(GEI_bw)/(sqrt(.N)),
                       DPI_mean = mean(DPI), 
                       DPI_sd = sd(DPI)/(sqrt(.N)),
                       DEI_mean = mean(DEI), 
                       DEI_sd = sd(DEI)/(sqrt(.N))
                       ),
                   
                   by = Diet]

#bar plot
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

#rail plot for crude/gross intakes
(SGrail <-
    ggplot()+
    geom_line(aes(y = CP_IR, x = GE_IR, group = Diet), data = rails)+
    geom_point(aes(x = mean(GEI_bw), y = mean(CPI_bw)), shape = 12, size = 3, data = sums) + 
    geom_point(aes(x = GEI_mean, y = CPI_mean), size = 2, shape = 1, data = Singlemeans)+
    geom_errorbar(aes(x = GEI_mean, y = CPI_mean, ymin = CPI_mean - CPI_sd, ymax = CPI_mean + CPI_sd), width = .5, data = Singlemeans)+
    geom_errorbar(aes(x = GEI_mean, y = CPI_mean, xmin = GEI_mean - GEI_sd, xmax = GEI_mean + GEI_sd), width = .5, data = Singlemeans)+
    geom_text(aes(x = max_GE + 50, y = max_CP, label = Diet), family = "serif", data = dietlabs)+
    ylab(expression(CP~intake~(gDM/kg^0.75/day)))+
    xlab(expression(GE~intake~(kJ/kg^0.75/day)))+
    ggtitle("No-choice", subtitle = "D")+
    themerails)

#rail plot for digestible intake rates
(SDrail <-
    ggplot()+
    geom_line(aes(y = DP_IR, x = DE_IR, group = Diet), data = rails)+
    geom_point(aes(x = mean(DEI_bw), y = mean(DPI_bw)), shape = 12, size = 3, data = sums) + 
    geom_point(aes(x = DEI_mean, y = DPI_mean), size = 2, shape = 1, data = Singlemeans)+
    geom_errorbar(aes(x = DEI_mean, y = DPI_mean, ymin = DPI_mean - DPI_sd, ymax = DPI_mean + DPI_sd), width = .5, data = Singlemeans)+
    geom_errorbar(aes(x = DEI_mean, y = DPI_mean, xmin = DEI_mean - DEI_sd, xmax = DEI_mean + DEI_sd), width = .5, data = Singlemeans)+
    geom_text(aes(x = max_DE + 50, y = max_DP, label = Diet), family = "serif", data = dietlabs)+
    ylab(expression(DP~intake~(gDM/kg^0.75/day)))+
    xlab(expression(DE~intake~(kJ/kg^0.75/day)))+
    ggtitle("No-choice", subtitle = "D")+
    themerails)


Intake <- ggarrange(Mbar, Sbar, 
                    MGrail, SGrail,
                    MDrail, SDrail, 
                    nrow = 3, ncol = 2)



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
    labs(y = "Apparent digestibility (%)", x = "", title = "A) Dry matter")+
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
    labs(y = "Apparent digestibility (%)", x = "Diet", title = "B) Protein")+
    themerails+
    theme(strip.background = element_blank()))

dietdigest <- ggarrange(DMDplot, DPplot, ncol = 1, nrow = 2)


#save plots
ggsave("Output/figures/intakebarandrail.jpeg", Intake, width = 7, height = 10, unit = "in")
ggsave("Output/figures/weightchangebar.jpeg", WeightChange, width = 4, height = 4, unit = "in")
ggsave("Output/figures/dietdigestion.jpeg", dietdigest, width = 5, height = 8 )
