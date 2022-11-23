
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


# read in data ------------------------------------------------------------


#read in data for diet nutritional rails
rails <- fread("Output/data/dietrails.rds")

#read in multichoice trial sums
sums <- readRDS("Output/data/multichoicesums.rds")

#read in full multichoice results
MC <- readRDS("Output/data/multichoiceresults.rds")

#read in daily feeding results
day<- readRDS("Output/data/dailyresultscleaned.rds")



# plot a bar graph and rail plot for intake rates  ----------------------------------------


#calculate mean intakes
Intakemeans <- MC[, .(mean(DMI_bw), sd(DMI_bw)), by = Diet]
names(Intakemeans) <-  c("Diet", "Intake_mean", "Intake_SD")

#bar graph
(NaiveIntakes<-
    ggplot(Intakemeans)+
    geom_bar(aes(y = Intake_mean, x = Diet), width = .75, stat = "identity", fill = "grey70")+
    geom_errorbar(aes(x = Diet, ymax = Intake_mean + Intake_SD, ymin = Intake_mean - Intake_SD), width = .2, color = "grey30")+
    labs(y = "Dry matter intake (g DM/kg^0.75/day)", x = "Diet")+
    themerails+
    theme(axis.ticks.x = element_blank()))

#rail plot for target intake according to naiive multi choice trials
(target <-
    ggplot(rails)+
    geom_line(aes(y = CP_IR, x = NDF_IR, group = Diet))+
    geom_point(aes(x = DMI_NDF_bw, y = DMI_CP_bw), size = 2, data = sums)+
    geom_point(aes(x = mean(DMI_NDF_bw), y = mean(DMI_CP_bw)), shape = 12, size = 3, data = sums)+
    labs(y = "CP Intake (g DM/kg^0.75/day)", x = "NDF Intake (g DM/kg^0.75/day)")+
    themerails)

(naivechoice <- ggarrange(NaiveIntakes, target, nrow = 2, ncol = 1))




# Remove the food strikes; NS = no strike ------------------------------------------------


foodstrikers <- sums[DMI_bw < 30, return(ID)]

sumsNS <- sums[!ID %in% foodstrikers]
MCNS <- MC[!ID %in% foodstrikers]


#calculate mean intakes
IntakemeansNS <- MCNS[, .(mean(DMI_bw), sd(DMI_bw)), by = Diet]
names(IntakemeansNS) <-  c("Diet", "Intake_mean", "Intake_SD")

#bar graph
(NaiveIntakesNS<-
    ggplot(NSIntakemeans)+
    geom_bar(aes(y = Intake_mean, x = Diet), width = .75, stat = "identity", fill = "grey70")+
    geom_errorbar(aes(x = Diet, ymax = Intake_mean + Intake_SD, ymin = Intake_mean - Intake_SD), width = .2, color = "grey30")+
    labs(y = "Dry matter intake (g DM/kg^0.75/day)", x = "Diet")+
    themerails+
    theme(axis.ticks.x = element_blank()))

#rail plot for target intake according to naiive multi choice trials
(targetNS <-
    ggplot(rails)+
    geom_line(aes(y = CP_IR, x = NDF_IR, group = Diet))+
    geom_point(aes(x = DMI_NDF_bw, y = DMI_CP_bw), size = 2, data = sumsNS)+
    geom_point(aes(x = mean(DMI_NDF_bw), y = mean(DMI_CP_bw)), shape = 12, size = 3, data = sumsNS)+
    labs(y = "CP Intake (g DM/kg^0.75/day)", x = "NDF Intake (g DM/kg^0.75/day)")+
    themerails)

(naivechoiceNS <- ggarrange(NaiveIntakes, target, nrow = 2, ncol = 1))





# save both figures -------------------------------------------------------

#including food strikers
ggsave("Output/figures/targetintake.jpeg", naivechoice, width = 4, height = 7, units = "in")

#excluding food strikers 
ggsave("Output/figures/targetintakeNS.jpeg", naivechoiceNS, width = 4, height = 7, units = "in")
