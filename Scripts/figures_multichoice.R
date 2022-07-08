
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


#calculate mean intakes and weight change
Intakemeans <- MC[, .(mean(Intake_bw), sd(Intake_bw)), by = Diet]
names(Intakemeans) <-  c("Diet", "Intake_mean", "Intake_SD")

#bar graph
(NaiveIntakes<-
    ggplot(Intakemeans)+
    geom_bar(aes(y = Intake_mean, x = Diet), width = .75, stat = "identity", fill = "grey70")+
    geom_errorbar(aes(x = Diet, ymax = Intake_mean + Intake_SD, ymin = Intake_mean - Intake_SD), width = .2, color = "grey30")+
    labs(y = "Total Consumption (g DM/kg/day)", x = "Diet")+
    themerails+
    theme(axis.ticks.x = element_blank()))



#rail plot for target intake according to naiive multi choice trials
(target <-
    ggplot(rails)+
    geom_line(aes(y = CP_IR, x = NDF_IR, group = Diet))+
    geom_point(aes(x = NDF, y = CP), size = 2, data = sums)+
    geom_point(aes(x = mean(NDF), y = mean(CP)), shape = 12, size = 3, data = sums)+
    labs(y = "CP Intake (g DM/day)", x = "NDF Intake (g DM/day)")+
    themerails)

(naivechoice <- ggarrange(NaiveIntakes, target, nrow = 2, ncol = 1))


ggsave("Output/figures/targetintake.jpeg", naivechoice, width = 4, height = 7, units = "in")
