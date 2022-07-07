
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



# bar graph of intake rate by diet ----------------------------------------


#calculate mean intakes and weight change
Intakemeans <- MC[, .(mean(Intake_bw), sd(Intake_bw)), by = Diet]
names(Intakemeans) <-  c("Diet", "Intake_mean", "Intake_SD")


(NaiveIntakes<-
    ggplot(Intakemeans)+
    geom_bar(aes(y = Intake_mean, x = Diet), width = .75, stat = "identity", fill = "grey70")+
    geom_errorbar(aes(x = Diet, ymax = Intake_mean + Intake_SD, ymin = Intake_mean - Intake_SD), width = .2, color = "grey30")+
    labs(y = "Total Consumption (g DM/kg/day)", x = "Diet")+
    themerails+
    theme(axis.ticks.x = element_blank()))



# target intake with only naive hares -------------------------------------


#target intake according to naiive multi choice trials
(target <-
    ggplot(rails)+
    geom_line(aes(y = CP_IR, x = NDF_IR, group = Diet))+
    geom_point(aes(x = NDF, y = CP), size = 2, data = sums)+
    geom_point(aes(x = mean(NDF), y = mean(CP)), shape = 12, size = 3, data = sums)+
    labs(y = "CP Intake (g DM/day)", x = "NDF Intake (g DM/day)")+
    themerails)



(naivechoice <- ggarrange(NaiveIntakes, target, nrow = 2, ncol = 1))

# target intake with compromise -------------------------------------------


#calculate mean intake rates by diet
meanday <- day[, .(mean(CP_in_bw), sd(CP_in_bw), mean(NDF_in_bw), sd(CP_in_bw)), Diet]
names(meanday) <- c("Diet", "CP", "CPsd", "NDF", "NDFsd")

#investigating rule of compromise
(compromise <-
ggplot(rails)+
  geom_line(aes(y = CP_IR, x = NDF_IR, group = Diet))+
  geom_point(aes(x = mean(NDF), y = mean(CP)), shape = 12, size = 3, data = sums)+
  geom_point(aes(x = NDF, y = CP), size = 3, data = meanday)+
  geom_errorbar(aes(x = NDF, y = CP, ymin = CP - CPsd, ymax = CP + CPsd), width = .5, data = meanday)+
  geom_errorbar(aes(x = NDF, y = CP,xmin = NDF - NDFsd, xmax = NDF + NDFsd), width = .5, data = meanday)+
  labs(y = "CP Intake (g DM/day)", x = "NDF Intake (g DM/day)")+
  themerails)




ggsave("Output/figures/targetintake.jpeg", naivechoice, width = 4, height = 7, units = "in")
ggsave("Output/figures/compromiseintake.jpeg", compromise, width = 4, height = 3, units = "in")
