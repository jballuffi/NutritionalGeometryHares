
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in data for diet nutritional rails
rails <- fread("Output/data/dietrails.rds")

#read in multichoice trial sums
sums <- readRDS("Output/data/multichoicesums.rds")

#read in daily feeding results
day<- readRDS("Output/data/dailyresultscleaned.rds")

#calculate mean intake rates by diet
meanday <- day[, .(mean(CP_in_bw), sd(CP_in_bw), mean(NDF_in_bw), sd(CP_in_bw)), Diet]
names(meanday) <- c("Diet", "CP", "CPsd", "NDF", "NDFsd")


#target intake according to naiive multi choice trials
(target <-
    ggplot(rails)+
    geom_line(aes(y = CP_IR, x = NDF_IR, group = Diet))+
    geom_point(aes(x = NDF, y = CP), size = 2, data = sums)+
    geom_point(aes(x = mean(NDF), y = mean(CP)), shape = 12, size = 3, data = sums)+
    labs(y = "CP Intake (g DM/day)", x = "NDF Intake (g DM/day)")+
    themerails)

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




ggsave("Output/figures/targetintake.jpeg", target, width = 4, height = 3, units = "in")
ggsave("Output/figures/compromiseintake.jpeg", compromise, width = 4, height = 3, units = "in")
