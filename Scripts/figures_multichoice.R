
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in data for diet nutritional rails
rails <- fread("Output/data/dietrails.rds")

#read in multichoice trial sums
sums <- readRDS("Output/data/multichoicesums.rds")


(feedingchoice <-
    ggplot(rails)+
    geom_line(aes(y = CP_IR, x = NDF_IR, group = Diet))+
    geom_point(aes(x = NDF, y = CP), size = 2, data = sums)+
    geom_point(aes(x = mean(NDF), y = mean(CP)), shape = 12, size = 3, data = sums)+
    labs(y = "CP Intake (g DM/day)", x = "NDF Intake (g DM/day)")+
    themerails)


ggsave("Output/figures/multichoicerails.jpeg", feedingchoice, width = 4, height = 3, units = "in")
