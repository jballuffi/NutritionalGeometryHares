
#script for rail plots

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in data for diet nutritional rails
rails <- fread("Output/data/dietrails.rds")

#read in single choice results
trials <- readRDS("Output/data/trialresultscleaned.rds")
days <- readRDS("Output/data/dailyresultscleaned.rds")



# wieght change in response to nutrient intake ----------------------------


(proteinintake <-
   ggplot(trials)+
   geom_point(aes(x = CP_in_bw, y = Weight_change), size = 1.75, color = "grey20")+
   labs(y = "Weight change (%/Day)", x = "Protein intake (g DM/kg/day)")+
   themerails)

(NDFintake <-
    ggplot(trials)+
    geom_point(aes(x = NDF_in_bw, y = Weight_change), size = 1.75, color = "grey20")+
    labs(y = "Weight change (%/Day)", x = "NDF intake (g DM/kg/day)")+
    themerails)



ggsave("Output/figures/proteinintake.jpeg", proteinintake, width = 5, height = 3, unit = "in")
ggsave("Output/figures/NDFintake.jpeg", NDFintake, width = 5, height = 3, unit = "in")
