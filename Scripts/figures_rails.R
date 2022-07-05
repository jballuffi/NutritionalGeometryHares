
#script for rail plots

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in data for diet nutritional rails
rails <- fread("Output/dietrails.rds")

#read in single choice results
trials <- readRDS("Output/trialresultscleaned.rds")
days <- readRDS("Output/dailyresultscleaned.rds")



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



# using results by trial --------------------------------------------------


#plot showing weight change response
(feedingratesrails<-
    ggplot()+
    geom_line(aes(x = NDF_IR, y = CP_IR, group = Diet), color = "black", data = rails)+
    geom_point(aes(x = NDF_in_bw, y = CP_in_bw, color = Weight_change), size = 4, alpha = .7, data = trials)+
    scale_color_gradient(low = "yellow2", high = "Blue3", name = "Weight change (%/day)")+
    labs(x="NDF intake (g DM/kg/day)", y="CP intake (g DM/kg/day)")+
    themerails)



# using results by day ----------------------------------------------------


#protein digestibility
ggplot()+
  geom_line(aes(x = NDF_IR, y = CP_IR, group = Diet), color = "black", data = rails)+
  geom_point(aes(x = NDF_in_bw, y = CP_in_bw, color = CP_dig), size = 4, alpha = .7, data = day)+
  scale_color_gradient(low = "yellow2", high = "Blue3", name = "CP Digestability")+
  labs(x="NDF intake (g DM/kg/day)", y="CP intake (g DM/kg/day)")+
  themerails

#NDF digestibility
ggplot()+
  geom_line(aes(x = NDF_IR, y = CP_IR, group = Diet), color = "black", data = rails)+
  geom_point(aes(x = NDF_in_bw, y = CP_in_bw, color = NDF_dig), size = 4, alpha = .7, data = day)+
  scale_color_gradient(low = "yellow2", high = "Blue3", name = "NDF Digestability")+
  labs(x="NDF intake (g DM/kg/day)", y="CP intake (g DM/kg/day)")+
  themerails


ggsave("Output/figures/proteinintake.jpeg", proteinintake, width = 5, height = 3, unit = "in")
ggsave("Output/figures/NDFintake.jpeg", NDFintake, width = 5, height = 3, unit = "in")
ggsave("Output/figures/singlechoicerails.jpeg", feedingratesrails, width = 5, height = 3, unit = "in")
