
#script for rail plots

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in data for diet nutritional rails
rails <- fread("Output/dietrails.rds")

#read in single choice results
trials <- readRDS("Output/trialresultscleaned.rds")

#plot showing individual responses
(feedingratesrails<-
    ggplot()+
    geom_line(aes(x = NDF_IR, y = CP_IR, group = Diet), color = "black", data = rails)+
    geom_point(aes(x = NDF_in_bw, y = CP_in_bw, color = Weight_change), size = 4, alpha = .7, data = trials)+
    scale_color_gradient(low = "yellow2", high = "Blue3", name = "Weight change (%/day)")+
    labs(x="NDF intake (g DM/kg/day)", y="CP intake (g DM/kg/day)")+
    themerails)

ggplot()+
  geom_line(aes(x = NDF_IR, y = CP_IR, group = Diet), color = "black", data = rails)+
  geom_point(aes(x = NDF_in_bw, y = CP_in_bw, color = CP_dig), size = 4, alpha = .7, data = trials)+
  scale_color_gradient(low = "yellow2", high = "Blue3", name = "CP Digestability")+
  labs(x="NDF intake (g DM/kg/day)", y="CP intake (g DM/kg/day)")+
  themerails


ggplot()+
  geom_line(aes(x = NDF_IR, y = CP_IR, group = Diet), color = "black", data = rails)+
  geom_point(aes(x = NDF_in_bw, y = CP_in_bw, color = NDF_dig), size = 4, alpha = .7, data = trials)+
  scale_color_gradient(low = "yellow2", high = "Blue3", name = "NDF Digestability")+
  labs(x="NDF intake (g DM/kg/day)", y="CP intake (g DM/kg/day)")+
  themerails

ggsave("Output/singlechoicerails.jpeg", feedingratesrails, width = 5, height = 3, unit = "in")
