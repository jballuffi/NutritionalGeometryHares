
#script for rail plots

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in data for diet nutritional rails
rails <- fread("Output/dietrails.rds")

#read in single choice results
single <- readRDS("Output/trialresultscleaned.rds")

#plot showing individual responses
(feedingratesrails<-
    ggplot()+
    geom_line(aes(x = NDF_IR, y = CP_IR, group = Diet), color = "black", data = rails)+
    geom_point(aes(x = Consumed_NDF, y = Consumed_CP, color = Weight_change), size = 4, alpha = .7, data = SCdiets)+
    scale_color_gradient(low = "yellow2", high = "Blue3", name = "Weight change (%/day)")+
    geom_point(aes(x = mean(NDF), mean(CP)), size = 3, shape = 10, data = multi)+
    labs(x="Fibre intake (g/kg/day)", y="Protein intake (g/kg/day)")+
    themerails)






ggsave("Output/singlechoicerails.jpeg", feedingratesrails, width = 5, height = 3, unit = "in")
