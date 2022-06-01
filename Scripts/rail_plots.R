
#script for rail plots

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in data for diet nutritional rails
rails <- fread("Output/dietrails.rds")

#read in multichoice results
multi <- readRDS("Output/multichoicemeans.rds")

#plot showing individual responses
(feedingratesrails<-
    ggplot()+
    geom_line(aes(x = F1I, y = P1I), color = "black", data = rails)+
    geom_line(aes(x = F2I, y = P2I), color = "black", data = rails)+
    geom_line(aes(x = F3I, y = P3I), color = "black", data = rails)+
    geom_line(aes(x = F4I, y = P4I), color = "black", data = rails)+
    geom_point(aes(x = Consumed_NDF, y = Consumed_CP, color = Weight_change), size = 4, alpha = .7, data = SCdiets)+
    scale_color_gradient(low = "yellow2", high = "Blue3", name = "Weight change (%/day)")+
    geom_point(aes(x = mean(NDF), mean(CP)), size = 3, shape = 10, data = multi)+
    labs(x="Fibre intake (g/kg/day)", y="Protein intake (g/kg/day)")+
    themerails)



#plot showing mean responses and multi-choice target intake
(meanintakerails<-
    ggplot()+
    geom_line(aes(x = F1I, y = P1I), color = "black", data = rails)+
    geom_line(aes(x = F2I, y = P2I), color = "black", data = rails)+
    geom_line(aes(x = F3I, y = P3I), color = "black", data = rails)+
    geom_line(aes(x = F4I, y = P4I), color = "black", data = rails)+
    geom_point(aes(x = NDF_mean, y = CP_mean), size = 3, data = Macromeans)+
    geom_point(aes(x = mean(NDF), mean(CP)), size = 3, shape = 10, data = multi)+
    labs(x="Fibre intake (g/kg/day)", y="Protein intake (g/kg/day)")+
    themerails)


ggsave("Output/singlechoicerails.jpeg", feedingratesrails, width = 5, height = 3, unit = "in")
ggsave("Output/meanintakerails.jpeg", meanintakerails, width = 5, height = 3, unit = "in")
