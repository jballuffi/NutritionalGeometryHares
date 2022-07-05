
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in data for diet nutritional rails
rails <- fread("Output/data/dietrails.rds")





(feedingchoice<-ggplot()+
  geom_line(aes(x = F1I, y = P1I), color = "black", data = rails)+
  geom_line(aes(x = F2I, y = P2I), color = "black", data = rails)+
  geom_line(aes(x = F3I, y = P3I), color = "black", data = rails)+
  geom_line(aes(x = F4I, y = P4I), color = "black", data = rails)+
  geom_point(aes(x = NDF, y = CP), size = 2, data = MCtotals)+
  geom_point(aes(x = mean(NDF), y = mean(CP)), shape = 12, size = 3, data = MCtotals)+
  labs(x="Fibre intake (g/kg/day)", y="Protein intake (g/kg/day)")+
  themerails)


ggsave("Output/multichoicerails.jpeg", feedingchoice, width = 4, height = 3, units = "in")
