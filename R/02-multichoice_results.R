#script to plot results from naiive multi-choice trials

library(ggplot2)
library(data.table)


#read in feeding trials results
MC <- fread("Input/Multichoice_results.csv")

MC[, Consumed_weight := Consumed/(Start_weight/1000)]

#read in the nutritional compositions of each diet
diets <- fread("Input/Diet_nutrient_compositions.csv")

#merge feeding results with diet compositions by diet
MCdiets <- merge(MC, diets, by = "Diet", all.x = TRUE)

#calculate the intake of protein by diet 
MCdiets[, Consumed_CP := Consumed_weight*(Protein/100)]
#calculate the intake of fibre by diet
MCdiets[, Consumed_NDF := Consumed_weight*(NDF/100)]


#calculate total protein and fibre consumed from all diets in one day
MCtotals <- MCdiets[, .(sum(Consumed_CP), sum(Consumed_NDF)), by = ID]
names(MCtotals) <- c("ID", "CP", "NDF")
#what is the mean target intake?
MCtotals[, .(mean(CP), mean(NDF))]

#read in data for diet nutritional rails
rails <- fread("Output/dietrails.rds")

#get custom ggplot themes
source("R/ggplot_themes.R")

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
saveRDS(MCtotals, "Output/multichoicemeans.rds")

