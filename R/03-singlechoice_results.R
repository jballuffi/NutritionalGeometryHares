#Script to plot results from single choice feeding trials

library(ggplot2)
library(data.table)

#read in feeding trial data
SC <- fread("Input/Singlechoice_results.csv")
SC <- SC[!is.na(Trial)] #remove space holders

#Calculate intake rates and weight loss
SC[, D1 := D1offer_wet - D1end_wet] #day 1 of consumption
SC[, D2 := D2offer_wet - D2end_wet] #day 2 of consumption
SC[, D3 := D3offer_wet - D3end_wet] #day 3 of consumption
SC[, Consumed := ((D1 + D2 + D3)/(Weight_start/1000))/3] #consumption by kg bodyweight
SC[, Weight_change := ((Weight_end - Weight_start)/Weight_start)/3]

#read in the nutritional compositions of each diet
diets <- fread("Input/Diet_nutrient_compositions.csv")

#merge feeding results with diet compositions by diet
SCdiets <- merge(SC, diets, by = "Diet", all.x = TRUE)

SCdiets <- SCdiets[!is.na(Consumed)]

#calculate the intake of protein (g/kg body mass/3 days) by diet 
SCdiets[, Consumed_CP := Consumed*(Protein/100)]
#calculate the intake of fibre (g/kg body mass/3 days) by diet
SCdiets[, Consumed_NDF := Consumed*(NDF/100)]




SCmeans <- SCdiets[, .(mean(Consumed), sd(Consumed)), by = Diet]
names(SCmeans) <- c("Diet", "Mean", "SD")


# Plots


source("R/ggplot_themes.R")


(ConsumptionRates<-
  ggplot(SCmeans)+
  geom_bar(aes(y = Mean, x = Diet), width = .5, stat = "identity", fill = "grey70")+
  geom_errorbar(aes(x = Diet, ymax = Mean + SD, ymin = Mean - SD), width = .2, color = "grey30")+
  labs(y = "Total Consumption (g/kg/day)")+
  themerails)


(WeightChange<-
  ggplot(SCdiets)+
  geom_boxplot(aes(x = Diet, y = Weight_change), width = .75)+
  geom_jitter(aes(x = Diet, y = Weight_change), size = 2, shape = 1, width = .3)+
  labs(y = "Weight change (%/Day)")+
  themerails)


#read in data for diet nutritional rails
rails <- fread("Output/dietrails.rds")

(feedingratesrails<-
  ggplot()+
  geom_line(aes(x = F1I, y = P1I), color = "black", data = rails)+
  geom_line(aes(x = F2I, y = P2I), color = "black", data = rails)+
  geom_line(aes(x = F3I, y = P3I), color = "black", data = rails)+
  geom_line(aes(x = F4I, y = P4I), color = "black", data = rails)+
  geom_point(aes(x = Consumed_NDF, y = Consumed_CP, color = ID), size = 2.5, data = SCdiets)+
  labs(x="Fibre intake (g/kg/day)", y="Protein intake (g/kg/day)")+
  themerails)



#save plots
ggsave("Output/consumptionbarplot.jpeg", ConsumptionRates, width = 4, height = 3, unit = "in")
ggsave("Output/weightboxplot.jpeg", WeightChange, width = 4, height = 3, unit = "in")
ggsave("Output/singlechoicerails.jpeg", feedingratesrails, width = 5, height = 3, unit = "in")
