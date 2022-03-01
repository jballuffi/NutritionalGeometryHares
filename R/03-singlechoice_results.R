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

#calculate the intake of protein (g/kg body mass/3 days) by diet 
SCdiets[, Consumed_CP := TotalConsumption_weight*(Protein/100)]
#calculate the intake of fibre (g/kg body mass/3 days) by diet
SCdiets[, Consumed_NDF := TotalConsumption_weight*(NDF/100)]


source("R/ggplot_themes.R")


ggplot(SCdiets)+
  geom_bar(aes(y = TotalConsumption_weight/3, x = Diet), width = .5, stat = "identity")+ #divide by three to get daily rate
  geom_errorbarh(aes(x = Diet, y = TotalConsumption_weight/3))+
  labs(y = "Total Consumption (g/kg/day)")+
  themerails

ggplot(SCdiets)+
  geom_boxplot(aes(x = Diet, y = Weight_change/3))+
  geom_jitter(aes(x = Diet, y = Weight_change/3))+
  labs(y = "Weight change (%/Day)")+
  themerails

