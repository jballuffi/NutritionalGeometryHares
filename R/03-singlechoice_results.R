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
SC[, TotalConsumption := D1 + D2 + D3] #total consumption for all days
SC[, MeanConsumption := ((D1 + D2 + D3)/3)] #average consumption
SC[, TotalConsumption_weight := TotalConsumption/(Weight_start/1000)] #consumption by kg bodyweight
SC[, Weight_change := (Weight_end - Weight_start)/Weight_start]

#read in the nutritional compositions of each diet
diets <- fread("Input/Diet_nutrient_compositions.csv")

#merge feeding results with diet compositions by diet
SCdiets <- merge(SC, diets, by = "Diet", all.x = TRUE)

