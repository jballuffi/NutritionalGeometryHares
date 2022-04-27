#Script to plot results from single choice feeding trials

library(ggplot2)
library(data.table)
library(lubridate)

#read in feeding trial data
SC <- fread("Input/Singlechoice_results.csv")
SC <- SC[!is.na(Trial)] #remove space holders

#read in the nutritional compositions of each diet
diets <- fread("Input/Diet_nutrient_compositions.csv")

#read in temp data
temp <- fread("Input/temperatures_SW_2022.csv")

#merge date and time into a datetime
temp[, DateTime := as_datetime(paste0(Date, " ", Time, " ", TimeStamp))]

#cut for only dates in which feeding trials occurred
temp <- temp[Date > '2022-02-08' & Date < '2022-03-15']

#plot that shows temp over time for the whole study period
ggplot(temp)+
  geom_line(aes(y = Temp, x = DateTime))+
  labs(x = "Date", y = "Temperature (C)")+
  theme_minimal()

#create a daily start and end time for feeding trials
SC[, Time_start := "10:00:00"][, Time_end := "10:00:00"]

SC[, DateTime_start := as_datetime(paste0(Date_start, " ", Time_start))]
SC[, DateTime_end := as_datetime(paste0(Date_end, " ", Time_end))]


#Calculate intake rates and weight loss
SC[, D1 := D1offer_wet - D1end_wet] #day 1 of consumption
SC[, D2 := D2offer_wet - D2end_wet] #day 2 of consumption
SC[, D3 := D3offer_wet - D3end_wet] #day 3 of consumption
SC[, Consumed := ((D1 + D2 + D3)/(Weight_start/1000))/3] #consumption by kg bodyweight
SC[, Weight_change := (((Weight_end - Weight_start)/Weight_start)*100)/3]



#merge feeding results with diet compositions by diet
SCdiets <- merge(SC, diets, by = "Diet", all.x = TRUE)

SCdiets <- SCdiets[!is.na(Consumed)]

#calculate the intake of protein (g/kg body mass/3 days) by diet 
SCdiets[, Consumed_CP := Consumed*(Protein/100)]
#calculate the intake of fibre (g/kg body mass/3 days) by diet
SCdiets[, Consumed_NDF := Consumed*(NDF/100)]




SCmeans <- SCdiets[, .(mean(Consumed), sd(Consumed), mean(Weight_change), sd(Weight_change)), by = Diet]
names(SCmeans) <- c("Diet", "Consumed_mean", "Consumed_SD", "Weight_mean", "Weight_SD")

Macromeans <- SCdiets[, .(mean(Consumed_CP), sd(Consumed_CP), mean(Consumed_NDF), sd(Consumed_NDF)), by = Diet]
names(Macromeans) <- c("Diet", "CP_mean", "CP_SD", "NDF_mean", "NDF_SD")

# Plots


source("R/ggplot_themes.R")


(ConsumptionRates<-
  ggplot(SCmeans)+
  geom_bar(aes(y = Consumed_mean, x = Diet), width = .5, stat = "identity", fill = "grey70")+
  geom_errorbar(aes(x = Diet, ymax = Consumed_mean + Consumed_SD, ymin = Consumed_mean - Consumed_SD), width = .2, color = "grey30")+
  labs(y = "Total Consumption (g/kg/day)")+
  themerails)


(WeightChange<-
  ggplot(SCdiets)+
  geom_boxplot(aes(x = Diet, y = Weight_change), outlier.shape = NA, width = .75)+
  geom_jitter(aes(x = Diet, y = Weight_change), shape = 1, size = 2, width = .3)+
  labs(y = "Weight change (%/Day)")+
  themerails)


(proteinintake <-
  ggplot(SCdiets)+
  geom_point(aes(x = Consumed_CP, y = Weight_change, color = Diet), size = 2)+
  labs(y = "Weight change (%/Day)", x = "Protein intake (g/kg/day)")+
  themerails)


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


#save plots
ggsave("Output/consumptionbarplot.jpeg", ConsumptionRates, width = 4, height = 3, unit = "in")
ggsave("Output/weightboxplot.jpeg", WeightChange, width = 4, height = 3, unit = "in")
ggsave("Output/singlechoicerails.jpeg", feedingratesrails, width = 5, height = 3, unit = "in")
ggsave("Output/proteinintake.jpeg", proteinintake, width = 5, height = 3, unit = "in")
ggsave("Output/meanintakerails.jpeg", meanintakerails, width = 5, height = 3, unit = "in")
