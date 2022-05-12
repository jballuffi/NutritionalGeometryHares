#Script to plot results from single choice feeding trials

library(ggplot2)
library(data.table)
library(lubridate)




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

#exploratory plots with temp data 
ggplot(SCdiets)+
  geom_point(aes(x = Temp, y = Weight_change, color = Diet))+
  theme_minimal()

ggplot(SCdiets)+
  geom_point(aes(x = Temp, y = Consumed, color = Diet))+
  theme_minimal()


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
