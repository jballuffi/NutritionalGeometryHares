#script to plot results from naiive multi-choice trials

library(ggplot2)
library(data.table)


#read in feeding trials results
MC <- fread("Input/Multichoice_results.csv")
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


#create theme for ggplots
themerails<-theme(axis.title = element_text(size=13),
                  axis.text = element_text(size=10),
                  axis.line.x.top = element_blank(),
                  axis.line.y.right = element_blank(),
                  axis.line.x.bottom = element_line(size=.5),
                  axis.line.y.left = element_line(size=.5),
                  legend.key = element_blank(),
                  legend.text = element_text(size=13),
                  panel.background = element_blank())


rails <- fread("Output/dietrails.rds")

ggplot()+
  geom_line(aes(x = F1I, y = P1I), color = "black", data = rails)+
  geom_line(aes(x = F2I, y = P2I), color = "black", data = rails)+
  geom_line(aes(x = F3I, y = P3I), color = "black", data = rails)+
  geom_line(aes(x = F4I, y = P4I), color = "black", data = rails)+
  geom_point(aes(x = NDF, y = CP, color = ID), size = 2, data = MCtotals)+
  labs(x="Fibre intake (g/kg/day)", y="Protein intake (g/kg/day)")+
  themerails

ggplot(MCdiets)+
  geom_boxplot(aes(x = Diet, y = Consumed_weight))+
  themerails
  


##good but 
##control for body mass
##add in a point that represents equal feeding 