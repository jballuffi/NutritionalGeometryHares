#exploring different diet formulations for feeding trials

library(data.table)
library(ggplot2)

wp<- fread("Input/plants_winter2021_composition_clean.csv")
wp[, CP := CP/100]
wp<- wp[!is.na(CP)]

PIGLAf<- wp[Species == "Picea gluaca adult", mean(NDF)]
PIGLAp<- wp[Species == "Picea gluaca adult", mean(CP)]

PIGLJf<- wp[Species == "Picea glauca juvenile", mean(NDF)]
PIGLJp<- wp[Species == "Picea glauca juvenile", mean(CP)]

SASPf<- wp[Species == "Salix", mean(NDF)]
SASPp<- wp[Species == "Salix", mean(CP)]

BEGLf<- wp[Species == "Betula glandulosa", mean(NDF)]
BEGLp<- wp[Species == "Betula glandulosa", mean(CP)]


themerails<-theme(axis.title = element_text(size=13),
                  axis.text = element_text(size=10),
                  axis.line.x.top = element_blank(),
                  axis.line.y.right = element_blank(),
                  axis.line.x.bottom = element_line(size=.5),
                  axis.line.y.left = element_line(size=.5),
                  legend.key = element_blank(),
                  legend.text = element_text(size=13),
                  panel.background = element_blank())

#create blank data frame with just intake rate
data<- data.table(IR = seq(1,120, by = 1))

#add in protein values (%)
data[, P1 := .05]
data[, P2 := .0833]
data[, P3 := 0.117]
data[, P4 := 0.15]

#add in NDF values (%)
data[, F1 := .60241]
data[, F2 := .365] 
data[, F3 := .321] 
data[, F4 := .3]

#calculate protein intake rates
data[, P1I := P1*IR]
data[, P2I := P2*IR]
data[, P3I := P3*IR]
data[, P4I := P4*IR]


#calculate NDF intake rates
data[, F1I := F1*IR]
data[, F2I := F2*IR]
data[, F3I := F3*IR]
data[, F4I := F4*IR]


#save diet formulas at this stage
data2 <- data


#adding in values of natural browse/forage species

#protein compositions
data[, PIGLAp := PIGLAp]
data[, PIGLJp := PIGLJp]
data[, SASPp := SASPp]
data[, BEGLp := BEGLp]

#NDF compositions
data[, PIGLAf := PIGLAf]
data[, PIGLJf := PIGLJf]
data[, SASPf := SASPf]
data[, BEGLf := BEGLf]

#proteinintake
data[, PIGLApI := PIGLAp*IR]
data[, PIGLJpI := PIGLJp*IR]
data[, SASPpI := SASPp*IR]
data[, BEGLpI := BEGLp*IR]

#NDF intake
data[, PIGLAfI := PIGLAf*IR]
data[, PIGLJfI := PIGLJf*IR]
data[, SASPfI := SASPf*IR]
data[, BEGLfI := BEGLf*IR]

(withforage <- ggplot(data)+
  geom_line(aes(x = F1I, y = P1I))+
  geom_line(aes(x = F2I, y = P2I))+
  geom_line(aes(x = F3I, y = P3I))+
  geom_line(aes(x = F4I, y = P4I))+
  geom_line(aes(x = PIGLAfI, y = PIGLApI), linetype = 3)+
  geom_line(aes(x = PIGLJfI, y = PIGLJpI), linetype = 3)+
  geom_line(aes(x = SASPfI, y = SASPpI), linetype = 3)+
  geom_line(aes(x = BEGLfI, y = BEGLpI), linetype = 3)+
  labs(x="Fibre intake (g/day)", y="Protein intake (g/day)")+
  themerails)

(withoutforage <- ggplot(data2)+
    geom_line(aes(x = F1I, y = P1I))+
    geom_line(aes(x = F2I, y = P2I))+
    geom_line(aes(x = F3I, y = P3I))+
    geom_line(aes(x = F4I, y = P4I))+
    labs(x="Fibre intake (g/day)", y="Protein intake (g/day)")+
    themerails)

ggsave("Output/dietrailswithforage.jpeg", withforage, width = 4, height = 3, unit = "in")
ggsave("Output/dietrailswithoutforage.jpeg", withoutforage, width = 4, height = 3, unit = "in")

fwrite(data, "Output/dietrailsandforage.rds")
fwrite(data2, "Output/dietrails.rds")

