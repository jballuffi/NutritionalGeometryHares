#exploring different diet formulations for feeding trials

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


#read in formulated diet compositions
diets <- fread("Input/Diet_compositions.csv")

#read in winter plant compositions
wp <- fread("Input/Plants_winter2021_compositions_cleaned.csv")

diets[Diet == "A", return(CP_DM_pred)]



PIGLf<- wp[Species == "Picea glauca", mean(NDF_DM)]
PIGLp<- wp[Species == "Picea glauca", mean(CP_DM)]

SASPf<- wp[Species == "Salix spp", mean(NDF_DM)]
SASPp<- wp[Species == "Salix spp", mean(CP_DM)]

BEGLf<- wp[Species == "Betula glandulosa", mean(NDF_DM)]
BEGLp<- wp[Species == "Betula glandulosa", mean(CP_DM)]



#create blank data frame with just intake rate

DA <- data.table(IR = seq(1, 120, by = 1), 
                 CP = diets[Diet == "A", return(CP_DM_pred)]/100,
                 NDF = diets[Diet == "A", return(NDF_DM_pred)]/100)

DA[, c("CP_IR", "NDF_IR") := .(IR*CP, IR*NDF)]

DB <- data.table(IR = seq(1, 120, by = 1), 
                 CP = diets[Diet == "B", return(CP_DM_pred)]/100,
                 NDF = diets[Diet == "B", return(NDF_DM_pred)]/100)

DB[, c("CP_IR", "NDF_IR") := .(IR*CP, IR*NDF)]

DC <- data.table(IR = seq(1, 120, by = 1), 
                 CP = diets[Diet == "C", return(CP_DM_pred)]/100,
                 NDF = diets[Diet == "C", return(NDF_DM_pred)]/100)

DC[, c("CP_IR", "NDF_IR") := .(IR*CP, IR*NDF)]

DD <- data.table(IR = seq(1, 120, by = 1), 
                 CP = diets[Diet == "D", return(CP_DM_pred)]/100,
                 NDF = diets[Diet == "D", return(NDF_DM_pred)]/100)

DD[, c("CP_IR", "NDF_IR") := .(IR*CP, IR*NDF)]




#save diet formulas at this stage
data2 <- data


#adding in values of natural browse/forage species

#protein compositions
data[, PIGLp := PIGLp/100]
data[, SASPp := SASPp/100]
data[, BEGLp := BEGLp/100]

#NDF compositions
data[, PIGLf := PIGLf/100]
data[, SASPf := SASPf/100]
data[, BEGLf := BEGLf/100]

#proteinintake
data[, PIGLpI := PIGLp*IR]
data[, SASPpI := SASPp*IR]
data[, BEGLpI := BEGLp*IR]

#NDF intake
data[, PIGLfI := PIGLf*IR]
data[, SASPfI := SASPf*IR]
data[, BEGLfI := BEGLf*IR]

(withforage <- ggplot(data)+
  geom_line(aes(x = F1I, y = P1I))+
  geom_line(aes(x = F2I, y = P2I))+
  geom_line(aes(x = F3I, y = P3I))+
  geom_line(aes(x = F4I, y = P4I))+
  geom_line(aes(x = PIGLfI, y = PIGLpI), linetype = 3)+
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

