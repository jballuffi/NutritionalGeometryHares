#exploring different diet formulations for feeding trials

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


#read in formulated diet compositions
diets <- fread("Input/Diet_compositions.csv")

#read in winter plant compositions
wp <- fread("Input/Plants_winter2021_compositions_cleaned.csv")



# create diet rails -------------------------------------------------------

#create list of intake rates and nutritional values for each diet
dietintakes<- list(

#diet A
  data.table(IR = seq(1, 120, by = 1), 
                CP = diets[Diet == "A", return(CP_DM_pred)]/100,
                NDF = diets[Diet == "A", return(NDF_DM_pred)]/100,
                Diet = "A"),
#diet B
  data.table(IR = seq(1, 120, by = 1), 
                CP = diets[Diet == "B", return(CP_DM_pred)]/100,
                NDF = diets[Diet == "B", return(NDF_DM_pred)]/100,
                Diet = "B"),
#diet c
  data.table(IR = seq(1, 120, by = 1), 
                CP = diets[Diet == "C", return(CP_DM_pred)]/100,
                NDF = diets[Diet == "C", return(NDF_DM_pred)]/100,
                Diet = "C"),
#diet D
  data.table(IR = seq(1, 120, by = 1), 
                CP = diets[Diet == "D", return(CP_DM_pred)]/100,
                NDF = diets[Diet == "D", return(NDF_DM_pred)]/100,
                Diet = "D")
)

#combine all diet intake rates into one table
dietrails <- rbindlist(dietintakes)

#calculate the intake rates of CP and NDF
dietrails[, c("CP_IR", "NDF_IR") := .(IR*CP, IR*NDF)]

#create columnn indicating that this is a diet
dietrails[, Type := "Diet"]




# forage rails ------------------------------------------------------------

#collect avg nutritional compositions by species
wp_means <- wp[, .((mean(CP_DM))/100,(mean(NDF_DM))/100), by = Species]
names(wp_means) <- c("Species", "CP", "NDF")

#create list of intake rates and nutritional values for each species
forageintakes <- list(
#BEGL
data.table(IR = seq(1, 120, by = 1), 
           CP = wp_means[Species == "Betula glandulosa", return(CP)],
           NDF = wp_means[Species == "Betula glandulosa", return(NDF)],
           Diet = "BEGL"),
#PIGL
data.table(IR = seq(1, 120, by = 1), 
           CP = wp_means[Species == "Picea glauca", return(CP)],
           NDF = wp_means[Species == "Picea glauca", return(NDF)],
           Diet = "PIGL"),
#SASP
data.table(IR = seq(1, 120, by = 1), 
           CP = wp_means[Species == "Salix spp", return(CP)],
           NDF = wp_means[Species == "Salix spp", return(NDF)],
           Diet = "SASP")

)

#combine all intake rates of browse species
foragerails <- rbindlist(forageintakes)

#calculate the intake rates of CP and NDF
foragerails[, c("CP_IR", "NDF_IR") := .(IR*CP, IR*NDF)]

#create column indicating that this is values from natural browse
foragerails[, Type := "Forage"]




# collect all intakes and plot rails --------------------------------------

#rbindlist the diet and forage rails in one
allrails <- rbind(foragerails, dietrails)

#plot just the diet rails
(dietrailplot <-
  ggplot(dietrails)+
  geom_line(aes(y = CP_IR, x = NDF_IR, group = Diet))+
  labs(y = "CP Intake (g DM/day)", x = "NDF Intake (g DM/day)")+
  themerails)

#plot diet and forage rails
(foragerailplot <- 
  ggplot(allrails)+
  geom_line(aes(y = CP_IR, x = NDF_IR, group = Diet, linetype = Type))+
  labs(y = "CP Intake (g DM/day)", x = "NDF Intake (g DM/day)")+
  themerails)
#how do I add labels to these lines???


ggsave("Output/dietrailswithforage.jpeg", foragerailplot, width = 4.5, height = 3, unit = "in")
ggsave("Output/dietrails.jpeg", dietrailplot, width = 4, height = 3, unit = "in")

fwrite(allrails, "Output/data/dietrailsandforage.rds")
fwrite(dietrails, "Output/data/dietrails.rds")

