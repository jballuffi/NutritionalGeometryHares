# script to collect digestability results

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in daily feeding trial results
days <- readRDS("Output/dailyresultscleaned.rds")

days[, Weight_start := Weight_start/1000]




# Total intake and excretion ----------------------------------------------

ggplot(days)+
  geom_point(aes(x = Intake_bw, y = Total_out/Weight_start, color = Diet))+
  themerails

ggplot(days)+
  geom_boxplot(aes(x = Diet, y = Total_out/Weight_start))


# Nutrient tntake and excretion rates ----------------------------------------------


ggplot(days)+
  geom_point(aes(x = CP_in_bw, y = CP_out/Weight_start, color = Diet))+
  labs(x = "Protein Intake (g DM/day)", y = "Protein Excretion (g DM/day")+
  themerails

ggplot(days)+
  geom_point(aes(x = NDF_in, y = NDF_out, color = Diet))+
  labs(x = "NDF Intake (g DM/day)", y = "NDF Excretion (g DM/day")+
  themerails

ggplot(days)+
  geom_point(aes(x = ADF_in, ADF_out, color = Diet))+
  labs(x = "ADF Intake (g DM/day", y = "ADF Extretion (g DM/day)")+
  themerails




# Digestability by diet ---------------------------------------------------


#subset to just digestability columns
dig <- days[, .(Diet, CP_dig, NDF_dig, ADF_dig)]

#melt columns to have nutrient as a new variable
digmelt <- melt(dig, measure.vars = c("CP_dig", "NDF_dig", "ADF_dig"), 
                variable.name = "nutrient", 
                value.name = "digestability")

#remove the "_dig" from the nutrient values (for label in facet wrap)
digmelt[, nutrient := gsub("_dig", "", nutrient)]

ggplot(digmelt)+
  geom_boxplot(aes(x = Diet, y = digestability))+
  facet_wrap(~nutrient, nrow = 1, ncol = 3)+
  themerails

