#script to plot results from naiive multi-choice trials


#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


# Read in data ------------------------------------------------------------


#read in habituation feeding trials results
MC <- fread("Input/Results_multichoice.csv")

#read in habituation dry matters
DM <- fread("Input/Habituation_DryMatter.csv")

#read in the nutritional compositions of each diet
diets <- fread("Input/Diet_compositions.csv")

#read in data for diet nutritional rails
rails <- fread("Output/data/dietrails.rds")



# merge data together -----------------------------------------------------

#cut diet compositions to just be DM
dietDM <- diets[, .(Diet, CP_DM_pred/100, NDF_DM_pred/100, ADF_DM_pred/100, ADL_DM_pred/100, C_DM/100)]
names(dietDM) <- c("Diet", "CP_diet", "NDF_diet", "ADF_diet", "ADL_diet", "C_diet") #C isnt predicted it was measured after (not for paper)

#subset DM data
DM <- DM[, .(Sample, DM)]

#split sample col into date and diet
DM[, Diet := tstrsplit(Sample, " ", keep = 3)]
DM[, Date := tstrsplit(Sample, " ", keep = 2)]
DM[, Date := ymd(Date)]
DM[, Enclosure := tstrsplit(Sample, " ", keep = 1)]

#set date in results
MC[, Date := ymd(End_date)]

#merge results with DM
DT <- merge(MC, DM, by = c("Enclosure", "Date", "Diet"), all.x = TRUE)

#merge results with diet compositions
DT <- merge(DT, dietDM, by = "Diet", all.x = TRUE)



# Calculations ------------------------------------------------------------

DT[, Offer_DM := ]

#calculate consumption rates on a per kg basis
MC[,  := Consumed/(Start_weight/1000)]

#merge feeding results with diet compositions by diet
MCdiets <- merge(MC, diets, by = "Diet", all.x = TRUE)

#calculate the intake of protein by diet 
MCdiets[, Consumed_CP := Consumed_weight*(Protein/100)]
#calculate the intake of fibre by diet
MCdiets[, Consumed_NDF := Consumed_weight*(NDF/100)]


#calculate total protein and fibre consumed from all diets in one day
MCtotals <- MCdiets[, .(sum(Consumed_CP), sum(Consumed_NDF)), by = ID]
names(MCtotals) <- c("ID", "CP", "NDF")
#what is the mean target intake?
MCtotals[, .(mean(CP), mean(NDF))]


(feedingchoice<-ggplot()+
  geom_line(aes(x = F1I, y = P1I), color = "black", data = rails)+
  geom_line(aes(x = F2I, y = P2I), color = "black", data = rails)+
  geom_line(aes(x = F3I, y = P3I), color = "black", data = rails)+
  geom_line(aes(x = F4I, y = P4I), color = "black", data = rails)+
  geom_point(aes(x = NDF, y = CP), size = 2, data = MCtotals)+
  geom_point(aes(x = mean(NDF), y = mean(CP)), shape = 12, size = 3, data = MCtotals)+
  labs(x="Fibre intake (g/kg/day)", y="Protein intake (g/kg/day)")+
  themerails)


ggsave("Output/multichoicerails.jpeg", feedingchoice, width = 4, height = 3, units = "in")
saveRDS(MCtotals, "Output/multichoicemeans.rds")

