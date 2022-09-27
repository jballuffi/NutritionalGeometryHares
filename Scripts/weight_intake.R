#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)



# read in cleaned results -------------------------------------------------


#read in cleaned multi-choice results
MC <- readRDS("Output/data/multichoiceresults.rds") #all results by diet
sums <- readRDS("Output/data/multichoicesums.rds") #sums of nutrient intakes per trial

#read in cleaned feeding trial results
trials <- readRDS("Output/data/trialresultscleaned.rds") #by trial
day<- readRDS("Output/data/dailyresultscleaned.rds") # by day


ggplot(day)+
  geom_point(aes(x = log(Weight_start), y = log(Intake), colour = Diet))



ggplot(day)+
  geom_point(aes(x = (Weight_start^.75), y = Intake, colour = Diet))


