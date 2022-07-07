#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in cleaned multi-choice results
MC <- readRDS("Output/data/multichoiceresults.rds")

#read in multichoice trial sums
sums <- readRDS("Output/data/multichoicesums.rds")

#mean intake rates
meanNI <- round(mean(sums$Intake), digits = 2)
sdNI <- round(sd(sums$Intake), digits = 2)


#mean intake rates of CP and NDF during naive trials
meanNCP <- round(mean(sums$CP), digits = 2)
sdNCP <- round(sd(sums$CP), digits = 2)
meanNNDF <- round(mean(sums$NDF), digits = 2)
sdNNDF <- round(sd(sums$NDF), digits = 2)

#collect mean intake rates by diet for naive hares
NIdient <- MC[, mean(Intake_bw), Diet]


#ANOVA testing for significance between treatments
lmMC <- lm(MC$Intake_bw ~ MC$Diet)
aMC <- anova(lmMC)
aMC$`Pr(>F)`[1]


