#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in cleaned multi-choice results
MC <- readRDS("Output/data/multichoiceresults.rds")

#read in multichoice trial sums
sums <- readRDS("Output/data/multichoicesums.rds")

#mean intake rate across all diets
meanNI <- round(mean(sums$Intake), digits = 2)
sdNI <- round(sd(sums$Intake), digits = 2)

# #mean intake rates by diet
# NIA <- MC[Diet == "A", round(mean(Intake_bw), digits = 2)]
# NIB <- MC[Diet == "B", round(mean(Intake_bw), digits = 2)]
# NIC <- MC[Diet == "C", round(mean(Intake_bw), digits = 2)]
# NID <- MC[Diet == "D", round(mean(Intake_bw), digits = 2)]

#mean intake rates of CP and NDF when summing all diets
meanNCP <- round(mean(sums$CP), digits = 2)
sdNCP <- round(sd(sums$CP), digits = 2)
meanNNDF <- round(mean(sums$NDF), digits = 2)
sdNNDF <- round(sd(sums$NDF), digits = 2)


#ANOVA testing for significance between treatments
lmMC <- lm(MC$Intake_bw ~ MC$Diet)
aMC <- anova(lmMC)
#setnames(aMC, "Pr(>F)", "pval", skip_absent=TRUE)
aMCpval <- round(aMC$`Pr(>F)`[1], 2)


