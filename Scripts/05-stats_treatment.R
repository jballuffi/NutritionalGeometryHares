#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)



# read in cleaned results -------------------------------------------------


#read in cleaned multi-choice results
MC <- readRDS("Output/data/multichoiceresults.rds") #all results by diet
sums <- readRDS("Output/data/multichoicesums.rds") #sums of nutrient intakes per trial

#read in cleaned feeding trial results
trials <- readRDS("Output/data/trialresultscleaned.rds") #by trial
day<- readRDS("Output/data/dailyresultscleaned.rds") # by day




# stats for multi-choice trials ----------------------------------------------------

# #mean intake rates by diet
# NIA <- MC[Diet == "A", round(mean(Intake_bw), digits = 2)]
# NIB <- MC[Diet == "B", round(mean(Intake_bw), digits = 2)]
# NIC <- MC[Diet == "C", round(mean(Intake_bw), digits = 2)]
# NID <- MC[Diet == "D", round(mean(Intake_bw), digits = 2)]


#ANOVA testing for significance between treatments
lmMC <- lm(MC$Intake_bw ~ MC$Diet)
aMC <- anova(lmMC)
aMCpval <- round(aMC$`Pr(>F)`[1], 2)



# stats for feeding trials; treatment analysis -----------------------------


#Intake rate by day
intake <- lm(day$Intake_bw ~ day$Diet)
summary(intake)
anova(intake)
aI <- aov(intake)
posthocI <- TukeyHSD(x = aI, 'day$Diet', conf.level = 0.95)
posthocI


#Weight change by trial 
weight <- lm(trials$Weight_change ~ trials$Diet)
summary(weight)
anova(weight)
aW <- aov(weight)
posthocW <- TukeyHSD(x = aW, 'trials$Diet', conf.level = 0.95)
posthocW


#CP digestion by day
CPdig <- lm(day$CP_dig ~ day$Diet)
summary(CPdig)
anova(CPdig)
aCP <- aov(CPdig)
posthocCP <- TukeyHSD(x = aCP, 'day$Diet', conf.level = 0.95)
posthocCP


#NDF digestion by day
NDFdig <- lm(day$NDF_dig ~ day$Diet)
summary(NDFdig)
anova(NDFdig)
aNDF <- aov(NDFdig)
posthocNDF <- TukeyHSD(x = aNDF, 'day$Diet', conf.level = 0.95)
posthocNDF


#ADF digestion by day
ADFdig <- lm(day$ADF_dig ~ day$Diet)
summary(ADFdig)
anova(ADFdig)
aADF <- aov(ADFdig)
posthocADF <- TukeyHSD(x = aADF, 'day$Diet', conf.level = 0.95)
posthocADF

