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
MCpval <- round(aMC$`Pr(>F)`[1], 2)



# intake rate for normal feeding trials -----------------------------


#Intake rate by day
IR <- lm(day$Intake_bw ~ day$Diet) #make model
aIR <- anova(IR) #take ANOVA table from linear regression
IRpval <- round(aIR$`Pr(>F)`[1], 5) #pull out pvalue from ANOVA
#tukey test on ANOVA
aovIR <- aov(IR)
posthocIR <- TukeyHSD(x = aovIR, 'day$Diet', conf.level = 0.95)
posthocIR


# weight change for feeding trials ----------------------------------------


#Weight change by trial 
WC <- lm(trials$Weight_change ~ trials$Diet)
aWC <- anova(WC)
WCpval <- round(aWC$`Pr(>F)`[1], 2)
aovWC <- aov(WC)
posthocWC <- TukeyHSD(x = aovWC, 'trials$Diet', conf.level = 0.95)
posthocWC


# CP digestion for feeding trials -----------------------------------------


#CP digestion by day
CPdig <- lm(day$CP_dig ~ day$Diet)
summary(CPdig)
anova(CPdig)
aCP <- aov(CPdig)
posthocCP <- TukeyHSD(x = aCP, 'day$Diet', conf.level = 0.95)
posthocCP


# NDF digestion for feeding trials ----------------------------------------


#NDF digestion by day
NDFdig <- lm(day$NDF_dig ~ day$Diet)
summary(NDFdig)
anova(NDFdig)
aNDF <- aov(NDFdig)
posthocNDF <- TukeyHSD(x = aNDF, 'day$Diet', conf.level = 0.95)
posthocNDF


# ADF digestion for feeding trials ----------------------------------------


#ADF digestion by day
ADFdig <- lm(day$ADF_dig ~ day$Diet)
summary(ADFdig)
anova(ADFdig)
aADF <- aov(ADFdig)
posthocADF <- TukeyHSD(x = aADF, 'day$Diet', conf.level = 0.95)
posthocADF

