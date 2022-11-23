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

#ANOVA testing for significance between treatments
lmMC <- lm(MC$DMI_bw ~ MC$Diet)
aMC <- anova(lmMC)
MCpval <- round(aMC$`Pr(>F)`[1], 2)



# intake rate for normal feeding trials -----------------------------


#Intake rate by day
IR <- lm(day$DMI_bw ~ day$Diet) #make model
aIR <- anova(IR) #take ANOVA table from linear regression
IRpval <- round(aIR$`Pr(>F)`[1], 3) #pull out pvalue from ANOVA
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



# DMD for feeding trials ----------------------------------------

#DMD by day
DMD <- lm(day$DMD ~ day$Diet)

aCDMD <- anova(DMD)
DMDpval <- round(aCDMD$`Pr(>F)`[1], 2)

aovDMD <- aov(DMD)
posthocDMD <- TukeyHSD(x = aovDMD, 'day$Diet', conf.level = 0.95)
posthocDMD




# CP digestion for feeding trials -----------------------------------------


#CP digestion by day
CPdig <- lm(day$DP ~ day$Diet)

aCPdig <- anova(CPdig)
CPdigpval <- round(aCPdig$`Pr(>F)`[1], 2)

aovCP <- aov(CPdig)
posthocCP <- TukeyHSD(x = aovCP, 'day$Diet', conf.level = 0.95)
posthocCP


# NDF digestion for feeding trials ----------------------------------------


#NDF digestion by day
NDFdig <- lm(day$DNDF ~ day$Diet)

aNDFdig <- anova(NDFdig)
NDFdigpval <- round(aNDFdig$`Pr(>F)`[1], 2)

aovNDF <- aov(NDFdig)
posthocNDF <- TukeyHSD(x = aovNDF, 'day$Diet', conf.level = 0.95)
posthocNDF


# ADF digestion for feeding trials ----------------------------------------


#ADF digestion by day
ADFdig <- lm(day$DADF ~ day$Diet)

aADFdig <- anova(ADFdig)
ADFdigpval <- round(aADFdig$`Pr(>F)`[1], 2)

aovADF <- aov(ADFdig)
posthocADF <- TukeyHSD(x = aovADF, 'day$Diet', conf.level = 0.95)
posthocADF

