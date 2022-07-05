#Stats on treatment effects


#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


#read in results
trials <- readRDS("Output/data/trialresultscleaned.rds")
day<- readRDS("Output/data/dailyresultscleaned.rds")


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
