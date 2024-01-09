#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)



# read in cleaned results -------------------------------------------------


#read in cleaned multi-choice results
MC <- readRDS("Output/data/multichoiceresults.rds") #all results by diet
targets <- readRDS("Output/data/multichoicesums.rds") #sums of nutrient intakes per trial

#read in cleaned feeding trial results
trials <- readRDS("Output/data/trialresultscleaned.rds") #by trial
day<- readRDS("Output/data/dailyresultscleaned.rds") # by day




# stats for multi-choice trials ----------------------------------------------------

#ANOVA testing for significance between treatments
lmMC <- lm(MC$DMI_bw ~ MC$Diet)
aMC <- anova(lmMC)
MCpval <- round(aMC$`Pr(>F)`[1], 2)
aovMC <- aov(lmMC)
posthocMC <- TukeyHSD(x = aovMC, 'MC$Diet', conf.level = 0.95)
posthocMC

#how much more of diet B did hares eat than other diets
effectofB <- round((MC[Diet == "B", mean(DMI_bw)])/(MC[, mean(DMI_bw)]), digits = 2)


# intake rate for normal feeding trials -----------------------------

#Intake rate by day
IR <- lm(day$DMI_bw ~ day$Diet) #make model
aIR <- anova(IR) #take ANOVA table from linear regression
IRpval <- round(aIR$`Pr(>F)`[1], 3) #pull out pvalue from ANOVA
#tukey test on ANOVA
aovIR <- aov(IR)
posthocIR <- TukeyHSD(x = aovIR, 'day$Diet', conf.level = 0.95)
posthocIR

effectofA <- round((day[Diet == "A", mean(DMI_bw)])/(day[, mean(DMI_bw)]), digits = 2)



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




# collect model outputs into one table ------------------------------------

mods <- list(lmMC, IR, WC, DMD, CPdig, NDFdig, ADFdig)
names <- c("multi-choice", "single choice", "weight", "DMD", "CP digestion", "NDF digestion", "ADF digestion")



lm_out <- function(model) {
  #summarize model
  out <- summary(model)
  
  #collect coef values
  coefOut <- data.table(t(out$coefficients[, 1]))
  coefOut<-round(coefOut, 3)
  
  #collect standard errors
  seOut <- data.table(t(out$coefficients[, 2]))
  seOut<-round(seOut, 3)
  
  #collect p-values
  pvals <- data.table(t(out$coefficients[, 4]))
  pvals <- round(pvals, 3)
  
  #Paste coef and standard errors together, rename cols
  coefse<-data.table(t(paste0(coefOut, " ± ", seOut, " (", pvals, ")")))
  setnames(coefse, paste0(colnames(coefOut)))
  
  #collect R2s and change column name
  rsqOut <- data.table(rsq(model))
  names(rsqOut)<-c("rsq")
  rsqOut <- round(rsqOut, 3)
  
  
  
  #return each datatable binded together by row
  return(data.table(coefse, rsqOut))
}



#apply to same list of models as in AIC
OutAll<-lapply(mods, lm_out)
OutAll<-rbindlist(OutAll, fill = TRUE)
OutAll$Model<-names
