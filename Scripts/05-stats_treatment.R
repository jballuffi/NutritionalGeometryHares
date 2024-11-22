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

meanDMImc <- round(mean(targets$DMI_bw, na.rm = TRUE), digits = 1)
seDMImc <- round(sd(targets$DMI_bw, na.rm = TRUE)/sqrt(length(targets)), digits = 1)

meanCPmc <- round(mean(targets$CPI_bw), digits = 1)
seCPmc <- round(sd(targets$CPI_bw)/sqrt(length(targets)), digits = 1)

meanCEmc <- round(mean(targets$GEI_bw), digits = 1)
seCEmc <- round(sd(targets$GEI_bw)/sqrt(length(targets)), digits = 1)

meanDPmc <- round(mean(targets$DPI_bw), digits = 1)
seDPmc <- round(sd(targets$DPI_bw)/sqrt(length(targets)), digits = 1)

meanDEmc <- round(mean(targets$DEI_bw), digits = 1)
seDEmc <- round(sd(targets$DEI_bw)/sqrt(length(targets)), digits = 1)


#ANOVA testing for significance between treatments
lmMC <- lm(MC$DMI_bw ~ MC$Diet)
aMC <- anova(lmMC)
MCpval <- round(aMC$`Pr(>F)`[1], 2)
aovMC <- aov(lmMC)
posthocMC <- TukeyHSD(x = aovMC, 'MC$Diet', conf.level = 0.95)
posthocMC


#how many times more B than A
diffBtoA <- round((MC[Diet == "B", mean(DMI_bw)]) / (MC[Diet == "A", mean(DMI_bw)]), digits = 1)

diffBtoD <- round((MC[Diet == "B", mean(DMI_bw)]) / (MC[Diet == "D", mean(DMI_bw)]), digits = 1)



# intake rate for normal feeding trials -----------------------------

#Intake rate by day
IR <- lm(day$DMI_bw ~ day$Diet) #make model
aIR <- anova(IR) #take ANOVA table from linear regression
IRpval <- round(aIR$`Pr(>F)`[1], 5) #pull out pvalue from ANOVA
#tukey test on ANOVA
aovIR <- aov(IR)
posthocIR <- TukeyHSD(x = aovIR, 'day$Diet', conf.level = 0.95)
posthocIR

effectofA <- round((day[Diet == "A", mean(DMI_bw)])/(day[, mean(DMI_bw)]), digits = 2)

diffAtoC <- round((day[Diet == "A", mean(DMI_bw)])/(day[Diet == "C", mean(DMI_bw)]), digits = 2)



# weight change for feeding trials ----------------------------------------

#Weight change by trial 
WC <- lm(trials$Weight_change ~ trials$Diet)
aWC <- anova(WC)
WCpval <- round(aWC$`Pr(>F)`[1], 5)
aovWC <- aov(WC)
posthocWC <- TukeyHSD(x = aovWC, 'trials$Diet', conf.level = 0.95)
posthocWC

diffAtoBwc <- round((trials[Diet == "B", mean(Weight_change)]) / (trials[Diet == "A", mean(Weight_change)]), digits = 2)



# DMD for feeding trials ----------------------------------------

#DMD by day
DMD <- lm(day$DMD ~ day$Diet)
aCDMD <- anova(DMD)
DMDpval <- round(aCDMD$`Pr(>F)`[1], 5)
aovDMD <- aov(DMD)
posthocDMD <- TukeyHSD(x = aovDMD, 'day$Diet', conf.level = 0.95)
posthocDMD



# CP digestion for feeding trials -----------------------------------------

#CP digestion by day
CPdig <- lm(day$CPD ~ day$Diet)
aCPdig <- anova(CPdig)
CPdigpval <- round(aCPdig$`Pr(>F)`[1], 5)
aovCP <- aov(CPdig)
posthocCP <- TukeyHSD(x = aovCP, 'day$Diet', conf.level = 0.95)
posthocCP

diffDtoAcp <- round((day[Diet == "D", mean(CPD)])/(day[Diet == "A", mean(CPD)]), digits = 2)




# collect model outputs into one table ------------------------------------

mods <- list(lmMC, IR, WC, DMD, CPdig)
names <- c("multi-choice", "single choice", "weight", "DMD", "CP digestion")



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
