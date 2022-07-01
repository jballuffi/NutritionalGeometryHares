
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


#read in results
trials <- readRDS("Output/data/trialresultscleaned.rds")
day<- readRDS("Output/data/dailyresultscleaned.rds")




# Weight change -----------------------------------------------------------

int_lm <- lm(Weight_change ~ CP_in_bw*NDF_in_bw, trials)
int_poly <- lm(Weight_change ~ poly(CP_in_bw, 2)*poly(NDF_in_bw, 2), trials)
lmm <- lm(Weight_change ~ CP_in_bw + NDF_in_bw, trials)
polym <- lm(Weight_change ~ poly(CP_in_bw, 2) + poly(NDF_in_bw, 2), trials)


Mods <- list(int_lm, int_poly, lmm, polym)
names <- c("int_lm", "int_poly", "lmm", "polym")

AIC<-as.data.table(aictab(REML=F, cand.set = Mods, modnames = names, sort = TRUE))
AIC[,ModelLik:=NULL]
AIC[,Cum.Wt:=NULL]
#round whole table to 3 dec places
AIC<-AIC %>% mutate_if(is.numeric, round, digits=3)


ggplot(trials)+
  geom_point(aes(x = NDF_in_bw, y = Weight_change))+
  stat_smooth(aes(x = NDF_in_bw, y = Weight_change), method='lm', formula = y ~ poly(x,2), size = 1)

ggplot(trials)+
  geom_point(aes(x = CP_in_bw, y = Weight_change))+
  stat_smooth(aes(x = CP_in_bw, y = Weight_change), method='lm', formula = y ~ poly(x,2), size = 1)




# Protein digestion -------------------------------------------------------

CP <- lm(CP_dig ~ poly(CP_in_bw, 2)*poly(NDF_in_bw, 2), day)
summary(CP)

summary(lm(CP_dig ~ poly(CP_in_bw, 2), day))
summary(lm(CP_dig ~ CP_in_bw, day))


ggplot(day)+
  geom_point(aes(x = CP_in_bw, y = CP_dig))+
  stat_smooth(aes(x = CP_in_bw, y = CP_dig), method='lm', formula = y ~ poly(x,2), size = 1)



# NDF digestion -----------------------------------------------------------

NDF <- lm(NDF_dig ~ poly(CP_in_bw, 2) + poly(NDF_in_bw, 2), day)
summary(NDF)

