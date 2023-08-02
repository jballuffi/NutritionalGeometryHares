
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


#read in results
trials <- readRDS("Output/data/trialresultscleaned.rds")
day<- readRDS("Output/data/dailyresultscleaned.rds")




# Weight change figures -----------------------------------------------------------

test <- lm(Weight_change ~ DMI_CP_bw + DMI_NDF_bw + 0, trials)
summary(test)

ggplot(trials)+
  geom_point(aes(x = DMI_NDF_bw, y = Weight_change))+
  geom_abline(intercept = 0, slope = -0.021005)+
  xlim(0, 80)

ggplot(trials)+
  geom_point(aes(x = DMI_CP_bw, y = Weight_change))+
  geom_abline(intercept = 0, slope = 0.034366)+
  xlim(0, 17)



# models for non-digestible nutrients -------------------------------------

ND1 <- gam(Weight_change ~ DMI_CP_bw + DMI_NDF_bw, data = trials)
ND2 <- gam(Weight_change ~ DMI_CP_bw*DMI_NDF_bw, data = trials)
ND3 <- gam(Weight_change ~ s(DMI_CP_bw) + s(DMI_NDF_bw), data = trials)
ND4 <- gam(Weight_change ~ s(DMI_CP_bw, DMI_NDF_bw), data = trials)
ND5 <- gam(Weight_change ~ s(DMI_CP_bw, by = DMI_NDF_bw), data = trials)
ND6 <- gam(Weight_change ~ s(DMI_NDF_bw, by = DMI_CP_bw), data = trials)



# models for digestible nutrient intake -----------------------------------

D1 <- gam(Weight_change ~ DPI + DNDFI, data = trials)
D2 <- gam(Weight_change ~ DPI*DNDFI, data = trials)
D3 <- gam(Weight_change ~ s(DPI) + s(DNDFI), data = trials)
D4 <- gam(Weight_change ~ s(DPI, DNDFI), data = trials)
D5 <- gam(Weight_change ~ s(DPI, by = DNDFI), data = trials)
D6 <- gam(Weight_change ~ s(DNDFI, by = DPI), data = trials)



# basic linear models for digestability effects on weight change -----------------------------------

summary(lm(Weight_change ~ DMDI, trials))
summary(lm(Weight_change ~ DMI, trials))




# poly models -------------------------------------------------------------

int_lm <- lm(Weight_change ~ DMI_CP_bw*DMI_NDF_bw, trials)
int_poly <- lm(Weight_change ~ poly(DMI_CP_bw, 2)*poly(DMI_NDF_bw, 2), trials)
lmm <- lm(Weight_change ~ DMI_CP_bw + DMI_NDF_bw, trials)
polym <- lm(Weight_change ~ poly(DMI_CP_bw, 2) + poly(DMI_NDF_bw, 2), trials)


Mods <- list(int_lm, int_poly, lmm, polym)
names <- c("int_lm", "int_poly", "lmm", "polym")

AIC<-as.data.table(aictab(REML=F, cand.set = Mods, modnames = names, sort = TRUE))
AIC[, ModelLik := NULL]
AIC[, Cum.Wt := NULL]
#round whole table to 3 dec places
AIC <- AIC %>% mutate_if(is.numeric, round, digits=3)



