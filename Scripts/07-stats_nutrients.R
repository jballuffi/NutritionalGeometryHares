
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


#read in results
trials <- readRDS("Output/data/trialresultscleaned.rds")
day<- readRDS("Output/data/dailyresultscleaned.rds")




# Weight change figures -----------------------------------------------------------

#regular NDF intake
ggplot(trials)+
  geom_point(aes(x = DMI_NDF_bw, y = Weight_change))+
  theme_minimal()

#digestible NDF intake
ggplot(trials)+
  geom_point(aes(x = DNDFI, y = Weight_change))+
  theme_minimal()

#regular protein intake
ggplot(trials)+
  geom_point(aes(x = DMI_CP_bw, y = Weight_change))+
  theme_minimal()

#digestible protein intake
ggplot(trials)+
  geom_point(aes(x = DPI, y = Weight_change))+
  theme_minimal()




# models for non-digestible intake on weight change -------------------------------------

bodyNDlm <- gam(Weight_change ~ DMI_NDF_bw*DMI_CP_bw, data = trials)
BodyNDgam <- gam(Weight_change ~ s(DMI_NDF_bw, DMI_CP_bw), data = trials)



# models for digestible intake on weight change -----------------------------------

bodyDlm <- gam(Weight_change ~ DNDFI*DPI, data = trials)
bodyDgam <- gam(Weight_change ~ s(DNDFI, DPI), data = trials)



#  models for intake on DMD -----------------------------------------------

DMDlm <- gam(DMD ~ DMI_NDF_bw*DMI_CP_bw, data = day)
DMDgam <- gam(DMD ~ s(DMI_NDF_bw, DMI_CP_bw), data = day)



        