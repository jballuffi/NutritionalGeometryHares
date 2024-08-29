
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)
library(tidymv)


#read in results
trials <- readRDS("Output/data/trialresultscleaned.rds")

#nutritional rails
rails <- fread("Output/data/dietdigestionrails.rds")

#target intake rates
MCsums <- readRDS("Output/data/multichoicesums.rds")


# model weight change ~ crude NDF -------------------------------------

#bodyCNDF <- gam(Weight_change ~ s(DMI_NDF_bw, DMI_CP_bw), data = trials)
bodyCNDF <- gam(Weight_change ~ s(DMI_NDF_bw) + s(DMI_CP_bw) + s(DMI_NDF_bw, DMI_CP_bw), data = trials)

#save summary of model
sumCNDF <- summary(bodyCNDF)

#make p-table, and indicate model, this is for intercept stuff
sumpCNDF <- as.data.table(round((sumCNDF$p.table), 4))
sumpCNDF[, Model := "Crude NDF"]
sumpCNDF[, `Dev. Explained` := round(sumCNDF$dev.expl, 2)]

#make s-table, and indicate model, this is for variable stuff
sumsCNDF <- as.data.table(round((sumCNDF$s.table), 4))
sumsCNDF[, Model := "Crude NDF"]




# model weight change ~ digestible NDF -----------------------------------

#bodyDNDF <- gam(Weight_change ~ s(DNDFI, DPI), data = trials)
bodyDNDF <- gam(Weight_change ~ s(DNDFI) + s(DPI) + s(DNDFI, DPI), data = trials)

#save summary of model
sumDNDF <- summary(bodyDNDF)

#make p-table, and indicate model, this is for intercept stuff
sumpDNDF <- as.data.table(round((sumDNDF$p.table), 4))
sumpDNDF[, Model := "Digestible NDF"]
sumpDNDF[, `Dev. Explained` := round(sumDNDF$dev.expl, 2)]

#make s-table, and indicate model, this is for variable stuff
sumsDNDF <- as.data.table(round((sumDNDF$s.table), 4))
sumsDNDF[, Model := "Digestible NDF"]

