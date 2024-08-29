
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



#extas
(c <- ggplot()+
    geom_raster(aes(x = DMI_NDF_bw, y = DMI_CP_bw, z = fit, fill = fit), data = CNDF)+
    geom_contour(aes(x = DMI_NDF_bw, y = DMI_CP_bw, z = fit), bins = 5, colour = "grey90", data = CNDF)+
    scale_fill_continuous(name = "%/day", type = "viridis")+
    geom_line(aes(x = NDF_IR, y = CP_IR, group = Diet, linetype = Diet), size = .8, data = rails)+
    scale_linetype_manual(values = dietlines, guide = NULL)+
    geom_point(aes(x = meanNDFI, y = meanCPI), data = targets)+
    xlim(min(CNDF$DMI_NDF_bw), max(CNDF$DMI_NDF_bw))+
    ylim(min(CNDF$DMI_CP_bw), max(CNDF$DMI_CP_bw))+
    xlab(expression(NDF~intake~(g/kg^0.75/day)))+
    ylab(expression(CP~intake~(g/kg^0.75/day)))+
    labs(title = "A) Crude NDF and protein")+
    themerails)

d <- ggplot()+
  geom_raster(aes(x = DNDFI, y = DPI, z = fit, fill = fit), data = DNDF)+
  geom_contour(aes(x = DNDFI, y = DPI, z = fit), bins = 5, colour = "grey90", data = DNDF)+
  scale_fill_continuous(name = "%/day", type = "viridis")+
  geom_line(aes(x = DNDF_IR, y = DP_IR, group = Diet, linetype = Diet), size = .8, data = rails, )+
  scale_linetype_manual(values = dietlines, guide = NULL)+
  geom_point(aes(x = meanDNDFI, y = meanDPI), data = targets)+
  xlim(min(DNDF$DNDFI), max(DNDF$DNDFI))+
  ylim(min(DNDF$DPI), max(DNDF$DPI))+
  xlab(expression(DNDF~intake~(g/kg^0.75/day)))+
  ylab(expression(DP~intake~(g/kg^0.75/day)))+
  labs(title = "C) Digestible NDF and protein")+
  themerails


