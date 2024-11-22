  
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)
library(tidymv)
  
  
#read in results
trials <- readRDS("Output/data/trialresultscleaned.rds")
  
#nutritional rails (long version)
rails <- fread("Output/data/digestibilerails_long.rds")
  
#target intake rates
MCsums <- readRDS("Output/data/multichoicesums.rds")



# model weight change ~ crude energy --------------------

#bodyCE <- gam(Weight_change ~ s(DMI_energy_bw, DMI_CP_bw), data = trials)
bodyCE <- gam(Weight_change ~ s(GEI_bw) + s(CPI_bw) + s(GEI_bw, CPI_bw), data = trials)

#save summary of model
sumCE <- summary(bodyCE)

#make p-table, and indicate model, this is for intercept stuff
sumpCE <- as.data.table(round((sumCE$p.table), 4))
sumpCE[, Model := "Crude energy"]
sumpCE[, `Dev. Explained` := round(sumCE$dev.expl, 1)]

#make s-table, and indicate model, this is for variable stuff
sumsCE <- as.data.table(round((sumCE$s.table), 4))
sumsCE[, Model := "Crude energy"]



# model weight change ~ digestible energy -----------------------------------

#bodyDE <- gam(Weight_change ~ s(DEI, DPI), data = trials)
bodyDE <- gam(Weight_change ~ s(DEI) + s(DPI) + s(DEI, DPI), data = trials)

#save summary of model
sumDE <- summary(bodyDE)

#make p-table, and indicate model, this is for intercept stuff
sumpDE <- as.data.table(round((sumDE$p.table), 4))
sumpDE[, Model := "Digestible energy"]
sumpDE[, `Dev. Explained` := round(sumDE$dev.expl, 1)]

#make s-table, and indicate model, this is for variable stuff
sumsDE <- as.data.table(round((sumDE$s.table), 4))
sumsDE[, Model := "Digestible energy"]



# Merge output tables -----------------------------------------------------

#bind all s tables
allsums <- rbind(sumsCE, sumsDE)

setcolorder(allsums, c("Model", "edf", "Ref.df", "F", "p-value"))

#round the table to 2 decimal places
summarytable <- allsums %>% mutate_if(is.numeric, round, digits = 2)



# make surface heat maps for each model --------------------------------

#create predictive datasets based on gams
crude <- as.data.table(predict_gam(bodyCE))
digestible <- as.data.table(predict_gam(bodyDE))

#remove cases with high standard error
crude <- crude[`se.fit` < 0.4]
digestible <- digestible[`se.fit` < 0.4]

#set line types for diets
dietlines <- c("A" = "solid", "B" = "longdash", "C" = "dotdash", "D" = "dotted")

(a <- ggplot()+
  geom_raster(aes(x = GEI_bw, y = CPI_bw, z = fit, fill = fit), data = crude)+
  geom_contour(aes(x = GEI_bw, y = CPI_bw, z = fit), bins = 5, colour = "grey90", data = crude)+
  scale_fill_continuous(name = "%/day", type = "viridis")+
  geom_line(aes(x = GE_IR, y = CP_IR, group = Diet, linetype = Diet), size = .8, data = rails)+
  scale_linetype_manual(values = dietlines, guide = NULL)+
  xlim(min(crude$GEI_bw), max(crude$GEI_bw))+
  ylim(min(crude$CPI_bw), max(crude$CPI_bw))+
  xlab(expression(GE~intake~(kJ/kg^0.75/day)))+
  ylab(expression(CP~intake~(g/kg^0.75/day)))+
  labs(title = "A)")+
  themerails)

(b <- ggplot()+
  geom_raster(aes(x = DEI, y = DPI, z = fit, fill = fit), data = digestible)+
  geom_contour(aes(x = DEI, y = DPI, z = fit), bins = 5, colour = "white", data = digestible)+
  scale_fill_continuous(name = "%/day", type = "viridis", guide = NULL)+
  geom_line(aes(x = DE_IR, y = DP_IR, group = Diet, linetype = Diet), size = .8, data = rails)+
  scale_linetype_manual(values = dietlines)+
  xlim(min(digestible$DEI), max(digestible$DEI))+
  ylim(min(digestible$DPI), max(digestible$DPI))+
  xlab(expression(DE~intake~(kJ/kg^0.75/day)))+
  ylab(expression(DP~intake~(g/kg^0.75/day)))+
  labs(title = "B)")+
  themerails)

surfaceplot <- ggarrange(a, b, ncol = 1, nrow = 2)



# Save --------------------------------------------------------------

write.csv(summarytable, "Output/stats/GAMoutputs.csv")

ggsave("Output/figures/surfaceplots.jpeg", surfaceplot, width = 6, height = 10, unit = "in")
        
