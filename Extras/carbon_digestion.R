# exploring carbon digestability

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in daily feeding trial results
days <- readRDS("Output/data/dailyresultscleaned.rds")

days[, Weight_start := Weight_start/1000]




# melt data  -----------------------------------

Cdig <- days[, .(Diet, C_dig, CP_in_bw, C_in_bw, NDF_in_bw, ADF_in_bw)]

Cdigmelt <- melt(Cdig, measure.vars = c("CP_in_bw", "C_in_bw", "NDF_in_bw", "ADF_in_bw"), 
                 variable.name = "nutrient", 
                 value.name = "intake")

Cdigmelt[, nutrient := tstrsplit(nutrient, "_", keep = 1)]


# plot --------------------------------------------------------------------




(cdigplot<- 
  ggplot(Cdigmelt)+
  geom_point(aes(x = intake, y = C_dig))+
  themepoints+
  labs(x = "nutrient intake (g DM/kg/day)", y = "Carbon digestability (%)")+
  facet_wrap(~nutrient, scales = "free"))

ggsave("Output/figures/Cdigestability.jpeg", cdigplot, width = 8, height = 5, units = "in")
