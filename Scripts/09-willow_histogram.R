
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#these are the results for plant samples taken before winter 2022-2023
nuts <- fread("Input/Willow_spruce_nutrition.csv")



# prep variable names  -------------------------------------------

#create species column with sample id
nuts[grep("S", Sample), Species := "Spruce"]
nuts[grep("W", Sample), Species := "Willow"]

#create height column with sample id
nuts[grep("L", Sample), Height := "low"]
nuts[grep("M", Sample), Height := "medium"]
nuts[grep("H", Sample), Height := "high"]

#remove random value with no species or height
nuts <- nuts[!is.na(Species)]

#subset to main variables
nuts <- nuts[, .(Species, Height, Grid, Loc, CP_F, NDF_F, ADF_F, ADL_F)]

#remove impossibly low protein or impossible high protein
nuts <- nuts[!CP_F < 2]
nuts <- nuts[!CP_F > 9]

nuts[CP_F > 6.5, Quality := "fair quality"]
nuts[CP_F < 6.5, Quality := "poor quality"]


qualcols <- c("poor quality" = "grey90", "fair quality" = "grey40")


(hist <- ggplot(nuts)+
  geom_histogram(aes(x = CP_F, fill = Quality), color = "black", bins = 10)+
  scale_fill_manual(values = qualcols)+
  labs(x = "CP composition (%)", y = "Count")+
  facet_wrap(~Species, nrow = 2, ncol = 1)+
  themethesistop)

sum <- nuts[, .N, by = .(Quality, Species)]
sum[, total := sum(N), Species]
sum[, prop := N/total]
sum


ggsave("Output/Figures/willow_spruce_histogram.jpeg", hist, width = 5, height = 8, units = "in")
