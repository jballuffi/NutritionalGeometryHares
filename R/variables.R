#list of column/variables that you want taken out of the cleaned sheet

dayvars <- c("CP_diet", "NDF_diet", "ADF_diet", "ADL_diet", "GE_diet", #diet compositions
             "DMI", "CPI", "NDFI", "GEI", #dry matter intakes (g/day)
             "Weight_start", "Weight_end", #weights (g)
             "DMI_bw", "CPI_bw", "NDFI_bw", "GEI_bw", #dry matter intake by weight (g/kg^.75/day)
             "DMO", "CPO", "NDFO", #fecal outputs (g/day) 
             "CP_F", "NDF_F", #fecal compositions (%)
             "DMD", "CPD", "NDFD", "GED",  #digestibility (%)
             "DMDI", "DPI", "DNDFI", "DEI" #digestible intake rate (g/kg^.75/day)
             
)
