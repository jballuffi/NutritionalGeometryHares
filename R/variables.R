#list of column/variables that you want taken out of the cleaned sheet

dayvars <- c("CP_diet", "NDF_diet", "ADF_diet", "ADL_diet", "Energy_diet", #diet compositions (%)
             "DMI", "DMI_CP", "DMI_NDF", "DMI_ADF", "DMI_ADL", "DMI_energy", #dry matter intakes (g/day)
             "Weight_start", "Weight_end", #weights (g)
             "DMI_bw", "DMI_CP_bw", "DMI_NDF_bw", "DMI_ADF_bw", "DMI_ADL_bw", "DMI_energy_bw", #dry matter intake by weight (g/kg^.75/day)
             "DMF", "DMF_CP", "DMF_NDF", "DMF_ADF", "DMF_ADL", #fecal outputs (g/day) 
             "CP_F", "NDF_F", "ADF_F", "ADL_F", #fecal compositions (%)
             "DMD", "DP", "DNDF", "DADF", "DADL", "DE",  #digestibility (%)
             "DMDI", "DPI", "DNDFI", "DADFI", "DEI", #digestible intake rate (g/kg^.75/day)
             "Temp") #temp (C)
