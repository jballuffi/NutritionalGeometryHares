
#function that averages hare responses across all days within one trial
#enter what results you want averaged in the cols <- c()
#this function should be run by ID and Diet/trial
#only argument is the the datatable of daily responses


trialavg <- function(dt){
  cols <- c("CP_diet", "NDF_diet", "ADF_diet", "ADL_diet",
            "DMI", "DMI_CP", "DMI_NDF", "DMI_ADF", "DMI_ADL", 
            "Weight_start", "Weight_end", #weights are the same for every day because we only took a measure at the start and end of trital
            "DMI_bw", "DMI_CP_bw", "DMI_NDF_bw", "DMI_ADF_bw", "DMI_ADL_bw", 
            "DMD", "DMF", "DP", "DNDF", "DADF", "DADL", 
            "Temp")
  out <- dt[, lapply(.SD, mean), .SDcols = cols]
  return(out)
}
