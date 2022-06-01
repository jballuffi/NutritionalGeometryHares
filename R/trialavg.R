
#function that averages hare responses across all days within one trial
#enter what results you want averaged in the cols <- c()
#this function should be run by ID and Diet/trial
#only argument is the the datatable of daily responses


trialavg <- function(dt){
  cols <- c("Intake_bw", "CP_in_bw", "NDF_in_bw", "ADF_in_bw", 
            "Weight_start", "Weight_end", #weights are the same for every day because we only took a measure at the start and end of trital
            "Total_out", "CP_dig", "NDF_dig", "ADF_dig", 
            "Temp")
  out <- dt[, lapply(.SD, mean), .SDcols = cols]
  return(out)
}