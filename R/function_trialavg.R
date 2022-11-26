
#function that averages hare responses across all days within one trial
#enter what results you want averaged in the cols <- c()
#this function should be run by ID and Diet/trial
#only argument is the the datatable of daily responses


trialavg <- function(dt){
  out <- dt[, lapply(.SD, mean), .SDcols = dayvars] #dayvars in variables script
  return(out)
}
