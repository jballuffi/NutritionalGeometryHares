
#function that calculates mean temperature between start and end date-times of feeding trials

#start = start date column
#end = end date column


tempcalc <- function(start, end) {
  avgtemp <- temp[DateTime > start & DateTime < end, mean(Temp)]
}
