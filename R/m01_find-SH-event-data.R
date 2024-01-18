#' for an individual patient, find self-harm days which have n days between
#' events
#' 
#' @param x PC_data$SelfHarmEvent for an individual patient
#' @returns row indeces of the self-harm events
find_sh_events <- function(x, days_between = 5) {
  rles <- rle(x)
  sh_event.i <- list()
  event_number <- 0
  for (i in 2:length(rles[[1]]))
    if(rles[[1]][i-1] >= days_between && rles[[2]][i-1] == 0 && rles[[2]][i] == 1) {
      event_number <- event_number +1
      sh_event.i[[event_number]] <- sum(rles[[1]][1:i])  #!!!! fix this line. replace index to left of assignment!
    }
  unlist(sh_event.i)
}

#' for an individual patient, find self-harm days which have n days between
#' events
#' 
#' when days_between = 0, function returns data for all self-harm days, for the 
#' patient
#' 
#' @param x PC_data$SelfHarmEvent for an individual patient
#' @param days_between Integer specifying the days to include in the lead up
#'     to a self-harm event.
#' @returns row indeces of the n-days leading up to self-harm events
#' @export
sh_event_data_1patient <- function(x, days_between = 5) {
  stopifnot(sum(x[["SelfHarmEvent"]]) > 0)
  rles <- rle(x[["SelfHarmEvent"]])
  #sh_event.i <- vector("numeric", length = sum(rles[[2]]))
  sh_event.i <- list()
  event_number <- 0
  for (i in 2:length(rles[[1]]))
    if(length(rles[[2]]) == 1 && rles[[2]] == 1) {
      sh_event.i <- seq_len(rles[[1]])
    }
  else if(rles[[1]][i-1] >= days_between && rles[[2]][i-1] == 0 && rles[[2]][i] == 1) {
    event_number <- event_number + 1
    sh_event.i[[event_number]] <- sum(rles[[1]][1:i])  #!!!! fix this line. replace index to left of assignment!
  }
  sh_event.i <- unlist(sh_event.i)
  return_rows <- list()
  for (i in seq_along(sh_event.i)) {
    return_rows[[i]] <- (sh_event.i[i] - days_between):sh_event.i[i]
  }
  return_rows <- unlist(return_rows)
  x[return_rows, ]
}


#' Return data frame for n-day lead up to self-harm events
#' 
#' @param PCdata data.frame
#' @param days_lag Days to include in the lead up to a SH event
#' @export
get_self_harm_data <- function(PCdata, days_lag){
  patient_data <- split(pc_data, pc_data$NewMRN)
  # find patients who have had a sh event
  sh_patients <- which(sapply(patient_data, function(x) any(x[["SelfHarmEvent"]] == 1)))
  sh_patient_data <- list()
  for(i in sh_patients) {
    sh_patient_data[[i]] <- sh_event_data_1patient(patient_data[[i]], days_between = days_lag)
  }
  return.df <- do.call(rbind, sh_patient_data)
}
