
#' Collect responses n days prior to a self-harm event for a single WB10 item
#' 
#' This is a simple function which ignores the presence of other self-harm
#' events within the specified time span.
#' 
#' 
#' @seealso [find_sh_event()] for finding all sh events per patient, with 
#'     at least n days between events
wb10_values_ndays_ago <- function(x, xvar, days_back) {
  s_harm.i <- which(pc_data$SelfHarmEvent == 1)
  xvar_data <- list()
  for (i in 0:days_back) {
    index <- s_harm.i - i
    index <- index[which(pc_data[index, "CurrentLOS"] >= i)] # no CurrentLOS < 0
    xvar_data[[i+1]] <- pc_data[(index - i), xvar]
  }
  xvar_data
}


#' Plot score frequencies over n days preceeding a self-harm event
#' 
#' @param x data.frame. PC data
#' @param xvar Character vector. Name of WB10 variable to plot
barplot_5days_back <- function(x, xvar) {     # make to any number of days back later
  # put these on the same plot
  DEP_6days <- wb10_values_ndays_ago(pc_data, xvar, 5)
  po <- matrix(nrow = 6, ncol = (max(unlist(DEP_6days), na.rm = TRUE) - min(unlist(DEP_6days), na.rm = TRUE) + 1))
  colnames(po) <- names(table(DEP_6days[[1]]))
  po[1, ] <- unname(table(DEP_6days[[1]]))
  po[2, ] <- unname(table(DEP_6days[[2]]))
  po[3, ] <- unname(table(DEP_6days[[3]]))
  po[4, ] <- unname(table(DEP_6days[[4]]))
  po[5, ] <- unname(table(DEP_6days[[5]]))
  po[6, ] <- unname(table(DEP_6days[[6]]))
  
  barplot(po, beside = TRUE,
          legend.text = c("day 0", "day -1", "day -2", "day -3", "day -4", "day -5"),
          args.legend = list(x = "topleft", inset = c(0.07, 0.07)),
          main = paste(xvar, "\n (days since self-harm event)",
                       xlab = "score",
                       ylab = "frequency"))
}
