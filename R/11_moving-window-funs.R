# create extra column names (eg. *_t-1)
make_extra_WB10_names <- function(x = WB10_items, X_window = X_window){
  dayn_WB10_items <- list()
  #dayn_WB10_items[[1]] <- x#  just combine the original col labels with extra ones
  for (i in seq_len(X_window-1)) {
    dayn_WB10_items[[i]] <- paste0(x, "_t_", i)
  }
  rv <- do.call(c, dayn_WB10_items)
}

# add other days WB10 response values to single row for moving_window_X()
combine_extra_WB10_values <- function(x, i=i, X_window = X_window, items = items) {
  subx <- list()
  for (w in seq_len(X_window-1)){
    subx[[w]] <- x[i-w, items]
  }
  unlist(subx)
}


#' create moving window in X
#' Combine multiple days worth of WB10 data in a single row, along with the
#' other explanatory variables, for the purpose of predicting SH in the future
#' function formats X data. Use ___ to prepare Y data.
#'  note: have "WB10_items" character vector loaded
#'
#' @param x data.frame of predictors and response
#' @param X_window. Size of moving window (in days). Must be integer, value of
#'    2 or more.
#' @param items Vector of character strings. Names of items to use in time series
#'    embedding.
#' @export
moving_window_X <- function(x, X_window = 2, items = WB10_items) {
  stopifnot(any(names(x)=="NewMRN"))
  n_cols <- ncol(x)
  day2on_WB10_items <- make_extra_WB10_names(x = items, X_window = X_window)
  person_visit.list <- split(x, x$NewMRN)
  # person_moving_window_X: data.frame for each person (return value component)
  person_moving_window_X <- list()
  for (p in seq_along(person_visit.list)) {
    if(nrow(person_visit.list[[p]]) >= X_window) {  # else NULL list element
      person_moving_window_X[[p]] <- data.frame(matrix(NA, nrow = nrow(person_visit.list[[p]])-(X_window-1),
                                                       ncol = n_cols+length(day2on_WB10_items))) # initialise  (or just use indices instead and specify col names at the end)
      colnames(person_moving_window_X[[p]]) <- c(colnames(x), day2on_WB10_items)
      for (i in seq_len(nrow(person_moving_window_X[[p]]))) {
        person_moving_window_X[[p]][i, seq_len(n_cols)] <- person_visit.list[[p]][i+X_window-1, ]
        person_moving_window_X[[p]][i, day2on_WB10_items] <-
          combine_extra_WB10_values(x=person_visit.list[[p]], i=i+X_window-1, X_window = X_window, items = items)
      }
    }
  }
  rv <- do.call(rbind, person_moving_window_X)
  rv
  #person_moving_window_X   # use list for testing.
}

#' Make prediction window from response vector
#' 
#' prediction_window_Y makes positive class blocks of length \code{prediction_window}.
#' @returns Vector of length nrow(x) - X_window + 1
#' @param x data.frame
#' @param X_window integer. days worth of daily data used in X, per row
#' @param prediction_window integer. Number of days.
prediction_window_Y <- function(x, X_window, prediction_window){
  stopifnot(any(names(x)=="NewMRN") & any(names(x)=="SelfHarmEvent"))
  person_visit.list <- split(x, x$NewMRN) # use only NewMRNs that exist in corresp. X
  Y <- list()
  for (p in seq_along(person_visit.list)) {
    if(nrow(person_visit.list[[p]]) >= X_window) {  # else NULL list element
      # expand SH data, assuming no SH on days within prediction window of discharge
      SH <- person_visit.list[[p]]$SelfHarmEvent
      SH <- c(SH, rep(0, prediction_window))
      Y[[p]] <- vector("integer", length = nrow(person_visit.list[[p]]) - (X_window-1))
      for (i in seq_along(Y[[p]])) {
        #id <- person_visit.list[[p]]$NewMRN
        if(any(SH[(i+X_window):(i+X_window+prediction_window-1)] == 1)) {
          Y[[p]][i] <- 1L
        } else {
          Y[[p]][i] <- 0L
        }
        
      }
    }
  }
  # return value (include NewMRN, reference days)
  do.call(c, Y)
}
