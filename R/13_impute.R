#' Memory property for imputation
#' 
#' Impute for 1 to days_mem forward from last known value
mem_loss <- function(is_missing, days_mem) {
  rv <- vector("numeric", length(is_missing))
  rv[1:days_mem] <- 0
  for (i in (1+days_mem):length(is_missing)) {
    rv[i] <- all(is_missing[i:(i-days_mem)]==1)
  }
  rv
}

#' Impute using last known value
#' 
#' Impute by replacing NA with last known value up to 'mem' days ahead of
#' the last known value
#' @param x Vector to impute for
#' @param mem number of time steps ahead to impute
#' @return Vector of length x containing values in x and imputed values
#' @export
var_impute_lkv <- function(x, mem=3) {
  stopifnot(length(x) >= 2)   # ! avoid out of bounds in mem_loss
  is_missing <- as.numeric(is.na(x))
  memory_loss <- mem_loss(is_missing, days_mem = mem)
  for (i in 2:length(x)) {
    if (is.na(x[i]) & !is.na(x[i-1]) & !memory_loss[i]) {
      x[i] <- x[i-1]
    }
  }
  rv <- data.frame(x, is_missing)
  #colnames(rv) <- c(names(x), paste0(names(x), ".is_missing"))
  rv
}

#' Impute using last known values
#' 
#' For items in x, impute missing values, up to 'memory' time steps ahead of
#' the last known value, replacing these missing values with the last known 
#' value.
#' 
#' @param x data.frame to impute
#' @param items Charactor vector. Variable names in x to impute for
#' @param memory Integer. Number of time steps ahead of last known values to 
#'     impute for.
#' @return data.frame with same dimenstions as \code{x}
#' @export
impute_LastKnown <- function(x, items, memory) {
  for (item in items) {
    x[[item]] <- var_impute_lkv(x = x[[item]], mem = memory)
  }
  x
}