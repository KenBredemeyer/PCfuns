#' Trend Imputation
#' 
#' Impute 1 missing value ahead of last known value
#' using 3 time point trend, or last known value
#' @param x Numeric vector.
#' @export
impute_trend <- function(x) {
  stopifnot(length(x) >= 4)
  max_x <- max(x, na.rm = TRUE)
  min_x <- min(x, na.rm = TRUE)
  to_impute <- vector("logical", length = length(x)-3)
  for (i in 4:length(x)) {
    to_impute[i] <- is.na(x[i]) && !is.na(x[i-1]) && !is.na(x[i-2]) && !is.na(x[i-3])
  }
  to_impute.i <- which(to_impute)
  for (imp in to_impute.i) { 
    if(x[imp-1] < max_x && x[imp-1] > x[imp-2] && x[imp-2] > x[imp-3]) {  # upward trend
      x[imp] <- x[imp-1] + 1
    } else if(x[imp-1] > min_x && x[imp-1] < x[imp-2] && x[imp-2] < x[imp-3]) { # downward trend
      x[imp] <- x[imp-1] - 1
    } else {
      x[imp] <- x[imp-1]
    }
  }
  x
}

#' L2 Norm Error
#' @param x original (true) vector
#' @param x_imp modified x
#' @return abs(x - x_imp) / n
L2_error <- function(x, x_imp) {
  L2e <- sum(abs(x_imp - x)) / length(na.omit(x))
  L2e
}
