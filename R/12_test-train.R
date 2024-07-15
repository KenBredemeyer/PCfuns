#' test / train split
#' 
#' Split on persons - no persons common to both test and training data
#' 
#' @param x data.frame Data to split into training data and test data
#' @param train_size proportion of persons to put into the training data
#' @export
train_test <- function(x, train_size = 0.7) {
  person_data.list <- split(x, x[["MRN"]])
  train.i <- sample(length(person_data.list), size = ceiling(0.7 * length(person_data.list)))
  train.list <- person_data.list[train.i]
  test.list <- person_data.list[-train.i]
  train <- do.call(rbind, train.list)
  test <- do.call(rbind, test.list)
  
  return_values <- list(train, test)
  return_values
}
