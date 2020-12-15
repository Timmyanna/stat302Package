#' My version of k-Nearest Neighbors Cross-Validation
#'
#' This function predict output class of input data by
#'   K-nearest neighbor algorithm
#' @importFrom dplyr select filter
#' @param train input data frame
#' @param cl true class value of your training data
#' @param k_nn a integer representing the number of neighbors
#' @param k_cv a integer representing the number of folds
#'
#' @keywords prediction
#'
#' @return a \code{list} with following objects:
#'
#'   \code{class}: a vector of the predicted class for all observations
#'
#'   \code{cv_error}: a numeric with the cross-validation misclassification error
#'
#' @examples
#' ## Demonstrate using my_penguins dataset
#' # get training data
#' penguins_train <- dplyr::select(na.omit(my_penguins),
#'   bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g)
#' # get true class
#' penguins_cl <- as.character(na.omit(my_penguins)$species)
#' my_knn_cv(penguins_train, penguins_cl, 1, 5)
#'
#' @export
my_knn_cv <- function(train, cl, k_nn, k_cv) {
  # create a random sample
  fold <- sample(rep(1:k_cv, length = nrow(train)))
  # create a new variable in the input date frame as split which take the value of fold
  train$split <- fold
  # set a variable to record the result of cv errors
  err_total <- c()
  #iterate through k folds
  for (i in 1:k_cv) {
    # update the train data and test data
    data_train <- train %>% filter(split != i) %>% select(-split)
    data_test <- train %>% filter(split == i) %>% select(-split)
    # update the train class and test class
    cl_train <- cl[fold != i]
    cl_test <- cl[fold == i]
    #record the prediction
    pr <- class::knn(data_train, data_test, cl_train, k_nn)
    #record the misclassification rate
    mis_rate <- sum(pr != cl_test) / length(pr)
    err_total <- append(err_total, mis_rate)
  }
  # store the vector class as the output with the full data as both the training and the test data
  class <- class::knn(train, train, cl, k_nn)
  # store the cv_err as the average misclassification rate from cross validation
  cv_err <- mean(err_total)
  #store in a list of objects
  result <- list(class, cv_err)
  return(result)
}
