#' My version of fitting linear models
#'
#' This function fits a linear model in R.
#' @param formula a \code{formula} class object.
#' @param data input data frame.
#'
#' @keywords inference/prediction
#'
#' @return a \code{table} similar to the coefficient table from \code{summary()},
#'   with rows for each coefficient and columns for the \code{Estimate},
#'   \code{Std. Error}, \code{t value}, \code{Pr(>|t|)}.
#'
#' @examples
#' ## Demonstrate a regression using my_gapminder dataset.
#' my_lm(lifeExp ~ gdpPercap + continent, data = my_gapminder)
#'
#' @export
my_lm <- function(formula, data) {
  # extract the model matrix X according to the formula from the given data frame
  X <- model.matrix(formula, data)
  # extract a model frame object according to the given formula from the given data frame
  frame <- model.frame(formula, data)
  # extract the model response y vector according to the frame object
  y <- model.response(frame)

  # solve for betas using the given formula
  betas <- solve(t(X) %*% X) %*% t(X) %*% y
  # calculate the degree of freedom as sample size minus the number of covariates
  df <- nrow(X) - ncol(X)
  # calculate the sigma^2 by the given formula
  sigma_square <- sum((y - X %*% betas)^2 / df)
  # calculate the standard error by the given formula
  stand_error <- diag(sqrt(sigma_square * (solve(t(X) %*% X))))
  # calculate the t statistic for the two-sided t tests
  test_stat <- betas / stand_error
  # calculate the p value of the given t statistic as the area under the t-distribution
  # since it's a two-sided test and the distribution is symmetric, so times 2 to get the correct p value
  p_val <- 2 * pt(abs(test_stat), lower.tail = FALSE, df = df)

  # bind other results with estimate table
  result_table <- cbind(betas, stand_error, test_stat, p_val)
  # rename the table with required names
  colnames(result_table) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  # return the table
  return(result_table)
}
