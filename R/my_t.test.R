#' My version of t-test
#'
#' This function performs a one sample t-test in R.
#'
#' @param x a (non-empty) numeric vector of data values to be tested.
#' @param alternative a character string specifying the alternative hypothesis,
#'    must be one of "two.sided", "greater" or "less".
#' @param mu a numeric input indicating the null hypothesis value of the mean.
#'
#' @keywords inference
#'
#' @return a \code{list} with following element:
#'
#'   \code{test_stat}: the numeric test statistic.
#'
#'   \code{df}: the degrees of freedom.
#'
#'   \code{alternative}: the value of the parameter \code{alternative}.
#'
#'   \code{p_val}: the numeric p-value.
#'
#' @examples
#' my_t.test(my_gapminder$lifeExp, "two.sided", 60)
#' my_t.test(my_gapminder$lifeExp, "greater", 60)
#' my_t.test(my_gapminder$lifeExp, "less", 60)
#'
#' @export
my_t.test <- function(x, alternative, mu) {
  # calculate the t statistic for the given data
  test_stat <- (mean(x) - mu) / (sd(x) / sqrt(length(x)))
  # calculate the p value based on the type of alternative hypothesis
  if (alternative == "two.sided") {
    # since it's a two-sided test and the distribution is symmetric, so times 2 to get the correct p value
    p_val <- 2 * pt(abs(test_stat), lower.tail = FALSE, df = length(x) - 1)
  } else if (alternative == "less") {
    # get the area from the lower tail
    p_val <- pt(test_stat, df = length(x) - 1)
  } else if (alternative == "greater") {
    # get the area from the higher tail
    p_val <- pt(test_stat, lower.tail = FALSE, df = length(x) - 1)
  } else {
    # give a error message if the alternative hypothesis is not valid
    stop("Please give a valid alternative hypothesis.")
  }
  # create a list record the results
  result <- list("test_stat" = test_stat, "df" = length(x) - 1,
                 "alternative" = alternative, "p_val" = p_val)
  # return the result list object
  return(result)
}
