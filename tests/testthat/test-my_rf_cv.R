test_that("The result is a numeric", {
  expect_is(my_rf_cv(5), "numeric")
})
