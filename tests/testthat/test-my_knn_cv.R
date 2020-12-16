# get training data
penguins_train <- na.omit(my_penguins) %>%
  select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g)
# get true class
penguins_cl <- as.character(na.omit(my_penguins)$species)
result <- my_knn_cv(penguins_train, penguins_cl, 1, 5)

test_that("The result is a list", {
  expect_is(result, "list")
})

cv_error <- result[[2]]
test_that("The cv error is between 0 and 1 ", {
  expect_true(0 <= cv_error & cv_error <= 1)
})


