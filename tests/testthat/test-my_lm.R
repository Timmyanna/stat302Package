test_that("All elements of my result table is correct", {
  expect_equal(my_lm(lifeExp ~ gdpPercap + continent, data = my_gapminder),
               summary(lm(lifeExp ~ gdpPercap + continent, data = my_gapminder))[["coefficients"]])
})
