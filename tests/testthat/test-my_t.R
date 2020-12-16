test_that("t statistic is calculated right", {
  expect_equal(my_t.test(my_gapminder$lifeExp, "two.sided", 60)$test_stat,
               as.numeric(t.test(my_gapminder$lifeExp, mu = 60)$statistic))

  expect_equal(my_t.test(my_gapminder$lifeExp, "greater", 60)$test_stat,
               as.numeric(t.test(my_gapminder$lifeExp, alternative = "greater", mu = 60)$statistic))

  expect_equal(my_t.test(my_gapminder$lifeExp, "less", 60)$test_stat,
               as.numeric(t.test(my_gapminder$lifeExp, alternative = "less", mu = 60)$statistic))
})

test_that("the degree of freedom is calculated right", {
  expect_equal(my_t.test(my_gapminder$lifeExp, "two.sided", 60)$df,
               as.numeric(t.test(my_gapminder$lifeExp, mu = 60)$parameter))

  expect_equal(my_t.test(my_gapminder$lifeExp, "greater", 60)$df,
               as.numeric(t.test(my_gapminder$lifeExp, alternative = "greater", mu = 60)$parameter))

  expect_equal(my_t.test(my_gapminder$lifeExp, "less", 60)$df,
               as.numeric(t.test(my_gapminder$lifeExp, alternative = "less", mu = 60)$parameter))
})

test_that("alternative shows correctly", {
  expect_match(my_t.test(my_gapminder$lifeExp, "two.sided", 60)$alternative,
               t.test(my_gapminder$lifeExp, mu = 60)$alternative)

  expect_match(my_t.test(my_gapminder$lifeExp, "greater", 60)$alternative,
               t.test(my_gapminder$lifeExp, alternative = "greater", mu = 60)$alternative)

  expect_match(my_t.test(my_gapminder$lifeExp, "less", 60)$alternative,
               t.test(my_gapminder$lifeExp, alternative = "less", mu = 60)$alternative)
})

test_that("Character strings other than required throw an error", {
  expect_error(my_t.test(my_gapminder$lifeExp, "compare", 60))
})

test_that("the p-value is calculated right", {
  expect_equal(my_t.test(my_gapminder$lifeExp, "two.sided", 60)$p_val,
               t.test(my_gapminder$lifeExp, mu = 60)$p.value)

  expect_equal(my_t.test(my_gapminder$lifeExp, "greater", 60)$p_val,
               t.test(my_gapminder$lifeExp, alternative = "greater", mu = 60)$p.value)

  expect_equal(my_t.test(my_gapminder$lifeExp, "less", 60)$p_val,
               t.test(my_gapminder$lifeExp, alternative = "less", mu = 60)$p.value)
})




