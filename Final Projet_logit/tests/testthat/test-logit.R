context("test-logit")

test_that("logit works", {
  set.seed(5)
  data <- data.frame("y" = rbinom(n = 100, size = 1, prob = 0.5),
                     "a" = rnorm(n = 100),
                     "b" = rnorm(n = 100, mean = 2, sd = 0.5))

  statsResult <-
    stats::glm(y ~ a + b, data = data, family = binomial("logit"))
  myResult <- logit(y ~ a + b, data = data)

  statsResultSummary <- summary(statsResult)
  myResultSummary <- summary(myResult)

  expect_equivalent(statsResult$coefficients, myResult$coefficients)
  expect_that(myResult, is_a("logit"))

  expect_equivalent(statsResultSummary$coefficients, myResultSummary$coefficients)
  expect_that(myResultSummary, is_a("summary.logit"))
})
