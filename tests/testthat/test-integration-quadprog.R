library(ROI.plugin.quadprog)
library(ROI.plugin.alabama)

test_that("solve quad problem wit quadratic obj", {
  solver <- ROI_solver("quadprog")
  model <- MIPModel(solver)
  # a simple quadratic program from the ROI docs
  x <- model$add_variable(i = 1:3)
  model$set_objective(-5 * x[2] + x[1]^2 + x[2]^2 + x[3] * x[3], sense = "min")
  model$add_constraint(-4 * x[1] - 3 * x[2] >= -8)
  model$add_constraint(2 * x[1] + x[2] >= 2)
  model$add_constraint(-2 * x[2] + x[3] >= 0)
  model$optimize()
  res <- model$get_variable_value(x[i])
  res <- res[order(res$i), ]
  expect_equal(res$value, c(0.7142857, 0.5714286, 1.1428571), tolerance = 0.002)
})

test_that("solve quad problem wit quadratic obj and constraint", {
  solver <- ROI_solver("alabama", control = list(start = c(0, 0, 0)))
  model <- MIPModel(solver)
  # a simple quadratic program from the ROI docs
  x <- model$add_variable(i = 1:3, lb = 0, ub = 10)
  model$set_objective(-5 * x[2] + x[1]^2 + x[2]^2 + x[3] * x[3], sense = "min")
  model$add_constraint(-4 * x[1] - 3 * x[2] >= -8)
  model$add_constraint(2 * x[1]^2 + x[2] >= 2)
  model$add_constraint(-2 * x[2] + x[3] >= 0)
  model$optimize()
  res <- model$get_variable_value(x[i])
  res <- res[order(res$i), ]
  # TODO: find a better example
  expect_equal(res$value, c(1.1832224, 0.5999848, 1.1999696), tolerance = 0.002)
})

test_that("account for 1/2 in constraints", {
  b <- rnorm(10)
  y <- 2.3 + b * 5 + rnorm(10)
  expected <- coef(lm(y ~ b))
  X <- matrix(c(rep.int(1, 10), b), ncol = 2)
  model <- Model(solver = ROI_solver("quadprog"))
  beta <- model$add_variable(i = 1:2)
  model$set_objective(
    sum_expr((y[i] - sum_expr(beta[j] * X[i, j], j = 1:2))^2, i = 1:10)
  )
  model$optimize()
  res <- model$get_variable_value(beta[i])$value
  expect_equivalent(
    as.numeric(res),
    as.numeric(expected),
    tolerance = 0.001
  )
})
