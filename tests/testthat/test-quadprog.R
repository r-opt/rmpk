library(ROI.plugin.quadprog)
library(ROI.plugin.alabama)

test_that("solve quad problem wit quadratic obj", {
  solver <- ROI_solver("quadprog")
  model <- MIPModel(solver)
  # a simple quadratic program from the ROI docs
  model$add_variable(x[i], i = 1:3)
  model$set_objective(-5 * x[2] + x[1]^2 + x[2]^2 + x[3] * x[3], sense = "min")
  model$add_constraint(-4 * x[1] - 3 * x[2] >= -8)
  model$add_constraint(2 * x[1] + x[2] >= 2)
  model$add_constraint(-2 * x[2] + x[3] >= 0)
  model$optimize()
  res <- model$get_variable_value(x[i])
  expect_equal(res$value, c(0.4761905, 1.0476190, 2.0952381), tolerance = 0.002)
})

test_that("solve quad problem wit quadratic obj and constraint", {
  solver <- ROI_solver("alabama", control = list(start = c(0, 0, 0)))
  model <- MIPModel(solver)
  # a simple quadratic program from the ROI docs
  model$add_variable(x[i], i = 1:3, lb = 0, ub = 10)
  model$set_objective(-5 * x[2] + x[1]^2 + x[2]^2 + x[3] * x[3], sense = "min")
  model$add_constraint(-4 * x[1] - 3 * x[2] >= -8)
  model$add_constraint(2 * x[1]^2 + x[2] >= 2)
  model$add_constraint(-2 * x[2] + x[3] >= 0)
  model$optimize()
  res <- model$get_variable_value(x[i])
  # TODO: find a better example
  expect_equal(res$value, c(0.9486833, 1.0999999, 2.1999998), tolerance = 0.002)
})
