library(ROI.plugin.glpk)

test_that("solve a knapsack problem", {
  solver <- ROI_solver("glpk")

  v <- rnorm(10)
  w <- rnorm(10)

  model <- MIPModel(solver)
  model$add_variable(x[i], type = "binary", i = 1:10)
  model$set_objective(sum_expr(v[i] * x[i], i = 1:10), sense = "max")
  model$add_constraint(sum_expr(w[i] * x[i], i = 1:10) <= 10)
  model$optimize()
  expect_equal(model$termination_status(), TERMINATION_STATUS$SUCCESS)
})

test_that("solve a bounded knapsack problem", {
  solver <- ROI_solver("glpk")
  model <- MIPModel(solver)
  model$add_variable(x[i], type = "integer", lb = 0, ub = 1, i = 1:10) #ROI has problems with binary and bounds
  model$set_objective(sum_expr(x[i], i = 1:10), sense = "max")
  model$add_constraint(sum_expr(x[i], i = 1:10) <= 10)
  model$set_bounds(x[i], ub = 0, i = 1:8)
  model$set_bounds(x[9], ub = 0)
  model$optimize()
  res <- model$get_variable_value(x[i])
  ones <- res[res$value == 1, ]
  zeros <- res[res$value == 0, ]
  expect_equal(nrow(ones), 1)
  expect_equal(as.integer(ones$i), 10) #TODO: this will be an integer
  expect_equal(nrow(zeros), 9)
})
