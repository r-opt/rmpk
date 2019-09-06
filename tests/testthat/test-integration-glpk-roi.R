library(ROI.plugin.glpk)

test_that("solve a knapsack problem", {
  solver <- ROI_solver("glpk")

  v <- rnorm(10)
  w <- rnorm(10)

  model <- MIPModel(solver)
  model$add_variable(x[i], type = "binary", i = 1:10)
  model$set_objective(sum_expr(v[i] * x[i], i = 1:10))
  model$add_constraint(sum_expr(w[i] * x[i], i = 1:10) <= 10)
  model$optimize()
  expect_equal(model$termination_status(), TERMINATION_STATUS$SUCCESS)
})
