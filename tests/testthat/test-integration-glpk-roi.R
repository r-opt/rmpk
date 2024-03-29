library(ROI.plugin.glpk)

test_that("solve a knapsack problem", {
  solver <- ROI_optimizer("glpk")

  v <- rnorm(10)
  w <- rnorm(10)

  model <- optimization_model(solver)
  x <- model$add_variable("x", i = 1:10, type = "binary")
  model$set_objective(sum_expr(v[i] * x[i], i = 1:10), sense = "max")
  model$add_constraint(sum_expr(w[i] * x[i], i = 1:10) <= 10)
  model$optimize()
  expect_equal(model$termination_status(), MOI_SUCCESS)
})

test_that("solve a knapsack problem #", {
  solver <- ROI_optimizer("glpk")

  v <- rnorm(10)
  w <- rnorm(10, mean = 4)

  model <- optimization_model(solver)
  x <- model$add_variable("x", i = 1:10, type = "binary")
  model$set_objective(sum_expr(v[i] * x[i], i = 1:10), sense = "max")
  model$add_constraint(sum_expr(w[i] * x[i], i = 1:10) + 6, .in_set = moi_less_than_set(10))
  model$optimize()
  expect_equal(model$termination_status(), MOI_SUCCESS)
  res <- model$get_variable_value(x[i])
  res <- res[order(res$i), ]
  expect_true(
    crossprod(res$value, w) + 6 <= 10
  )
})

test_that("solve a bounded knapsack problem", {
  solver <- ROI_optimizer("glpk")
  model <- optimization_model(solver)
  x <- model$add_variable("x", type = "integer", lb = 0, ub = 1, i = 1:10) # ROI has problems with binary and bounds
  model$set_objective(sum_expr(x[i], i = 1:10), sense = "max")
  model$add_constraint(sum_expr(x[i], i = 1:10) <= 10)
  model$set_bounds(x[i], ub = 0, i = 1:10, i <= 8)
  model$set_bounds(x[i], ub = 1, i = 1, i > 1)
  model$set_bounds(x[9], ub = 0)
  model$optimize()
  res <- model$get_variable_value(x[i])
  ones <- res[res$value == 1, ]
  zeros <- res[res$value == 0, ]
  expect_equal(nrow(ones), 1)
  expect_equal(ones$i, 10L)
  expect_equal(nrow(zeros), 9)
})

test_that("it supports column/row duals", {
  model <- optimization_model(ROI_optimizer("glpk"))
  x <- model$add_variable("x", i = 1:10)
  model$set_objective(sum_expr(x[i], i = 1:10), sense = "min")
  model$add_constraint(x[i] >= i, i = 1:10)
  model$optimize()

  column_duals <- model$get_variable_dual(x[i])
  row_duals <- model$get_row_duals()

  expected_col_duals <- rep.int(0, 10)
  expected_row_duals <- rep.int(1, 10)

  expect_equal(column_duals$value, expected_col_duals)
  expect_equal(row_duals$value, expected_row_duals)
})

test_that("it handles constant in objective function", {
  solver <- ROI_optimizer("glpk")
  model <- optimization_model(solver)
  x <- model$add_variable("x", type = "binary")
  model$set_objective(x + 1, sense = "max")
  model$optimize()
  expect_equal(model$objective_value(), 2)
})

test_that("ROI returns OTHER_ERROR on time limit", {
  solver <- ROI_optimizer(
    "glpk",
    control = list(tm_limit = 1, verbose = FALSE)
  )

  v <- rnorm(100)
  w <- runif(100)

  model <- optimization_model(solver)
  x <- model$add_variable("x", i = 1:100, type = "binary")
  model$set_objective(sum_expr(v[i] * x[i], i = 1:100), sense = "max")
  model$add_constraint(sum_expr(w[i] * x[i], i = 1:100) <= 10)
  model$optimize()
  expect_equal(model$termination_status(), MOI_OTHER_ERROR)
  expect_true(is.list(model$termination_solver_message()))
})

test_that("lower bounds on variables work", {
  solver <- ROI_optimizer("glpk")
  model <- optimization_model(solver)
  x <- model$add_variable("x", type = "integer", i = 1:10, lb = 5)
  model$set_bounds(x[i], lb = 10, i = 5:10)
  model$set_objective(sum_expr(x[i], i = 1:10), sense = "min")
  model$optimize()
  expect_equal(model$objective_value(), 5 * 4 + 10 * 6)
})
