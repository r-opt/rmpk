test_that("adding variables calls the solver", {
  registered_vars <- 0L
  mock_solver <- list(
    add_variable = function(...) {
      registered_vars <<- registered_vars + 1L
    }
  )
  instance <- RlpMipModel$new(mock_solver)
  instance$add_variable(x[i, j], i = 1:10, j = 1:10)
  expect_equal(registered_vars, 10 * 10)

  registered_vars <- 0L
  instance <- RlpMipModel$new(mock_solver)
  instance$add_variable(x)
  expect_equal(registered_vars, 1)
})

test_that("building a simple model", {
  solver <- ROI_solver("glpk")
  model <- MIPModel(solver)
  model$add_variable(x[i, j], i = 1:10, j = 1:10)
  model$set_objective(sum_expr(x[i, j], i = 1:10, j = 1:10))
  model$add_constraint(sum_expr(x[i, j], j = 1:i) <= 10, i = 1:10)
  expect_equal(as.numeric(as.matrix(solver$objective_coefficients())), rep.int(1, 100))
  mat <- solver$constraint_matrix()
  expect_equal(nrow(mat), 10)
})

test_that("correct index types are used in get_variable_value", {
  solver <- ROI_solver("glpk")
  model <- MIPModel(solver)
  model$add_variable(x[i, j, k], i = 1:10, j = as.character(1:10), k = c("a", "b"))
  model$optimize()
  ret <- model$get_variable_value(x[i, j, k])
  expect_true(is.character(ret$name))
  expect_true(is.character(ret$j))
  expect_true(is.character(ret$k))
  expect_true(is.integer(ret$i))
})

test_that("you can only use characters and integers as indexes", {
  solver <- ROI_solver("glpk")
  model <- MIPModel(solver)
  expect_error(
    model$add_variable(x[i], i = as.factor(1:10)),
    regexp = "integer|character"
  )
})

test_that("using not the right number of indeses errors in get_var_value", {
  solver <- ROI_solver("glpk")
  model <- MIPModel(solver)
  model$add_variable(x[i, j, k], i = 1:10, j = as.character(1:10), k = c("a", "b"))
  expect_error(model$get_variable_value(x[i, j]), "indexes")
})

test_that("A constant objective function is possible", {
  solver <- ROI_solver("glpk")
  model <- MIPModel(solver)
  model$add_variable(x)
  expect_silent(model$set_objective(5))
})

test_that("MIPModel prints some information", {
  solver <- ROI_solver("glpk")
  model <- MIPModel(solver)
  model$add_variable(x)
  model$add_constraint(x <= 1)
  expect_output(model$print(), "1")
})

test_that("add_variables exposes the variable classes to the env", {
  solver <- ROI_solver("glpk")
  model <- MIPModel(solver)
  model$add_variable(x[i], i = 1:3)
  expect_true(x[2]@variable_index == 2)
})

test_that("sum_expr works outside of the model context", {
  solver <- ROI_solver("glpk")
  model <- MIPModel(solver)
  model$add_variable(x[i], i = 1:3)
  res <- sum_expr(x[i], i = 1:3) + 10
  expect_true(res@constant == 10)
})
