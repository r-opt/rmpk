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
