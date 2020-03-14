test_that("MIP works without a solver", {
  expect_silent({
    model <- MIPModel()
    x <- model$add_variable()
    model$set_objective(x)
    model$add_constraint(x <= 10)
  })
})
