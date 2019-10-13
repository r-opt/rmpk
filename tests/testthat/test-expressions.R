test_that("multiplication works", {
  x <- new("RLPVariable", coefficient = 1, variable_index = 1L)
  y <- new("RLPVariable", coefficient = 1, variable_index = 2L)
  result <- (-x + y * 2 - 2) * 18
  vars <- result@variables$as_list()
  expect_equal(result@constant, -2 * 18)
  expect_equal(vars[["1"]]@coefficient, -18)
  expect_equal(vars[["2"]]@coefficient, 36)
})

test_that("complex test case #1", {
  u1 <- new("RLPVariable", coefficient = 1, variable_index = 1L)
  u2 <- new("RLPVariable", coefficient = 1, variable_index = 2L)
  x <- new("RLPVariable", coefficient = 1, variable_index = 3L)
  n <- 10
  expect_silent(
    result <- u1 - u2 + 1 - ((n - 1) * (1 - x))
  )
  vars <- result@variables$as_list()
  expect_equal(result@constant, 2 - n)
  expect_equal(vars[["1"]]@coefficient, 1)
  expect_equal(vars[["2"]]@coefficient, -1)
  expect_equal(vars[["3"]]@coefficient, n - 1)
})

test_that("equations can be splitted", {
  expr <- quote(a + b <= 10)
  res <- split_equation(expr)
  expect_equal(res$operator, "<=")
  expect_equal(res$lhs, quote(a + b))
  expect_equal(res$rhs, quote(10))

  res <- split_equation(quote(a + b >= 10))
  expect_equal(res$operator, ">=")

  res <- split_equation(quote(a + b == 10))
  expect_equal(res$operator, "==")

  expect_error(split_equation(quote(a + b)))
  expect_error(split_equation(quote(a + b < 10)))
  expect_error(split_equation(quote(a + b > 10)))
})

test_that("linear expressions hold only variables once", {
  x <- new("RLPVariable", coefficient = 1, variable_index = 1L)
  y <- new("RLPVariable", coefficient = 1, variable_index = 2L)
  expr <- x + x + y
  vars <- expr@variables$as_list()
  expect_equal(length(vars), 2)
  expect_equal(vars[["1"]]@coefficient, 2)
  expect_equal(vars[["2"]]@coefficient, 1)
})

test_that("quadratic expressions are supported", {
  x <- new("RLPVariable", coefficient = 1, variable_index = 1L)
  y <- new("RLPVariable", coefficient = 1, variable_index = 2L)
  result <- (-x + y * 2 * y - 2) * 18
  expect_equal(result@linear_part@constant, -2 * 18)
  expect_equal(result@linear_part@variables$get("1")@coefficient, -18)
  quad_vars <- result@quadratic_variables$as_list()
  expect_equal(quad_vars[[1L]]@coefficient, 36)
  expect_equal(quad_vars[[1L]]@variable1@variable_index, 2L)
  expect_equal(quad_vars[[1L]]@variable2@variable_index, 2L)
})

test_that("pow 2", {
  x <- new("RLPVariable", coefficient = 1, variable_index = 1L)
  y <- new("RLPVariable", coefficient = 1, variable_index = 2L)
  result <- (x^2 + y^2) * 18
  quad_vars <- result@quadratic_variables$as_list()
  expect_equal(quad_vars[[1L]]@coefficient, 18)
  expect_equal(quad_vars[[2L]]@coefficient, 18)
  expect_equal(quad_vars[["1_1"]]@variable1@variable_index, 1L)
  expect_equal(quad_vars[["1_1"]]@variable2@variable_index, 1L)
  expect_equal(quad_vars[["2_2"]]@variable1@variable_index, 2L)
  expect_equal(quad_vars[["2_2"]]@variable2@variable_index, 2L)
})

test_that("quadprog complex", {
  x <- new("RLPVariable", coefficient = 1, variable_index = 1L)
  y <- new("RLPVariable", coefficient = 1, variable_index = 2L)
  z <- new("RLPVariable", coefficient = 1, variable_index = 3L)
  result <- -5 * y + x^2 + y^2 + z^2
  quad_vars <- result@quadratic_variables$as_list()
  expect_equal(quad_vars[[1L]]@coefficient, 1)
  expect_equal(quad_vars[[2L]]@coefficient, 1)
  expect_equal(quad_vars[[3L]]@coefficient, 1)
  expect_equal(result@linear_part@variables$as_list()[[1L]]@coefficient, -5)
})

test_that("adding the same quad tuples increaes coef", {
  x <- new("RLPVariable", coefficient = 1, variable_index = 1L)
  y <- new("RLPVariable", coefficient = 1, variable_index = 2L)
  result <- x * y + x * y
  expect_equal(result@coefficient, 2)
})

test_that("more examples work", {
  x <- new("RLPVariable", coefficient = 1, variable_index = 1L)
  result <- 1 - (5 + (x + (2 - x)))
  expect_equal(result@constant, -6)
  expect_equal(result@variables$as_list()[[1L]]@coefficient, 0)
})

test_that("even more examples work", {
  x <- new("RLPVariable", coefficient = 1, variable_index = 1L)
  result <- 2 * (x * x + 2 + (1 + x * x) + (x^2 - 2) + (5 - x^2))
  expect_equal(result@linear_part@constant, 12)
  expect_equal(result@quadratic_variables$as_list()[[1]]@coefficient, 4)
})

test_that("quadratic expression merge", {
  x <- new("RLPVariable", coefficient = 1, variable_index = 1L)
  y <- new("RLPVariable", coefficient = 1, variable_index = 2L)
  result <- (x^2 + 1) + x + x^2 + (y^2 + 1)
  expect_equal(result@linear_part@constant, 2)
  quad_vars <- result@quadratic_variables$as_list()
  expect_equal(quad_vars[["1_1"]]@coefficient, 2)
  expect_equal(quad_vars[["2_2"]]@coefficient, 1)
})

test_that("quadratic expression and linear expression", {
  x <- new("RLPVariable", coefficient = 1, variable_index = 1L)
  y <- new("RLPVariable", coefficient = 1, variable_index = 2L)
  result <- (x + 1) + (4 * y * x)
  expect_equal(result@linear_part@constant, 1)
  quad_vars <- result@quadratic_variables$as_list()
  expect_equal(quad_vars[["1_2"]]@coefficient, 4)
})

test_that("multiplication by 0 yields numeric", {
  x <- new("RLPVariable", coefficient = 1, variable_index = 1L)
  y <- new("RLPVariable", coefficient = 1, variable_index = 2L)
  expect_equal(((x + 1) + (4 * y * x)) * 0, 0)
  expect_equal((x + 1) * 0, 0)
})

test_that("subtracting variables work", {
  x <- new("RLPVariable", coefficient = 1, variable_index = 1L)
  y <- new("RLPVariable", coefficient = 1, variable_index = 2L)
  result <- x + x - y
  vars <- result@variables$as_list()
  expect_equal(vars[[1]]@coefficient, 2)
  expect_equal(vars[[2]]@coefficient, -1)
})
