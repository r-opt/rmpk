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
