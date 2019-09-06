test_that("multiplication works", {
  x <- new("RLPVariable", coefficient = 1, index = 1L)
  y <- new("RLPVariable", coefficient = 1, index = 2L)
  result <- (-x + y * 2 - 2) * 18
  expect_equal(result@constant, -2 * 18)
  expect_equal(result@variables[[1L]]@coefficient, -18)
  expect_equal(result@variables[[2L]]@coefficient, 36)
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
