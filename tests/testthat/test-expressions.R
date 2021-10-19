test_that("multiplication works", {
  x <- moi_scalar_affine_term(1, RMPK_variable(index = 1L))
  y <- moi_scalar_affine_term(1, RMPK_variable(index = 2L))
  result <- (-x + y * 2 - 2) * 18
  vars <- result@terms
  expect_equal(result@constant, -2 * 18)
  expect_equal(var_by_index(vars, 1L)@coefficient, -18)
  expect_equal(var_by_index(vars, 2L)@coefficient, 36)
})

test_that("complex test case #1", {
  u1 <- moi_scalar_affine_term(1, moi_variable_index(1L))
  u2 <- moi_scalar_affine_term(1, moi_variable_index(2L))
  x <- moi_scalar_affine_term(1, moi_variable_index(3L))
  n <- 10
  expect_silent(
    result <- u1 - u2 + 1 - ((n - 1) * (1 - x))
  )
  vars <- result@terms
  expect_equal(result@constant, 2 - n)
  expect_equal(var_by_index(vars, 1L)@coefficient, 1)
  expect_equal(var_by_index(vars, 2L)@coefficient, -1)
  expect_equal(var_by_index(vars, 3L)@coefficient, n - 1)
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
  x <- moi_scalar_affine_term(1, RMPK_variable(index = 1L))
  y <- moi_scalar_affine_term(1, RMPK_variable(index = 2L))
  expr <- x + x + y
  vars <- canonicalize(expr)@terms
  expect_equal(length(vars), 2)
  expect_equal(var_by_index(vars, 1L)@coefficient, 2)
  expect_equal(var_by_index(vars, 2L)@coefficient, 1)
})

test_that("quadratic expressions are supported", {
  x <- moi_scalar_affine_term(1, RMPK_variable(index = 1L))
  y <- moi_scalar_affine_term(1, RMPK_variable(index = 2L))
  result <- (-x + y * 2 * y - 2) * 18
  expect_equal(result@constant, -2 * 18)

  expect_equal(result@affine_terms[[1]]@coefficient, -18)

  quad_vars <- result@quadratic_terms
  expect_equal(quad_vars[[1L]]@coefficient, 36)
  expect_equal(quad_vars[[1L]]@variable1@value, 2L)
  expect_equal(quad_vars[[1L]]@variable2@value, 2L)
})

test_that("pow 2", {
  x <- moi_scalar_affine_term(1, RMPK_variable(index = 1L))
  y <- moi_scalar_affine_term(1, RMPK_variable(index = 2L))
  result <- (x^2 + y^2) * 18
  quad_vars <- result@quadratic_terms
  q1 <- quad_vars[[1L]]
  q2 <- quad_vars[[2L]]
  expect_equal(length(quad_vars), 2)
  expect_equal(q1@coefficient, 18)
  expect_equal(q2@coefficient, 18)
  if (q1@variable1@value == 2L) {
    temp <- q1
    q1 <- q2
    q2 <- q1
  }
  expect_equal(q1@variable1@value, 1L)
  expect_equal(q1@variable2@value, 1L)
  expect_equal(q2@variable1@value, 2L)
  expect_equal(q2@variable2@value, 2L)
})

test_that("quadprog complex", {
  x <- moi_scalar_affine_term(1, RMPK_variable(index = 1L))
  y <- moi_scalar_affine_term(1, RMPK_variable(index = 2L))
  z <- moi_scalar_affine_term(1, RMPK_variable(index = 3L))
  result <- -5 * y + x^2 + y^2 + z^2
  quad_vars <- result@quadratic_terms
  expect_equal(quad_vars[[1L]]@coefficient, 1)
  expect_equal(quad_vars[[2L]]@coefficient, 1)
  expect_equal(quad_vars[[3L]]@coefficient, 1)
  expect_equal(result@affine_terms[[1L]]@coefficient, -5)
})

test_that("adding the same quad tuples increaes coef", {
  x <- moi_scalar_affine_term(1, RMPK_variable(index = 1L))
  y <- moi_scalar_affine_term(1, RMPK_variable(index = 2L))
  result <- x * y + x * y
  expect_equal(result@quadratic_terms[[1L]]@coefficient, 1)
  expect_equal(result@quadratic_terms[[2L]]@coefficient, 1)
  expect_equal(canonicalize(result)@quadratic_terms[[1L]]@coefficient, 2)
})

test_that("more examples work", {
  x <- moi_scalar_affine_term(1, RMPK_variable(index = 1L))
  result <- 1 - (5 + (x + (2 - x)))
  expect_equal(result@constant, -6)
  expect_equal(canonicalize(result)@terms[[1L]]@coefficient, 0)
})

test_that("even more examples work", {
  x <- moi_scalar_affine_term(1, RMPK_variable(index = 1L))
  result <- 2 * (x * x + 2 + (1 + x * x) + (x^2 - 2) + (5 - x^2))
  expect_equal(result@constant, 12)
  expect_equal(canonicalize(result)@quadratic_terms[[1]]@coefficient, 4)
})

test_that("quadratic expression merge", {
  x <- moi_scalar_affine_term(1, RMPK_variable(index = 1L))
  y <- moi_scalar_affine_term(1, RMPK_variable(index = 2L))
  result <- (x^2 + 1) + x + x^2 + (y^2 + 1)
  expect_equal(result@constant, 2)
  quad_vars <- canonicalize(result)@quadratic_terms
  q1 <- quad_vars[[1L]]
  q2 <- quad_vars[[2L]]
  expect_equal(length(quad_vars), 2)
  if (q1@variable1@value == 2L) {
    temp <- q1
    q1 <- q2
    q2 <- q1
  }
  expect_equal(q1@coefficient, 2)
  expect_equal(q2@coefficient, 1)
})

test_that("quadratic expression and linear expression", {
  x <- moi_scalar_affine_term(1, RMPK_variable(index = 1L))
  y <- moi_scalar_affine_term(1, RMPK_variable(index = 2L))
  result <- (x + 1) + (4 * y * x)
  expect_equal(result@constant, 1)
  quad_vars <- canonicalize(result)@quadratic_terms
  expect_equal(quad_vars[[1]]@coefficient, 4)
})

test_that("multiplication by 0 yields numeric", {
  x <- moi_scalar_affine_term(1, RMPK_variable(index = 1L))
  y <- moi_scalar_affine_term(1, RMPK_variable(index = 2L))
  expect_equal(((x + 1) + (4 * y * x)) * 0, 0)
  expect_equal((x + 1) * 0, 0)
})

test_that("subtracting variables work", {
  x <- moi_scalar_affine_term(1, RMPK_variable(index = 1L))
  y <- moi_scalar_affine_term(1, RMPK_variable(index = 2L))
  result <- x + x - y
  vars <- canonicalize(result)@terms
  coefs <- vapply(vars, function(x) x@coefficient, numeric(1))
  expect_setequal(coefs, c(2, -1))
})

test_that("squared linear expression", {
  x <- moi_scalar_affine_term(1, RMPK_variable(index = 1L))
  result <- canonicalize((x - 10)^2)
  expect_equal(result@quadratic_terms[[1]]@coefficient, 1)
  expect_equal(result@affine_terms[[1]]@coefficient, -20)
  expect_equal(result@constant, 100)
})

test_that("linear expression times variable", {
  x <- moi_scalar_affine_term(1, RMPK_variable(index = 1L))
  expect_silent(result <- canonicalize((-3 * x + 1) * x))
  quad_vars <- result@quadratic_terms
  lin_vars <- result@affine_terms
  expect_equal(quad_vars[[1]]@coefficient, -3)
  expect_equal(quad_vars[[1]]@variable1@value, 1)
  expect_equal(quad_vars[[1]]@variable2@value, 1)
  expect_equal(result@constant, 0)
  expect_equal(lin_vars[[1]]@variable, x@variable)
})

test_that("pow of difference of two variables works", {
  x <- moi_scalar_affine_term(1, RMPK_variable(index = 1L))
  y <- moi_scalar_affine_term(1, RMPK_variable(index = 2L))
  expect_silent(result <- canonicalize((x - y)^2))
  quad_part <- result@quadratic_terms
  expect_equal(length(quad_part), 3)
})

test_that("difference on number and variable works", {
  y <- moi_scalar_affine_term(1, RMPK_variable(index = 1L))
  expect_silent(result <- y - (1 - y))
})
