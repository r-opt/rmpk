test_that("generate variable names from a single variable", {
  res <- generate_variable_names(quote(x))
  expect_equal(res$base_name, "x")
  expect_equal(res$var_names, "x")
  expect_equal(res$is_indexed_var, FALSE)
})

test_that("generate variable names indexed variables", {
  res <- generate_variable_names(quote(x[i, j]), i = 1:2, j = c("a", "b"))
  expect_equal(res$base_name, "x")
  expect_equal(res$arity, 2)
  expect_equal(res$var_names, c("x/1/a", "x/2/a", "x/1/b", "x/2/b"))
  expect_equal(res$is_indexed_var, TRUE)
})

test_that("it extracts the type information of the quantifiers", {
  res <- generate_variable_names(quote(x[i, j]), i = 1:2, j = c("a", "b"))
  expect_equal(res$index_types, c("integer", "character"))
})

test_that("it fails if quantifier has wrong type", {
  expect_error(
    generate_variable_names(quote(x[i]), i = as.factor(1:2)),
    regexp = "integer"
  )
})
