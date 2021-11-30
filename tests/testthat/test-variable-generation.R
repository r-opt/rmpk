test_that("generate variable names from a single variable", {
  res <- generate_variable_names()
  expect_equal(res$is_indexed_var, FALSE)
  expect_equal(res$arity, 0)
})

test_that("generate variable names indexed variables", {
  res <- generate_variable_names(i = 1:2, j = c("a", "b"))
  expect_equal(res$arity, 2)
  expect_equal(res$var_names, c("1/a", "1/b", "2/a", "2/b"))
  expect_equal(res$is_indexed_var, TRUE)
})

test_that("it extracts the type information of the quantifiers", {
  res <- generate_variable_names(i = 1:2, j = c("a", "b"))
  expect_equal(res$index_types, c("integer", "character"))
})
