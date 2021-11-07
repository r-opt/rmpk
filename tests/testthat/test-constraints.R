test_that("constraints for moi_scalar_affine_term work", {
  x <- moi_scalar_affine_term(1, RMPK_variable(index = 1L))
  test_constraint_combinations(x)
})

test_that("constraints for scalar_affine_functions work", {
  x <- moi_scalar_affine_function(list(), 0)
  test_constraint_combinations(x)
})

test_that("constraints for moi_scalar_quadratic_term work", {
  x <- moi_scalar_quadratic_term(
    41,
    RMPK_variable(index = 1L),
    RMPK_variable(index = 2L)
  )
  test_constraint_combinations(x)
})

test_that("constraints for moi_scalar_quadratic_function work", {
  x <- moi_scalar_quadratic_function(list(), list(), 0)
  test_constraint_combinations(x)
})
