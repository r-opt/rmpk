var_by_index <- function(variable_list, index) {
  (Find(function(x) x@variable@value == index, variable_list))
}

test_constraint_combinations <- function(x) {
  res <- 5 <= x
  expect_s4_class(res, "RMPK_constraint")
  expect_equal(res@set@upper, -5)
  res <- x <= 5
  expect_s4_class(res, "RMPK_constraint")
  expect_equal(res@set@upper, 5)
  res <- x <= x
  expect_s4_class(res, "RMPK_constraint")
  expect_equal(res@set@upper, 0)

  res <- 5 == x
  expect_s4_class(res, "RMPK_constraint")
  expect_equal(res@set@value, 5)
  res <- x == 5
  expect_s4_class(res, "RMPK_constraint")
  expect_equal(res@set@value, 5)
  res <- x == x
  expect_s4_class(res, "RMPK_constraint")
  expect_equal(res@set@value, 0)

  res <- 5 >= x
  expect_s4_class(res, "RMPK_constraint")
  expect_equal(res@set@lower, -5)
  res <- x >= 5
  expect_s4_class(res, "RMPK_constraint")
  expect_equal(res@set@lower, 5)
  res <- x >= x
  expect_s4_class(res, "RMPK_constraint")
  expect_equal(res@set@lower, 0)
}
