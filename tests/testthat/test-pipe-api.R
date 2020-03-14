library(magrittr)
test_that("ompr pipe api works", {
  model <- MIPModel() %>%
    add_variable(x[i, j], i = 1:10, j = 1:10, lb = 0) %>%
    set_objective(sum_expr(x[i, j], i = 1:10, j = 1:10)) %>%
    add_constraint(sum_expr(x[i, j], i = 1:10, j = 1:10) <= 10) %>%
    set_bounds(x[1, 5], ub = 5) %>%
    solve_model(with_ROI("glpk"))
  res <- get_solution(model, x[i, j])
})
