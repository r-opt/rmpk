TSP
================

``` r
# Model reference https://www.unc.edu/~pataki/papers/teachtsp.pdf
library(ROI)
```

    ## ROI: R Optimization Infrastructure

    ## Registered solver plugins: nlminb, alabama, cbc, glpk, quadprog.

    ## Default solver: auto.

``` r
library(ROI.plugin.glpk)
library(magrittr)
set.seed(42)

n <- 50
max_x <- 500
max_y <- 500
cities <- data.frame(id = 1:n, x = runif(n, max = max_x), y = runif(n, max = max_y))
distance <- as.matrix(dist(dplyr::select(cities, x, y), diag = TRUE, upper = TRUE))

rmpk <- function(solver = rmpk::ROI_solver("glpk")) {
  withr::with_package("rmpk", {
  model <- rmpk::MIPModel(solver)
  
  # we create a variable that is 1 iff we travel from city i to j
  x <- model$add_variable(i = 1:n, j = 1:n, 
               type = "integer", lb = 0, ub = 1)
  
  # a helper variable for the MTZ formulation of the tsp
  u <- model$add_variable(i = 1:n, lb = 1, ub = n)
  
  # minimize travel distance
  model$set_objective(sum_expr(distance[i, j] * x[i, j], i = 1:n, j = 1:n), "min")
  
  # you cannot go to the same city
  # bounds not yet supported
  model$set_bounds(x[i, i], ub = 0, i = 1:n)
  
  # leave each city
  model$add_constraint(sum_expr(x[i, j], j = 1:n) == 1, i = 1:n)
  
  # visit each city
  model$add_constraint(sum_expr(x[i, j], i = 1:n) == 1, j = 2:n)
  
  # ensure no subtours (arc constraints)
  model$add_constraint(u[i] >= 2, i = 2:n)
  model$add_constraint(u[i] - u[j] + 1 <= (n - 1) * (1 - x[i, j]), i = 2:n, j = 2:n)
  })
}

ompr_model <- function() {
  withr::with_package("ompr", {
    MIPModel() %>% 
    add_variable(x[i,j], i = 1:n, j = 1:n, 
               type = "integer", lb = 0, ub = 1) %>% 
    add_variable(u[i], i = 1:n, lb = 1, ub = n) %>% 
    set_objective(sum_expr(distance[i, j] * x[i, j], i = 1:n, j = 1:n), "min") %>% 
    set_bounds(x[i, i], ub = 0, i = 1:n) %>% 
    add_constraint(sum_expr(x[i, j], j = 1:n) == 1, i = 1:n) %>% 
    add_constraint(sum_expr(x[i, j], i = 1:n) == 1, j = 2:n) %>% 
    add_constraint(u[i] >= 2, i = 2:n) %>% 
    add_constraint(u[i] - u[j] + 1 <= (n - 1) * (1 - x[i, j]), i = 2:n, j = 2:n)
  })
}

microbenchmark::microbenchmark(rmpk(), ompr_model(), times = 5)
```

    ## Unit: seconds
    ##          expr       min        lq      mean    median        uq       max
    ##        rmpk()  5.689904  5.915012  6.224658  6.133286  6.190224  7.194861
    ##  ompr_model() 12.136823 12.768666 15.263420 14.313765 16.463583 20.634265
    ##  neval
    ##      5
    ##      5

``` r
system.time(rmpk(solver = rmpk.glpk::GLPK()))
```

    ##    user  system elapsed 
    ##   2.449   0.041   2.858

``` r
# the fastest solver
NoOPSolver <- R6::R6Class(
  "NoOPSolver",
  portable = TRUE,
  public = list(
    add_variable = function(type, lower_bound = -Inf, upper_bound = Inf) {
      0L
    },
    add_linear_constraint = function(linear_expr, type, rhs) {
      0L
    },
    set_linear_objective = function(linear_expr, sense) {
    },
    add_quadratic_constraint = function(quadratic_expr, type, rhs) {
    },
    set_quadratic_objective = function(quadratic_expr, sense) {
    },
    set_variable_lb = function(variable_index, value) {
    },
    set_variable_ub = function(variable_index, value) {
    },
    nvars = function() {
      0
    },
    nconstraints = function() {
      0
    },
    optimize = function() {
    },
    get_variable_value = function(var_index) {
    },
    get_variable_dual = function(var_index) {
    },
    get_row_dual = function(row_index) {
    },
    set_variable_value = function(var_index, value) {
    },
    get_objective_value = function() {
    },
    get_termination_status = function() {
    }
  )
)
```

``` r
microbenchmark::microbenchmark(rmpk(solver = NoOPSolver$new()), times = 5)
```

    ## Unit: seconds
    ##                             expr      min       lq     mean   median
    ##  rmpk(solver = NoOPSolver$new()) 1.601221 1.675597 1.757414 1.714991
    ##        uq      max neval
    ##  1.770802 2.024459     5

``` r
profvis::profvis(rmpk(solver = NoOPSolver$new()))
```
