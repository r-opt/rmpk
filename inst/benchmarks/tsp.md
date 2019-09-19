TSP
================

``` r
# Model reference https://www.unc.edu/~pataki/papers/teachtsp.pdf
library(rmpk)
library(ROI)
```

    ## ROI: R Optimization Infrastructure

    ## Registered solver plugins: nlminb, alabama, cbc, glpk, quadprog.

    ## Default solver: auto.

``` r
library(ROI.plugin.glpk)
set.seed(42)

n <- 50
max_x <- 500
max_y <- 500
cities <- data.frame(id = 1:n, x = runif(n, max = max_x), y = runif(n, max = max_y))
distance <- as.matrix(dist(dplyr::select(cities, x, y), diag = TRUE, upper = TRUE))

rmpk <- function(solver = ROI_solver("glpk")) {
  model <- MIPModel(solver)
  
  # we create a variable that is 1 iff we travel from city i to j
  model$add_variable(x[i, j], i = 1:n, j = 1:n, 
               type = "integer", lb = 0, ub = 1)
  
  # a helper variable for the MTZ formulation of the tsp
  model$add_variable(u[i], i = 1:n, lb = 1, ub = n)
  
  # minimize travel distance
  model$set_objective(sum_expr(distance[i, j] * x[i, j], i = 1:n, j = 1:n), "min")
  
  # you cannot go to the same city
  # bounds not yet supported
  # set_bounds(x[i, i], ub = 0, i = 1:n) %>%
  model$add_constraint(x[i, i] == 0, i = 1:n)
  # leave each city
  model$add_constraint(sum_expr(x[i, j], j = 1:n) == 1, i = 1:n)
  
  # visit each city
  model$add_constraint(sum_expr(x[i, j], i = 1:n) == 1, j = 1:n)
  
  # ensure no subtours (arc constraints)
  model$add_constraint(u[i] >= 2, i = 2:n)
  model$add_constraint(u[i] - u[j] + 1 <= (n - 1) * (1 - x[i, j]), i = 2:n, j = 2:n)
  
}

microbenchmark::microbenchmark(rmpk(), times = 5)
```

    ## Unit: seconds
    ##    expr      min       lq     mean   median       uq      max neval
    ##  rmpk() 6.946105 7.383062 8.764974 7.406077 9.461817 12.62781     5

``` r
#system.time(rmpk(solver = rmpk.glpk::GLPK()))
```

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
    ##  rmpk(solver = NoOPSolver$new()) 2.035732 2.182463 2.324922 2.380111
    ##        uq      max neval
    ##  2.466109 2.560195     5

``` r
profvis::profvis(rmpk(solver = NoOPSolver$new()))
```
