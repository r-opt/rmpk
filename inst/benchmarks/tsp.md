TSP
================

``` r
# Model reference https://www.unc.edu/~pataki/papers/teachtsp.pdf
library(rmpk)
library(ROI)
```

    ## ROI: R Optimization Infrastructure

    ## Registered solver plugins: nlminb, cbc, glpk.

    ## Default solver: auto.

``` r
library(ROI.plugin.glpk)
set.seed(42)

n <- 50
max_x <- 500
max_y <- 500
cities <- data.frame(id = 1:n, x = runif(n, max = max_x), y = runif(n, max = max_y))
distance <- as.matrix(dist(dplyr::select(cities, x, y), diag = TRUE, upper = TRUE))

rmpk <- function() {
  solver <- ROI_solver("glpk")
  
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

system.time(rmpk())
```

    ##    user  system elapsed 
    ##   5.520   0.361   6.087

``` r
profvis::profvis(rmpk())
```
