Knapsack
================

You cannot really beat the benchmark here, but I still include it as a
baseline for simple examples.

``` r
library(rmpk)
library(ROI)
```

    ## ROI: R Optimization Infrastructure

    ## Registered solver plugins: nlminb, alabama, cbc, glpk, quadprog.

    ## Default solver: auto.

``` r
library(ROI.plugin.glpk)
library(magrittr)
set.seed(42)
v <- rnorm(10)
w <- rnorm(10)

roi <- function() {
  obj <- L_objective(w)
  constr <- L_constraint(v, "<=", 10)
  op <- OP(objective = obj, constraints = constr, types = rep.int("B", 10), maximum = FALSE)
  ROI::ROI_solve(op, "glpk")
}

rmpk <- function() {
  solver <- ROI_solver("glpk")
  model <- MIPModel(solver)
  x <- model$add_variable(type = "binary", i = 1:10)
  model$set_objective(sum_expr(v[i] * x[i], i = 1:10))
  model$add_constraint(sum_expr(w[i] * x[i], i = 1:10) <= 10)
  model$optimize()
}

microbenchmark::microbenchmark(
  roi(),
  rmpk()
)
```

    ## Unit: milliseconds
    ##    expr       min        lq      mean    median        uq      max neval
    ##   roi()  6.390367  6.577635  8.554846  6.952926  9.625678 30.57380   100
    ##  rmpk() 11.964894 12.426721 15.787545 13.744604 17.035401 61.05705   100
