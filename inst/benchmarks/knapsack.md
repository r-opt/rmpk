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
  model$add_variable(x[i], type = "binary", i = 1:10)
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
    ##    expr      min       lq     mean   median       uq       max neval
    ##   roi() 10.65872 20.34249 29.73607 26.86282 33.96264  81.85952   100
    ##  rmpk() 20.83997 38.80773 56.65038 48.70410 62.20311 309.85208   100
