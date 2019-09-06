Knapsack
================

You cannot really beat the benchmark here, but I still include it as a
baseline for simple examples.

``` r
library(rmpk)
library(ROI)
```

    ## ROI: R Optimization Infrastructure

    ## Registered solver plugins: nlminb, cbc, glpk.

    ## Default solver: auto.

``` r
library(ROI.plugin.glpk)
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
    ##    expr       min        lq     mean    median       uq       max neval
    ##   roi()  5.610874  6.415033 11.06927  7.811034 14.87915  41.70504   100
    ##  rmpk() 16.307007 18.624825 31.59259 26.221017 36.89769 116.45479   100
