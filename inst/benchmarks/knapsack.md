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

rmpk_pipe <- function() {
  solver <- ROI_solver("glpk")
  model <- MIPModel(solver) %>% 
    add_variable(x[i], type = "binary", i = 1:10) %>%
    set_objective(sum_expr(v[i] * x[i], i = 1:10)) %>%
    add_constraint(sum_expr(w[i] * x[i], i = 1:10) <= 10)
  model$optimize()
}

microbenchmark::microbenchmark(
  roi(),
  rmpk(),
  rmpk_pipe()
)
```

    ## Unit: milliseconds
    ##         expr       min        lq     mean   median       uq      max neval
    ##        roi()  5.594965  6.350374 10.88386  7.20174 10.23632 150.4404   100
    ##       rmpk() 11.664490 14.051158 20.83366 15.96785 21.00672 201.3955   100
    ##  rmpk_pipe() 11.785360 13.431408 20.46575 16.74110 24.69841 142.0868   100
