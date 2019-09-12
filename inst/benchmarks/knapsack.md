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
    ##         expr       min        lq      mean    median       uq       max
    ##        roi()  6.365677  6.953112  9.002122  7.435448 10.11517  21.68163
    ##       rmpk() 12.642395 13.750954 18.942623 16.038571 21.58338 106.77154
    ##  rmpk_pipe() 12.969410 13.984766 17.912392 15.345006 22.12276  36.49844
    ##  neval
    ##    100
    ##    100
    ##    100
