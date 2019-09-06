
<!-- README.md is generated from README.Rmd. Please edit that file -->

# High Performance Algebraic Mixed Integer Programming Modelling

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

`rmpk` is a lightweight package to model mixed integer linear programs.
It is based on the API of the
[ompr](https://github.com/dirkschumacher/ompr) package and is also
inspired by the architecture of [Julia
JuMP](https://github.com/JuliaOpt/JuMP.jl).

The goal is to provide a modelling package that can both be used in
packages and also in interactive analyses. It also has a different
architecture as the modelling layer modifies a central solver. That
solver could be an interface to
[ROI](https://CRAN.R-project.org/package=ROI) or a shared pointer to a
specific solver. Thus giving the option to directly communicate with the
solver while still using an algebraic modelling framework.

This is currently work in progress and experimental. I might merge it
with [ompr](https://github.com/dirkschumacher/ompr) but it could also
become the successor of [ompr](https://github.com/dirkschumacher/ompr) …
not sure yet.

Happy to receive feedback\!

*Still under development. Anything can change*

## Installation

You can install the released version of rlp from
[CRAN](https://CRAN.R-project.org) with:

``` r
remotes::install_github("rmpk")
```

## Design Goals and TODOS

  - Almost as fast as matrix code
  - Could be used in packages as well as in interactive analyses
  - Fun to use
  - Optimized for readability
  - Support all features of `ompr`
  - Fix all issues with `ompr`
  - Be able to directly pass values to the solver
  - Lazy constraints / column generation should be possible
  - Better set of solution statuses similiar to JuMP
  - Get row and column duals of LPs
  - initial solutions (not supported by ROI directly AFAIK)
  - It optionally ships a ROI solver binding without any additional
    packages. So you can start right away.
  - Maybe: quadratic programming
  - …

## Low Level ROI Example

``` r
library(rmpk)
library(ROI.plugin.glpk)
set.seed(42)
solver <- ROI_solver("glpk")
v <- rnorm(10)
w <- rnorm(10)
model <- MIPModel(solver)
model$add_variable(x[i], type = "binary", i = 1:10)
model$set_objective(sum_expr(v[i] * x[i], i = 1:10))
model$add_constraint(sum_expr(w[i] * x[i], i = 1:10) <= 10)
model$optimize()
model$get_value(x[i])
#>    name  i value
#> 1     x  9     0
#> 2     x  3     0
#> 3     x  2     1
#> 4     x  8     1
#> 5     x 10     1
#> 6     x  6     1
#> 7     x  5     0
#> 8     x  1     0
#> 9     x  4     0
#> 10    x  7     0
```

## There will be an API that supports pipes

``` r
library(magrittr)
library(rmpk)
library(ROI.plugin.glpk)
solver <- ROI_solver("glpk", control = list(verbose = TRUE))
model <- MIPModel(solver) %>% 
  add_variable(x[i, j], i = 1:10, j = 1:10) %>% 
  set_objective(sum_expr(x[i, j], i = 1:10, j = 1:10)) %>%
  add_constraint(sum_expr(x[i, j], j = 1:i) <= 10, i = 1:10)
```

## Contribute

The best way at the moment to contribute is to test the package, write
documentation, propose features. Soon, code contributions are welcome as
well.

Please note that the ‘rmpk’ project is released with a [Contributor Code
of Conduct](CODE_OF_CONDUCT.md). By contributing to this project, you
agree to abide by its terms.
