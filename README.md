
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MIP Modelling in R

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.org/dirkschumacher/rmpk.svg?branch=master)](https://travis-ci.org/dirkschumacher/rmpk)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/dirkschumacher/rmpk?branch=master&svg=true)](https://ci.appveyor.com/project/dirkschumacher/rmpk)
[![Codecov test
coverage](https://codecov.io/gh/dirkschumacher/rmpk/branch/master/graph/badge.svg)](https://codecov.io/gh/dirkschumacher/rmpk?branch=master)
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

This is currently work in progress and experimental - but working. I
might merge it with [ompr](https://github.com/dirkschumacher/ompr) but
it could also become the successor of
[ompr](https://github.com/dirkschumacher/ompr) … not sure yet.

If you want to see the package in action take a look at the
[articles](https://dirkschumacher.github.io/rmpk/) in the docs.

Happy to receive feedback\!

*Still under development. Anything can change*

## Installation

You can install the released version of RMPK from
[CRAN](https://CRAN.R-project.org) with:

``` r
remotes::install_github("dirkschumacher/rmpk")
```

## Supported types

  - Linear Programming (LP)
  - Mixed Integer Linear Programming (MILP)
  - Mixed Integer Quadratic Programming (MIQP)
  - Mixed Integer Quadratically Constrained Programming (MIQCP)

## Features

  - ✅ Algebraic modelling of mixed integer programming problems

  - ✅ Integer, binary and continious variables

  - ✅ Linear and quadratic constraints/objective

  - ✅ Bindings to most popular solvers through
    [ROI](https://CRAN.R-project.org/package=ROI)

  - ✅ Support for character variable indexes

  - ✅ Access row/column duals of Linear Programs

  - ✅ Row generation through solver callbacks (e.g. for models with
    exponential many constraints)

  - 🚧 Variable and constraint names

  - 🚧 Initial feasible solutions

  - 🚧 Almost as fast as matrix code

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
x <- model$add_variable("x", type = "binary", i = 1:10)
model$set_objective(sum_expr(v[i] * x[i], i = 1:10), sense = "max")
model$add_constraint(sum_expr(w[i] * x[i], i = 1:10) <= 10)
model$optimize()
model$get_variable_value(x[i])
#>    name  i value
#> 1     x  1     1
#> 2     x  7     1
#> 3     x  5     1
#> 4     x  8     0
#> 5     x  2     0
#> 6     x 10     0
#> 7     x  9     1
#> 8     x  3     1
#> 9     x  6     0
#> 10    x  4     1
```

## Contribute

The best way at the moment to contribute is to test the package, write
documentation, propose features. Soon, code contributions are welcome as
well.

Please note that the ‘rmpk’ project is released with a [Contributor Code
of Conduct](CODE_OF_CONDUCT.md). By contributing to this project, you
agree to abide by its terms.

## License

MIT

## References and Inspiration

  - Dunning, Iain, Joey Huchette, and Miles Lubin. “JuMP: A modeling
    language for mathematical optimization.” SIAM Review 59.2 (2017):
    295-320.
  - [ompr](https://github.com/dirkschumacher/ompr)
  - [pulp](https://github.com/coin-or/pulp)
