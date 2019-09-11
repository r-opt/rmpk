#' @include linear-expression-class.R
#' @include variable-class.R
#' @include quadratic-expression-class.R
NULL

setMethod("*", signature(e1 = "RLPVariable", e2 = "RLPVariable"), function(e1, e2) {
  new("RMPKQuadraticVariableTuple", variable1 = e1, variable2 = e2, coefficient = e1@coefficient * e2@coefficient)
})

setMethod("^", signature(e1 = "RLPVariable", e2 = "numeric"), function(e1, e2) {
  stopifnot(e2 == 2)
  new("RMPKQuadraticVariableTuple", variable1 = e1, variable2 = e1, coefficient = e1@coefficient^2)
})

setMethod("*", signature(e1 = "RMPKQuadraticVariableTuple", e2 = "numeric"), function(e1, e2) {
  e1@coefficient <- e1@coefficient * e2
  e1
})

setMethod("*", signature(e1 = "numeric", e2 = "RMPKQuadraticVariableTuple"), function(e1, e2) {
  e2 * e1
})

setMethod("*", signature(e1 = "RMPKQuadraticExpression", e2 = "numeric"), function(e1, e2) {
  keys <- e1@quadratic_variables$keys()
  for (key in keys) {
    var <- e1@quadratic_variables$get(key)
    new_var <- var * e2
    e1@quadratic_variables$set(key, new_var)
  }
  e1@linear_part <- e1@linear_part * e2
  e1
})

setMethod("*", signature(e1 = "numeric", e2 = "RMPKQuadraticExpression"), function(e1, e2) {
  e2 * e1
})

setMethod("+", signature(e1 = "RMPKQuadraticVariableTuple", e2 = "numeric"), function(e1, e2) {
  quad_vars <- fastmap::fastmap()
  indexes <- c(e1@variable1@variable_index, e1@variable2@variable_index)
  key <- paste0(sort(indexes), collapse = "_")
  quad_vars$set(key, e1)
  new("RMPKQuadraticExpression",
    quadratic_variables = quad_vars,
    linear_part = new("RMPKLinearExpression", variables = fastmap::fastmap(), constant = e2)
  )
})

setMethod("+", signature(e1 = "numeric", e2 = "RMPKQuadraticVariableTuple"), function(e1, e2) {
  e2 + e1
})

setMethod("-", signature(e1 = "RMPKQuadraticVariableTuple", e2 = "numeric"), function(e1, e2) {
  e1 + (-1 * e2)
})

setMethod("-", signature(e1 = "numeric", e2 = "RMPKQuadraticVariableTuple"), function(e1, e2) {
  (-1 * e2) - (-1 * e1)
})

setMethod("+", signature(e1 = "RMPKQuadraticVariableTuple", e2 = "RMPKQuadraticVariableTuple"), function(e1, e2) {
  quad_vars <- fastmap::fastmap()
  key1 <- quad_variable_tuple_key(e1)
  key2 <- quad_variable_tuple_key(e2)
  if (key1 == key2) {
    e1@coefficient <- e1@coefficient + e2@coefficient
    return(e1)
  }
  quad_vars$set(key1, e1)
  quad_vars$set(key2, e2)
  new("RMPKQuadraticExpression",
    quadratic_variables = quad_vars,
    linear_part = new("RMPKLinearExpression", variables = fastmap::fastmap(), constant = 0)
  )
})

setMethod("+", signature(e1 = "RMPKQuadraticVariableTuple", e2 = "RLPVariable"), function(e1, e2) {
  quad_vars <- fastmap::fastmap()
  key <- quad_variable_tuple_key(e1)
  quad_vars$set(key, e1)
  linear_part <- new("RMPKLinearExpression", variables = fastmap::fastmap(), constant = 0)

  new("RMPKQuadraticExpression",
    quadratic_variables = quad_vars,
    linear_part = linear_part + e2
  )
})

setMethod("+", signature(e1 = "RLPVariable", e2 = "RMPKQuadraticVariableTuple"), function(e1, e2) {
  e2 + e1
})

setMethod("-", signature(e1 = "RMPKQuadraticVariableTuple", e2 = "RLPVariable"), function(e1, e2) {
  e1 + (-1 * e2)
})

setMethod("-", signature(e1 = "RLPVariable", e2 = "RMPKQuadraticVariableTuple"), function(e1, e2) {
  (-1 * e2) - (-1 * e1)
})

setMethod("+", signature(e1 = "RMPKQuadraticExpression", e2 = "numeric"), function(e1, e2) {
  e1@linear_part <- e1@linear_part + e2
  e1
})

setMethod("+", signature(e1 = "numeric", e2 = "RMPKQuadraticExpression"), function(e1, e2) {
  e2 + e1
})

setMethod("+", signature(e1 = "RMPKQuadraticExpression", e2 = "RLPVariable"), function(e1, e2) {
  e1@linear_part <- e1@linear_part + e2
  e1
})

setMethod("+", signature(e1 = "RMPKQuadraticExpression", e2 = "RMPKQuadraticVariableTuple"), function(e1, e2) {
  key2 <- quad_variable_tuple_key(e2)
  if (e1@quadratic_variables$has(key2)) {
    var <- e1@quadratic_variables$get(key2)
    var@coefficient <- var@coefficient + e2@coefficient
    e1@quadratic_variables$set(key2, var)
  } else {
    e1@quadratic_variables$set(key2, e2)
  }
  e1
})

setMethod("+", signature(e1 = "RMPKQuadraticExpression", e2 = "RMPKQuadraticExpression"), function(e1, e2) {
  for (key in e2@quadratic_variables$keys()) {
    var2 <- e2@quadratic_variables$get(key)
    if (e1@quadratic_variables$has(key)) {
      var1 <- e1@quadratic_variables$get(key)
      var1@coefficient <- var1@coefficient + var2@coefficient
      e1@quadratic_variables$set(key, var1)
    } else {
      e1@quadratic_variables$set(key, var2)
    }
  }
  e1@linear_part <- e1@linear_part + e2@linear_part
  e1
})

setMethod("+", signature(e1 = "RMPKQuadraticVariableTuple", e2 = "RMPKQuadraticExpression"), function(e1, e2) {
  e2 + e1
})

setMethod("-", signature(e1 = "RMPKQuadraticExpression", e2 = "RMPKQuadraticVariableTuple"), function(e1, e2) {
  e1 + (-1 * e2)
})

setMethod("-", signature(e1 = "RMPKQuadraticVariableTuple", e2 = "RMPKQuadraticExpression"), function(e1, e2) {
  (-1 * e2) - (-1 * e1)
})

setMethod("-", signature(e1 = "RMPKQuadraticExpression", e2 = "numeric"), function(e1, e2) {
  e1 + (-1 * e2)
})

setMethod("-", signature(e1 = "numeric", e2 = "RMPKQuadraticExpression"), function(e1, e2) {
  (-1 * e2) - (-1 * e1)
})

setMethod("+", signature(e1 = "RLPVariable", e2 = "RMPKQuadraticExpression"), function(e1, e2) {
  e2 + e1
})

setMethod("-", signature(e1 = "RMPKQuadraticExpression", e2 = "RLPVariable"), function(e1, e2) {
  e1 + (-1 * e2)
})

setMethod("-", signature(e1 = "RLPVariable", e2 = "RMPKQuadraticExpression"), function(e1, e2) {
  (-1 * e2) - (-1 * e1)
})

quad_variable_tuple_key <- function(var) {
  indexes <- c(var@variable1@variable_index, var@variable2@variable_index)
  paste0(sort(indexes), collapse = "_")
}

ensure_quadratic_expression <- function(expr) {
  if ("RMPKQuadraticExpression" %in% class(expr)) {
    return(expr)
  }
  if ("RMPKQuadraticVariableTuple" %in% class(expr)) {
    return(expr + 0)
  }
  stop("expr is not well-formed", call. = FALSE)
}
