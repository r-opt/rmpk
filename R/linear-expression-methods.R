#' @include variable-methods.R
#' @include linear-expression-class.R
setMethod("+", signature(e1 = "RMPKLinearExpression", e2 = "numeric"), function(e1, e2) {
  slot(e1, "constant", check = FALSE) <- e1@constant + e2
  e1
})

setMethod("+", signature(e1 = "numeric", e2 = "RMPKLinearExpression"), function(e1, e2) {
  slot(e2, "constant", check = FALSE) <- e2@constant + e1
  e2
})

setMethod("+", signature(e1 = "RMPKLinearExpression", e2 = "RMPKVariable"), function(e1, e2) {
  slot(e1, "variables", check = FALSE) <- merge_with_single_variable(e1@variables, e2)
  e1
})

setMethod("+", signature(e1 = "RMPKVariable", e2 = "RMPKLinearExpression"), function(e1, e2) {
  e2 + e1
})

#' @include helper.R
setMethod("+", signature(e1 = "RMPKLinearExpression", e2 = "RMPKLinearExpression"), function(e1, e2) {
  slot(e1, "variables", check = FALSE) <- merge_variables(e1@variables, e2@variables) # O(n)
  slot(e1, "constant", check = FALSE) <- e1@constant + e2@constant
  e1
})

setMethod("-", signature(e1 = "RMPKLinearExpression", e2 = "RMPKLinearExpression"), function(e1, e2) {
  # TODO: room for optimization here
  e1 + -1 * e2
})

setMethod("-", signature(e1 = "RMPKLinearExpression", e2 = "RMPKVariable"), function(e1, e2) {
  e1 + -1 * e2
})

setMethod("-", signature(e1 = "RMPKVariable", e2 = "RMPKLinearExpression"), function(e1, e2) {
  e1 + -1 * e2
})

setMethod("-", signature(e1 = "RMPKLinearExpression", e2 = "numeric"), function(e1, e2) {
  slot(e1, "constant", check = FALSE) <- e1@constant - e2
  e1
})

setMethod("-", signature(e1 = "numeric", e2 = "RMPKLinearExpression"), function(e1, e2) {
  (-1 * e2) - (-1 * e1)
})

setMethod("*", signature(e1 = "RMPKLinearExpression", e2 = "numeric"), function(e1, e2) {
  if (e2 == 0) {
    return(e2)
  }
  slot(e1, "constant", check = FALSE) <- e1@constant * e2
  for (var in e1@variables$as_list()) {
    slot(var, "coefficient", check = FALSE) <- var@coefficient * e2
    e1@variables$set(as.character(var@variable_index), var)
  }
  e1
})

setMethod("*", signature(e1 = "numeric", e2 = "RMPKLinearExpression"), function(e1, e2) {
  e2 * e1
})

setMethod("*", signature(e1 = "RMPKLinearExpression", e2 = "RMPKVariable"), function(e1, e2) {
  Reduce(function(acc, el) {
    acc + e2 * el
  }, e1@variables$as_list(), init = 0) + e2 * e1@constant
})

setMethod("*", signature(e1 = "RMPKVariable", e2 = "RMPKLinearExpression"), function(e1, e2) {
  e2 * e1
})

setMethod("*", signature(e1 = "RMPKLinearExpression", e2 = "RMPKLinearExpression"), function(e1, e2) {
  Reduce(function(acc, el) {
    acc + e1 * el
  }, e2@variables$as_list(), init = 0) + e1 * e2@constant
})

setMethod("^", signature(e1 = "RMPKLinearExpression", e2 = "numeric"), function(e1, e2) {
  stopifnot(e2 == 2)
  e1 * e1
})

ensure_linear_expression <- function(expr) {
  if (is_linear_expression(expr)) {
    return(expr)
  }
  if (inherits(expr, "RMPKVariable")) {
    return(expr + 0)
  }
  if (is.numeric(expr)) {
    return(new("RMPKLinearExpression"))
  }
  stop("expr is not well-formed", call. = FALSE)
}
