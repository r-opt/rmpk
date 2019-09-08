#' @include variable-methods.R
#' @include linear-expression-class.R
setMethod("+", signature(e1 = "RMPKLinearExpression", e2 = "numeric"), function(e1, e2) {
  e1@constant <- e1@constant + e2
  e1
})

setMethod("+", signature(e1 = "numeric", e2 = "RMPKLinearExpression"), function(e1, e2) {
  e2 + e1
})

setMethod("+", signature(e1 = "RMPKLinearExpression", e2 = "RLPVariable"), function(e1, e2) {
  e1@variables <- merge_variables(e1@variables, var_to_map(e2)) # O(n)
  e1
})

setMethod("+", signature(e1 = "RLPVariable", e2 = "RMPKLinearExpression"), function(e1, e2) {
  e2 + e1
})

#' @include helper.R
setMethod("+", signature(e1 = "RMPKLinearExpression", e2 = "RMPKLinearExpression"), function(e1, e2) {
  e1@variables <- merge_variables(e1@variables, e2@variables) # O(n)
  e1@constant <- e1@constant + e2@constant
  e1
})

setMethod("-", signature(e1 = "RMPKLinearExpression", e2 = "RMPKLinearExpression"), function(e1, e2) {
  e1 + -1 * e2
})

setMethod("-", signature(e1 = "RMPKLinearExpression", e2 = "numeric"), function(e1, e2) {
  e1 + -1 * e2
})

setMethod("-", signature(e1 = "numeric", e2 = "RMPKLinearExpression"), function(e1, e2) {
  (-1 * e2) - (-1 * e1)
})

setMethod("*", signature(e1 = "RMPKLinearExpression", e2 = "numeric"), function(e1, e2) {
  e1@constant <- e1@constant * e2
  variables <- e1@variables
  for (key in variables$keys()) {
    var <- variables$get(key)
    var@coefficient <- var@coefficient * e2
    variables$set(key, var)
  }
  e1@variables <- variables #not really necessariy
  e1
})

setMethod("*", signature(e1 = "numeric", e2 = "RMPKLinearExpression"), function(e1, e2) {
  e2 * e1
})
