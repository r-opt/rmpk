#' @export
setClass("RMPKQuadraticExpression", slots = c(quadratic_variables = "ANY", linear_part = "RMPKLinearExpression"))


#' @export
setClass("RMPKQuadraticVariableTuple", slots = c(variable1 = "RLPVariable", variable2 = "RLPVariable", coefficient = "numeric"))

is_quadratic_expression <- function(x) {
  inherits(x, c("RMPKQuadraticExpression", "RMPKQuadraticVariableTuple"))
}
