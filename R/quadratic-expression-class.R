#' @export
setClass("RMPKQuadraticExpression", slots = c(quadratic_variables = "ANY", linear_part = "RMPKLinearExpression"))


#' @export
setClass("RMPKQuadraticVariableTuple", slots = c(variable1 = "RMPKVariable", variable2 = "RMPKVariable", coefficient = "numeric"))

is_quadratic_expression <- function(x) {
  inherits(x, c("RMPKQuadraticExpression", "RMPKQuadraticVariableTuple"))
}
