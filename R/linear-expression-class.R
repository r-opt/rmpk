#' @export
setClass("RMPKLinearExpression",
  slots = c(variables = "ANY", constant = "numeric"),
  prototype = list(variables = fastmap::fastmap(), constant = 0)
)

is_linear_expression <- function(x) {
  inherits(x, "RMPKLinearExpression")
}
