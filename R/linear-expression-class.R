#' @export
setClass("RMPKLinearExpression", slots = c(variables = "ANY", constant = "numeric"))

is_linear_expression <- function(x) "RMPKLinearExpression" %in% class(x)
