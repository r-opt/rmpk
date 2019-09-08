#' @export
setClass("RMPKLinearExpression", slots = c(variables = "list", constant = "numeric"))

is_linear_expression <- function(x) "RMPKLinearExpression" %in% class(x)
