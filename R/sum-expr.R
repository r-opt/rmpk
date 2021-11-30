#' Helper function to sum over a set of indexes
#'
#' @param expr A numeric, linear or quadratic expression
#' @param ... a number of named indexes or filter expression
#' @importFrom rlang enquo
#' @importFrom rlang enquos
#' @importFrom listcomp gen_list
#' @export
sum_expr <- function(expr, ...) {
  dots <- enquos(...)
  expr <- enquo(expr)
  summands <- gen_list(!!expr, !!!dots, .env = parent.frame())
  Reduce(`+`, summands, init = 0)
}
