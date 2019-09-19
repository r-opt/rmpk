#' Helper function to sum over a set of indexes
#'
#' @param expr A numeric, linear or quadratic expression
#' @param ... a number named indexes
#'
#' @export
sum_expr <- function(expr, ...) {
  expr <- rlang::enquo(expr)

  quantifiers <- expand.grid(..., stringsAsFactors = FALSE)
  quantifier_names <- names(quantifiers)
  row_indexes <- seq_len(nrow(quantifiers))

  # we put all column vectors of quantifiers into an environment for fast acccess
  quantifier_env <- as.environment(quantifiers)

  # we usually have very few quantifiers, so there is probably too much overhead
  # when using a hashtable (according to some benchmarks)
  envir <- new.env(parent = rlang::get_env(expr), hash = FALSE)
  bare_expr <- rlang::get_expr(expr)

  result <- 0
  for (i in row_indexes) {
    for (name in quantifier_names) {
      envir[[name]] <- quantifier_env[[name]][i]
    }
    result <- result + eval(bare_expr, envir = envir, baseenv())
  }
  result
}
