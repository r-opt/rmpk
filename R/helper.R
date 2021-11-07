#' @importFrom rlang enquos
#' @importFrom rlang eval_tidy
#' @importFrom rlang quo
construct_quantifiers <- function(...) {
  quosures <- enquos(...)
  is_index <- names(quosures) != ""
  quantifiers <- eval_tidy(
    quo(expand.grid(!!!quosures[is_index], stringsAsFactors = FALSE))
  )
  filter_guard <- Reduce(function(acc, el) {
    quo(!!acc & !!el)
  }, quosures[!is_index], init = TRUE)
  eval_tidy(
    quo(
      quantifiers[!!filter_guard, , drop = FALSE]
    ),
    data = quantifiers
  )
}
