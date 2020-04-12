construct_quantifiers <- function(...) {
  quosures <- rlang::enquos(...)
  is_index <- names(quosures) != ""
  quantifiers <- rlang::eval_tidy(
    rlang::quo(expand.grid(!!!quosures[is_index], stringsAsFactors = FALSE))
  )
  filter_guard <- Reduce(function(acc, el) {
    rlang::quo(!!acc & !!el)
  }, quosures[!is_index], init = TRUE)
  rlang::eval_tidy(
    rlang::quo(
      quantifiers[!!filter_guard, , drop = FALSE]
    ),
    data = quantifiers
  )
}
