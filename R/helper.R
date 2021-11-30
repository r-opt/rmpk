#' @importFrom rlang enquos
#' @importFrom rlang syms
#' @importFrom rlang get_env
#' @importFrom listcomp gen_list
#' @noRd
construct_quantifiers <- function(.env = parent.frame(), ...) {
  # TODO: At the moment, it will silently convert factors to characters
  quosures <- enquos(...)
  if (length(quosures) == 0) {
    return(data.frame())
  }
  is_index <- names(quosures) != ""
  index_symbols <- syms(names(quosures[is_index]))
  do.call(
    rbind,
    gen_list(data.frame(!!!index_symbols), !!!quosures, .env = .env)
  )
}
