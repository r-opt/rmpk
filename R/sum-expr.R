make_sum_expr <- function(enclosing_envir) {
  function(expr, ...) {
    expr <- substitute(expr)
    quantifiers <- expand.grid(..., stringsAsFactors = FALSE)
    quantifier_names <- names(quantifiers)
    quantifiers_len <- ncol(quantifiers)
    row_indexes <- seq_len(nrow(quantifiers))
    quantifier_indexes <- seq_len(quantifiers_len)
    result <- 0
    for (i in row_indexes) {
      row <- numeric(quantifiers_len)
      for (j in quantifier_indexes) {
        row[[j]] <- quantifiers[[j]][[i]]
      }
      envir <- new.env(parent = enclosing_envir)
      for (j in quantifier_indexes) {
        envir[[quantifier_names[[j]]]] <- row[[j]]
      }
      result <- result + eval(expr, envir = envir)
    }
    result
  }
}
