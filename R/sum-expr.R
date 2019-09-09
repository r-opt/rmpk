make_sum_expr <- function(enclosing_envir) {
  function(expr, ...) {
    expr <- substitute(expr)
    quantifiers <- expand.grid(..., stringsAsFactors = FALSE)
    quantifier_names <- names(quantifiers)
    quantifiers_len <- ncol(quantifiers)
    row_indexes <- seq_len(nrow(quantifiers))
    evaluated_expressions <- lapply(row_indexes, function(i) {
      row <- numeric(quantifiers_len)
      for (j in 1:quantifiers_len) {
        row[[j]] <- quantifiers[[j]][[i]]
      }
      envir <- new.env(parent = enclosing_envir)
      for (j in 1:quantifiers_len) {
        envir[[quantifier_names[[j]]]] <- row[[j]]
      }
      eval(expr, envir = envir)
    })
    Reduce(`+`, evaluated_expressions)
  }
}
