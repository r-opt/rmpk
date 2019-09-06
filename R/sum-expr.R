make_sum_expr <- function(enclosing_envir) {
  function(expr, ...) {
    expr <- substitute(expr)
    modifiers <- expand.grid(...)
    modifier_names <- names(modifiers)
    modifiers_len <- ncol(modifiers)
    row_indexes <- seq_len(nrow(modifiers))
    evaluated_expressions <- lapply(row_indexes, function(i) {
      row <- numeric(modifiers_len)
      for (j in 1:modifiers_len) {
        row[[j]] <- modifiers[[j]][[i]]
      }
      envir <- new.env(parent = enclosing_envir)
      for (j in 1:modifiers_len) {
        envir[[modifier_names[[j]]]] <- row[[j]]
      }
      eval(expr, envir = envir)
    })
    Reduce(`+`, evaluated_expressions)
  }
}
