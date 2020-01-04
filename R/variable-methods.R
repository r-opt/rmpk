#' @include variable-class.R
#' @include linear-expression-class.R
#' @include helper.R
setMethod("+", signature(e1 = "RMPKVariable", e2 = "numeric"), function(e1, e2) {
  val <- new("RMPKLinearExpression")
  slot(val, "variables", check = FALSE) <- var_to_map(e1)
  if (e2 != 0) {
    slot(val, "constant", check = FALSE) <- e2
  }
  val
})

setMethod("+", signature(e1 = "numeric", e2 = "RMPKVariable"), function(e1, e2) {
  e2 + e1
})

setMethod("+", signature(e1 = "RMPKVariable", e2 = "missing"), function(e1, e2) {
  e1
})

setMethod("+", signature(e1 = "RMPKVariable", e2 = "RMPKVariable"), function(e1, e2) {
  val <- new("RMPKLinearExpression")
  slot(val, "variables", check = FALSE) <- merge_with_single_variable(var_to_map(e1), e2)
  val
})

setMethod("-", signature(e1 = "RMPKVariable", e2 = "numeric"), function(e1, e2) {
  e1 + -1 * e2
})

setMethod("-", signature(e1 = "numeric", e2 = "RMPKVariable"), function(e1, e2) {
  (-1 * e2) - (-1 * e1)
})

setMethod("-", signature(e1 = "RMPKVariable", e2 = "RMPKVariable"), function(e1, e2) {
  e1 + -1 * e2
})

setMethod("-", signature(e1 = "RMPKVariable", e2 = "missing"), function(e1, e2) {
  (-1) * e1
})

setMethod("*", signature(e1 = "RMPKVariable", e2 = "numeric"), function(e1, e2) {
  if (e2 == 0) {
    return(e2)
  }
  e1@coefficient <- e1@coefficient * e2
  e1
})

setMethod("*", signature(e1 = "numeric", e2 = "RMPKVariable"), function(e1, e2) {
  e2 * e1
})

# --- RMPKVariableList
setMethod("[", signature("RMPKVariableList", i = "ANY", j = "ANY", drop = "missing"), function(x, i, j, ..., drop) {
  indexes <- list()
  if (!missing(i)) {
    indexes[[length(indexes) + 1L]] <- i
  }
  if (!missing(j)) {
    indexes[[length(indexes) + 1L]] <- j
  }
  for (arg in list(...)) {
    indexes[[length(indexes) + 1L]] <- arg
  }
  # stopifnot(all(vapply(indexes, length, integer(1L)) == 1L))
  # TODO: implement this without the linear overhead
  var_name <- paste0(indexes, collapse = "/")
  x@variables_map$get(var_name)
})
