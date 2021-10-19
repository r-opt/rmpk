#' @include variable-class.R
#' @include helper.R
setMethod("+", signature(e1 = "MOI_scalar_affine_term", e2 = "numeric"), function(e1, e2) {
  moi_scalar_affine_function(list(e1), e2)
})

setMethod("+", signature(e1 = "numeric", e2 = "MOI_scalar_affine_term"), function(e1, e2) {
  e2 + e1
})

setMethod("+", signature(e1 = "MOI_scalar_affine_term", e2 = "missing"), function(e1, e2) {
  e1
})

setMethod("+", signature(e1 = "MOI_scalar_affine_term", e2 = "MOI_scalar_affine_term"), function(e1, e2) {
  moi_scalar_affine_function(list(e1, e2), 0)
})

setMethod("-", signature(e1 = "MOI_scalar_affine_term", e2 = "numeric"), function(e1, e2) {
  e1 + -1 * e2
})

setMethod("-", signature(e1 = "numeric", e2 = "MOI_scalar_affine_term"), function(e1, e2) {
  (-1 * e2) - (-1 * e1)
})

setMethod("-", signature(e1 = "MOI_scalar_affine_term", e2 = "MOI_scalar_affine_term"), function(e1, e2) {
  e1 + -1 * e2
})

setMethod("-", signature(e1 = "MOI_scalar_affine_term", e2 = "missing"), function(e1, e2) {
  (-1) * e1
})

setMethod("*", signature(e1 = "MOI_scalar_affine_term", e2 = "numeric"), function(e1, e2) {
  e1@coefficient <- e1@coefficient * e2
  e1
})

setMethod("*", signature(e1 = "numeric", e2 = "MOI_scalar_affine_term"), function(e1, e2) {
  e2 * e1
})

setMethod("[", signature("RMPK_variable_list", i = "ANY", j = "ANY", drop = "missing"), function(x, i, j, ..., drop) {
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
