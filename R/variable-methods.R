#' Add a scalar affine term and a numeric
#' @param e1 A scalar affine term
#' @param e2 A scalar numeric
#' @include variable-class.R
#' @include helper.R
setMethod("+", signature(e1 = "MOI_scalar_affine_term", e2 = "numeric"), function(e1, e2) {
  moi_scalar_affine_function(list(e1), e2)
})

#' Add a scalar affine term and a numeric
#' @param e1 A scalar numeric
#' @param e2 A scalar affine term
setMethod("+", signature(e1 = "numeric", e2 = "MOI_scalar_affine_term"), function(e1, e2) {
  e2 + e1
})

#' Unary plus of a scalar affine term
#' @param e1 A scalar affine term
#' @param e2 missing
setMethod("+", signature(e1 = "MOI_scalar_affine_term", e2 = "missing"), function(e1, e2) {
  e1
})

#' Add a scalar affine term and a scalar affine term
#' @param e1 A scalar affine term
#' @param e2 A scalar affine term
setMethod("+", signature(e1 = "MOI_scalar_affine_term", e2 = "MOI_scalar_affine_term"), function(e1, e2) {
  moi_scalar_affine_function(list(e1, e2), 0)
})

#' Substract a scalar affine term and a numeric
#' @param e1 A scalar affine term
#' @param e2 A scalar numeric
setMethod("-", signature(e1 = "MOI_scalar_affine_term", e2 = "numeric"), function(e1, e2) {
  e1 + -1 * e2
})

#' Substract a scalar affine term and a numeric
#' @param e2 A scalar affine term
#' @param e1 A scalar numeric
setMethod("-", signature(e1 = "numeric", e2 = "MOI_scalar_affine_term"), function(e1, e2) {
  (-1 * e2) - (-1 * e1)
})

#' Substract a scalar affine term and a scalar affine term
#' @param e1 A scalar affine term
#' @param e2 A scalar affine term
setMethod("-", signature(e1 = "MOI_scalar_affine_term", e2 = "MOI_scalar_affine_term"), function(e1, e2) {
  e1 + -1 * e2
})

#' Unary substraction of a scalar affine term
#' @param e1 A scalar affine term
#' @param e2 missing
setMethod("-", signature(e1 = "MOI_scalar_affine_term", e2 = "missing"), function(e1, e2) {
  (-1) * e1
})

#' Multiply a scalar affine term and a numeric
#' @param e1 A scalar affine term
#' @param e2 A scalar numeric
setMethod("*", signature(e1 = "MOI_scalar_affine_term", e2 = "numeric"), function(e1, e2) {
  e1@coefficient <- e1@coefficient * e2
  e1
})

#' Multiply a scalar affine term and a numeric
#' @param e2 A scalar affine term
#' @param e1 A scalar numeric
setMethod("*", signature(e1 = "numeric", e2 = "MOI_scalar_affine_term"), function(e1, e2) {
  e2 * e1
})

#' Access a variable list
#' @param x a variable list
#' @param i First index
#' @param j Second index
#' @param ... More indexes
#' @param drop missing
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
