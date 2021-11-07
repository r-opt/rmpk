#' Add a scalar affine function and a numeric
#' @include variable-methods.R
#' @param e1 A scalar affine function
#' @param e2 A scalar numeric
setMethod("+", signature(e1 = "MOI_scalar_affine_function", e2 = "numeric"), function(e1, e2) {
  slot(e1, "constant", check = FALSE) <- e1@constant + e2
  e1
})

#' Add a scalar affine function and a numeric
#' @param e1 A scalar numeric
#' @param e2 A scalar affine function
setMethod("+", signature(e1 = "numeric", e2 = "MOI_scalar_affine_function"), function(e1, e2) {
  slot(e2, "constant", check = FALSE) <- e2@constant + e1
  e2
})

#' Add a scalar affine function and a scalar affine term
#' @param e1 A scalar affine function
#' @param e2 A scalar affine term
setMethod("+", signature(e1 = "MOI_scalar_affine_function", e2 = "MOI_scalar_affine_term"), function(e1, e2) {
  slot(e1, "terms", check = FALSE) <- c(e1@terms, list(e2))
  e1
})

#' Add a scalar affine function and a scalar affine term
#' @param e1 A scalar affine term
#' @param e2 A scalar affine function
setMethod("+", signature(e1 = "MOI_scalar_affine_term", e2 = "MOI_scalar_affine_function"), function(e1, e2) {
  e2 + e1
})

#' Add a scalar affine function and a scalar affine function
#' @param e1 A scalar affine function
#' @param e2 A scalar affine function
#' @include helper.R
setMethod("+", signature(e1 = "MOI_scalar_affine_function", e2 = "MOI_scalar_affine_function"), function(e1, e2) {
  slot(e1, "terms", check = FALSE) <- c(e1@terms, e2@terms) # O(n)
  slot(e1, "constant", check = FALSE) <- e1@constant + e2@constant
  e1
})

#' Substract a scalar affine function and a scalar affine function
#' @param e1 A scalar affine function
#' @param e2 A scalar affine function
setMethod("-", signature(e1 = "MOI_scalar_affine_function", e2 = "MOI_scalar_affine_function"), function(e1, e2) {
  # TODO: room for optimization here
  e1 + -1 * e2
})

#' Substract a scalar affine function and a scalar affine term
#' @param e1 A scalar affine function
#' @param e2 A scalar affine term
setMethod("-", signature(e1 = "MOI_scalar_affine_function", e2 = "MOI_scalar_affine_term"), function(e1, e2) {
  e1 + -1 * e2
})

#' Substract a scalar affine function and a scalar affine term
#' @param e1 A scalar affine term
#' @param e2 A scalar affine function
setMethod("-", signature(e1 = "MOI_scalar_affine_term", e2 = "MOI_scalar_affine_function"), function(e1, e2) {
  e1 + -1 * e2
})

#' Substract a scalar affine function and a numeric
#' @param e1 A scalar affine function
#' @param e2 A numeric
setMethod("-", signature(e1 = "MOI_scalar_affine_function", e2 = "numeric"), function(e1, e2) {
  slot(e1, "constant", check = FALSE) <- e1@constant - e2
  e1
})

#' Substract a scalar affine function and a numeric
#' @param e1 A numeric
#' @param e2 A scalar affine function
setMethod("-", signature(e1 = "numeric", e2 = "MOI_scalar_affine_function"), function(e1, e2) {
  (-1 * e2) - (-1 * e1)
})

#' Multiply a scalar affine function and a numeric
#' @param e1 A scalar affine function
#' @param e2 A numeric
setMethod("*", signature(e1 = "MOI_scalar_affine_function", e2 = "numeric"), function(e1, e2) {
  if (e2 == 0) {
    return(e2)
  }
  slot(e1, "constant", check = FALSE) <- e1@constant * e2
  slot(e1, "terms", check = FALSE) <- lapply(e1@terms, function(x) {
    x * e2
  })
  e1
})

#' Multiply a scalar affine function and a numeric
#' @param e1 A numeric
#' @param e2 A scalar affine function
setMethod("*", signature(e1 = "numeric", e2 = "MOI_scalar_affine_function"), function(e1, e2) {
  e2 * e1
})

#' Multiply a scalar affine function and a scalar affine term
#' @param e1 A scalar affine function
#' @param e2 A scalar affine term
setMethod("*", signature(e1 = "MOI_scalar_affine_function", e2 = "MOI_scalar_affine_term"), function(e1, e2) {
  Reduce(function(acc, el) {
    acc + e2 * el
  }, e1@terms, init = 0) + e2 * e1@constant
})

#' Multiply a scalar affine function and a scalar affine term
#' @param e1 A scalar affine term
#' @param e2 A scalar affine function
setMethod("*", signature(e1 = "MOI_scalar_affine_term", e2 = "MOI_scalar_affine_function"), function(e1, e2) {
  e2 * e1
})

#' Multiply two scalar affine functions
#' @param e1 A scalar affine function
#' @param e2 A scalar affine function
setMethod("*", signature(e1 = "MOI_scalar_affine_function", e2 = "MOI_scalar_affine_function"), function(e1, e2) {
  Reduce(function(acc, el) {
    acc + e1 * el
  }, e2@terms, init = 0) + e1 * e2@constant
})

#' Exponentiate a scalar affine function by a numeric
#' @param e1 A scalar affine function
#' @param e2 A numeric of value 2, anything else is not supported
setMethod("^", signature(e1 = "MOI_scalar_affine_function", e2 = "numeric"), function(e1, e2) {
  stopifnot(e2 == 2)
  e1 * e1
})
