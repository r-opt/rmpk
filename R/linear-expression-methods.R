#' @include variable-methods.R
setMethod("+", signature(e1 = "MOI_scalar_affine_function", e2 = "numeric"), function(e1, e2) {
  slot(e1, "constant", check = FALSE) <- e1@constant + e2
  e1
})

setMethod("+", signature(e1 = "numeric", e2 = "MOI_scalar_affine_function"), function(e1, e2) {
  slot(e2, "constant", check = FALSE) <- e2@constant + e1
  e2
})

setMethod("+", signature(e1 = "MOI_scalar_affine_function", e2 = "MOI_scalar_affine_term"), function(e1, e2) {
  slot(e1, "terms", check = FALSE) <- c(e1@terms, list(e2))
  e1
})

setMethod("+", signature(e1 = "MOI_scalar_affine_term", e2 = "MOI_scalar_affine_function"), function(e1, e2) {
  e2 + e1
})

#' @include helper.R
setMethod("+", signature(e1 = "MOI_scalar_affine_function", e2 = "MOI_scalar_affine_function"), function(e1, e2) {
  slot(e1, "terms", check = FALSE) <- c(e1@terms, e2@terms) # O(n)
  slot(e1, "constant", check = FALSE) <- e1@constant + e2@constant
  e1
})

setMethod("-", signature(e1 = "MOI_scalar_affine_function", e2 = "MOI_scalar_affine_function"), function(e1, e2) {
  # TODO: room for optimization here
  e1 + -1 * e2
})

setMethod("-", signature(e1 = "MOI_scalar_affine_function", e2 = "MOI_scalar_affine_term"), function(e1, e2) {
  e1 + -1 * e2
})

setMethod("-", signature(e1 = "MOI_scalar_affine_term", e2 = "MOI_scalar_affine_function"), function(e1, e2) {
  e1 + -1 * e2
})

setMethod("-", signature(e1 = "MOI_scalar_affine_function", e2 = "numeric"), function(e1, e2) {
  slot(e1, "constant", check = FALSE) <- e1@constant - e2
  e1
})

setMethod("-", signature(e1 = "numeric", e2 = "MOI_scalar_affine_function"), function(e1, e2) {
  (-1 * e2) - (-1 * e1)
})

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

setMethod("*", signature(e1 = "numeric", e2 = "MOI_scalar_affine_function"), function(e1, e2) {
  e2 * e1
})

setMethod("*", signature(e1 = "MOI_scalar_affine_function", e2 = "MOI_scalar_affine_term"), function(e1, e2) {
  Reduce(function(acc, el) {
    acc + e2 * el
  }, e1@terms, init = 0) + e2 * e1@constant
})

setMethod("*", signature(e1 = "MOI_scalar_affine_term", e2 = "MOI_scalar_affine_function"), function(e1, e2) {
  e2 * e1
})

setMethod("*", signature(e1 = "MOI_scalar_affine_function", e2 = "MOI_scalar_affine_function"), function(e1, e2) {
  Reduce(function(acc, el) {
    acc + e1 * el
  }, e2@terms, init = 0) + e1 * e2@constant
})

setMethod("^", signature(e1 = "MOI_scalar_affine_function", e2 = "numeric"), function(e1, e2) {
  stopifnot(e2 == 2)
  e1 * e1
})
