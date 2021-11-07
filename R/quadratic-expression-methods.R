#' Multiply two scalar affine terms
#' @param e1 A scalar affine term
#' @param e2 A scalar affine term
#' @include variable-class.R
setMethod("*", signature(e1 = "MOI_scalar_affine_term", e2 = "MOI_scalar_affine_term"), function(e1, e2) {
  moi_scalar_quadratic_term(
    variable1 = e1@variable,
    variable2 = e2@variable,
    coefficient = e1@coefficient * e2@coefficient
  )
})

#' Exponentiate a scalar affine term by 2
#' @param e1 A scalar affine term
#' @param e2 A scalar numeric of value 2
setMethod("^", signature(e1 = "MOI_scalar_affine_term", e2 = "numeric"), function(e1, e2) {
  stopifnot(e2 == 2)
  moi_scalar_quadratic_term(variable1 = e1@variable, variable2 = e1@variable, coefficient = e1@coefficient^2)
})

#' Multiply a scalar quadratic term and a numeric
#' @param e1 A scalar quadratic term
#' @param e2 A scalar numeric
setMethod("*", signature(e1 = "MOI_scalar_quadratic_term", e2 = "numeric"), function(e1, e2) {
  if (e2 == 0) {
    return(0)
  }
  slot(e1, "coefficient", check = FALSE) <- e1@coefficient * e2
  e1
})

#' Multiply a scalar quadratic term and a numeric
#' @param e1 A scalar numeric
#' @param e2 A scalar quadratic term
setMethod("*", signature(e1 = "numeric", e2 = "MOI_scalar_quadratic_term"), function(e1, e2) {
  e2 * e1
})

#' Multiply a scalar quadratic function and a numeric
#' @param e2 A scalar numeric
#' @param e1 A scalar quadratic function
setMethod("*", signature(e1 = "MOI_scalar_quadratic_function", e2 = "numeric"), function(e1, e2) {
  if (e2 == 0) {
    return(e2)
  }
  slot(e1, "affine_terms", check = FALSE) <- lapply(e1@affine_terms, `*`, e2)
  slot(e1, "quadratic_terms", check = FALSE) <- lapply(e1@quadratic_terms, `*`, e2)
  slot(e1, "constant", check = FALSE) <- e1@constant * e2
  e1
})

#' Multiply a scalar quadratic function and a numeric
#' @param e1 A scalar numeric
#' @param e2 A scalar quadratic function
setMethod("*", signature(e1 = "numeric", e2 = "MOI_scalar_quadratic_function"), function(e1, e2) {
  e2 * e1
})

#' Add a scalar quadratic function and a numeric
#' @param e2 A scalar numeric
#' @param e1 A scalar quadratic function
setMethod("+", signature(e1 = "MOI_scalar_quadratic_term", e2 = "numeric"), function(e1, e2) {
  moi_scalar_quadratic_function(
    constant = e2,
    affine_terms = list(),
    quadratic_terms = list(e1)
  )
})

#' Add a scalar quadratic term and a numeric
#' @param e1 A scalar numeric
#' @param e2 A scalar quadratic term
setMethod("+", signature(e1 = "numeric", e2 = "MOI_scalar_quadratic_term"), function(e1, e2) {
  e2 + e1
})

#' Substract a scalar quadratic term and a numeric
#' @param e2 A scalar numeric
#' @param e1 A scalar quadratic term
setMethod("-", signature(e1 = "MOI_scalar_quadratic_term", e2 = "numeric"), function(e1, e2) {
  e1 + (-1 * e2)
})

#' Substract a scalar quadratic term and a numeric
#' @param e1 A scalar numeric
#' @param e2 A scalar quadratic term
setMethod("-", signature(e1 = "numeric", e2 = "MOI_scalar_quadratic_term"), function(e1, e2) {
  (-1 * e2) - (-1 * e1)
})

#' Add a scalar quadratic term and a scalar quadratic term
#' @param e1 A scalar quadratic term
#' @param e2 A scalar quadratic term
setMethod("+", signature(e1 = "MOI_scalar_quadratic_term", e2 = "MOI_scalar_quadratic_term"), function(e1, e2) {
  moi_scalar_quadratic_function(
    quadratic_terms = list(e1, e2),
    affine_terms = list(),
    constant = 0
  )
})

#' Add a scalar quadratic term and a scalar affine term
#' @param e1 A scalar quadratic term
#' @param e2 A scalar affine term
setMethod("+", signature(e1 = "MOI_scalar_quadratic_term", e2 = "MOI_scalar_affine_term"), function(e1, e2) {
  moi_scalar_quadratic_function(
    quadratic_terms = list(e1),
    affine_terms = list(e2),
    constant = 0
  )
})

#' Add a scalar quadratic term and a scalar affine term
#' @param e1 A scalar affine term
#' @param e2 A scalar quadratic term
setMethod("+", signature(e1 = "MOI_scalar_affine_term", e2 = "MOI_scalar_quadratic_term"), function(e1, e2) {
  e2 + e1
})

#' Substract a scalar quadratic term and a scalar affine term
#' @param e1 A scalar quadratic term
#' @param e2 A scalar affine term
setMethod("-", signature(e1 = "MOI_scalar_quadratic_term", e2 = "MOI_scalar_affine_term"), function(e1, e2) {
  e1 + (-1 * e2)
})

#' Substract a scalar quadratic term and a scalar affine term
#' @param e1 A scalar affine term
#' @param e2 A scalar quadratic term
setMethod("-", signature(e1 = "MOI_scalar_affine_term", e2 = "MOI_scalar_quadratic_term"), function(e1, e2) {
  (-1 * e2) - (-1 * e1)
})

#' Add a scalar quadratic function and a scalar numeric
#' @param e1 A scalar quadratic function
#' @param e2 A scalar numeric
setMethod("+", signature(e1 = "MOI_scalar_quadratic_function", e2 = "numeric"), function(e1, e2) {
  e1@constant <- e1@constant + e2
  e1
})

#' Add a scalar quadratic function and a scalar numeric
#' @param e1 A scalar numeric
#' @param e2 A scalar quadratic function
setMethod("+", signature(e1 = "numeric", e2 = "MOI_scalar_quadratic_function"), function(e1, e2) {
  e2 + e1
})

#' Add a scalar quadratic function and a scalar affine term
#' @param e1 A scalar quadratic function
#' @param e2 A scalar affine term
setMethod("+", signature(e1 = "MOI_scalar_quadratic_function", e2 = "MOI_scalar_affine_term"), function(e1, e2) {
  slot(e1, "affine_terms", check = FALSE) <- c(e1@affine_terms, list(e2))
  e1
})

#' Add a scalar quadratic function and a scalar quadratic term
#' @param e1 A scalar quadratic function
#' @param e2 A scalar quadratic term
setMethod("+", signature(e1 = "MOI_scalar_quadratic_function", e2 = "MOI_scalar_quadratic_term"), function(e1, e2) {
  slot(e1, "quadratic_terms", check = FALSE) <- c(e1@quadratic_terms, list(e2))
  e1
})

#' Add a scalar quadratic function and a scalar quadratic function
#' @param e1 A scalar quadratic function
#' @param e2 A scalar quadratic function
setMethod("+", signature(e1 = "MOI_scalar_quadratic_function", e2 = "MOI_scalar_quadratic_function"), function(e1, e2) {
  slot(e1, "quadratic_terms", check = FALSE) <- c(e1@quadratic_terms, e2@quadratic_terms)
  slot(e1, "affine_terms", check = FALSE) <- c(e1@affine_terms, e2@affine_terms)
  e1@constant <- e1@constant + e2@constant
  e1
})

#' Add a scalar quadratic term and a scalar quadratic function
#' @param e1 A scalar quadratic term
#' @param e2 A scalar quadratic function
setMethod("+", signature(e1 = "MOI_scalar_quadratic_term", e2 = "MOI_scalar_quadratic_function"), function(e1, e2) {
  e2 + e1
})

#' Substract a scalar quadratic term and a scalar quadratic function
#' @param e1 A scalar quadratic function
#' @param e2 A scalar quadratic term
setMethod("-", signature(e1 = "MOI_scalar_quadratic_function", e2 = "MOI_scalar_quadratic_term"), function(e1, e2) {
  e1 + (-1 * e2)
})

#' Substract a scalar quadratic term and a scalar quadratic function
#' @param e1 A scalar quadratic term
#' @param e2 A scalar quadratic function
setMethod("-", signature(e1 = "MOI_scalar_quadratic_term", e2 = "MOI_scalar_quadratic_function"), function(e1, e2) {
  (-1 * e2) - (-1 * e1)
})


#' Substract a scalar quadratic term and a scalar numeric
#' @param e1 A scalar quadratic function
#' @param e2 A scalar numeric
setMethod("-", signature(e1 = "MOI_scalar_quadratic_function", e2 = "numeric"), function(e1, e2) {
  e1 + (-1 * e2)
})

#' Substract a scalar quadratic term and a scalar numeric
#' @param e1 A scalar numeric
#' @param e2 A scalar quadratic function
setMethod("-", signature(e1 = "numeric", e2 = "MOI_scalar_quadratic_function"), function(e1, e2) {
  (-1 * e2) - (-1 * e1)
})

#' Add a scalar affine term and a scalar quadratic function
#' @param e1 A scalar affine term
#' @param e2 A scalar quadrati function
setMethod("+", signature(e1 = "MOI_scalar_affine_term", e2 = "MOI_scalar_quadratic_function"), function(e1, e2) {
  e2 + e1
})

#' Substract a scalar affine term and a scalar quadratic function
#' @param e1 A scalar quadratic function
#' @param e2 A scalar affine term
setMethod("-", signature(e1 = "MOI_scalar_quadratic_function", e2 = "MOI_scalar_affine_term"), function(e1, e2) {
  e1 + (-1 * e2)
})

#' Substract a scalar affine term and a scalar quadratic function
#' @param e1 A scalar affine term
#' @param e2 A scalar quadratic function
setMethod("-", signature(e1 = "MOI_scalar_affine_term", e2 = "MOI_scalar_quadratic_function"), function(e1, e2) {
  (-1 * e2) - (-1 * e1)
})

#' Add a scalar affine function and a scalar quadratic function
#' @param e1 A scalar quadratic function
#' @param e2 A scalar affine function
setMethod("+", signature(e1 = "MOI_scalar_quadratic_function", e2 = "MOI_scalar_affine_function"), function(e1, e2) {
  e1@affine_terms <- c(e1@affine_terms, e2@terms)
  e1@constant <- e1@constant + e2@constant
  e1
})

#' Add a scalar affine function and a scalar quadratic function
#' @param e1 A scalar affine function
#' @param e2 A scalar quadratic function
setMethod("+", signature(e1 = "MOI_scalar_affine_function", e2 = "MOI_scalar_quadratic_function"), function(e1, e2) {
  e2 + e1
})

#' Substract a scalar affine function and a scalar quadratic function
#' @param e1 A scalar quadratic function
#' @param e2 A scalar affine function
setMethod("-", signature(e1 = "MOI_scalar_quadratic_function", e2 = "MOI_scalar_affine_function"), function(e1, e2) {
  e1 + (-1 * e2)
})

#' Substract a scalar affine function and a scalar quadratic function
#' @param e1 A scalar affine function
#' @param e2 A scalar quadratic function
setMethod("-", signature(e1 = "MOI_scalar_affine_function", e2 = "MOI_scalar_quadratic_function"), function(e1, e2) {
  (-1 * e2) - (-1 * e1)
})

#' Add a scalar affine function and a scalar quadratic term
#' @param e1 A scalar affine function
#' @param e2 A scalar quadratic term
setMethod("+", signature(e1 = "MOI_scalar_affine_function", e2 = "MOI_scalar_quadratic_term"), function(e1, e2) {
  (e2 + 0) + e1
})

#' Add a scalar affine function and a scalar quadratic term
#' @param e1 A scalar quadratic term
#' @param e2 A scalar affine function
setMethod("+", signature(e1 = "MOI_scalar_quadratic_term", e2 = "MOI_scalar_affine_function"), function(e1, e2) {
  e2 + e1
})

#' Substract a scalar affine function and a scalar quadratic term
#' @param e1 A scalar affine function
#' @param e2 A scalar quadratic term
setMethod("-", signature(e1 = "MOI_scalar_affine_function", e2 = "MOI_scalar_quadratic_term"), function(e1, e2) {
  e1 + (-1 * e2)
})

#' Substract a scalar affine function and a scalar quadratic term
#' @param e1 A scalar quadratic term
#' @param e2 A scalar affine function
setMethod("-", signature(e1 = "MOI_scalar_quadratic_term", e2 = "MOI_scalar_affine_function"), function(e1, e2) {
  (-1 * e2) - (-1 * e1)
})
