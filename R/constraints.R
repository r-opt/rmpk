#' An abstract index
#' @export
#' @keywords internal
setClass("RMPK_abstract_constraint")

#' A constraint
#' @slot fun a MOI_abstract_function
#' @slot set a MOI_abstract set
#' @export
#' @keywords internal
setClass("RMPK_constraint", representation(fun = "MOI_abstract_function", set = "MOI_abstract_set"), contains = "RMPK_abstract_constraint")

new_rmpk_constraint <- function(fun, set) {
  new("RMPK_constraint", fun = fun, set = set)
}

substract_over <- function(lhs, rhs, set_fun) {
  x <- lhs - rhs
  const <- x@constant
  x@constant <- 0
  new_rmpk_constraint(x, set_fun(-1 * const))
}

#' leq relation
#' @param e2 a numeric
#' @param e1 a MOI_abstract_function
#' @keywords internal
setMethod("<=", signature(e1 = "numeric", "MOI_abstract_function"), function(e1, e2) {
  new_rmpk_constraint(-1 * e2, moi_less_than_set(-1 * e1))
})

#' leq relation
#' @param e1 a MOI_abstract_function
#' @param e2 a numeric
#' @keywords internal
setMethod("<=", signature(e1 = "MOI_abstract_function", "numeric"), function(e1, e2) {
  new_rmpk_constraint(e1, moi_less_than_set(e2))
})

#' leq relation
#' @param e1 a MOI_abstract_function
#' @param e2 a MOI_abstract_function
#' @keywords internal
setMethod("<=", signature(e1 = "MOI_abstract_function", "MOI_abstract_function"), function(e1, e2) {
  substract_over(e1, e2, moi_less_than_set)
})

#' eq relation
#' @param e1 a numeric
#' @param e2 a MOI_abstract_function
#' @keywords internal
setMethod("==", signature(e1 = "numeric", "MOI_abstract_function"), function(e1, e2) {
  new_rmpk_constraint(e2, moi_equal_to_set(e1))
})

#' eq relation
#' @param e1 a MOI_abstract_function
#' @param e2 a numeric
#' @keywords internal
setMethod("==", signature(e1 = "MOI_abstract_function", "numeric"), function(e1, e2) {
  new_rmpk_constraint(e1, moi_equal_to_set(e2))
})

#' eq relation
#' @param e1 a MOI_abstract_function
#' @param e2 a MOI_abstract_function
#' @keywords internal
setMethod("==", signature(e1 = "MOI_abstract_function", "MOI_abstract_function"), function(e1, e2) {
  substract_over(e1, e2, moi_equal_to_set)
})


#' eq relation
#' @param e1 a numeric
#' @param e2 a MOI_abstract_function
#' @keywords internal
setMethod(">=", signature(e1 = "numeric", "MOI_abstract_function"), function(e1, e2) {
  new_rmpk_constraint(-1 * e2, moi_greater_than_set(-1 * e1))
})

#' eq relation
#' @param e1 a MOI_abstract_function
#' @param e2 a numeric
#' @keywords internal
setMethod(">=", signature(e1 = "MOI_abstract_function", "numeric"), function(e1, e2) {
  new_rmpk_constraint(e1, moi_greater_than_set(e2))
})

#' eq relation
#' @param e1 a MOI_abstract_function
#' @param e2 a MOI_abstract_function
#' @keywords internal
setMethod(">=", signature(e1 = "MOI_abstract_function", "MOI_abstract_function"), function(e1, e2) {
  substract_over(e1, e2, moi_greater_than_set)
})
