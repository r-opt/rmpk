setClass("RLPVariable", slots = c(
  coefficient = "numeric",
  index = "integer",
  type = "character",
  lower_bound = "numeric",
  upper_bound = "numeric"
))
setClass("RLPVariableList", slots = c(
  variables_map = "ANY",
  arity = "integer",
  base_name = "character"
))
setClass("RLPVariableListBuilder", slots = c(tmp = "logical"), prototype = list(tmp = FALSE))
setClass("RLPVariableSum", slots = c(variables = "list", constant = "numeric"))

is_rlp_variable_sum <- function(x) "RLPVariableSum" %in% class(x)

#--- RLPVariable
setMethod("+", signature(e1 = "RLPVariable", e2 = "numeric"), function(e1, e2) {
  new("RLPVariableSum", variables = list(e1), constant = e2)
})

setMethod("+", signature(e1 = "numeric", e2 = "RLPVariable"), function(e1, e2) {
  e2 + e1
})

setMethod("+", signature(e1 = "RLPVariable", e2 = "missing"), function(e1, e2) {
  e1
})

setMethod("+", signature(e1 = "RLPVariable", e2 = "RLPVariable"), function(e1, e2) {
  new("RLPVariableSum", variables = list(e1, e2), constant = 0)
})

setMethod("-", signature(e1 = "RLPVariable", e2 = "numeric"), function(e1, e2) {
  e1 + -1 * e2
})

setMethod("-", signature(e1 = "numeric", e2 = "RLPVariable"), function(e1, e2) {
  (-1 * e2) - (-1 * e1)
})

setMethod("-", signature(e1 = "RLPVariable", e2 = "RLPVariable"), function(e1, e2) {
  e1 + -1 * e2
})

setMethod("-", signature(e1 = "RLPVariable", e2 = "missing"), function(e1, e2) {
  (-1) * e1
})

setMethod("*", signature(e1 = "RLPVariable", e2 = "numeric"), function(e1, e2) {
  e1@coefficient <- e1@coefficient * e2
  e1
})

setMethod("*", signature(e1 = "numeric", e2 = "RLPVariable"), function(e1, e2) {
  e2 * e1
})

#--- RLPVariableSum
setMethod("+", signature(e1 = "RLPVariableSum", e2 = "numeric"), function(e1, e2) {
  e1@constant <- e1@constant + e2
  e1
})

setMethod("+", signature(e1 = "numeric", e2 = "RLPVariableSum"), function(e1, e2) {
  e2 + e1
})

setMethod("+", signature(e1 = "RLPVariableSum", e2 = "RLPVariable"), function(e1, e2) {
  e1@variables <- c(e1@variables, list(e2))
  e1
})

setMethod("+", signature(e1 = "RLPVariable", e2 = "RLPVariableSum"), function(e1, e2) {
  e2 + e1
})

setMethod("+", signature(e1 = "RLPVariableSum", e2 = "RLPVariableSum"), function(e1, e2) {
  e1@variables <- c(e1@variables, e2@variables) # O(n)
  e1@constant <- e1@constant + e2@constant
  e1
})

setMethod("-", signature(e1 = "RLPVariableSum", e2 = "RLPVariableSum"), function(e1, e2) {
  e1 + -1 * e2
})

setMethod("-", signature(e1 = "RLPVariableSum", e2 = "numeric"), function(e1, e2) {
  e1 + -1 * e2
})

setMethod("-", signature(e1 = "numeric", e2 = "RLPVariableSum"), function(e1, e2) {
  (-1 * e2) - (-1 * e1)
})

setMethod("*", signature(e1 = "RLPVariableSum", e2 = "numeric"), function(e1, e2) {
  e1@constant <- e1@constant * e2
  e1@variables <- lapply(e1@variables, function(var) {
    var@coefficient <- var@coefficient * e2
    var
  })
  e1
})

setMethod("*", signature(e1 = "numeric", e2 = "RLPVariableSum"), function(e1, e2) {
  e2 * e1
})

# --- RLPVariableList
setMethod("[", signature("RLPVariableList", i = "ANY", j = "ANY", drop = "missing"), function(x, i, j, ..., drop) {
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
  stopifnot(all(vapply(indexes, length, integer(1L)) == 1L))
  var_name <- paste0(x@base_name, "/", paste0(indexes, collapse = "/"), collapse = "/")
  x@variables_map$get(var_name)
})

# --- RLPVariableListBuilder
setMethod("[", signature("RLPVariableListBuilder", i = "ANY", j = "ANY", drop = "missing"), function(x, i, j, ..., drop) {
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
  indexes
})
