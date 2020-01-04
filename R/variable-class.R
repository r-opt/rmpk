#' A variable reference
#'
#' @export
#' @rdname variables
setClass("RLPVariable", slots = c(
  coefficient = "numeric",
  variable_index = "integer"
))

#' @export
#' @rdname variables
setMethod(
  "format",
  "RLPVariable",
  function(x, ...) {
    format(paste0("Variable (internal solver ref = ", x@variable_index, ")"))
  }
)

#' @export
#' @rdname variables
setMethod(
  "print",
  "RLPVariable",
  function(x, ...) {
    cat(format(x))
    invisible(x)
  }
)

#' @export
#' @rdname variables
setMethod(
  "show",
  "RLPVariable",
  function(object) {
    print(object)
  }
)

# TODO: turn the map into a fixed length list
# This might be faster, as we know the dimensions
#' @rdname variables
#' @export
setClass("RLPVariableList", slots = c(
  variables_map = "ANY",
  arity = "integer",
  index_types = "character"
))

#' @export
#' @rdname variables
setMethod(
  "format",
  "RLPVariableList",
  function(x, ...) {
    format(paste0("Variable container: size = ", x@variables_map$size()))
  }
)

#' @export
#' @rdname variables
setMethod(
  "print",
  "RLPVariableList",
  function(x, ...) {
    cat(format(x))
    invisible(x)
  }
)

#' @export
#' @rdname variables
setMethod(
  "show",
  "RLPVariableList",
  function(object) {
    print(object)
  }
)

is_variable <- function(x) {
  inherits(x, "RLPVariable")
}
is_variable_container <- function(x) {
  inherits(x, "RLPVariableList")
}
