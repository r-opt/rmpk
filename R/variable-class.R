#' A variable reference
#'
#' @export
#' @rdname variables
setClass("RMPKVariable", slots = c(
  coefficient = "numeric",
  variable_index = "integer"
))

#' @export
#' @rdname variables
setMethod(
  "format",
  "RMPKVariable",
  function(x, ...) {
    format(paste0("Variable (internal solver ref = ", x@variable_index, ")"))
  }
)

#' @export
#' @rdname variables
setMethod(
  "print",
  "RMPKVariable",
  function(x, ...) {
    cat(format(x))
    invisible(x)
  }
)

#' @export
#' @rdname variables
setMethod(
  "show",
  "RMPKVariable",
  function(object) {
    print(object)
  }
)

# TODO: turn the map into a fixed length list
# This might be faster, as we know the dimensions
#' @rdname variables
#' @export
setClass("RMPKVariableList", slots = c(
  variables_map = "ANY",
  arity = "integer",
  index_types = "character"
))

#' @export
#' @rdname variables
setMethod(
  "format",
  "RMPKVariableList",
  function(x, ...) {
    format(paste0("Variable container: size = ", x@variables_map$size()))
  }
)

#' @export
#' @rdname variables
setMethod(
  "print",
  "RMPKVariableList",
  function(x, ...) {
    cat(format(x))
    invisible(x)
  }
)

#' @export
#' @rdname variables
setMethod(
  "show",
  "RMPKVariableList",
  function(object) {
    print(object)
  }
)

is_variable <- function(x) {
  inherits(x, "RMPKVariable")
}
is_variable_container <- function(x) {
  inherits(x, "RMPKVariableList")
}
