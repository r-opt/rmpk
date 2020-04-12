#' A variable reference
#'
#' @export
#' @rdname variables
setClass("RMPK_variable",
  slots = c(
    model_ref = "ANY"
  ), contains = "MOI_variable_index"
)

RMPK_variable <- function(index, model_ref = NULL) {
  stopifnot(is.numeric(index), length(index) == 1)
  stopifnot(is.null(model_ref) || inherits(model_ref, "RMPKMipModel"))
  new("RMPK_variable", value = index, model_ref = model_ref)
}

#' A variable reference
#'
#' @export
#' @rdname variables
setClass("RMPK_constraint",
  slots = c(
    model_ref = "ANY"
  ), contains = "MOI_constraint_index"
)

RMPK_constraint <- function(index, model_ref = NULL) {
  stopifnot(is.numeric(index), length(index) == 1)
  stopifnot(is.null(model_ref) || inherits(model_ref, "RMPKMipModel"))
  new("RMPK_constraint", value = index, model_ref = model_ref)
}

setMethod(
  "format",
  "RMPK_variable",
  function(x, ...) {
    format(paste0("Variable (internal solver ref = ", x@value, ")"))
  }
)

setMethod(
  "print",
  "RMPK_variable",
  function(x, ...) {
    cat(format(x))
    invisible(x)
  }
)

setMethod(
  "show",
  "RMPK_variable",
  function(object) {
    print(object)
  }
)

# TODO: turn the map into a fixed length list
# This might be faster, as we know the dimensions
#' @rdname variables
#' @export
setClass("RMPK_variable_list", slots = c(
  variables_map = "ANY",
  arity = "integer",
  index_types = "character"
))

setMethod(
  "format",
  "RMPK_variable_list",
  function(x, ...) {
    format(paste0("Variable container: size = ", x@variables_map$size()))
  }
)

setMethod(
  "print",
  "RMPK_variable_list",
  function(x, ...) {
    cat(format(x))
    invisible(x)
  }
)

setMethod(
  "show",
  "RMPK_variable_list",
  function(object) {
    print(object)
  }
)
