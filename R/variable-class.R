setClass("RLPVariable", slots = c(
  coefficient = "numeric",
  variable_index = "integer"
))

# TODO: turn the map into a fixed length list
# This might be faster, as we know the dimensions
setClass("RLPVariableList", slots = c(
  variables_map = "ANY",
  arity = "integer",
  index_types = "character"
))
is_variable <- function(x) {
  inherits(x, "RLPVariable")
}
is_variable_container <- function(x) {
  inherits(x, "RLPVariableList")
}
