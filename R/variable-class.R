setClass("RLPVariable", slots = c(
  coefficient = "numeric",
  variable_index = "integer"
))

# TODO: turn the map into a fixed length list
# This might be faster, as we know the dimensions
setClass("RLPVariableList", slots = c(
  variables_map = "ANY",
  arity = "integer",
  base_name = "character",
  index_types = "character"
))
is_variable <- function(x) {
  "RLPVariable" %in% class(x)
}
is_variable_container <- function(x) {
  "RLPVariableList" %in% class(x)
}
setClass("RLPVariableListBuilder", slots = c(tmp = "logical"), prototype = list(tmp = FALSE))
