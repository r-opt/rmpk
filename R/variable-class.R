setClass("RLPVariable", slots = c(
  coefficient = "numeric",
  variable_index = "integer"
))
setClass("RLPVariableList", slots = c(
  variables_map = "ANY",
  arity = "integer",
  base_name = "character"
))
setClass("RLPVariableListBuilder", slots = c(tmp = "logical"), prototype = list(tmp = FALSE))
