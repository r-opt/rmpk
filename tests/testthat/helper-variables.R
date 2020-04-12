var_by_index <- function(variable_list, index) {
  (Find(function(x) x@variable@value == index, variable_list))
}
