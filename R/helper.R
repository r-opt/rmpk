# merge two variable lists in O(length(new_vars))
merge_variables <- function(variable_list, new_vars) {
  for (hash_key in new_vars$keys()) {
    if (variable_list$has(hash_key)) {
      old_var <- variable_list$get(hash_key)
      new_var <- new_vars$get(hash_key)
      old_var@coefficient <- old_var@coefficient + new_var@coefficient
      variable_list$set(hash_key, old_var)
    } else {
      variable_list$set(hash_key, new_vars$get(hash_key))
    }
  }
  variable_list
}

var_to_map <- function(var) {
  var_map <- fastmap::fastmap()
  var_map$set(as.character(var@variable_index), var)
  var_map
}
