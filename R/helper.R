# merge two variable lists in O(length(new_vars))
merge_variables <- function(variable_list, new_vars) {
  for (hash_key in new_vars$keys()) {
    new_var <- new_vars$get(hash_key)
    insert_or_update_variable_list(variable_list, new_var, hash_key)
  }
  variable_list
}

merge_with_single_variable <- function(variable_list, new_var) {
  key <- as.character(new_var@variable_index)
  insert_or_update_variable_list(variable_list, new_var, key)
}

insert_or_update_variable_list <- function(variable_list, new_var, hash_key) {
  if (variable_list$has(hash_key)) {
    old_var <- variable_list$get(hash_key)
    slot(old_var, "coefficient", check = FALSE) <- old_var@coefficient + new_var@coefficient
    variable_list$set(hash_key, old_var)
  } else {
    variable_list$set(hash_key, new_var)
  }
  variable_list
}

var_to_map <- function(var) {
  var_map <- fastmap::fastmap()
  var_map$set(as.character(var@variable_index), var)
  var_map
}

construct_quantifiers <- function(...) {
  quosures <- rlang::enquos(...)
  is_index <- names(quosures) != ""
  quantifiers <- rlang::eval_tidy(
    rlang::quo(expand.grid(!!!quosures[is_index], stringsAsFactors = FALSE))
  )
  filter_guard <- Reduce(function(acc, el) {
    rlang::quo(!!acc & !!el)
  }, quosures[!is_index], init = TRUE)
  rlang::eval_tidy(
    rlang::quo(
      quantifiers[!!filter_guard, , drop = FALSE]
    ),
    data = quantifiers
  )
}
