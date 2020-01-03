mip_model_impl_add_variable <- function(expr, ..., type = "continuous", lb = -Inf, ub = Inf) {
  stopifnot(length(type) == 1L, length(lb) == 1L, length(ub) == 1L)
  type <- match.arg(type, c("continuous", "integer", "binary"))
  expr <- rlang::enquo(expr)
  var_names <- generate_variable_names(rlang::get_expr(expr), ...)
  rlp_vars <- lapply(var_names$var_names, function(var_name) {
    var_idx <- private$solver$add_variable(type, lb, ub)
    new("RLPVariable",
      coefficient = 1,
      variable_index = var_idx
    )
  })
  names(rlp_vars) <- var_names$var_names
  variable_map <- fastmap::fastmap()
  variable_map$mset(.list = rlp_vars)
  variable <- if (var_names$is_indexed_var) {
    new("RLPVariableList",
      base_name = var_names$base_name,
      variables_map = variable_map,
      arity = var_names$arity,
      index_types = var_names$index_types
    )
  } else {
    rlp_vars[[1L]]
  }
  variable
}

mip_model_impl_set_objective <- function(obj_variables, sense = "min") {
  sense <- match.arg(sense, c("max", "min"))
  is_quadratic <- is_quadratic_expression(obj_variables)
  if (is_quadratic) {
    obj_variables <- ensure_quadratic_expression(obj_variables)
    private$solver$set_quadratic_objective(obj_variables, sense)
  } else {
    obj_variables <- ensure_linear_expression(obj_variables)
    private$solver$set_linear_objective(obj_variables, sense)
  }
  invisible()
}

mip_model_impl_set_bounds <- function(expr, lb = NULL, ub = NULL, ...) {
  expr <- rlang::enquo(expr)

  eval_per_quantifier(function(local_envir) {
    var <- rlang::eval_bare(rlang::get_expr(expr), env = local_envir)
    if (!is.null(lb)) {
      private$solver$set_variable_lb(var@variable_index, lb)
    }
    if (!is.null(ub)) {
      private$solver$set_variable_ub(var@variable_index, ub)
    }
  }, base_envir = rlang::get_env(expr), ...)

  invisible()
}

mip_model_impl_add_constraint <- function(expr, ...) {
  expr <- rlang::enquo(expr)
  eq <- split_equation(rlang::get_expr(expr))

  eval_per_quantifier(function(local_envir) {
    private$add_row(local_envir, eq)
  }, base_envir = rlang::get_env(expr), ...)

  invisible()
}

eval_per_quantifier <- function(eval_fun, base_envir, ...) {
  quantifiers <- expand.grid(..., stringsAsFactors = FALSE)
  quantifier_var_names <- names(quantifiers)
  no_quantifiers <- nrow(quantifiers) == 0L
  if (no_quantifiers) {
    local_envir <- new.env(parent = base_envir)
    eval_fun(local_envir)
  } else {
    for (i in seq_len(nrow(quantifiers))) {
      local_envir <- new.env(parent = base_envir)
      vars <- quantifiers[i, , drop = TRUE]
      for (j in seq_len(ncol(quantifiers))) {
        local_envir[[quantifier_var_names[j]]] <- vars[[j]]
      }
      eval_fun(local_envir)
    }
  }
}

mip_model_impl_optimize <- function() {
  private$solver$optimize()
  invisible()
}

mip_model_impl_termination_status <- function() {
  private$solver$get_termination_status()
}

mip_model_impl_get_value <- function(variable_selector) {
  extract_solver_variable_value(
    private,
    rlang::enquo(variable_selector),
    private$solver$get_variable_value
  )
}

mip_model_impl_get_variable_dual <- function(variable_selector) {
  extract_solver_variable_value(
    private,
    rlang::enquo(variable_selector),
    private$solver$get_variable_dual
  )
}

extract_solver_variable_value <- function(private, variable_expr,
                                          get_value_fun) {
  variable_expr_rl <- variable_expr
  variable_expr <- rlang::get_expr(variable_expr)
  envir <- rlang::get_env(variable_expr_rl)
  is_index_call <- is.call(variable_expr) && variable_expr[[1L]] == "["
  if (is_index_call) {
    var_name <- as.character(variable_expr[[2L]])
    indexes <- vapply(variable_expr[3:length(variable_expr)], function(x) {
      as.character(x)
    }, character(1L))
    variable_container <- envir[[var_name]]
    if (length(indexes) != variable_container@arity) {
      stop(var_name, " is a variable with ", variable_container@arity, " indexes. ",
        "But you used the variable with ", length(indexes), " indexes.",
        call. = FALSE
      )
    }
    variables_list <- variable_container@variables_map$as_list()
    relevant_keys <- names(variables_list)
    values <- vapply(variables_list, function(x) {
      index <- x@variable_index
      get_value_fun(index)
    }, numeric(1L))
    splitted_keys <- strsplit(relevant_keys, "/", fixed = TRUE)
    return_val <- t(as.data.frame(splitted_keys, stringsAsFactors = FALSE))
    return_val <- as.data.frame(return_val, stringsAsFactors = FALSE)
    rownames(return_val) <- NULL
    return_val[["value"]] <- values
    colnames(return_val) <- c("name", indexes, "value")
    # set the right types for the index columns
    for (i in seq_along(indexes)) {
      type <- variable_container@index_types[[i]]
      if (type == "character") {
        return_val[[1 + i]] <- as.character(return_val[[1 + i]])
      }
      if (type == "integer") {
        return_val[[1 + i]] <- as.integer(return_val[[1 + i]])
      }
    }
    return(return_val)
  } else if (is.symbol(variable_expr)) {
    variable <- rlang::eval_tidy(variable_expr_rl)
    return(get_value_fun(variable@variable_index))
  }
  stop("Wrong expression", call. = FALSE)
}

mip_model_impl_get_row_duals <- function() {
  rows <- private$row_indexes
  values <- vapply(rows, function(i) {
    private$solver$get_row_dual(i)
  }, numeric(1L))
  data.frame(
    row_index = rows,
    value = values
  )
}

mip_model_impl_objective_value <- function() {
  private$solver$get_objective_value()
}

generate_variable_names <- function(expr, ...) {
  if (is.name(expr)) {
    expr_chr <- as.character(expr)
    return(list(
      base_name = expr_chr,
      var_names = expr_chr,
      arity = 0L,
      is_indexed_var = FALSE
    ))
  }
  is_bracket_call <- is.call(expr) && expr[[1L]] == "["
  if (is_bracket_call) {
    stopifnot(is.name(expr[[2L]]))
    var_name <- as.character(expr[[2L]])
    var_builder <- new("RLPVariableListBuilder")
    envir <- new.env(parent = globalenv())
    envir[[var_name]] <- var_builder
    mod_envir <- build_modifier_envir(envir, ...)
    index_list <- eval(expr, mod_envir)
    index_list_data_type <- vapply(index_list, function(x) {
      if (is.character(x)) {
        "character"
      } else if (is.integer(x)) {
        "integer"
      } else {
        stop("Only integer and character quantifiers are supported. ",
          "One of your quantifiers has the classes: ",
          paste0(class(x), collapse = ","),
          call. = FALSE
        )
      }
    }, character(1L))
    index_combinations <- as.data.frame(index_list)
    names <- as.character(apply(index_combinations, 1L, function(row) {
      # TODO: check if any value in row has "/"
      paste0(var_name, "/", paste0(row, collapse = "/"), collapse = "/")
    }))
    return(list(
      base_name = var_name,
      var_names = names,
      arity = ncol(index_combinations),
      index_types = index_list_data_type,
      is_indexed_var = TRUE
    ))
  }
  stop("Expression is not supported", .call = FALSE)
}

split_equation <- function(expr) {
  stopifnot(is.call(expr))
  operator <- as.character(expr[[1L]])
  stopifnot(operator %in% c("<=", ">=", "=="))
  list(
    operator = operator,
    lhs = expr[[2L]],
    rhs = expr[[3L]]
  )
}

build_modifier_envir <- function(parent_envir, ...) {
  envir <- new.env(parent = parent_envir)
  quantifiers <- construct_quantifiers(...)
  quantifier_names <- names(quantifiers)
  for (mod_name in quantifier_names) {
    envir[[mod_name]] <- quantifiers[[mod_name]]
  }
  envir
}
