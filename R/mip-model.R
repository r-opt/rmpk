#' A new MILP Model
#'
#' @param solver a solver object
#' @include roi-solver.R
#' @export
MIPModel <- function(solver) {
  RlpMipModel$new(solver)
}

#' @export
add_variable <- function(model, expr, type = "continuous", lb = -Inf, ub = Inf, ...) {
  # TODO: capture the environment, probably using rlang
  eval(bquote(model$add_variable(.(substitute(expr)), type, lb, ub, ...)))
  model
}

#' @export
set_objective <- function(model, expr, sense = "min") {
  eval(bquote(model$set_objective(.(substitute(expr)), sense)))
  model
}

#' @export
add_constraint <- function(model, expr, ...) {
  eval(bquote(model$add_constraint(.(substitute(expr)), ...)))
  model
}

RlpMipModel <- R6::R6Class("RlpMipModel",
  public = list(
    initialize = function(solver) {
      private$solver <- solver
      private$variables <- fastmap::fastmap()
      private$rlp_variable_envir <- new.env(parent = globalenv())
    },
    add_variable = function(expr, type = "continuous", lb = -Inf, ub = Inf, ...) {
      stopifnot(length(type) == 1L, length(lb) == 1L, length(ub) == 1L)
      type <- match.arg(type, c("continuous", "integer", "binary"))
      expr <- substitute(expr)
      var_names <- generate_variable_names(expr, ...)
      rlp_vars <- lapply(var_names$var_names, function(var_name) {
        private$register_variable(var_name, type, lb, ub)
      })
      private$rlp_variable_envir[[var_names$base_name]] <-
        if (var_names$is_indexed_var) {
          new("RLPVariableList",
            base_name = var_names$base_name,
            variables_map = private$variables
          )
        } else {
          rlp_vars[[1L]]
        }
    },
    set_objective = function(expr, sense = "min") {
      sense <- match.arg(sense, c("max", "min"))
      private$solver$set_objective_sense(sense)

      expr <- substitute(expr)

      # TODO: not cool
      parent.env(private$rlp_variable_envir) <- parent.frame()
      envir <- new.env(parent = private$rlp_variable_envir)
      sum_expr <- make_sum_expr(envir)
      envir[["sum_expr"]] <- sum_expr
      obj_variables <- eval(expr, envir = envir)
      if (is_rlp_variable_sum(obj_variables)) {
        for (var in obj_variables@variables) {
          private$solver$set_objective_coefficient(var@index, var@coefficient)
        }
      } else {
        var <- obj_variables
        private$solver$set_objective_coefficient(var@index, var@coefficient)
      }
    },
    add_constraint = function(expr, ...) {
      eq <- split_equation(substitute(expr))
      modifiers <- expand.grid(...)
      modifier_var_names <- names(modifiers)
      no_modifiers <- nrow(modifiers) == 0L
      if (no_modifiers) {
        local_envir <- private$base_execution_envir(parent.frame())
        private$add_row(local_envir, eq)
      } else {
        for (i in seq_len(nrow(modifiers))) {
          local_envir <- private$base_execution_envir(parent.frame())
          vars <- modifiers[i, , drop = TRUE]
          for (j in seq_len(ncol(modifiers))) {
            local_envir[[modifier_var_names[j]]] <- vars[[j]]
          }
          private$add_row(local_envir, eq)
        }
      }
    },
    optimize = function() {
      private$solver$optimize()
      invisible()
    },
    termination_status = function() {
      private$solver$termination_status()
    },
    get_value = function(variable_expr) {
      variable_expr <- substitute(variable_expr)
      is_index_call <- is.call(variable_expr) && variable_expr[[1L]] == "["
      if (is_index_call) {
        var_name <- as.character(variable_expr[[2L]])
        indexes <- vapply(variable_expr[3:length(variable_expr)], function(x) {
          as.character(x)
        }, character(1L))
        keys <- private$variables$keys()
        relevant_keys <- keys[grepl(paste0("^", var_name), keys)]
        values <- vapply(relevant_keys, function(x) {
          index <- private$variables$get(x)@index
          private$solver$get_value(index)
        }, numeric(1L))
        splitted_keys <- strsplit(relevant_keys, "/", fixed = TRUE)
        return_val <- t(as.data.frame(splitted_keys, stringsAsFactors = FALSE))
        return_val <- as.data.frame(return_val, stringsAsFactors = FALSE)
        rownames(return_val) <- NULL
        return_val[["value"]] <- values
        colnames(return_val) <- c("name", indexes, "value")
        return(return_val)
      } else if (is.symbol(variable_expr)) {
        var <- private$variables$get(as.character(variable_expr))
        return(private$solver$get_value(var@index))
      }
      stop("Wrong expression", call. = FALSE)
    }
  ),
  private = list(
    solver = NULL,
    variables = NULL,
    rlp_variable_envir = NULL,

    register_variable = function(name, type, lower_bound, upper_bound) {
      var_idx <- private$solver$add_variable(type, lower_bound, upper_bound)
      # TODO: this should be stored in the solver
      rlp_var <- new("RLPVariable",
        coefficient = 1,
        index = var_idx,
        type = type,
        lower_bound = lower_bound,
        upper_bound = upper_bound
      )
      private$variables$set(name, rlp_var)
      rlp_var
    },
    base_execution_envir = function(parent_env) {
      # TODO: not cool
      `parent.env<-`(private$rlp_variable_envir, parent_env)
      local_envir <- new.env(parent = private$rlp_variable_envir)
      sum_expr <- make_sum_expr(local_envir)
      local_envir[["sum_expr"]] <- sum_expr
      local_envir
    },
    add_row = function(local_envir, eq) {
      lhs <- eval(eq$lhs, envir = local_envir) - eval(eq$rhs, envir = local_envir)
      rhs <- 0
      if (is_rlp_variable_sum(lhs)) {
        rhs <- lhs@constant * -1
      }
      indexes <- vapply(lhs@variables, function(x) x@index, integer(1L))
      coefficients <- vapply(lhs@variables, function(x) x@coefficient, numeric(1L))
      values <- slam::simple_triplet_matrix(rep.int(1L, length(indexes)), indexes, coefficients, nrow = 1, ncol = ncol(private$solver$constraint_matrix()))
      row_idx <- private$solver$add_row(
        values,
        type = eq$operator,
        rhs = rhs
      )
    }
  )
)

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
    index_combinations <- as.data.frame(index_list)
    names <- as.character(apply(index_combinations, 1L, function(row) {
      # TODO: check if any value in row has "/"
      paste0(var_name, "/", paste0(row, collapse = "/"), collapse = "/")
    }))
    return(list(
      base_name = var_name,
      var_names = names,
      arity = ncol(index_combinations),
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
  modifiers <- expand.grid(...)
  modifier_names <- names(modifiers)
  for (mod_name in modifier_names) {
    envir[[mod_name]] <- modifiers[[mod_name]]
  }
  envir
}
