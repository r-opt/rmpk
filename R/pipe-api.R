#' @export
add_variable <- function(model, variable_expr, ..., type = "continuous", lb = -Inf, ub = Inf) {
  UseMethod("add_variable")
}

add_variable.RMPKMipModel <- function(model, variable_expr, ..., type = "continuous", lb = -Inf, ub = Inf) {
  model <- as_pipe_mip_model(model)
  add_variable(model, !!rlang::enquo(variable_expr), ..., type = type, lb = lb, ub = ub)
}

add_variable.PipeRMPKMipModel <- function(model, variable_expr, ..., type = "continuous", lb = -Inf, ub = Inf) {
  model$assign_to_env(
    extract_var_name(rlang::enquo(variable_expr)),
    model$add_variable(..., type = type, lb = lb, ub = ub)
  )
  model
}

set_objective <- function(model, obj_variables, sense = "min") {
  UseMethod("set_objective")
}

set_objective.RMPKMipModel <- function(model, obj_variables, sense = "min") {
  model <- as_pipe_mip_model(model)
  set_objective(model, rlang::enquo(obj_variables), sense)
}

set_objective.PipeRMPKMipModel <- function(model, obj_variables, sense = "min") {
  model$with_variable_env(
    rlang::quo(model$set_objective(!!rlang::get_expr(rlang::enquo(obj_variables)), !!sense))
  )
  model
}

add_constraint <- function(model, obj_variables, sense = "min") {
  UseMethod("add_constraint")
}

add_constraint.RMPKMipModel <- function(model, expr, ...) {
  model <- as_pipe_mip_model(model)
  add_constraint(model, rlang::enquo(expr), ...)
}

add_constraint.PipeRMPKMipModel <- function(model, expr, ...) {
  args <- rlang::enexprs(expr, ...)
  model$with_variable_env(
    rlang::quo(model$add_constraint(!!!lapply(args, rlang::get_expr)))
  )
  model
}

set_bounds <- function(model, expr, ..., lb = NULL, ub = NULL) {
  UseMethod("set_bounds")
}

set_bounds.RMPKMipModel <- function(model, expr, ..., lb = NULL, ub = NULL) {
  model <- as_pipe_mip_model(model)
  set_bounds(model, rlang::enquo(expr), ..., lb = lb, ub = ub)
}

set_bounds.PipeRMPKMipModel <- function(model, expr, ..., lb = NULL, ub = NULL) {
  args <- rlang::enexprs(expr, ...)
  model$with_variable_env(
    rlang::quo(model$set_bounds(!!!lapply(args, rlang::get_expr), lb = !!lb, ub = !!ub))
  )
  model
}

get_solution <- function(model) {

}

solve_model <- function(model, solver) {
  stopifnot(inherits(model, "PipeRMPKMipModel"))

}

extract_var_name <- function(expr) {
  if (rlang::quo_is_symbol(expr)) {
    return(vctrs::vec_cast(rlang::quo_get_expr(expr), "character"))
  }
  quo <- rlang::quo_get_expr(expr)
  if (rlang::quo_is_call(expr) && length(quo) >= 2L &&
      quo[[1L]] == "[" && rlang::is_symbol(quo[[2L]])) {
    return(vctrs::vec_cast(as.character(quo[[2L]]), "character"))
  }
  stop("wat")
}

as_pipe_mip_model <- function(model) {
  stopifnot(inherits(model, "RMPKMipModel"))
  PipeRMPKMipModel$new(model)
}

PipeRMPKMipModel <- R6::R6Class(
  "PipeRMPKMipModel",
  public = list(
    initialize = function(mip_model) {
      private$mip_model <- mip_model
      private$variable_env <- rlang::new_environment()
    },
    assign_to_env = function(var_name, value) {
      assign(var_name, value, envir = private$variable_env)
    },
    with_variable_env = function(expr) {
      parent <- rlang::quo_get_env(expr)
      parent.env(private$variable_env) <- parent # TODO: this alters the parent
      rlang::eval_tidy(rlang::quo_get_expr(expr), env = private$variable_env)
    },
    add_variable = function(..., type = "continuous", lb = -Inf, ub = Inf) {
      private$mip_model$add_variable(..., type = type, lb = lb, ub = ub)
    },
    set_objective = function(obj_variables, sense = "min") {
      private$mip_model$set_objective(obj_variables, sense)
    },
    add_constraint = function(expr, ...) {
      args <- rlang::enquos(expr, ...)
      rlang::eval_tidy(
        rlang::quo(
          private$mip_model$add_constraint(!!!args)
        )
      )
    },
    set_bounds = function(expr, ..., lb = NULL, ub = NULL) {
      private$mip_model$set_bounds(expr, ..., lb = lb, ub = ub)
    },

    # optimize
    optimize = function() private$mip_model$optimize(),
    termination_status = function() {
      private$mip_model$termination_status()
    },
    termination_solver_message = function() {
      private$mip_model$termination_solver_message()
    },
    get_variable_value = function(variable_selector) {
      private$mip_model$get_variable_value(variable_selector)
    },
    get_variable_dual = function(variable_selector) {
      private$mip_model$get_variable_dual(variable_selector)
    },
    get_row_duals = function() {
      private$mip_model$get_row_duals()
    },
    objective_value = function() {
      private$mip_model$objective_value()
    },

    format = function(...) {
      format(private$mip_model)
    }
  ),
  private = list(
    mip_model = NULL,
    variable_env = NULL
  )
)
