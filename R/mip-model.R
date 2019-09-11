#' A new MILP Model
#'
#' @param solver a solver object
#' @include roi-solver.R
#' @export
MIPModel <- function(solver) {
  RlpMipModel$new(solver)
}

#' Add a variable to the model
#'
#' @param model the model
#' @param expr an expression
#' @param type a character, either continuous, integer or binary
#' @param lb the lower bound
#' @param ub the upper bound
#' @param ... any quantifiers
#'
#' @export
add_variable <- function(model, expr, type = "continuous", lb = -Inf, ub = Inf, ...) {
  # TODO: capture the environment, probably using rlang
  eval(bquote(model$add_variable(.(substitute(expr)), type, lb, ub, ...)))
  model
}

#' Set the objective of the model
#'
#' @param model the model
#' @param expr an expression
#' @param sense either max or min
#'
#' @export
set_objective <- function(model, expr, sense = "min") {
  eval(bquote(model$set_objective(.(substitute(expr)), sense)))
  model
}

#' Add a constraint to the model
#'
#' @param model the model
#' @param expr an expression
#' @param ... any quantifiers
#'
#' @export
add_constraint <- function(model, expr, ...) {
  eval(bquote(model$add_constraint(.(substitute(expr)), ...)))
  model
}

#' Set bounds of individual variables
#'
#' @param model the model
#' @param expr an expression
#' @param lb the lower bounds
#' @param ub the upper bounds
#' @param ... any quantifiers
#'
#' @export
set_bounds <- function(model, expr, lb = NULL, ub = NULL, ...) {
  eval(bquote(model$set_bounds(.(substitute(expr)), ...)))
  model
}

#' @include mip-model-methods.R
RlpMipModel <- R6::R6Class("RlpMipModel",
  public = list(
    initialize = function(solver) {
      private$solver <- solver
      private$variables <- fastmap::fastmap()
      private$row_indexes <- integer(0L)
      private$rlp_variable_envir <- new.env(parent = globalenv())
      private$variable_meta_info <- fastmap::fastmap()
    },
    # build it
    add_variable = mip_model_impl_add_variable,
    set_objective = mip_model_impl_set_objective,
    add_constraint = mip_model_impl_add_constraint,
    set_bounds = mip_model_impl_set_bounds,

    # optimize
    optimize = mip_model_impl_optimize,
    termination_status = mip_model_impl_termination_status,
    get_variable_value = mip_model_impl_get_value,
    get_variable_dual = mip_model_impl_get_variable_dual,
    get_row_duals = mip_model_impl_get_row_duals,
    objective_value = mip_model_impl_objective_value,

    # other stuff
    print = function(...) {
      cat("MIP Model: \n")
      cat("  Variables: ", private$solver$nvars(), "\n", sep = "")
      cat("  Constraints: ", private$solver$nconstraints(), "\n", sep = "")
      invisible(self)
    }
  ),
  private = list(
    solver = NULL,
    variables = NULL,
    row_indexes = NULL,
    rlp_variable_envir = NULL,
    variable_meta_info = NULL,

    register_variable = function(name, type, lower_bound, upper_bound) {
      var_idx <- private$solver$add_variable(type, lower_bound, upper_bound)
      rlp_var <- new("RLPVariable",
        coefficient = 1,
        variable_index = var_idx
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
      row_idx <- if (is_quadratic_expression(lhs)) {
        lhs <- ensure_quadratic_expression(lhs)
        rhs <- lhs@linear_part@constant * -1
        private$solver$add_quadratic_constraint(
          lhs,
          type = eq$operator,
          rhs = rhs
        )
      } else {
        lhs <- ensure_linear_expression(lhs)
        rhs <- lhs@constant * -1
        private$solver$add_linear_constraint(
          lhs,
          type = eq$operator,
          rhs = rhs
        )
      }
      private$row_indexes[[length(private$row_indexes) + 1L]] <- row_idx
      invisible()
    }
  )
)
