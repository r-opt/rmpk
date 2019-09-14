#' A new MILP Model
#'
#' @param solver a solver object
#' @include roi-solver.R
#' @export
MIPModel <- function(solver) {
  RlpMipModel$new(solver)
}

#' #' Add a variable to the model
#' #'
#' #' @param model the model
#' #' @param expr an expression
#' #' @param type a character, either continuous, integer or binary
#' #' @param lb the lower bound
#' #' @param ub the upper bound
#' #' @param ... any quantifiers
#' #'
#' #' @export
#' add_variable <- function(model, expr, type = "continuous", lb = -Inf, ub = Inf, ...) {
#'   rlang::eval_tidy(
#'     rlang::quo(model$add_variable(!!rlang::enquo(expr), !!type, !!lb, !!ub, ...))
#'   )
#'   model
#' }
#'
#' #' Set the objective of the model
#' #'
#' #' @param model the model
#' #' @param expr an expression
#' #' @param sense either max or min
#' #'
#' #' @export
#' set_objective <- function(model, expr, sense = "min") {
#'   rlang::eval_tidy(
#'     rlang::quo(model$set_objective(!!rlang::enquo(expr), !!sense))
#'   )
#'   model
#' }
#'
#' #' Add a constraint to the model
#' #'
#' #' @param model the model
#' #' @param expr an expression
#' #' @param ... any quantifiers
#' #'
#' #' @export
#' add_constraint <- function(model, expr, ...) {
#'   rlang::eval_tidy(
#'     rlang::quo(model$add_constraint(!!rlang::enquo(expr), ...))
#'   )
#'   model
#' }
#'
#' #' Set bounds of individual variables
#' #'
#' #' @param model the model
#' #' @param expr an expression
#' #' @param lb the lower bounds
#' #' @param ub the upper bounds
#' #' @param ... any quantifiers
#' #'
#' #' @export
#' set_bounds <- function(model, expr, lb = NULL, ub = NULL, ...) {
#'   rlang::eval_tidy(
#'     rlang::quo(model$set_bounds(!!rlang::enquo(expr), !!lb, !!ub, ...))
#'   )
#'   model
#' }

#' @include mip-model-methods.R
RlpMipModel <- R6::R6Class("RlpMipModel",
  public = list(
    initialize = function(solver) {
      private$solver <- solver
      private$row_indexes <- integer(0L)
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
    row_indexes = NULL,

    register_variable = function(name, type, lower_bound, upper_bound) {
      var_idx <- private$solver$add_variable(type, lower_bound, upper_bound)
      rlp_var <- new("RLPVariable",
        coefficient = 1,
        variable_index = var_idx
      )
      rlp_var
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
