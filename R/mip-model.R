#' A new mixed-integer programming Model
#'
#' @param solver a solver object
#' @include roi-solver.R
#' @export
MIPModel <- function(solver) {
  RlpMipModel$new(solver)
}

#' @include mip-model-methods.R
#' @noRd
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
