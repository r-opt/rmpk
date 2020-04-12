#' A new mixed-integer programming Model
#'
#' @param solver a solver object
#' @include roi-solver.R
#' @export
MIPModel <- function(solver) {
  RMPKMipModel$new(solver)
}

#' A new Model
#'
#' @param solver a solver object
#' @include roi-solver.R
#' @export
optimization_model <- function(solver) {
  RMPKMipModel$new(solver)
}


#' @include mip-model-methods.R
#' @noRd
RMPKMipModel <- R6::R6Class("RMPKMipModel",
  public = list(
    initialize = function(solver) {
      private$solver <- solver
      private$row_indexes <- integer(0L)
      private$variable_map <- fastmap::fastmap()
    },

    # build it
    add_variable = mip_model_impl_add_variable,
    set_objective = mip_model_impl_set_objective,
    add_constraint = mip_model_impl_add_constraint,
    set_bounds = mip_model_impl_set_bounds,

    # optimize
    optimize = mip_model_impl_optimize,
    termination_status = mip_model_impl_termination_status,
    termination_solver_message = mip_model_impl_termination_message,
    get_variable_value = mip_model_impl_get_value,
    get_variable_dual = mip_model_impl_get_variable_dual,
    get_row_duals = mip_model_impl_get_row_duals,
    objective_value = mip_model_impl_objective_value,

    # other stuff
    print = function(...) {
      cat("MIP Model: \n")
      cat("  Variables: ", moi_get(private$solver, MOI::number_of_variables), "\n", sep = "")
      cat("  Constraints: ", moi_get(private$solver, MOI::number_of_constraints), "\n", sep = "")
      invisible(self)
    },
    get_variable_ref = function(name) {
      stopifnot(is.character(name), length(name) == 1L)
      if (private$variable_map$has(name)) {
        return(private$variable_map$get(name))
      }
      stop("No variable is registered u\nder the name ", name, call. = FALSE)
    }
  ),
  private = list(
    solver = NULL,
    row_indexes = NULL,
    variable_map = NULL,
    register_variable = function(name, value) {
      stopifnot(is.character(name))
      if (private$variable_map$has(name)) {
        stop(
          "A variable with the name '",
          name,
          "' has already been added to the model",
          call. = FALSE
        )
      }
      private$variable_map$set(name, value)
    },
    add_row = function(local_envir, eq) {
      func <- eval(eq$lhs, envir = local_envir) - eval(eq$rhs, envir = local_envir)
      rhs <- func@constant * -1
      func@constant <- 0
      set <- if (eq$operator == "<=") {
        less_than_set(rhs)
      } else if (eq$operator == "==") {
        equal_to_set(rhs)
      } else if (eq$operator == ">=") {
        greater_than_set(rhs)
      } else {
        stop("unsupported operator", call. = FALSE)
      }
      row_idx <- moi_add_constraint(
        private$solver,
        func,
        set
      )
      private$row_indexes[[length(private$row_indexes) + 1L]] <- row_idx@value
      invisible()
    }
  )
)
