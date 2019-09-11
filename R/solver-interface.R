#' An interface R6 class for a solver
#'
#' You can also use any other data structure that implements the same methods.
#'
#' @export
RMPSolverInterface <- R6::R6Class(
  "RMPSolverInterface",
  portable = TRUE,
  public = list(
    add_variable = function(type, lower_bound = -Inf, upper_bound = Inf) {
      not_implemented()
    },
    add_linear_constraint = function(linear_expr, type, rhs) {
      not_implemented()
    },
    set_linear_objective = function(linear_expr, sense) {
      not_implemented()
    },
    add_quadratic_constraint = function(quadratic_expr, type, rhs) {
      not_implemented()
    },
    set_quadratic_objective = function(quadratic_expr, sense) {
      not_implemented()
    },
    set_variable_lb = function(variable_index, value) {
      not_implemented()
    },
    set_variable_ub = function(variable_index, value) {
      not_implemented()
    },
    nvars = function() {
      not_implemented()
    },
    nconstraints = function() {
      not_implemented()
    },
    optimize = function() {
      not_implemented()
    },
    get_variable_value = function(var_index) {
      not_implemented()
    },
    get_variable_dual = function(var_index) {
      not_implemented()
    },
    get_row_dual = function(row_index) {
      not_implemented()
    },
    set_variable_value = function(var_index, value) {
      not_implemented()
    },
    get_objective_value = function() {
      not_implemented()
    },
    get_termination_status = function() {
      not_implemented()
    }
  )
)

not_implemented <- function() {
  stop("This method is not implemented by the solver.", call. = FALSE)
}
