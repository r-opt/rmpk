#' A ROI solver
#'
#' @param solver the solver name
#' @param control a list of control options
#'
#' @export
ROI_solver <- function(solver, control = list()) {
  if (!requireNamespace("ROI", quietly = TRUE)) {
    stop("You need the package ROI to use this solver", call. = FALSE)
  }
  ROIMipSolver$new(solver, control)
}

ROIMipSolver <- R6::R6Class(
  "ROIMipSolver",
  public = list(
    initialize = function(solver, control = list()) {
      private$obj_vec <- numeric()
      private$A_mat <- slam::simple_triplet_zero_matrix(0L, 0L)
      private$col_type <- character()
      private$col_lb <- numeric()
      private$col_ub <- numeric()

      private$b_vec <- numeric()
      private$row_dir <- character()

      private$roi_solver_name <- solver
      private$roi_control_list <- control
    },
    add_variable = function(type, lower_bound = -Inf, upper_bound = Inf) {
      new_idx <- ncol(private$A_mat) + 1L
      private$col_type[[length(private$col_type) + 1L]] <- type
      private$obj_vec[[length(private$obj_vec) + 1L]] <- 0
      private$A_mat <- slam::simple_triplet_matrix(
        i = private$A_mat$i,
        j = private$A_mat$j,
        v = private$A_mat$v,
        nrow = nrow(private$A_mat),
        ncol = ncol(private$A_mat) + 1L,
        dimnames = NULL)
      private$col_lb[[new_idx]] <- lower_bound
      private$col_ub[[new_idx]] <- upper_bound
      new_idx
    },
    add_row = function(values, type, rhs) {
      private$A_mat <- slam::simple_triplet_matrix(
        i = c(private$A_mat$i, values$i + nrow(private$A_mat)), # O(N)
        j = c(private$A_mat$j, values$j),
        v = c(private$A_mat$v, values$v),
        nrow = nrow(private$A_mat) + 1L,
        ncol = ncol(private$A_mat),
        dimnames = NULL)
      private$b_vec[[length(private$b_vec) + 1L]] <- rhs
      private$row_dir[[length(private$row_dir) + 1L]] <- type
      nrow(private$A_mat)
    },
    set_objective_coefficient = function(variable_index, value) {
      private$obj_vec[[variable_index]] <- value
    },
    set_objective_sense = function(sense) {
      private$obj_sense <- sense
    },
    set_variable_lb = function(variable_index, value) {
      private$col_lb[[variable_index]] <- value
    },
    set_variable_ub = function(variable_index, value) {
      private$col_ub[[variable_index]] <- value
    },
    optimize = function() {
      obj <- ROI::L_objective(self$objective_coefficients())
      n <- nrow(self$constraint_matrix())
      constraints <- ROI::L_constraint(
        self$constraint_matrix(), self$constraint_direction(), self$rhs_vector()
      )
      var_types <- toupper(substr(private$col_type, 1, 1))
      n_vars <- length(private$col_lb)
      var_indexes <- seq_len(n_vars)

      finite_lb <- is.finite(private$col_lb)
      finite_ub <- is.finite(private$col_ub)
      var_bounds <- ROI::V_bound(
        var_indexes[finite_lb],
        var_indexes[finite_ub],
        private$col_lb[finite_lb],
        private$col_ub[finite_ub],
        nobj = n_vars,
        ld = -Inf, ud = Inf
      )
      op <- ROI::OP(
        obj, constraints,
        types = var_types,
        bounds = var_bounds,
        maximum = private$obj_sense == "max"
      )
      private$roi_result <- ROI::ROI_solve(
        op,
        private$roi_solver_name,
        control = private$roi_control_list
      )
    },
    get_value = function(var_index) {
      private$roi_result$solution[[var_index]]
    },
    termination_status = function() {
      if (is.null(private$roi_result)) {
        return(TERMINATION_STATUS$OPTIMIZE_NOT_CALLED)
      }
      if (private$roi_result$status$code == 0L) {
        return(TERMINATION_STATUS$SUCCESS)
      }
      if (private$roi_result$status$code == 1L) {
        return(TERMINATION_STATUS$OTHER_ERROR)
      }
      stop("Unknown ROI status code", call. = FALSE)
    },
    objective_coefficients = function() {
      private$obj_vec
    },
    constraint_matrix = function() {
      private$A_mat
    },
    rhs_vector = function() {
      private$b_vec
    },
    constraint_direction = function() {
      private$row_dir
    }
  ),
  private = list(
    obj_vec = NULL,
    A_mat = NULL,
    col_type = NULL,
    col_lb = NULL,
    col_ub = NULL,
    b_vec = NULL,
    row_dir = NULL,
    roi_solver_name = NULL,
    roi_control_list = NULL,
    roi_result = NULL,
    obj_sense = "min"
  )
)
