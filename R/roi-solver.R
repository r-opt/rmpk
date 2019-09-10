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

#' @include solver-interface.R
ROIMipSolver <- R6::R6Class(
  "ROIMipSolver",
  inherit = RMPSolverInterface,
  public = list(
    initialize = function(solver, control = list()) {
      private$linear_obj_vec <- numeric()
      private$A_mat <- slam::simple_triplet_zero_matrix(0L, 0L)
      private$obj_Q_mat <- slam::simple_triplet_zero_matrix(0L, 0L)
      private$Q_constraints <- fastmap::fastmap()
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
      private$linear_obj_vec[[length(private$linear_obj_vec) + 1L]] <- 0
      # TODO: why not require adding variables first?
      private$A_mat <- slam::simple_triplet_matrix(
        i = private$A_mat$i,
        j = private$A_mat$j,
        v = private$A_mat$v,
        nrow = nrow(private$A_mat),
        ncol = ncol(private$A_mat) + 1L,
        dimnames = NULL)
      private$obj_Q_mat <- slam::simple_triplet_zero_matrix(new_idx, new_idx)
      private$col_lb[[new_idx]] <- lower_bound
      private$col_ub[[new_idx]] <- upper_bound
      new_idx
    },
    add_linear_constraint = function(linear_expr, type, rhs) {
      variables <- linear_expr@variables$as_list()
      indexes <- vapply(variables, function(x) x@variable_index, integer(1L))
      coefficients <- vapply(variables, function(x) x@coefficient, numeric(1L))
      private$A_mat <- slam::simple_triplet_matrix(
        i = c(private$A_mat$i, rep.int(nrow(private$A_mat) + 1, length(indexes))), # O(N)
        j = c(private$A_mat$j, indexes),
        v = c(private$A_mat$v, coefficients),
        nrow = nrow(private$A_mat) + 1L,
        ncol = ncol(private$A_mat),
        dimnames = NULL)
      private$b_vec[[length(private$b_vec) + 1L]] <- rhs
      private$row_dir[[length(private$row_dir) + 1L]] <- type
      nrow(private$A_mat)
    },
    add_quadratic_constraint = function(quadratic_expr, type, rhs) {
      row_idx <- self$add_linear_constraint(quadratic_expr@linear_part, type, rhs)
      quad_vars <- quadratic_expr@quadratic_variables$as_list()
      new_Q <- private$to_Q_mat(quad_vars)
      private$Q_constraints$set(as.character(row_idx), new_Q)
      row_idx
    },
    set_linear_objective = function(linear_expr, sense) {
      for (var in linear_expr@variables$as_list()) {
        private$linear_obj_vec[[var@variable_index]] <- var@coefficient
      }
      private$objective_is_linear <- TRUE
      private$obj_sense <- sense
    },
    set_quadratic_objective = function(quadratic_expr, sense) {
      self$set_linear_objective(quadratic_expr@linear_part, sense)
      quad_vars <- quadratic_expr@quadratic_variables$as_list()
      private$obj_Q_mat <- private$to_Q_mat(quad_vars)
      private$objective_is_linear <- FALSE
      private$obj_sense <- sense
    },
    set_variable_lb = function(variable_index, value) {
      private$col_lb[[variable_index]] <- value
    },
    set_variable_ub = function(variable_index, value) {
      private$col_ub[[variable_index]] <- value
    },
    nvars = function() {
      ncol(private$A_mat)
    },
    nconstraints = function() {
      nrow(private$A_mat)
    },
    optimize = function() {
      obj <- if (private$objective_is_linear) {
        ROI::L_objective(self$objective_coefficients())
      } else {
        ROI::Q_objective(
          Q = private$obj_Q_mat,
          L = self$objective_coefficients()
        )
      }
      n <- nrow(self$constraint_matrix())
      has_any_quadratic_constraints <- private$Q_constraints$size() > 0L
      constraints <- if (has_any_quadratic_constraints) {
        constr_mat <- self$constraint_matrix()
        dir_vec <- self$constraint_direction()
        rhs_vec <- self$rhs_vector()
        Q_mats <- lapply(seq_len(self$nconstraints()), function(i) {
          private$Q_constraints$get(as.character(i), missing = NULL)
        })
        ROI::Q_constraint(
          Q_mats,
          self$constraint_matrix(), self$constraint_direction(), self$rhs_vector()
        )
      } else {
        ROI::L_constraint(
          self$constraint_matrix(), self$constraint_direction(), self$rhs_vector()
        )
      }
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
    get_variable_value = function(var_index) {
      private$roi_result$solution[[var_index]]
    },
    set_variable_value = function(var_index, value) {
      not_implemented()
    },
    get_objective_value = function() {
      # TODO: keep track of the constant
      private$roi_result$objval
    },
    get_termination_status = function() {
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
      private$linear_obj_vec
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
    linear_obj_vec = NULL,
    objective_is_linear = TRUE,
    obj_Q_mat = NULL,
    Q_constraints = NULL,
    A_mat = NULL,
    col_type = NULL,
    col_lb = NULL,
    col_ub = NULL,
    b_vec = NULL,
    row_dir = NULL,
    roi_solver_name = NULL,
    roi_control_list = NULL,
    roi_result = NULL,
    obj_sense = "min",
    to_Q_mat = function(quad_vars) {
      row_idx <- vapply(quad_vars, function(var) var@variable1@variable_index, integer(1L))
      col_idx <- vapply(quad_vars, function(var) var@variable2@variable_index, integer(1L))
      coefs <- vapply(quad_vars, function(var) var@coefficient, numeric(1L))
      slam::simple_triplet_matrix(
        row_idx, col_idx, coefs,
        nrow = self$nvars(),
        ncol = self$nvars()
      )
    }
  )
)
