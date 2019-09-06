# it follows the codes from JuMP
# http://www.juliaopt.org/JuMP.jl/v0.20.0/solutions/#Obtaining-solutions-1

#' Solver termination status codes
#'
#' @export
TERMINATION_STATUS <- new.env(parent = emptyenv())

# Nothing happend yet
TERMINATION_STATUS$OPTIMIZE_NOT_CALLED <- 0L

# OK
TERMINATION_STATUS$OPTIMAL <- 1L
TERMINATION_STATUS$INFEASIBLE <- 2L
TERMINATION_STATUS$SUCCESS <- 3

# Limit
TERMINATION_STATUS$TIME_LIMIT <- 10L + 0L
TERMINATION_STATUS$OTHER_LIMIT <- 10L + 9L

# Error
TERMINATION_STATUS$OTHER_ERROR <- 100L + 9L
