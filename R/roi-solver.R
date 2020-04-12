#' A ROI solver
#'
#' @param solver the solver name
#' @param control a list of control options
#'
#' @export
ROI_optimizer <- function(solver, control = list()) {
  if (!requireNamespace("ROIsolver", quietly = TRUE)) {
    stop("You need the package ROIsolver to use this optimizer", call. = FALSE)
  }
  ROIsolver::ROI_optimizer(solver, control)
}
