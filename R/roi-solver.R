#' A ROI solver
#'
#' @param solver the solver name
#' @param control a list of control options
#'
#' @export
ROI_optimizer <- function(solver, control = list()) {
  if (!requireNamespace("ROIoptimizer", quietly = TRUE)) {
    stop("You need the package ROIoptimizer to use this optimizer", call. = FALSE)
  }
  ROIoptimizer::ROI_optimizer(solver, control)
}
