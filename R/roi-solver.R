#' A ROI solver
#'
#' @param solver the solver name
#' @param control a list of control options
#'
#' @export
ROI_optimizer <- function(solver, control = list()) {
  if (!requireNamespace("ROIoptimzer", quietly = TRUE)) {
    stop("You need the package ROIoptimzer to use this optimizer", call. = FALSE)
  }
  ROIoptimzer::ROI_optimizer(solver, control)
}
