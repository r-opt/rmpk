#' A solver that caches all operations
#'
#' It cannot solve anything.
#'
#' @export
cache_solver <- function() {
  CachedMipSolver$new(NULL)
}

CachedMipSolver <- R6::R6Class(
  "CachedMipSolver",
  inherit = ROIMipSolver,
  public = list(
    format = function(...) {
      "No solver (caching)"
    },
    set_solver = function(solver) {
      stopifnot(inherts(solver, "RMPSolverInterface"))
      private$solver <- solver
    }
  ),
  private = list(
    solver = NULL
  )
)
