#' Distributional treatment effect
#'
#' Wrapper for `didFF(..., distDD=TRUE)`, which computes
#' the distributional treatment effects. This estimates the actual
#' distribution of `Y(1)` minus the implied distribution for `Y(0)` in
#' each outcome bin (for the treated group).
#'
#' @return A list object containing: The average treatment effects; a
#'   table with the estimated distributional TE, their SE, and bin levels.
#' @param ... Arguments passed to `didFF`.
#' @export
#'

distDD <-function(...) { base::return(didFF(..., distDD=TRUE)) }
