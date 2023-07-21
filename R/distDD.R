#' Distributional treatment effect
#'
#' Wrapper for `didFF(..., distributionalTE=TRUE)`, which computes
#' the distributional treatment effects. This estimates the actual
#' distribution of `Y(1)` minus the implied distribution for `Y(0)` in
#' each outcome bin (for the treated group).

distDD <-function(...) { base::return(didFF(..., distributionalTE=TRUE)) }
