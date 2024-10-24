#' Difference-in-Differences and Functional Form
#'
#' The `didFF` R package assesses when the validity of difference-in-differences and related estimators depends on functional form, based on the theoretical results in [Roth and Sant’Anna (2023)](https://doi.org/10.3982/ECTA19402).
#' The package provides a test for whether parallel trends is insensitive to functional form by estimating the implied density of potential outcomes under the null and checking if it is significantly below zero at some point.
#' It also provides a distributional DiD estimators based on a distributional parallel trends assumption.
#'
#' @keywords internal
#' @references
#' \cite{Roth, Jonathan and Sant'Anna, Pedro H. C. (2023),
#' "When is Parallel Trends Sensitive to Functional Form?"
#'  Econometrica, vol. 91 (2), pp. 737–747, \doi{10.3982/ECTA19402}}
"_PACKAGE"
NULL
utils::globalVariables('.data')
