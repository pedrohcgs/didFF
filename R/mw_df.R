#'  County Teen Employment Dataset
#'
#'  A dataset containing (the log of) teen employment in 500 counties in the
#'  U.S. from 2003 to 2007.  This is a subset of the dataset used in Callaway
#'  and Sant'Anna (2021).  See that paper for additional descriptions.
#'
#' @format A data frame with 2500 rows and 6 variables:
#' \describe{
#'   \item{year}{the year of the observation}
#'   \item{countyreal}{a unique identifier for a particular county}
#'   \item{lpop}{the log of 1000s of population for the county}
#'   \item{lemp}{the log of teen employment in the county}
#'   \item{first.treat}{the year that the state where the county is located
#'    raised its minimum wage, it is set equal to 0 for counties that have
#'    minimum wages equal to the federal minimum wage over the entire
#'    period.}
#'   \item{treat}{whether or not a particular county is treated in that year}
#' }
#' @source Callaway and Sant'Anna (2021)
#' @references
#'  \cite{Callaway, Brantly and Sant'Anna, Pedro H. C. (2021),
#' "Difference-in-Differences with Multiple Time Periods."
#'  Journal of Econometrics, vol. 225 (2), pp. 200-230, \doi{10.1016/j.jeconom.2020.12.001}}
"mw_df"

