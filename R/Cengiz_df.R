#'  Minimum wage dataset
#'
#'  A dataset containing number of people at each wage level in 25c bins, from
#'  2007 to 2015.  This is a subset of the dataset used in Roth and Sant'Anna
#'  (2023) that was in turn is derived from Cengiz et al. (2019). See those
#'  papers for additional descriptions.
#'
#' @format A data frame with 214812 rows and 9 variables:
#' \describe{
#'   \item{wagebins}{Wage bin}
#'   \item{statenum}{State ID}
#'   \item{year}{Year}
#'   \item{quarterdate}{Quarter}
#'   \item{overallcountpc}{Employment per capita in wage bin}
#'   \item{treated_quarter}{Was state treated that quarter}
#'   \item{treated_year}{Was state treated last 4 consecutive quarters}
#'   \item{MW}{Actual minimum wage}
#'   \item{population}{State population}
#' }
#' @source Roth and Sant'Anna (2023)
#' @export
"Cengiz_df"

