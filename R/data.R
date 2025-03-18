#' California Snow Load Data
#'
#' A subset of data from the 7964 measurement locations included in the 2020
#' National Snow Load Study. This data is basically on reliability-targeted
#' snow loads (RTSL) in the state of California.
#'
#' @format A data frame with 346 rows and 8 columns.
#'
#' \describe{
#'   \item{STATION}{Name of the snow measuring station}
#'   \item{LATITUDE}{Latitude coordinate position}
#'   \item{LONGITUDE}{Longitude coordinate position}
#'   \item{ELEVATION}{Elevation of the measring station (measured in meters)}
#'   \item{RTSL}{The hard data RTSL value}
#'   \item{LOWER}{The lower endpoint RTSL}
#'   \item{UPPER}{The upper endpoint RTSL}
#'   \item{TYPE}{Type of snow measurement, WESD is direct and SNWD is indirect
#'               measurement. Direct measurements are hard data and have the
#'               lower, upper and center values are the same. Indirect
#'               measurements have LOWER < RTSL < UPPER.}
#' }
#' @source
#' * [The 2020 National Snow Load Study](https://doi.org/10.26077/200k-pr86)
"california"

#' A hard and soft-interval design ground snow load dataset for Utah.
#'
#' A dataset containing the 67 hard data and 165 soft-interval data used in the
#' analysis of Duah et. al. (2025). The 232 measurement locations
#' included in the dataset are taken from The 2020 National Snow Load Study
#' (Bean et. al., 2021).
#'
#' @format A data frame with 232 rows and 5 variables:
#'
#'  \describe{
#'  \item{lat}{Latitude coordinate position}
#'  \item{lon}{Longitude coordinate position}
#'  \item{center}{The hard data value}
#'  \item{lower}{The lower endpoint of the soft-interval}
#'  \item{upper}{The upper endpoint of the soft-interval}
#'  }
#' @source
#' * [The 2020 National Snow Load Study](https://doi.org/10.26077/200k-pr86)
"utah"

