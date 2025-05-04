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
#' @source \url{https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/}
"california"

#' A projected hard and soft-interval design ground snow load dataset for Utah.
#'
#' A dataset comprising 65 hard data points and 167 soft-interval data points,
#' used in the analysis by Duah et al. (2025). The dataset includes 248
#' measurement locations, which are derived from the 2020 National Snow Load
#' Study (Bean et al., 2021). For a comprehensive understanding of the dataset
#' and its usage, please refer to Duah et al. (2025).
#'
#' @format A data frame with 232 rows and 7 variables:
#'
#'  \describe{
#'  \item{latitude}{Latitude coordinate position}
#'  \item{longitude}{Longitude coordinate position}
#'  \item{x}{Projected latitude coordinate}
#'  \item{y}{Projected longitude coordinate}
#'  \item{center}{The hard data value}
#'  \item{lower}{The lower endpoint of the soft-interval}
#'  \item{upper}{The upper endpoint of the soft-interval}
#'  }
#' @source \url{https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/}
"utah"
