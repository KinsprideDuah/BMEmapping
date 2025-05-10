#' California Snow Load Data
#'
#' A subset of data from the 7964 measurement locations included in the 2020
#' National Snow Load Study. This data is basically on reliability-targeted
#' snow loads (RTSL) in the state of California.
#' @name casnowload
#' @docType data
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
"casnowload"



#' A detrended reliability-targeted design ground snow loads in Utah
#'
#' This dataset contains detrended reliability-targeted design ground snow load
#' measurements from 232 locations in state of Utah. Of these, 65 sites report
#' precise measurements, treated as hard data, while the remaining 167 sites
#' report imprecise measurements, represented as interval (soft) data. The
#' dataset is structured such that the first 67 rows contain hard (point)
#' measurements, and the remaining rows represent soft data using lower and
#' upper interval bounds. For a detailed explanation of the dataset and its use,
#' refer to the related version described in Duah et al. (2025)
#' <doi:10.1016/j.spasta.2025.100894>
#' @name utahsnowload
#' @docType data
#' @format A data frame with 232 rows and 5 variables:
#'
#'  \describe{
#'  \item{latitude}{Latitude coordinate position}
#'  \item{longitude}{Longitude coordinate position}
#'  \item{hard}{The hard data value}
#'  \item{lower}{The lower endpoint of the soft-interval}
#'  \item{upper}{The upper endpoint of the soft-interval}
#'  }
#' @source \doi{10.1016/j.spasta.2025.100894}
"utsnowload"
