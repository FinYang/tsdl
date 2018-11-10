#' Time Series Data Library data
#'
#' The time series from the Time Series Data Library.
#'
#' @format \code{tsdl} is a list of 648 series of class \code{tsdl}.
#' Each series within \code{tsdl} is of class \code{ts} with the following attributes:
#' \describe{
#'   \item{tsp}{Start time in time units, End time and Frequency. See \code{?tsp} for details.}
#'   \item{class}{Class of \code{ts}}
#'   \item{source}{Source of the time series.}
#'   \item{description}{A short description of the time series.}
#'   \item{subject}{Subject (or type) of the time series.}
#' }
#' @author Rob Hyndman
#' @author Yangzhuoran Yang
#' @seealso \code{\link{meta_tsdl}}
#' @references
#' Hyndman, R.J. "Time Series Data Library",
#' \url{https://datamarket.com/data/list/?q=provider:tsdl}.
#'
#' @source
#' \url{https://datamarket.com/data/list/?q=provider:tsdl}.
#'
#' @keywords datasets
#' @examples
#' tsdl
#' subset(tsdl,"Sales")
"tsdl"
