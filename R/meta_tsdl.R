#' Time Series Data Library metadata.
#'
#' The metadata the Time Series Data Library.
#'
#' @format \code{meta_tsdl} is a data frame containing information about the tsdl series
#' with the following structure:
#' \describe{
#'   \item{source}{Source of the time series.}
#'   \item{description}{A short description of the time series.}
#'   \item{frequency}{Frequency of the time series.}
#'   \item{start}{Start year of the time series.}
#'   \item{subject}{Subject (or type) of the time series.}
#' }
#' @author Rob Hyndman
#' @author Yangzhuoran Yang
#' @seealso \code{\link{tsdl}}
#' @references
#' Hyndman, R.J. "Time Series Data Library",
#' \url{https://datamarket.com/data/list/?q=provider:tsdl}.
#'
#' @keywords datasets
#' @examples
#' str(meta_tsdl)
"meta_tsdl"
