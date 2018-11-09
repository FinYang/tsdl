#' @export
`[.tsdl` <- function(x, i)
{
  y <- NextMethod("[")
  class(y) <- c("tsdl", class(y))
  y
}


tsdl_sub <- function(x,getdata){

  if(is.character(getdata)){
    subject <- tolower(sapply(x, function(x) attr(x, "subject")))
    subjecttable <- unique(subject)
    getdata <- subjecttable[charmatch(tolower(getdata),subjecttable)]

    if(length(getdata) != 1)
      stop("Ambiguous data type")
    else if(is.na(getdata))
      stop("Unknown data type")

    choose <- subject %in% getdata
  }
  if(is.numeric(getdata)){
    frequency <- sapply(x, function(x) attr(x, "tsp")[3])
    choose <- frequency %in% getdata
  }

  if(sum(choose) == 0)
    stop("No data")

  return(x[choose])
}



#' Subset of time series from the Time Series Data Library
#'
#' \code{subset.tsdl} returns a subset of the time series data from
#' the Time Series Data Library. Subsets can be for specific periods,
#' or specific types of data or both.
#'
#'
#' See \code{unique(meta_tsdl$frequency)} for possible values
#' for \code{cond1} and \code{cond2} denoting frequency.
#'
#' See \code{unique(meta_tsdl$subject)} for possible values
#' for \code{cond1} and \code{cond2} denoting subject.
#'
#' Partial matching used for both conditions.
#'
#' @param x TSDL data or a subset of TSDL data
#' @param cond1 Subject (type) or frequency of the data. Subject is a character variable and
#' frequency is a numeric variable.
#' @param cond2 Optional second condition specifying type or period of the
#' data, depending on \code{cond1}.  If \code{cond1} denotes subject then
#' \code{cond2} would denote frequency, but if \code{cond1} denotes frequency then
#' \code{cond2} would denote subject.
#' @param ... Other arguments.
#' @return An object of class \code{tsdl} consisting of the selected series.
#'
#' @seealso \code{\link{tsdl}}
#' @seealso \code{\link{meta_tsdl}}
#'
#' @keywords data
#' @examples
#'
#' tsdl.quarterly <- subset(tsdl,4)
#' tsdl.daily.industry <- subset(tsdl,365,"Industry")
#' @export
#'
subset.tsdl <- function(x,cond1,cond2,...)
{
  T1 <- structure(tsdl_sub(x,cond1),class="Mcomp")
  if(!missing(cond2))
  {
    if(is.character(cond2))
    T2 <- structure(tsdl_sub(T1,cond2),class="Mcomp")
    return(T2)
  }
  else
    return(T1)
}
