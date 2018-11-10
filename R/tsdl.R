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

tsdl_addl <- function(x,getdata,field){

  if(is.numeric(getdata)){
    start <- sapply(x, function(x) attr(x, "tsp")[1])
    choose <- start %in% getdata
  }

  if(is.character(getdata)){
    if(field %in% "source"){
      source <- sapply(x, function(x) attr(x, "source"))
      choose <- grepl(getdata, source)
    } else {
      description <- sapply(x, function(x) attr(x, "description"))
      choose <- grepl(getdata, description)
    }
  }

  if(sum(choose) == 0 || length(choose) == 0)
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
#' @param cond1,cond2 Optional conditions specifying subject (type) or frequency of the data.
#' Character variable would be recognised as subject
#' and numeric variable would be recognised as frequency. Positions are interchangeable.
#' @param ... Other arguments specifying conditions for where to search and what to search.
#' \describe{
#' \item{\code{start}}{Numeric variable specifying starting year of the series.}
#' \item{\code{description}}{String to be matched in the description attribute of the time series.}
#' \item{\code{source}}{String to be matched in the source attribute of the time series.}
#' }
#' @return An object of class \code{tsdl} consisting of the selected series.
#'
#' @seealso \code{\link{tsdl}}
#' @seealso \code{\link{meta_tsdl}}
#'
#' @keywords data
#' @examples
#'
#' # Subset by frequency
#' tsdl_quarterly <- subset(tsdl,4)
#' # Subset by frequency and subject
#' tsdl_daily_industry <- subset(tsdl,365,"Industry")
#' # Subset by source
#' tsdl_abs <- subset(tsdl, source = "Australian Bureau of Statistics")
#' # Subset by starting year
#' tsdl_1948 <- subset(tsdl, start = 1948)
#' # Subset by description
#' tsdl_nettraffic <- subset(tsdl, description = "Internet traffic")
#' @export
#'
subset.tsdl <- function(x,cond1,cond2,...){
  if((!missing(cond1)) && (!missing(cond2)) && identical(class(cond1), class(cond2)))
    stop("Duplicated conditions")
  if(!missing(cond1))
    x <- structure(tsdl_sub(x,cond1),class="tsdl")
  if(!missing(cond2))
    x <- structure(tsdl_sub(x,cond2),class="tsdl")

  argls <- list(...)
  argnames <- names(list(...))

  if(any(!argnames %in% c("source", "start", "description")))
    stop("Unrecognised conditions")
  if(length(unique(argnames)) != length(argnames))
    stop("Duplicated conditions")

  if(any(sour <- (argnames %in% c("source"))))
    x <- structure(tsdl_addl(x,as.character(argls[[sour]]), field = "source"),class="tsdl")
  if(any(star <- (argnames %in% c("start"))))
    x <- structure(tsdl_addl(x,as.numeric(argls[[star]])),class="tsdl")
  if(any(desc <- (argnames %in% c("description"))))
    x <- structure(tsdl_addl(x,as.character(argls[[desc]]), field = "description"),class="tsdl")

  return(x)
}



