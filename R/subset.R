#' @export
`[.tsdl` <- function(x, i)
{
  y <- NextMethod("[")
  class(y) <- c("tsdl", class(y))
  y
}

#' @export
print.tsdl <- function(x,...){
  n <- length(x)
  subject <- sapply(x, function(x) attr(x, "subject"))
  frequency <- sapply(x, function(x) attr(x, "tsp")[3])

  tsub <- NULL
  tfre <- NULL
  if(length(unique(subject))==1)
    tsub <- subject[1]
  if(length(unique(frequency))==1)
    tfre <- frequency[1]

  cat(paste0("Time Series Data Library: ",n,
             switch(is.null(tsub)+1, paste0(" ",tsub), NULL),
             " time series ",
             switch(is.null(tfre)+1, paste0("with frequency ",tfre), NULL)),
      "\n\n")
  tab1 <- table(subject,frequency, dnn = c("Subject","Frequency"))
  # Unique frequency and subject
  if(length(unique(subject))==1 && length(unique(frequency))==1)
    return(print(tab1))
  csum <- margin.table(tab1,2)
  tab2 <- as.table(rbind(tab1,Total=csum))
  # Single frequency
  if(length(unique(frequency))==1){
    names(dimnames(tab2))  <-  c("Subject","Frequency")
    return(print(tab2))
  }
  # Single subject
  if(length(unique(subject))==1){
    rsum <- margin.table(tab1,1)
    tab3 <- as.table(cbind(tab1,Total=rsum))

  } else {
    # Multiple subject and frequency
    rsum <- margin.table(tab2,1)
    tab3 <- as.table(cbind(tab2,Total=rsum))
  }
  names(dimnames(tab3))  <-  c("Subject","Frequency")
  return(print(tab3))
}




tsdl_sub <- function(x,getdata){

  # Character Subject
  if(is.character(getdata)){
    subject <- tolower(sapply(x, function(x) attr(x, "subject")))
    subjecttable <- unique(subject)
    getdata <- subjecttable[charmatch(tolower(getdata),subjecttable)]

    if(length(getdata) != 1)
      stop("Ambiguous data type")
    else if(is.na(getdata))
      stop("No data. Check your input data type")

    choose <- subject %in% getdata
  }
  # Numeric Frequency
  if(is.numeric(getdata)){
    frequency <- sapply(x, function(x) attr(x, "tsp")[3])
    choose <- frequency %in% getdata
  }

  if(sum(choose) == 0)
    stop("No data")

  return(x[choose])
}

tsdl_addl <- function(x,getdata,field){

  # Numeric Start
  if(is.numeric(getdata)){
    start <- sapply(x, function(x) attr(x, "tsp")[1])
    choose <- start %in% getdata
  }
  # Character
  if(is.character(getdata)){
    # Source
    if(field %in% "source"){
      source <- sapply(x, function(x) attr(x, "source"))
      choose <- grepl(getdata, source)
    } else {
      # Description
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
#' @author Yangzhuoran Yang
#' @keywords data
#' @examples
#' # Subset by frequency
#' tsdl_quarterly <- subset(tsdl,4)
#' tsdl_quarterly
#'
#' # Subset by frequency and subject
#' tsdl_daily_industry <- subset(tsdl,12,"Industry")
#' tsdl_daily_industry
#'
#' # Subset by source
#' tsdl_abs <- subset(tsdl, source = "Australian Bureau of Statistics")
#' tsdl_abs
#'
#' # Subset by starting year
#' tsdl_1948 <- subset(tsdl, start = 1948)
#' tsdl_1948
#'
#' # Subset by description
#' tsdl_nettraffic <- subset(tsdl, description = "Internet traffic")
#' tsdl_nettraffic
#'
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



