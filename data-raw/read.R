tsdl <- read.csv("tsdl.csv",header=TRUE, stringsAsFactors = F)


fname <- function(folder,file)
{
  paste0(folder,"/",file)
}

read.tsdl <- function(i,silent=TRUE)
{
  fn <- fname(tsdl[i,1],tsdl[i,2])
  #print(fn)

  if(tsdl[i,2]=="niagra.dat") # Special case
  {
    x <- scan(fn,skip=tsdl[i,7])
    x <- x[x>2000]
  }
  else if(tsdl[i,9]=="Column")
  {
    if(class(try(x <- read.table(fn, skip=tsdl[i,7], header=TRUE, colClasses="character", flush=TRUE,fill=TRUE),silent=silent))=="try-error")
    {
      if(class(try(x <- read.csv(fn, sep=",",skip=tsdl[i,7], header=TRUE, colClasses="character", flush=TRUE,fill=TRUE),silent=silent))=="try-error")
        return(i)
    }
    if(ncol(x)==1) ## Didn't read properly
    {
      if(class(try(x <- read.table(fn, sep=",", skip=tsdl[i,7], header=TRUE, colClasses="character", flush=TRUE,fill=TRUE),silent=silent))=="try-error")
        return(i)
    }
    #Remove columns that are dates, time stamps or missing
    drop <- numeric(0)
    for(j in 1:ncol(x))
    {
      y <- as.numeric(gsub(",","",x[,j]))
      if(sum(is.na(y))==length(y))
        vy <- NA
      else if(sum(is.na(diff(y))) == length(y)-1)
        vy <- NA
      else
        vy <- var(y,na.rm=TRUE)
      if(is.na(vy))
        drop <- c(drop,j)
      else if(vy < 1e-10)
        drop <- c(drop,j)
      else if(var(diff(y),na.rm=TRUE) < 1e-10)
        drop <- c(drop,j)
      else if(is.element(colnames(x)[j],c("year","Year","year.","yr","Yrwk")))
        drop <- c(drop,j)
      else
        x[,j] <- y
    }
    if(length(drop)>0)
      x <- x[,-drop,drop=FALSE]
  }
  else
    x <- scan(fn,skip=tsdl[i,7],na.strings=c("NA","-","*","999999","000"))
  # Add time series characteristics
  if(is.na(tsdl[i,5]))
    tsdl[i,5] <- 1
  if(is.na(tsdl[i,6]))
    tsdl[i,6] <- 1
  x <- ts(x,frequency=tsdl[i,5],start=tsdl[i,6])
  attributes(x) <- c(attributes(x),source = tsdl[i,3], description = tsdl[i,4], subject = tsdl[i,8])
  return(x)
}

tsdl_data <- list()
for(i in 1:nrow(tsdl)){
  tsdl_data[[i]] <- read.tsdl(i)
}

meta_tsdl <- dplyr::select(tsdl, Source, Description, Frequency, Start, Subject)
names(meta_tsdl) <- tolower(names(meta_tsdl))
meta_tsdl$frequency[is.na(meta_tsdl$frequency)] <- 1
meta_tsdl$start[is.na(meta_tsdl$start)] <- 1

tsdl <- tsdl_data
class(tsdl) <- "tsdl"
usethis::use_data(tsdl, overwrite = T)
usethis::use_data(meta_tsdl, overwrite = T)

