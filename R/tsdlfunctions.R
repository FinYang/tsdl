fn <- function(i)
{
  fname(tsdl[i,1],tsdl[i,2])
}

fname <- function(folder,file)
{
  paste("http://robjhyndman.com/tsdldata/",folder,"/",file,sep="")
}

findfolder <- function(file)
{
  require(RCurl)
  if(url.exists(fname("data",file)))
    return("data")
  else if(url.exists(fname("annual",file)))
    return("annual")
  else if(url.exists(fname("monthly",file)))
    return("monthly")
  else if(url.exists(fname("misc",file)))
    return("misc")
  else if(url.exists(fname("roberts",file)))
    return("roberts")
  else if(url.exists(fname("askew",file)))
    return("askew")
  else if(url.exists(fname("astatkie",file)))
    return("astatkie")
  else if(url.exists(fname("baracos",file)))
    return("baracos")
  else if(url.exists(fname("blowfly",file)))
    return("blowfly")
  else if(url.exists(fname("boxjenk",file)))
    return("boxjenk")
  else if(url.exists(fname("cnelson",file)))
    return("cnelson")
  else if(url.exists(fname("commod",file)))
    return("commod")
  else if(url.exists(fname("ecology1",file)))
    return("ecology1")
  else if(url.exists(fname("epi",file)))
    return("epi")
  else if(url.exists(fname("htong",file)))
    return("htong")
  else if(url.exists(fname("hurst",file)))
    return("hurst")
  else if(url.exists(fname("korsan",file)))
    return("korsan")
  else if(url.exists(fname("lamarche",file)))
    return("lamarche")
  else if(url.exists(fname("londonwq",file)))
    return("londonwq")
  else if(url.exists(fname("noakes",file)))
    return("noakes")
  else if(url.exists(fname("prothero",file)))
    return("prothero")
  else if(url.exists(fname("pruscha",file)))
    return("pruscha")
  else if(url.exists(fname("finance",file)))
    return("finance")
  else if(url.exists(fname("sanfran",file)))
    return("sanfran")
  else if(url.exists(fname("thompsto",file)))
    return("thompsto")
  else if(url.exists(fname("wei",file)))
    return("wei")
  else if(url.exists(fname("wisconsi",file)))
    return("wisconsi")
  else
    return("unknown")
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
  return(x)
}

