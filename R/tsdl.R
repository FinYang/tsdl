source("tsdlfunctions.R")
tsdl <- read.csv("tsdl.csv",header=TRUE)
tsdl[,2] <- as.character(tsdl[,2])
tsdl[,1] <- as.character(tsdl[,1])
tsdl[,9] <- as.character(tsdl[,9])

for(i in 1:nrow(tsdl))
{
  x <- read.tsdl(i)
  if(is.null(ncol(x)))
    plot(x,main=tsdl[i,4],xlab=fn(i))
  else if(ncol(x)<11)
    plot(x,main=tsdl[i,4],xlab=fn(i))
  else
  {
    par(mfrow=c(1,2))
    plot(x[,1:10],main=tsdl[i,4],xlab=fn(i))
    plot(x[,-c(1:10)],main=tsdl[i,4],xlab=fn(i))
    par(mfrow=c(1,1))
  }
}
