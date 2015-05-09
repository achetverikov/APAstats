mymean <- function (x, ...) mean(x, na.rm=T, ...)

mysd <- function (x,...) sd(x, na.rm=T,...)

mysum <- function (x,...) sum(x, na.rm=T,...)

lengthu <- function (x) length(unique(x))

drop.empty.cols<-function(df){
  #from http://stackoverflow.com/a/2644009/1344028
  Filter(function(x) !all(is.na(x)), df)
}

binom.ci<-function (x){
  ci<-binconf(sum(x), length(x))
  c(y=ci[1],ymin=ci[2],ymax=ci[3],len=length(x))
}

mean.round<-function(x,digits=0){
  f.round(mymean(x), digits)
}
sd.round<-function(x,digits=0){
  f.round(mysd(x), digits)
}

load.libs<-function(libs){
  suppressMessages(invisible(lapply(libs, require, character.only=TRUE)))
}
