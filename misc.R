mymean <- function (x, ...) mean(x, na.rm=T, ...)

mysd <- function (x,...) sd(x, na.rm=T,...)

lengthu <- function (x) length(unique(x))

drop.empty.cols<-function(df){
  Filter(function(x) !all(is.na(x)), df)
}

binom.ci<-function (x){
  ci<-binconf(sum(x), length(x))
  c(y=ci[1],ymin=ci[2],ymax=ci[3],len=length(x))
}