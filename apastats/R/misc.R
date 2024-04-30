#' Mean with na.rm=T
#'
#' @param x a vector of numbers
#' @param ... other arguments passed to mean
#'
#' @return mean of x with NA removed
#' @export
#'
#' @examples
#' x<-c(NA, 10,90)
#' mean(x)
#' mymean(x)
mymean <- function (x, ...){
  mean(x, na.rm=T, ...)
}

#' SD with na.rm=T
#'
#' @param x a vector of numbers
#' @param ...
#'
#' @return sd of x with NA removed
#' @export
#'
#' @examples
#' x<-c(NA, 10,90)
#' sd(x)
#' mysd(x)
mysd <- function (x,...){
  sd(x, na.rm=T,...)
  }

#' Sum with na.rm=T
#'
#' @param x a vector of numbers
#' @param ...
#'
#' @return sum of x with NA removed
#' @export
#'
#' @examples
#' x<-c(NA, 10,90)
#' sum(x)
#' mysum(x)
mysum <- function (x,...){
  sum(x, na.rm=T,...)
}

#' Length of unique values
#'
#' Counts unique values
#'
#' @param x a vector
#'
#' @return number of unique values in x
#' @export
#'
#' @examples
#' x<-c(5,7,8,9,5,7)
#' length(x)
#' lengthu(x)
lengthu <- function (x){
  length(unique(x))
}

#' Drop empty columns from a data.frame
#'
#' Drop empty (consisting of NA only) columns from a data.frame. Based on http://stackoverflow.com/a/2644009/1344028
#'
#' @param df a data.frame
#'
#' @return data.frame without empty columns
#' @export
#'
#' @examples
#' df<-data.frame(x=rnorm(20), y=rep('A', 20), z=rep(NA, 20))
#' str(df)
#' df<-drop.empty.cols(df)
#' str(df)
#'
drop.empty.cols<-function(df){
  Filter(function(x) !all(is.na(x)), df)
}

#' Binomial confidence interval as a vector
#'
#' @param x a vector of 0 and 1
#'
#' @return a vector of mean, lower CI, upper CI, and length of x
#' @export
#'
#' @examples
#' binom.ci(rbinom(500, 1, prob=0.7))
binom.ci<-function (x){
  ci<-Hmisc::binconf(sum(x), length(x))
  c(y=ci[1],ymin=ci[2],ymax=ci[3],len=length(x))
}

#' Rounded mean
#'
#' Mean rounded to the specified number of digits
#'
#' @param x a number
#' @param digits decimal places
#'
#' @return Mean rounded to the specified number of digits (string)
#' @export mean.round
#' @method generic class
#'
#' @examples
#' mean.round(c(10,99))
#' mean.round(c(10,99,NA))
#' mean.round(c(10,99), 2)
mean.round<-function(x, ...){
  f.round(mymean(x), ...)
}

#' Rounded SD
#'
#' SD rounded to the specified number of digits
#'
#' @param x a number
#' @param digits decimal places
#'
#' @return Mean rounded to the specified number of digits (string)
#' @export
#'
#' @examples
#' sd.round(c(10,99))
#' sd.round(c(10,99,NA))
#' sd.round(c(10,99), 2)
sd.round<-function(x, ...){
  f.round(mysd(x), ...)
}

#' Quietly load libraries
#'
#' @param libs a vector of libraries names
#'
#' @return None
#' @export
#'
#' @examples
#' load.libs(c('ggplot2','apastats','Hmisc'))
load.libs<-function(libs){
  suppressMessages(invisible(lapply(libs, require, character.only=TRUE)))
}


int_to_bin<-function (x) {
  as.integer(paste(rev(as.integer(intToBits(x))), collapse=""))
}

