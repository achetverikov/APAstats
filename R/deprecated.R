#' Deprecated function(s) in the apastats package
#'
#' These functions are provided for compatibility with older version of
#' the apastats package. They will removed in later releases.
#' @rdname apastats-deprecated
#' @name apastats-deprecated
#' @param ... Parameters to be passed to the modern version of the function
#' @docType package
#' @export  mymean mysum mysd
#' @aliases mymean mysum mysd
#' @section Details:
#' \tabular{rl}{
#'   \code{mymean} \tab now a synonym for \code{\link{mean.nn}}\cr
#'   \code{mysum} \tab now a synonym for \code{\link{sum.nn}}\cr
#'   \code{mysd} \tab now a synonym for \code{\link{sd.nn}}\cr
#' }
#'
mymean <- function(...) {
  .Deprecated("mean.nn", package = "apastats")
  mean.nn(...)
}
mysum <- function(...) {
  .Deprecated("sum.nn", package = "apastats")
  sum.nn(...)
}
mysd <- function(...) {
  .Deprecated("sd.nn", package = "apastats")
  sd.nn(...)
}
