#' Euclidean algorithm
#'
#' Find the greatest common divisor of two numbers using the Euclidean algorithm.
#' The algorithm repeatedly applies the division algorithm until the remainder is zero.
#' The last non-zero remainder is the greatest common divisor.
#'
#'
#' @param a A number
#' @param b A number
#'
#' @returns The greatest common divisor of a and b
#' @export
#'
#' @examples
#' euclidean(123612, 13892347912)
#' euclidean(100, 1000)
#'
#' @references \url{https://en.wikipedia.org/wiki/Euclidean_algorithm}
euclidean <- function(a, b) {
  if(!is.numeric(a) || length(a)!=1){
    stop("a must be a numeric scalar")
  } else if(!is.numeric(b) || length(b)!=1){
    stop("b must be a numeric scalar")
  }
  while(b != 0){
    t <- b
    b <- a %% b
    a <- t
  }
  return(a)
}
