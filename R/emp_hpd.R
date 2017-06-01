##' @title Empirical Highest Posterior Density Interval
##' @description Computes empirical Highest Posterior Density (HPD) interval from a posterior sample. Works only for scalar marginal posteriors.
##' @usage hpd(x, prob = 0.95)
##'
##' @param x a univariata or marginal posterior sample.
##' @param prob the required posterior probability content.
##'
##' @export
hpd <- function(x, prob = 0.95){
  conf <- min(prob, 1-prob)
  n <- length(x)
  nn <- round( n*conf )
  x <- sort(x)
  xx <- x[ (n-nn+1):n ] - x[1:nn]
  m <- min(xx)
  nnn <- which(xx==m)[1]
  return( c( x[ nnn ], x[ n-nn+nnn ] ) )
}
