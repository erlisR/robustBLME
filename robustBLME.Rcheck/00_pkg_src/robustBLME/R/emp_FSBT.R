##' @title Full Significance Bayesian Testing
##' @description Performs Full Significance Bayesian Testing (FSBT) for univariate sharp
##' null hypothesis based on a posterior sample. The marginal posterior density is obtained by kernel density estimation from the posterior sample provided through the \code{sample} argument.
##' @usage emp_FSBT(H0, sample)
##'
##' @param H0 The value under the null hypothesis.
##' @param sample A monte Carlo sample from the marginal posterior distribution.
##'
##' @return double
##'
##' @references
##' Pereira, C. A. d. B., Stern, J. M. and Wechsler, S. (2008) Can a significance test be genuinely Bayesian? \emph{Baysian Analysis} \bold{3}, 79-100.
##'
##' @export
emp_FSBT <- function(H0, sample){

  den <- splinefun(density(sample))
  d.H0 <- den(H0)
  cent_dens <- median(sample)

  if(cent_dens < H0){
    param.H0.other <- uniroot(function(x)
      den(x)-d.H0, interval=c(min(sample), cent_dens))$root
    EV <- integrate(function(x) den(x), min(sample), param.H0.other)$value +
      integrate(function(x) den(x), H0, max(sample))$value
  } else {
    param.H0.other <- uniroot(function(x)
      den(x)-d.H0, interval=c(cent_dens, max(sample)))$root
    EV <- integrate(function(x) den(x), param.H0.other, max(sample))$value +
      integrate(function(x) den(x), min(sample), H0)$value
  }
  return(EV)
}