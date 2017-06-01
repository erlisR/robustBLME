##' @useDynLib robustBLME
##' @import Rcpp
##' @import doParallel
##' @import foreach
##' @import iterators
##' @importFrom numDeriv jacobian
##' @importFrom utils txtProgressBar setTxtProgressBar
##' @importFrom graphics abline plot
##' @importFrom stats splinefun uniroot integrate dnorm median nlminb density
##' @importFrom mvtnorm rmvnorm
##' @importFrom parallel mclapply
##' @importFrom lme4 lmer VarCorr getME
##' @importFrom stats getCall
##' @title Fits robust Bayesian linear mixed-effects models (BLMM) to data via robust REML estimating functions.
##'
##' @description This is the main function of the package which implements the method of Ruli et al. (2017). It fits robust Bayesian LMMs to data, via robust REML estimating functions. The robust estimating functions are those proposed by Richardson & Welsh (1995), which are robust versions of restricted maximum likelihood (REML) estimating equations. An ABC-MCMC algorithm is used and the data are summarised through a rescaled version of the aforementioned estimating functions. See Ruli et al. (2017) for the details of the method. The current version (0.1.2) supports only models with a single random effects. Extensions to more general settings will be provided in the future versions of the package.
##'
##' @usage rblme(nabc, h.obj,
##'         chain.control = list(trace.init = NULL, thin.by = NULL),
##'         n.cores = 1)
##' @param nabc the number of posterior samples to be drawn.
##' @param h.obj a list of objects as returned by the \code{tune.h} function.
##' @param chain.control parameters for tracing and thinning the chain.
##' @param n.cores the number of cores for parallel computation.
##'
##' @return as list with elements \code{abc} and \code{effi}. The latter is the actual acceptance rate of the ABC-MCMC algorithm whereas the former gives the posterior samples in a matrix of \eqn{(q + c) \times nabc}, where \eqn{q} is the number of fixed effects, i.e. the columns of the design matrix and \eqn{c = 2} is the number of variance components. Hence the first \eqn{q} rows of the matrix abc give the posterior samples of the fixed effects and the last two components give the posterior samples for the variance components.
##'
##' @references
##' Ruli E., Sartori N. & Ventura L. (2017)
##' Robust approximate Bayesian inference with an application to linear mixed models.
##' \url{http://arxiv.org/abs/??}
##'
##' Richardson A. M. & Welsh A. H. (1995) Robust restricted maximum likelihood in mixed linear models. \emph{Biometrics} \bold{51}, 1429-1439.
##'
##' @examples
##'
##' ## The following example is meant only to enhance function documentation.
##' ## For realistic applications probably you'll need to take a larger sample.
##' data(ergoStool)
##'
##' require(lme4)
##' fm1 <- lmer(effort~Type + (1| Subject), data = ergoStool)
##'
##' ## tune h to get 0.8% acceptance
##' hopt <- tune.h(effort~Type + (1|Subject), data = ergoStool,
##'                acc.rate = 0.008, n.sim.HJ = 500, grid.h = seq(0.3, 0.7, len = 10),
##'                prior = list(beta.sd = 10, s2.scale = 5), n.cores = 1)
##'
##' ## draw posterior samples with hopt.
##' abc.tmp <- rblme(nabc = 1e+5, h.obj = hopt,
##'                  n.cores = 1)


##' # process ABC samples
##' abc.sim <- t(abc.tmp$abc)
##' abc.sim[,c(5,6)] <- exp(abc.sim[,c(5,6)])

##' # ABC posterior
##' colMeans(abc.sim)

##' # REML estimates
##' summary(fm1)
##'
##' @export
rblme <- function(nabc,
                  h.obj,
                  chain.control = list(trace.init = NULL, thin.by = NULL),
                  n.cores = 1){

  if(missing(h.obj))
      stop("\'h.obj\' must be a valid object as returned by \'tune.h()\'")

  if(mode(chain.control$trace.int) != "numeric")
    chain.control$trace.int = floor(nabc/10)


  if(mode(chain.control$thin.by) != "numeric")
    chain.control$thin.by = floor(1/h.obj$h.opt)

  thin = which(rep(1:chain.control$thin.by, len = nabc) == chain.control$thin.by) - 1

  # parallel computing via forking under windows doesn't work!
  if(.Platform$OS.type != "unix" || n.cores < 2){
    out <- .Call('robustBLME_ABCkern_reml2',
                 PACKAGE = 'robustBLME',
                 nabc,
                 h.obj$h.opt,
                 h.obj$y,
                 h.obj$Xn,
                 h.obj$ZZt_b,
                 h.obj$ZZt_b_ii,
                 h.obj$ZZt_eps,
                 h.obj$ZZt_eps_ii,
                 h.obj$cHub,
                 h.obj$cHub2,
                 h.obj$K2n,
                 h.obj$param.hat,
                 h.obj$lch_prop_scale_mat,
                 h.obj$Psi.hat,
                 h.obj$lchol_J_Psi_hat,
                 h.obj$prior$beta.sd,
                 h.obj$prior$s2.scale,
                 thin,
                 trace_int = chain.control$trace.int,
                 h.obj$m_i,
                 h.obj$n_ind)

  } else {
    out <- mclapply(X = 1:n.cores,
                    FUN = function(i)
                      .Call('robustBLME_ABCkern_reml2',
                                PACKAGE = 'robustBLME',
                            nabc,
                            h.obj$h.opt,
                            h.obj$y,
                            h.obj$Xn,
                            h.obj$ZZt_b,
                            h.obj$ZZt_b_ii,
                            h.obj$ZZt_eps,
                            h.obj$ZZt_eps_ii,
                            h.obj$cHub,
                            h.obj$cHub2,
                            h.obj$K2n,
                            h.obj$param.hat,
                            h.obj$lch_prop_scale_mat,
                            h.obj$Psi.hat,
                            h.obj$lchol_J_Psi_hat,
                            h.obj$prior$beta.sd,
                            h.obj$prior$s2.scale,
                            thin,
                            trace_int = chain.control$trace.int,
                            h.obj$m_i,
                            h.obj$n_ind))
  }

  return(out)
}