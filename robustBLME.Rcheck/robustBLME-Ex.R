pkgname <- "robustBLME"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "robustBLME-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('robustBLME')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("kdeFSBT")
### * kdeFSBT

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: kdeFSBT
### Title: Full Significance Bayesian Testing
### Aliases: kdeFSBT

### ** Examples


x <-  rnorm(1000, 0, 1)
kdeFSBT(-2, x)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("kdeFSBT", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rblme")
### * rblme

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rblme
### Title: Fits robust Bayesian Linear Mixed-effects Models (LMM) to data
###   via robust REML estimating functions.
### Aliases: rblme

### ** Examples


## The following example is meant for function documentation.
## For realistic use probably you'll need to take a larger sample and choose a
## "better" bandwidth h.

data(ergoStool)

require(lme4)
fm1 <- lmer(effort~Type + (1| Subject), data = ergoStool)

## tune h to get 0.8% acceptance
hopt <- tune.h(effort~Type + (1|Subject), data = ergoStool,
               acc.rate = 0.008, n.sim.HJ = 500, grid.h = seq(0.3, 0.7, len = 10),
               prior = list(beta.sd = 10, s2.scale = 5), n.cores = 1)

## draw posterior samples with hopt.
abc.tmp <- rblme(nabc = 1e+5, h.obj = hopt,
                 n.cores = 1)

# process ABC samples
abc.sim <- t(abc.tmp$abc)
abc.sim[,c(5,6)] <- exp(abc.sim[,c(5,6)])

# ABC posterior
colMeans(abc.sim)

# REML estimates
summary(fm1)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rblme", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("tune.h")
### * tune.h

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: tune.h
### Title: Tune ABC distance bandwidth
### Aliases: tune.h

### ** Examples

## tune h to get 0.8% acceptance
hopt <- tune.h(effort~Type + (1|Subject), data = ergoStool,
               acc.rate = 0.008, n.sim.HJ = 500, grid.h = seq(0.3, 0.7, len = 10),
               prior = list(beta.sd = 10, s2.scale = 5), n.cores = 1)
str(hopt)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("tune.h", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
