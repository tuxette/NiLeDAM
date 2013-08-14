ageEquation <- function(x, an.obs) {
  # Constants initialization
  L232 <- 4.9334*10^(-5)
  L235 <- 9.8485*10^(-4)
  L238 <- 1.55125*10^(-4)
  est.pb <- (an.obs[2]/232.04*(exp(L232*x)-1)+
               an.obs[1]/238.03*0.9928*(exp(L238*x)-1)+
               an.obs[1]/238.03*0.0072*(exp(L235*x)-1))-an.obs[3]/207.2
  
  return(est.pb)
}

ageEqDerivative <- function(x, an.obs) {
  # Constants initialization
  L232 <- 4.9334*10^(-5)
  L235 <- 9.8485*10^(-4)
  L238 <- 1.55125*10^(-4)
  est.pb <- (an.obs[2]/232.04*L232*exp(L232*x)+
               an.obs[1]/238.03*0.9928*L232*exp(L238*x)+
               an.obs[1]/238.03*0.0072*L235*exp(L235*x))
  
  return(as.matrix(est.pb))
}

calculateOneAge <- function(an.obs) {
  age.init <- an.obs[3]/208/(an.obs[2]*2.1325E-07+an.obs[1]*6.7724E-07)
  solution <- nleqslv(age.init, function(x) ageEquation(x,an.obs),
                      function(x) ageEqDerivative(x,an.obs))

  return(round(solution$x))
}

calculateOneDistribution <- function(d, nloops) {
  random.data <- cbind(rnorm(nloops, d[1], d[2]/2),
                       rnorm(nloops, d[3], d[4]/2),
                       rnorm(nloops, d[5], d[6]/2))
  t.random <- apply(random.data,1,calculateOneAge)
  
  return(t.random)
}

calculateAges <- function(measures, nloops=1000, level=0.05, verbose=TRUE,
                          seed=NULL) {
  ## TODO: add validity tests!!
  
  d <- as.matrix(measures)
  # Calculate the ages
  if (verbose) cat("Age estimation...\n")
  t.init <- apply(d[,c(1,3,5)], 1, calculateOneAge)
  names(t.init) <- rownames(measures)
  
  # Estimate confidence interval by MC simulations
  if (verbose) cat("MC simulations...\n",
                   "(it might take a while if 'nloops' is large...\n\n")
  if (!is.null(seed)) set.seed(seed)
  all.rand.t <- t(apply(d, 1, calculateOneDistribution, nloops=nloops))
  ci.bounds <- apply(all.rand.t, 1, quantile, probs=c(level/2,1-level/2))
  colnames(ci.bounds) <- rownames(measures)
  ages.sd <- apply(all.rand.t, 1, sd)
  names(ages.sd) <- rownames(measures)
  
  res <- new(Class="ages", data=measures, ages=t.init, ci=ci.bounds, sd=ages.sd,
             nloops=nloops, level=level)
                  
  if (verbose) print(res)
 
  invisible(res)
}
