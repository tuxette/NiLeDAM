popsError <- function(centers, ages, ages.sd) {
  best.pop <- sapply(ages, function(one.obs) which.min(abs(one.obs-centers)))
  pops.error <- sum((centers[best.pop]-ages)^2/ages.sd^2)
  return(pops.error)
}

testOnePop <- function(ages, ages.sd, level) {
  est.age <- sum(ages/ages.sd^2)/sum(1/ages.sd^2)
  est.sd <- 1/sqrt(sum(1/ages.sd^2))
  est.IC <- c(est.age-est.sd*qnorm(1-level/2),est.age+est.sd*qnorm(1-level/2))
  chisq.stat <- sum((ages-est.age)^2/ages.sd^2)
  return(list(ages=round(est.age), sd=round(est.sd), ic=round(est.IC),
              S=chisq.stat))
}

testPop <- function(object, nb.pop, level) {
  if (nb.pop==1) {
    res <- testOnePop(object@ages, object@sd, level)
    est.ages <- res$ages
    est.sd <- res$sd
    est.IC <- matrix(res$ic, ncol=2)
    chisq.stat <- res$S
    df <- length(object@ages)-1
    chisq.bound <- qchisq(1-level,df=df)
    is.acceptable <- (chisq.stat < chisq.bound)
    which.pop <- rep(1,length(object@ages))
  } else {
    # affect each observation to one of the population
    res.optim <- optim(rep(mean(object@ages),nb.pop), popsError,
                       ages=object@ages, ages.sd=object@sd)
    which.pop <- sapply(object@ages,
                        function(one.obs) which.min(abs(res.optim$par-one.obs)))
    # test
    res <- sapply(1:nb.pop, function(nb) {
      testOnePop(object@ages[which(which.pop==nb)],
                 object@sd[which(which.pop==nb)], level)
    })
    est.ages <- unlist(res[1,])
    est.sd <- unlist(res[2,])
    est.IC <- matrix(unlist(res[3,]), ncol=2,byrow=T)
    chisq.stat <- sum(unlist(res[4,]))
    df <- length(object@ages)-nb.pop
    chisq.bound <- qchisq(1-level,df=df)
    is.acceptable <- (chisq.stat < chisq.bound)
  }

  res <- new(Class="oneAgeTest", data=object, ages=est.ages, sd=est.sd,
             ic=est.IC, S=chisq.stat, thres=chisq.bound, df=df, level=level,
             h0=is.acceptable, which.pop=which.pop)
  
  invisible(res)
}

testAges <- function(object, nbmin, nbmax, level) {
  nb.pop <- nbmin:nbmax
  res <- lapply(as.list(nb.pop), function(nb) testPop(object, nb, level))
  res.h0 <- unlist(lapply(res, function(x) x@h0))
  if (sum(res.h0)==0) 
    stop(paste("None of the population number is acceptable at level",
               level,"."))

  best.ind <- min(which(res.h0))
  best.nb <- nb.pop[best.ind]
  best.res <- res[[best.ind]]
  res <- new(Class="ageTests",nb.pop=nb.pop,best.nb=best.nb,best.res=best.res)
  invisible(res)
}