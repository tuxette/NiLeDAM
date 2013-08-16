setClass("oneAgeTest", representation(data="ages",
                                      ages="vector",
                                      sd="vector",
                                      ic="matrix",
                                      S="numeric",
                                      thres="numeric",
                                      df="numeric",
                                      level="numeric",
                                      h0="logical",
                                      which.pop="vector"))
                                
# TODO: Add validity tests

setMethod("print", "oneAgeTest", function(x, ...) {
  cat("Test if the",length(x@which.pop),"estimated ages are coming from",
      length(x@ages),"population(s).\n\n")
  if (x@h0) {
    cat("    The ages are not found to be unlickely coming from",length(x@ages),
        "population(s) at level",x@level*100,"%.\n",
        "   Chi2 test statistic:",x@S," ~ df:",x@df,"\n\n")
    cat("    Estimated ages:", x@ages, "\n\n")
    cat("    Population numbers:\n", x@which.pop)
  } else {
    cat("    The ages are found to be unlickely coming from",length(x@ages),
        "population(s) at level",x@level*100,"%.\n",
        "   Chi2 test statistic:",x@S," ~ df:",x@df,"\n\n")
  }
})

setMethod("show", "oneAgeTest", function(object) {
  print(object)
})

setMethod("summary", "oneAgeTest", function(object, ...) {
  print(object)
})

setMethod("plot", "oneAgeTest", function(x, y=NULL, ...) {
  args <- list(...)
  args$type <- "n"
  if (is.null(args$xlab)) args$xlab <- "Ma"
  if (is.null(args$ylab)) args$ylab <- ""
  if (is.null(args$main)) args$main <- ""
  # densities for the estimated ages
  dens.x <- seq(min(x@data@ages)-5*x@data@sd[which.min(x@data@ages)],
                max(x@data@ages)+5*x@data@sd[which.max(x@data@ages)],
                1)
  dens.y <- apply(cbind(x@data@ages,x@data@sd),1,
                  function(one.obs) dnorm(dens.x,mean=one.obs[1],sd=one.obs[2]))
  y <- apply(cbind(x@ages, x@sd), 1,
             function(one.obs) dnorm(dens.x, mean=one.obs[1], sd=one.obs[2]))
  args$x <- dens.x[1]
  args$y <- dens.y[1,1]
  if (is.null(args$xlim)) args$xlim <- range(dens.x)
  if (is.null(args$ylim)) args$ylim <- c(0,max(y))
  do.call("plot",args)
  apply(dens.y, 2, function(y) lines(dens.x,y))
  sapply(1:length(x@data@ages), function(obs.nb) {
    one.obs <- x@data@ages[obs.nb]
    text(one.obs, dnorm(one.obs,one.obs,x@data@sd[obs.nb])*1.2,
         as.character(obs.nb), cex=0.8)
  })
  # display densities for the common age estimates
  if (is.null(args$lty)) args$lty <- 2
  if (is.null(args$lwd)) args$lwd <- 2
  if (is.null(args$col)) args$col <- "red"
  apply(y, 2, function(one.y) {
    lines(dens.x,one.y,lty=args$lty,lwd=args$lwd,col=args$col)
  })
  sapply(1:length(x@ages), function(nb){
    text(x@ages[nb]*1.15, max(y[,nb])*0.8,
         paste(x@ages[nb]," +/- ",round(x@sd[nb]*qnorm(1-x@level/2),0),sep=""),
         col=args$col,lty=2)
  })
  invisible()
})