setClass("ageTests", representation(nb.pop="vector",
                                    best.nb="numeric",
                                    best.res="oneAgeTest"))
                                
# TODO: Add validity tests

setMethod("print", "ageTests", function(x, ...) {
  cat("Test if the",length(x@best.res@data@ages),
      "estimated ages are coming from\n",x@nb.pop,"\npopulation(s).\n\n")
  cat("    The ages are found to be lickely coming from",x@best.nb,
      "population(s) at level",x@best.res@level*100,"%.\n",
      "   Chi2 test statistic:",x@best.res@S," ~ df:",x@best.res@df,"\n\n")
  cat("    Estimated ages:", x@best.res@ages, "\n\n")
  cat("    Population numbers:\n", x@best.res@which.pop)
})

setMethod("show", "ageTests", function(object) {
  print(object)
})

setMethod("summary", "ageTests", function(object, ...) {
  print(object)
})

setMethod("plot", "ageTests", function(x, y=NULL, ...) {
  args <- list(...)
  args$x <- x@best.res
  do.call("plot",args)
})