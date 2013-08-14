setClass("ages", representation(data="data.frame",
                                ages="vector",
                                ci="matrix",
                                sd="vector",
                                nloops="numeric",
                                level="numeric"))
# TODO: Add validity tests

setMethod("print","ages", function(x, ...) {
  cat(nrow(x@data),"ages and confidence intervals estimated from",
      x@nloops,"bootstrap samples. Summary:\n")
  print(summary(x@ages))
})

setMethod("show", "ages", function(object) {
  cat(nrow(object@data),"ages and confidence intervals estimated from",
      object@nloops,"bootstrap samples. Summary:\n")
  print(summary(object@ages))
})

setMethod("summary", "ages", function(object, ...) {
  cat(nrow(object@data),"ages and confidence intervals estimated from",
      object@nloops,"bootstrap samples. Summary:\n")
  print(summary(object@ages))
})

setGeneric("tests", function(object, ...) standardGeneric("tests"))

setMethod("tests", "ages", function(object, nbmin=1, nbmax=NULL,
                                   level=object@level, verbose=TRUE) {
  if (is.null(nbmax)) {
    res.test <- testPop(object, nbmin, level)
  } else res.test <- testAges(object, nbmin, nbmax, level)
  
  if (verbose) print(res.test)
  invisible(res.test)
})