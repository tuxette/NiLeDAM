setGeneric("tests", function(object, ...) standardGeneric("tests"))

setMethod("tests", "ages", function(object, nbmin=1, nbmax=NULL,
                                    level=object@level, verbose=TRUE) {
  if (is.null(nbmax)) {
    res.test <- testPop(object, nbmin, level)
  } else {
    if (nbmax <= nbmin)
      stop("'nbmax' must be larger than 'nbmin'.")
    res.test <- testAges(object, nbmin, nbmax, level)
  }
  
  if (verbose) print(res.test)
  invisible(res.test)
})