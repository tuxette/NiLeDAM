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

