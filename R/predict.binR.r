# vim: shiftwidth=2 tabstop=2
##------------------------------------------------------------------------------
#' Predict binR
#' 
#' Apply binning defined in binR object to new dataset
#' 
#' 
#' @param obj binR object
#' @param newdata the new data.frame to bin
#' @return a data.frame with the binned objects
#' 
#' @export
##------------------------------------------------------------------------------
predict.binR <- function(obj, newdata) {
  fx <- as.formula(paste('~',paste(bdat$vars, collapse="+")))
  mframe <- model.frame(fx, newdata)

  out <- foreach(d=obj$vars, .combine=data.frame, .multicombine=T) %dopar% {
    cut(mframe[ ,d], breaks=obj$breaks[[d]])
  }
  names(out) <- obj$vars
  out
}
