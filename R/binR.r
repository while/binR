# vim: shiftwidth=2 tabstop=2
##------------------------------------------------------------------------------
#' binR
#' 
#' Bin a dataset using a formula and a breaks function
#' 
#' Discretize a dataset using a formula.
#' 
#' @author Vilhelm von Ehrenheim
#' @import foreach
#' @import rpart
#' @export
##------------------------------------------------------------------------------
binR <- function(fx, data, algorithm=c("quantile", "rpart", "manual"), ...) {
  # Extract additional arguments
  dots <- list(...)

  # Match algorithm and run the respective function w additional args
  out <- switch(match.arg(algorithm, c("quantile", "rpart", "manual")),
                quantile=do.call(binR_quantile, c(list(fx), list(data), dots)),
                rpart=do.call(binR_rpart, c(list(fx), list(data), dots)),
                manual=do.call(binR_manual, c(list(fx), list(data), dots)))
  out
}
