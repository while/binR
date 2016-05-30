# vim: shiftwidth=2 tabstop=2
##------------------------------------------------------------------------------
#' binR
#' 
#' Bin a dataset using a formula and a breaks function
#' 
#' Bin or dkiscretize a dataset using a formula.
#' 
#' @author Vilhelm von Ehrenheim
#' @import foreach
#' @export
##------------------------------------------------------------------------------
binR <- function(fx, data, algorithm=c("quantile", "rpart", "manual"), ...) {
  dots <- list(...)

  out <- switch(match.arg(algorithm, c("quantile", "rpart", "manual")),
                quantile=do.call(binR_quantile, c(list(fx), list(data), dots)),
                rbind=do.call(binR_rbind, c(list(fx), list(data), dots)),
                manual=do.call(binR_manual, c(list(fx), list(data), dots)))
  out
}
