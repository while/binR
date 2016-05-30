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
binR <- function(fx, data, fun=quantile, cumulative=F) {
  mframe <- model.frame(fx, data)
  vars <- names(mframe)
  breaks = list()

  dat <- foreach(d=vars, .combine=data.frame) %do% {
    breaks[[d]] <- fun(mframe[ ,d])

    lb <- ifelse(all(mframe[ ,d] >= 0), 0, -Inf)
    ub <- ifelse(all(mframe[ ,d] <= 0), 0, Inf)
    breaks[[d]][1] <- lb
    breaks[[d]][length(breaks[[d]])] <- ub
    names(breaks[[d]]) <- NULL

    cut(mframe[ ,d], breaks=breaks[[d]], include.lowest=T, right=F)
  }
  names(dat) <- vars

  out <- list(breaks=breaks, vars=vars)
  class(out) <- "binR"
  out
}
