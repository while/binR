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
binR_quantile <- function(fx, data, probs = seq(0, 1, 0.25), cumulative=F) {
  mframe <- model.frame(fx, data)[ ,-1,drop=F]  # drop dependent variable
  vars <- names(mframe)
  breaks = list()

  dat <- foreach(d=vars, .combine=data.frame) %do% {
    breaks[[d]] <- quantile(mframe[ ,d], probs=probs)

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
