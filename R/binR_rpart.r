# vim: shiftwidth=2 tabstop=2
##------------------------------------------------------------------------------
#' binR
#' 
#' Bin a dataset using a formula and a breaks function
#' 
#' Bin or dkiscretize a dataset using a formula.
#' 
#' @author Vilhelm von Ehrenheim
#' @import rpart
#' @export
##------------------------------------------------------------------------------
binR_rpart <- function(fx, data, cumulative=F) {
  mframe <- model.frame(fx, data)[ ,-1,drop=F]  # drop dependent variable
  vars <- names(mframe)
  response <- all.vars(fx[[2]])

  if (length(response) != 1) stop("Incorrently defined response variable")

  breaks = list()

  dat <- foreach(d=vars, .combine=data.frame) %dopar% {
    breaks[[d]] <- rpart_breaks(as.formula(paste(response, '~', d)), data)

    lb <- ifelse(all(mframe[ ,d] >= 0), 0, -Inf)
    ub <- ifelse(all(mframe[ ,d] <= 0), 0, Inf)
    breaks[[d]] <- sort(c(lb, breaks[[d]], ub))
    names(breaks[[d]]) <- NULL
  }

  out <- list(breaks=breaks, vars=vars)
  class(out) <- "binR"
  out
}
