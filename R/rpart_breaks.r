##------------------------------------------------------------------------------
#' rpart Breaks
#' 
#' Compute breaks for binning using rpart
#' 
#' 
#' @param fx formula to use for computing breaks
#' @param data the dataset to use to compute braks
#' 
#' @author Vilhelm von Ehrenheim
#' @import rpart
#' @export
##------------------------------------------------------------------------------
rpart_breaks <- function(fx, data) {
        rp <- rpart(fx, data)
        out <- rp$splits[ ,"index"]
        names(out) <- NULL
        out
}
