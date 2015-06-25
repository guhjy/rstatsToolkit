##' Generate confidence intervals for GEE analyses
##'
##' @title Confidence interval for \code{geeglm} objects
##' @inheritParams stats::confint
##' @details This function was taken from
##' http://stackoverflow.com/a/21221995/2632184.
##' @return Returns the upper and lower confidence intervals
confint.geeglm <- function(object, parm, level = 0.95, ...) {
    cc <- coef(summary(object))
    mult <- qnorm((1+level)/2)
    citab <- with(as.data.frame(cc),
                  cbind(lwr=Estimate-mult*Std.err,
                        upr=Estimate+mult*Std.err))
    rownames(citab) <- rownames(cc)
    citab[parm,]
}
