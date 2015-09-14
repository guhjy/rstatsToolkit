##' Aide to basic statistics.
##'
##' Small helper (aide) functions for commonly used basic statistics
##' functions.
##' @title Basic statistics aide
##' @name aide
##' @param x vector of values
##' @param digits number of digits
##' @param y vector of paired values to \code{x}
##' @param method the correlation method to use
##' @param na.action what to do for missing values, especially for
##' statistics using paired data (correlation)
##' @author Luke W. Johnston
NULL

##' @rdname aide
##' @export
aide.median <- function(x, digits = 1) {
    round(median(x, na.rm = TRUE), digits)
}

##' @rdname aide
##' @export
aide.iqr <- function(x, digits = 1) {
    paste0(round(quantile(x, 0.25, na.rm = TRUE), digits),
           '-', round(quantile(x, 0.75, na.rm = TRUE), digits))
}

##' @rdname aide
##' @export
aide.correlate <- function(x,
                      y,
                      digits = 2,
                      method = 'spearman',
                      na.action = 'complete.obs') {
    round(cor(x, y, use = na.action,
        method = method), digits)
}

##' @rdname aide
##' @export
aide.medianIQR <- function(x, digits = 1) {
    paste0(aide.median(x, digits), ' (', aide.iqr(x, digits), ')')
}

##' @rdname aide
##' @export
aide.mean <- function(x, digits = 1) {
    format(round(mean(x, na.rm = TRUE), digits), nsmall = digits)
}

##' @rdname aide
##' @export
aide.stddev <- function(x, digits = 1) {
    format(round(sd(x, na.rm = TRUE), digits), nsmall = digits)
}

##' @rdname aide
##' @export
aide.meanSD <- function(x, digits = 1) {
    paste0(aide.mean(x, digits), ' (', aide.stddev(x, digits), ')')
}

##' @rdname aide
##' @export
aide.tertile <- function(x) {
    cut(x, quantile(x, c(0, .333, .666, 1), na.rm = TRUE),
        include.lowest = TRUE)
}
