##' Generates a boxplot of a factor variable on one axis with raw
##' values "jittered" as dots underneath the box.
##'
##' This function is useful for exploring the distribution of a series
##' of variables that share a common unit, such as kilogram.  The
##' values for each variable are plotted as jittered dots with a
##' boxplot of the distribution layered on top of the dots.  Can add
##' axis labels, themes, etc, through \code{\link[ggplot2]{ggplots}}
##' interface (eg. \code{ylab}).
##' 
##' @title Jittered-dot boxplot
##' @param data The dataset with the variables of interest
##' @param x Variable for the x-axis
##' @param y Variable for the y-axis
##' @param groups Optional, split boxplots and jittered dots by group.
##' @export
##' @author Luke W. Johnston
##' @examples
##' 
##' data(state)
##' cbind(state.region, state.x77) %>%
##'   as.data.frame() %>%
##'   mutate(Region = as.factor(state.region),
##'          MedianIncome = cut(Income, breaks = c(-Inf, median(Income), Inf),
##'                             labels = c('Lower', 'Higher'))) %>%
##'   plotBoxWithJitter(., 'Region', 'Population', groups = 'MedianIncome')
##' 
plotBoxWithJitter <- function(data, x, y, groups = NULL) {
    ## Uses ggplot2

    if ( is.null(groups) ) {
        p <- ggplot(data, aes_string(x = x, y = y)) +
          geom_jitter()
    } else if ( !is.null(groups) & is.character(groups) ) {
        p <- ggplot(data, aes_string(x = x, y = y, fill = groups)) +
          geom_point(position = position_jitterdodge(dodge.width = 0.9))
    } else {
        stop('Groups should either be NULL or a character variable (string)',
             call. = FALSE)
    }

    p <- p + geom_boxplot(outlier.shape = NA) +
      coord_flip()

    return(p)
}
