##' Generate a forest plot without the traditional side table.
##'
##' Create a forest plot, with a dot and confidence line, though
##' without the usual side table that contains the raw data values.
##' If the \code{pvalue.factor} argument is supplied, the dots and
##' confidence lines increase in size and opacity as significance
##' increases.  If \code{groups} is also supplied, the forest plot
##' will be split up for each grouping.  Thus, a large amount of
##' information on the results can be provided in a fairly small
##' amount of space.
##' 
##' @title Forest plot
##' @param data Dataset for the forest plot.
##' @param coefficient The column that contains the beta
##' estimate/coefficient.
##' @param y.axis.variables The column with the exposure variables
##' that will be placed on the y-axis of the forest plot.
##' @param confid.interval A vector that contains the lower and
##' upper confidence interval.
##' @param pvalue.factor The column that contains the p-value in the
##' form of a factor variable (ie. with levels such as '>0.05' and
##' '<0.05').
##' @param groups The variable to split the plot up, as a formula
##' (var1 ~ var2, or ~ var2, etc).
##' @param y.axis.label The y-axis label.
##' @param x.axis.label The x-axis label.
##' @export
##' @return A forest plot
##' @author Luke W. Johnston
##' @examples
##'
##' \dontrun{
##' data(state)
##' ds <- data.frame(state.region, state.x77)
##' geefit <- loopGEE(ds, c('Income', 'Frost'), c('Population', 'Murder'), 'state.region')
##'   filter(term == 'independent') %>%
##' filtered <- dplyr::filter(geefit, term == 'independent')
##' 
##' plotForest(filtered)
##' plotForest(filtered, groups = ' ~ dep')
##' plotForest(filtered, pvalue.factor = 'f.pvalue', groups = ' ~ dep')
##' }
##' 
plotForest <- function(data, coefficient = 'estimate',
                       y.axis.variables = 'indep',
                       confid.interval = c('conf.low', 'conf.high'),
                       pvalue.factor = NULL, groups = NULL,
                       y.axis.label = 'Exposures',
                       x.axis.label = 'Beta estimates') {

    ## Main forest plot
    p <-  ggplot(data, aes_string(x = coefficient, y = y.axis.variables)) +
      geom_errorbarh(aes_string(xmin = confid.interval[1],
                                xmax = confid.interval[2],
                                alpha = pvalue.factor),
                     height = 0) +
      geom_vline(xintercept = 0, linetype = 'dashed') +
      labs(y = y.axis.label, x = x.axis.label)

    ## Change the size of the dot and line based on significance.
    if (!is.null(pvalue.factor)) {
        p <- p + geom_point(aes_string(size = pvalue.factor,
                                       alpha = pvalue.factor)) +
          scale_alpha_discrete(name = 'P-value') +
          scale_size_discrete(name = 'P-value')
    } else {
        p <- p + geom_point()
    }

    ## Split the plot up by a group variable.
    if (!is.null(groups)) {
        p <- p + facet_grid(as.formula(groups))
    }

    return(p)
}

