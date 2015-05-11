##' Generate a forest plot without the traditional side table .
##'
##' Create a forest plot, with a dot and 95% confidence line, though
##' without the usual side table that contains the raw data values.
##' If the \code{dot.pval} argument is supplied, the dots and
##' confidence lines increase in size and opacity as significance
##' increases.  If \code{groups} is also supplied, the forest plot
##' will be split up vertically for each grouping.  Thus, a large
##' amount of information on the results can be provided in a fairly
##' small amount of space.
##' @title Forest plot
##' @param data Dataset for the forest plot.
##' @param coefficient,coeff The column that contains the beta
##' estimate/coefficient.
##' @param y.variables.column,yvar The column with the exposure
##' variables that will be places on the y-axis of the forest plot.
##' @param confid.interval,ci A vector that contains the lower and
##' upper confidence interval.
##' @param pvalue.factor.column,dot.pval The column that contains the
##' p-value in the form of a factor variable (ie. with levels such as
##' '>0.05' and '<0.05').
##' @param groups The variable to split the plot up.
##' @param y.axis.label,ylab The y-axis label.
##' @param x.axis.label,xlab The x-axis label.
##' @export
##' @return A forest plot
##' @author Luke W. Johnston
##' @examples
##'
##' data(state)
##' 
##' ## Very simple test example.  Merely to show how the function is used.
##' outcomes <- c('Income', 'Population')
##' exposures <- c('Frost', 'Illiteracy')
##' covariates <- c('Murder', 'LifeExp')
##' 
##' ## This uses the dplyr package.
##' ds <- cbind(state.region, state.x77) %>%
##'   as.data.frame() %>%
##'   rename(LifeExp = `Life Exp`,
##'          ## Need to rename the id variable to SID (see description
##'          ## above)
##'          SID = state.region) %>%
##'   arrange(SID)
##' 
##' loopOutputToListGEE(ds, outcomes, exposures, covariates,
##'                     corstr = 'exchangeable') %>%
##'   extractBetaFromListGEE() %>%
##'   unlistAndFilterIndep(., exposures) %>%
##'   createCI() %>%
##'   plotForest(., dot.pval = 'f.pvalue', groups = 'dep')
##' 
plotForest <- function(data, coeff = coefficient, yvar = y.variables.column,
                       ylab = y.axis.label, xlab = x.axis.label,
                       ci = confid.interval, dot.pval = pvalue.factor.column,
                       coefficient = 'Estimate',
                       y.variables.column = 'indep',
                       confid.interval = c('lowerCI', 'upperCI'),
                       pvalue.factor.column = 'NULL', groups = NULL,
                       y.axis.label = 'Exposures',
                       x.axis.label = 'Beta estimate') {

    ## Make sure each argument has the same values.
    if (coeff != coefficient | yvar != y.variables.column |
        ylab != y.axis.label | xlab != x.axis.label | (ci != confid.interval)[1] |
        dot.pval != pvalue.factor.column) {
        coefficient <- coeff
        y.variables.column <- yvar
        y.axis.label <- ylab
        x.axis.label <- xlab
        confid.interval <- ci
        pvalue.factor.column <- dot.pval
    }

    ## Main forest plot
    p <- data %>%
      ggplot(., aes_string(x = coefficient, y = y.variables.column)) +
      geom_errorbarh(aes_string(xmin = confid.interval[1],
                                xmax = confid.interval[2],
                                alpha = pvalue.factor.column),
                     height = 0) +
      geom_vline(xintercept = 0, linetype = 'dashed') +
      labs(y = y.axis.label, x = x.axis.label) +
      theme_bw()

    ## Change the size of the dot and line based on significance.
    if (!is.null(pvalue.factor.column)) {
        p <- p + geom_point(aes_string(size = pvalue.factor.column,
                                       alpha = pvalue.factor.column)) +
          scale_alpha_discrete(name = 'P-value') +
          scale_size_discrete(name = 'P-value')
    }

    ## Split the plot up by a group variable.
    if (!is.null(groups)) {
        p <- p + facet_grid(reformulate(groups, '.'))
    }

    return(p)
}

