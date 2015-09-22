##' Generates a plot similar to the GWAS Manhattan plots, which are
##' useful to show significance across multiple significance testings.
##'
##' See the example for a better idea of how to use the function.
##' This style of plot is really useful to use when you have run many
##' eg. interaction testing in a regression analysis and you want to
##' see which variables are barely significant vs very significant,
##' etc.  Thus, multiple comparison problems can be dealt with as the
##' plot shows how significant a variable is compared to the rest of
##' the significance tests.  This is generally the same reason why
##' GWAS studies use Manhattan plots.
##' 
##' @title Manhattan style plot
##' @param data Dataset from a regression with the p-values.
##' @param y The column in the dataset that contains the independent
##' variables and/or the interaction variables.  Must be as a
##' character/string.
##' @param x The column that contains the p-value data.  The argument
##' must be a character/string.
##' @param groups The column that splits the tests up, usually is the
##' dependent variable if the data has been looped through a
##' regression test (eg. see \code{\link{loopOutputToListGEE}}).
##' @param y.axis.label The label for the y-axis.
##' @export
##' @author Luke W. Johnston
##' @examples
##' 
##' data(state)
##' 
##' ## Very simple test example.  Merely to show how the function is used.
##' outcomes <- c('Income', 'Population')
##' exposures <- c('Frost', 'Illiteracy')
##' covariates <- c('Murder', 'LifeExp')
##' interaction <- 'LifeExp'
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
##'                     interactions = interaction,
##'                     corstr = 'exchangeable') %>%
##'   extractBetaFromListGEE() %>%
##'   unlistAndFilterIndep(., ':', pattern = TRUE) %>%
##'   createCI() %>% 
##'   plotManhattanStyle(., 'indep', 'pvalue', groups = '~ dep')
##' 
plotManhattanStyle <- function(data, y, x, groups = NULL,
                               y.axis.label = 'Exposures') {
    ## Uses ggplot2 and dplyr packages

    p <- data %>% 
      ## Need to change the variable from y to 'P' to work better with
      ## the visuals of the plot.
      rename_(P = x, y = y) %>%
      ## -log10(P) is typically used for Manhattan Plots in GWAS.
      ggplot(., aes(-log10(P), y)) +
      geom_point() +
      ## Puts a line at the p = 0.05 threshold
      geom_vline(xintercept = -log10(0.05), linetype = 'dotted') +
      ## Puts a line at the p = 0.01 threshold
      ##geom_vline(xintercept = -log10(0.01), linetype = 'dotdash') +
      ## Puts a line at the p = 0.001 threshold
      geom_vline(xintercept = -log10(0.001), linetype = 'dashed') +
      ## Upper limit set at p < 0.001... may need to change.. FIXME
      coord_cartesian(xlim = c(0, -log10(0.001) + 0.5)) +
      geom_segment(aes(x = 0, xend = -log10(P), y = y, yend = y)) +
      theme_bw() +
      labs(y = y.axis.label) +
      theme(legend.position = 'none') +
      xlab('-log10(P)\nDotted line: p<0.05\nDashed line: p<0.001')

      if (!is.null(groups)) {
          p <- p + facet_grid(as.formula(groups))
      }

    return(p)
}
