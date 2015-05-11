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
##' @param x The column in the dataset that contains the independent
##' variables and/or the interaction variables.  Must be as a
##' character/string.
##' @param y The column that contains the p-value data.  The argument
##' must be a character/string.
##' @param groups The column that splits the tests up, usually is the
##' dependent variable if the data has been looped through a
##' regression test (eg. see \code{\link{loopOutputToListGEE}}).
##' @param x.axis.label The label for the x-axis.
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
plotManhattanStyle <- function(data, x, y, groups = NULL,
                               x.axis.label = 'Exposures') {
    ## Uses ggplot2 and dplyr packages

    p <- data %>% 
      ## Need to change the variable from y to 'P' to work better with
      ## the visuals of the plot.
      rename_(P = y, x = x) %>%
      ## -log10(P) is typically used for Manhattan Plots in GWAS.
      ggplot(., aes(x, -log10(P))) +
      geom_point() +
      ## Puts a line at the p = 0.05 threshold
      geom_hline(yintercept = -log10(0.05), linetype = 'dotted') +
      ## Puts a line at the p = 0.01 threshold
      geom_hline(yintercept = -log10(0.01), linetype = 'dashed') +
      ## Upper limit set at p < 0.001... may need to change.. FIXME
      coord_cartesian(ylim = c(0, -log10(0.001))) +
      geom_linerange(aes(x = x, ymin = 0, ymax = -log10(P))) +
      theme_bw() +
      labs(x = x.axis.label) +
      theme(legend.position = 'none') +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
      ylab('-log10(P)\nDotted line: P < 0.05\nDashed line: P < 0.01')

      if (!is.null(groups)) {
          p <- p + facet_grid(as.formula(groups))
      }

    return(p)
}
