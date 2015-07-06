##' Generate a matrix or non-matrix style heatmap of correlation
##' coefficients.
##'
##' This function takes two arguments, the x variables and the y
##' variables, and generates a heatmap from the variables.  A
##' correlation matrix is computed from the data, melted (reshape
##' package), and input into ggplot2 to generate a heatmap.  The
##' output is the correlations and the plot object.
##' 
##' @title Correlation heatmap
##' @param data The dataset to plot.
##' @param x The x axis variables.
##' @param y The y axis variables.
##' @param x.name.sub The substitutions to the x axis names, incase
##' the original variables need to be clarified (eg. 'Wgt' to
##' 'Weight').  Use (probably) best command to use is the \code{gsub}
##' command.
##' @param y.name.sub Same as the \code{x.name.sub}, but for the y
##' axis variables.
##' @param heat.colours The spectrum of colours for the heat map, as a
##' vector between the lowest (negative) correlation and the highest
##' (positive) correlation.
##' @param show.corr.values Logical; add the correlation values to the
##' heatmap.
##' @param print.corr.values Logical; If true, prints the correlation values.
##' @param y.axis.label,ylab 
##' @param x.axis.label,xlab
##' @export
##' @author Luke W. Johnston
##' @examples
##' 
##' data(state)
##' plotHeatmapCorr(state.x77, c('Income', 'Population'), c('Frost', 'Murder'))
##' 
##' xvars <- c('Income', 'Population', 'Area', 'Frost')
##' ## Replace Area with 'Land Area (km^2)', etc, on the axes using the gsub command
##' plotHeatmapCorr(state.x77, xvars, print.corr.values = TRUE,
##'                 x.name.sub = xvars %>%
##'                   gsub('Area', 'Land Area (km^2)', .) %>%
##'                   gsub('Income', 'Income ($)', .)
##'                 )
##' 
plotHeatmapCorr <- function(data, x, y = NULL, x.name.sub, y.name.sub,
                            heat.colours = c('darkorange2', 'skyblue4'),
                            show.corr.values = TRUE, ylab = y.axis.label,
                            xlab = x.axis.label, print.corr.values = FALSE,
                            y.axis.label = NULL, x.axis.label = NULL) {
    ## This function uses dplyr, ggplot2, and reshape2 packages.

    if ( !is.null(ylab) | !is.null(xlab) ) {
        x.axis.label <- xlab
        y.axis.label <- ylab
    }

    if (length(y) == 0) {
        warning('This may be buggy; make sure that the plot is as expected (no missing variables).',
                call. = FALSE)
        corData <- cor(data[, x], use = 'complete.obs',
                       method = 'spearman') %>%
          melt() %>%
          ## If x.name.sub has something assigned to it, this is where
          ## the axis names will be changed to the new format.
          mutate(Var1 = factor(Var1, levels = unique(Var1), labels = x.name.sub),
                 Var2 = factor(Var2, levels = rev(unique(Var2)), labels = x.name.sub,
                               ordered = TRUE)) %>%
          ## This could cause some problems later on... FIXME
          ## Basically, these next two command makes the plot into a
          ## matrix style format.
          filter(duplicated(value, formLast = TRUE)) %>%
          rename(Var2 = Var1, Var1 = Var2) %>%
          mutate(value = round(value, 2))
    } else if (length(y) > 0) {
        corData <- cor(data[, x], data[, y], use = 'pairwise.complete.obs',
                       method = 'spearman') %>%
          melt() %>%
          mutate(Var1 = factor(Var1, levels = unique(Var1), labels = x.name.sub),
                 Var2 = factor(Var2, levels = unique(Var2), labels = y.name.sub),
                 value = round(value, 2))
    }

    warning('Caution, using "x.name.sub" or "y.name.sub" may result in some
variables being named differently.  Please, confirm that they have not.  To
confirm, set "print.corr.values" to TRUE.', call. = FALSE)

    if (print.corr.values == TRUE) {
        print(corData)
    }

    p <- plotHeatmap(corData, x = 'Var1', y = 'Var2', heat.colours = heat.colours,
                     show.corr.values = show.corr.values, ylab = y.axis.label,
                     y.axis.label = y.axis.label, xlab = x.axis.label,
                     x.axis.label = x.axis.label)

    return(p)

}
##' Create a heatmap.
##'
##' Used with \code{\link{plotHeatmapCorr}}.
##' @title Heatmap
##' @inheritParams plotHeatmapCorr
##' @return Heatmap
##' @export
##' @author Luke W. Johnston
plotHeatmap <- function(data, x = 'Var1', y = 'Var2', 
                        heat.colours = c('darkorange2', 'skyblue4'),
                        show.corr.values = TRUE,
                        ylab = y.axis.label,
                        xlab = x.axis.label, 
                        y.axis.label = NULL,
                        x.axis.label = NULL) {
    ## This function uses dplyr, ggplot2, and reshape2 packages.

    if ( !is.null(ylab) | !is.null(xlab) ) {
        x.axis.label <- xlab
        y.axis.label <- ylab
    }

    ## Color Palette for the heatmap
    ltom <- colorRampPalette(c(heat.colours[1], "white"))
    mtoh <- colorRampPalette(c("white", heat.colours[2]))

    ## Main plot
    p <- ggplot(data, aes_string(x = x, y = y)) +
      geom_tile(aes(fill = value)) +
      scale_fill_gradient(name=expression("Spearman" * ~ rho),
                          low = ltom(100), high = mtoh(100),
                          breaks = seq(-1.0, 1.0, by = 0.5),
                          limits = c(-1.0, 1.0)) +
      scale_x_discrete(expand = c(0, 0)) +
      scale_y_discrete(expand = c(0, 0)) +
      theme(axis.text.x = element_text(angle = 45,
                                       vjust = 1, hjust = 1),
            axis.text = element_text(colour = 'black'),
            panel.grid.major = element_blank(),
            panel.border = element_rect(colour = 'black', fill = NA),
            panel.background = element_blank(),
            axis.ticks = element_blank(),
            legend.direction = "vertical") +
      labs(x = x.axis.label, y = y.axis.label)

    ## Add the correlation values to the plot if TRUE.
    if (show.corr.values == TRUE) {
        p <- p + geom_text(aes(label = value), color = "#073642")
    }

    return(p)
}
