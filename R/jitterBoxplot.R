##' Generates a boxplot of variables on one axis with raw values
##' "jittered" as dots underneath.  The variables need to represent a
##' similar concept or have the same units for the plot to make sense.
##' 
##' This function is useful for exploring the distribution of a series
##' of variables that share a common unit, such as kilogram.  The
##' values for each variable are plotted as jittered dots with a
##' boxplot of the distribution layered on top of the dots.  The
##' function takes a subsetted dataset that contains only the series
##' of variables that share a common unit.  The output object is the
##' plot.  This function depends on \pkg{ggplot2} and \pkg{reshape2}.
##' 
##' @title Univariate jittered boxplot
##' @param subset.ds The dataset that only contains the series of
##' variables that will be plotted
##' @param dot.size Size of the dot for \code{\link[ggplot2]{geom_jitter}}
##' @param dot.colour Color of the dots for \code{\link[ggplot2]{geom_jitter}}
##' @param custom.var.names List object that contains the custom
##' (alternative) variable names for the variable (column) names you
##' passed into the function
##' @param xlab The x-axis label
##' @param ylab The y-axis label
##' @return Outputs the plot object
##' @author Luke Johnston
##' @examples
##' 
##' varnames <- list("Name of X1" = "X1", "Name of X2" = "X2", ...)
##' df <- subset(ds, VN == 0, select=nefa)
##' names(df)
##' jitter.boxplot(df, xlab="Concentration (nmol/mL)",
##'                ylab="Fatty acid", custom.var.names=varnames)
##' 
jitterBoxplot <- function(subset.ds, dot.size=2, dot.colour="grey50",
                          custom.var.names=NULL, xlab=NULL, ylab=NULL) {

    ## Call required libraries in order for this function to work
    require(ggplot2)
    require(reshape2)
    
    ## Load dataset into function environment so ggplot can detect it
    p.ds <- subset.ds
    #print(names(p.ds))
    p.ds <- melt(p.ds)

    ## If specified in the function argument, will change the name of
    ## the variables you are passing into the function dataset
    if (!is.null(custom.var.names)) {
        levels(p.ds$variable) <- custom.var.names
    }

    ## Generate plot
    p <- ggplot(melt(p.ds), aes(variable, value)) +
        geom_jitter(size = dot.size, colour = dot.colour) +
            geom_boxplot(outlier.shape = NA) +
                coord_flip()

    ## Set the x or y axis label if one has been provided. Because the
    ## axes have been flipped (coord_flip above), the ggplot ylab()
    ## corresponds to the x axis label and vice versa for the xlab()
    if (!is.null(xlab)) {
        p <- p + ylab(xlab)
    }

    if (!is.null(ylab)) {
        p <- p + xlab(ylab)
    }

        
    ## Output the jittered boxplot
    return(p)
}
