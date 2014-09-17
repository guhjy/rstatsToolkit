##' Generates plots of a linear regression model which includes
##' confounding variables.
##' 
##' This function runs a linear regression on the specified variables
##' and plots the partial residuals.  This allows for visualizing the
##' relationship between the outcome and the exposure, after adjusting
##' for confounders.  A linear slope is plotted through the partial
##' residuals, with a confidence interval band around it.  The output
##' is a plot.  This function depends on \pkg{visreg}.
##'
##' @title Visualizing adjusted linear regression models
##' @param data Dataset with the variables of interest
##' @param y The dependent or outcome variable in the regression
##' equation
##' @param x The independent or exposure variable in the regression
##' equation
##' @param covar The confounding variables, that is the variables being adjusted for
##' @param ylabel The y-axis label
##' @param xlabel The x-axis label
##' @param ... Other options. In development
##' @return Outputs a plot of the regression model
##' @export
##' @author Luke Johnston
plotVisreg <- function(data, y, x, covar, ylabel = x, xlabel = y, ...) {

    require(visreg)

    ## Import data, run a linear regression
    p.df <- data[,c(y, x)]
    p.df <- as.data.frame(append(p.df, data[,covar]))
    fit <- lm(p.df[,1] ~ ., data=p.df[,-1], na.action=na.omit)

    ## Create variables for pasting as titles and labels
    y <- names(p.df[1])
    x <- names(p.df[2])
    covar <- paste(names(p.df[,3:ncol(p.df)]), collapse = " ", sep = " ")

    ## Extract p-value from lm
    pval <- summary(fit)$coefficient[2,4]
    if (pval < 0.001) {
        pvalue <- "p-value < 0.001"
    } else {
        pvalue <- paste("p-value = ", round(pval, 3))
    }
   
    ## Set plot parameters
    par(bty = 'n')

    ## Run visualize regression (visreg)
    visreg(fit, x, ylab = ylabel, xlab = xlabel,
           points = list(cex = 0.33, col = "grey30"),
           ...)
    mtext(pvalue, line = -1.5, adj = 1)

    # Reset plot parameters
    par(bty = 'o')

}
