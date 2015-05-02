##' Generates a plot of two variables, particularly for longitudinal
##' data.  The x-axis is typically the time variable.
##'
##' This function creates a plot with smooth lines, typically using
##' the loess method, along with the means of a discrete time variable
##' (eg. time 0, 1, 2, 3 etc).
##'
##' Dependencies are: \pkg{ggplot2}
##' 
##' @title Smooth time plots
##' @param dsn The dataset name.
##' @param x Specify the variable for the x-axis.  Must be
##' either in quotes or as a number.
##' @param y As with \code{x}, but for the y-axis.
##' @param id Specify the ID variable for longitudinal or repeated
##' measures type data.
##' @param smooth.type Set the smoothing method, which includes
##' "loess", "lm", "rlm".
##' @param ylab Optionally set the y-axis label.
##' @param xlab Optionally set the x-axis label.
##' @return Outputs a ggplot object
##' @author Luke Johnston
##' @export
##' 
smoothTimePlot <- function(dsn, x, y, id,
                             smooth.type = "loess",
                             ylab = y, xlab = x) {

    ## Load the required package:
    require(ggplot2)
    
    ## Assign the variables into the local environment (for ggplot)
    dsn$x <- dsn[, x]
    dsn$y <- dsn[, y]
    dsn$id <- dsn[, id]
    dsn <- subset(x = dsn, select = c(x, y, id))

    ## Calculate the p-value for difference between times. The time
    ## variable is set as a factor and the continuous y variable is
    ## log transformed.
    fit <- lm(log(dsn[,2]) ~ as.factor(dsn[,1]), data = dsn)
    pvalue <- anova(fit)$"Pr(>F)"[1]
    if (pvalue >= 0.001){
        pvalue <- paste("p = ", round(pvalue, digits = 3))
    } else {
        pvalue <- paste("p < 0.001")
    }

    ## Assign graph positions for the p-value text that will be on the
    ## graph.
    ## FIXME: determine some way to calculate a xpos for
    ## categorical variables.
    xpos <- max(dsn[, 1], na.rm = TRUE) - 0.25
    yoffset <- sd(dsn[, 2], na.rm = TRUE) / 4
    ypos <- aggregate(dsn[, 2],
                     by = list(dsn[,1]),
                     FUN = mean,
                     na.rm = TRUE)[3,2] + yoffset

    ## Create the plot
    p <- ggplot(dsn, aes(x, y, group = id))

    ## Assign the smoothing of the data
    p <- p + stat_smooth(aes(group = 1),
                         method = smooth.type,
                         colour = "black")

    ## Create points based on the mean of the data
    p <- p + stat_summary(aes(group = 1), geom = "point",
                          fun.y = mean,
                          shape = 16, size = 3)

    ## Create labels for the x- and y-axis
    p <- p + labs(x = xlab, y = ylab)

    ## Place the p-value text onto the graph
    p <- p + annotate("text", label = pvalue,
                      x = xpos, y = ypos, size = 5)

    return(p)

    ## Development pieces of code:
    ## Calculate a pvalue for trend or difference in categories
    ## Include an F-statistic for the anova results
    ## if (is.factor(p.df[,2])){
    ## fit <- lm(p.df[,3] ~ as.factor(p.df[,2]), data = p.df)
    ## pvalue <- anova(fit)$"Pr(>F)"[1]
    ## if (pvalue >= 0.001){
    ##     pvalue <- paste("p = ",round(pvalue, digits = 3))
    ## } else {
    ##     pvalue <- paste("p < 0.001")
    ## }
    ## } else if (is.numeric(p.df[,2])){
    ##     r <- cor.test(p.df[,2], p.df[,3], method = "spearman")
    ##     if (r$p.value >= 0.001){
    ##         pvalue <- expression(paste(p[s]," = ",round(r$estimate, digits = 2),
    ##             ", p = ",round(r$p.value, digits = 3)))
    ##     } else {
    ##         pvalue <- expression(paste(p[s]," = ",round(r$estimate, digits = 2),
    ##             " p < 0.001"))
    ##     }
    ## }
    
}

