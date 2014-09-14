##' In development .. 
##'
##' .. content for details ..
##' @title Bivariate plot
##' @param x 
##' @param y 
##' @param data 
##' @param ... 
##' @return Outputs a plot
##' @author Luke Johnston
bivarPlot <- function(x, y, data, ...) {
    require(ggplot2)
    p.df <- data[,c(1,x,y)]
    sum.df <- summarySE(p.df, measurevar=names(p.df[3]),
                        groupvars=names(p.df[2]), na.rm=TRUE)
    fit <- lm(p.df[,3] ~ factor(p.df[,2]), data = p.df)
    pvalue <- anova(fit)$"Pr(>F)"[1]
    if (pvalue >= 0.001){
        pvalue <- paste("p = ",round(pvalue, digits = 3))
    } else {
        pvalue <- paste("p < 0.0001")
    }
    xpos <- max(p.df[,2], na.rm = TRUE) - 0.25
    yoffset <- sd(p.df[,3], na.rm = TRUE)/5
    ypos <- aggregate(p.df[,3],
                      by = list(p.df[,2]),
                      FUN = mean,
                      na.rm = TRUE)[3,2] + yoffset

    p <- ggplot(sum.df, aes(x=sum.df[,1], y=sum.df[,3]),
                environment = environment())
    p <- p + geom_errorbar(aes(ymin=sum.df[,3]-ci, ymax=sum.df[,3]+ci),
                           width = .1)
    p <- p + geom_line(size=0.75)
    p <- p + theme(axis.line = element_line(colour = "black"))
    p <- p + geom_point(size=2.5)
    p <- p + ylab(paste(names(sum.df[3])))
    p <- p + xlab("Clinic Visit Number")
    p <- p + scale_x_continuous(breaks=c(0, 1, 2),
                                labels=c("0", "3", "6"))
    p <- p + annotate("text", label = pvalue,
                      x = xpos, y = ypos, size = 3.5)
    p <- p + theme(plot.title = element_text(hjust=0,
                       face = "bold", size=11))
    p <- p + theme(text = element_text(size=11))
    return(p)
}
