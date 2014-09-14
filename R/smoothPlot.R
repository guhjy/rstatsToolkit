##' In development .. 
##'
##' .. content for details ..
##' @title Smooth plot
##' @param x 
##' @param y 
##' @param data 
##' @param ... 
##' @return Outputs a plot
##' @author Luke Johnston
smoothPlot <- function(x, y, data, ...){
    require(ggplot2)
    p.df <- data[,c(1,x,y)]
    fit <- lm(p.df[,3] ~ factor(p.df[,2]), data = p.df)
    pvalue <- anova(fit)$"Pr(>F)"[1]
    if (pvalue >= 0.001){
        pvalue <- paste("p = ",round(pvalue, digits = 3))
    } else {
        pvalue <- paste("p < 0.0001")
    }
    xpos <- max(p.df[,2], na.rm = TRUE) - 0.25
    yoffset <- sd(p.df[,3], na.rm = TRUE)/18
    ypos <- aggregate(p.df[,3],
                      by = list(p.df[,2]),
                      FUN = mean,
                      na.rm = TRUE)[3,2] + yoffset
    
    p <- ggplot(data = p.df, aes(x = p.df[,2], y = p.df[,3],
               group = p.df[,1]), environment = environment())
    p <- p + stat_smooth(aes(group = 1), method = "loess",
                  colour = "black") 
    p <- p + stat_summary(aes(group = 1), geom = "point", fun.y = mean,
                   shape = 16, size = 3) 
    p <- p + theme(axis.line = element_line(colour = "black")) 
    p <- p + xlab("Clinic Visit Number") 
    p <- p + scale_x_continuous(breaks=c(0, 1, 2),
                         labels=c("0", "3", "6"))
    p <- p + ylab(paste(names(p.df[3])))
    p <- p + annotate("text", label = pvalue,
                      x = xpos, y = ypos, size = 3.5)
    return(p)
}

