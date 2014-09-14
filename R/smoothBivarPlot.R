##' In development .. 
##'
##' .. content for details ..
##' @title Smooth bivariate plot
##' @param x 
##' @param y 
##' @param data 
##' @param ... 
##' @return Outputs a plot
##' @author Luke Johnston
smoothBivarPlot <- function(x, y, data, ...){
    require(ggplot2)
    ## Use custom function to make a blank graph
    ##theme_set(theme_bw())
    ##theme_white()

    p.df <- data[,c(1,x,y)]
    p.df <- p.df[complete.cases(p.df),]

    ## Calculate a pvalue for trend or difference in categories
    ## Include an F-statistic for the anova results
    ## if (is.factor(p.df[,2])){
    ##     fit <- lm(p.df[,3] ~ p.df[,2], data = p.df)
    ##     pvalue <- anova(fit)$"Pr(>F)"[1]
    ##     if (pvalue >= 0.001){
    ##         pvalue <- paste("p = ",round(pvalue, digits = 3))
    ##     } else {
    ##         pvalue <- paste("p < 0.001")
    ##     }
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

    ## determine some way to calculate a xpos for categorical variables.
    ##xpos <- max(p.df[,2], na.rm = TRUE) - 0.25
    ##yoffset <- sd(p.df[,3], na.rm = TRUE)/18
    ##ypos <- aggregate(p.df[,3],
    ##                  by = list(p.df[,2]),
    ##                  FUN = mean,
    ##                  na.rm = TRUE)[3,2] + yoffset
    
    p <- ggplot(data = p.df, aes(x = p.df[,2], y = p.df[,3],
               group = p.df[,1]), environment = environment())
    p <- p + stat_smooth(aes(group = 1), method = "loess",
                  colour = "black") 
    p <- p + stat_summary(aes(group = 1), geom = "point", fun.y = mean,
                   shape = 16, size = 2) 
    p <- p + geom_jitter()
    p <- p + theme(axis.line = element_line(colour = "black")) 
    p <- p + xlab(paste(names(p.df[2])))
    p <- p + ylab(paste(names(p.df[3])))
    ##p <- p + annotate("text", label = pvalue,
    ##                  x = xpos, y = ypos, size = 3.5)
    p <- p + theme_classic()
    return(p)
}

