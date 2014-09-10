#########################################################
## This file was created 2014-01-21 by Luke Johnston.  It contains
## functions used in R for various projects, including this one.

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## This function was taken on 2014-01-21 from:
## http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_%28ggplot2%29/#Helper%20functions
## It summarizes data by giving count, mean, standard deviation, standard
## error of the mean, and confidence interval (default 95%).
##
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
##
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
    require(plyr)

    ## New version of length which can handle NA's: if na.rm==T, don't count them
    length2 <- function (x, na.rm=FALSE) {
        if (na.rm) sum(!is.na(x))
        else       length(x)
    }

    ## This does the summary. For each group's data frame, return a vector with
    ## N, mean, and sd
    datac <- ddply(data, groupvars, .drop=.drop,
      .fun = function(xx, col) {
        c(N    = length2(xx[[col]], na.rm=na.rm),
          mean = mean   (xx[[col]], na.rm=na.rm),
          sd   = sd     (xx[[col]], na.rm=na.rm)
        )
      },
      measurevar
    )

    ## Rename the "mean" column    
    datac <- rename(datac, c("mean" = measurevar))

    datac$se <- datac$sd / sqrt(datac$N)  ## Calculate standard error of the mean

    ## Confidence interval multiplier for standard error
    ## Calculate t-statistic for confidence interval: 
    ## e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    ciMult <- qt(conf.interval/2 + .5, datac$N-1)
    datac$ci <- datac$se * ciMult

    return(datac)
}


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## This function is used to make ggplot2 plots blank to be able to
## better customize them.
##
theme_white <- function() {
  theme_update(panel.background = element_blank(),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               panel.border = element_blank(),
               axis.line = element_line(colour = "black"),
               axis.text.x=element_text(colour="black"),
               axis.text.y=element_text(colour="black")
               )
}


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Taken from http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_%28ggplot2%29/
## Multiple plot function
##
## ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
## - cols:   Number of columns in layout
## - layout: A matrix specifying the layout. If present, 'cols' is ignored.
##
## If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
## then plot 1 will go in the upper left, 2 will go in the upper right, and
## 3 will go all the way across the bottom.
##
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

  ## Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  ## If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    ## Make the panel
    ## ncol: Number of columns of plots
    ## nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    ## Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    ## Make each plot, in the correct location
    for (i in 1:numPlots) {
      ## Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## smooth.bivar.plot
## 	- 

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Depracated. Generate a heatmap of correlation coefficients
#'
#' This function takes a specific dataset that contains only the
#' variables that are to be used in the heatmap (i.e. subset the
#' original dataset into only those variables that will be in the
#' heatmap).  The function takes the dataset, the number of rows used,
#' and the labels for each of the levels.  A correlation matrix is
#' computed from the data, melted (reshape package), and the labels are
#' applied to each level of the melted dataset.  Then ggplot2
#' generates heatmaps.  The output is the plot object.
#'
#' Dependencies are: reshape and ggplot2
#'
#' @param data The dataframe that **ONLY
#' @param rows Number of rows that the heatmap will have
#' @param levels.lab A list object that contains the x- and y-axis
#' labels for each of the "levels" or variables in the dataset
#' @param ... For additional options that I may add in the future
#' @keywords heatmap correlations ggplot2 reshape
#' @export 
#' @author Luke Johnston
#' @examples
#'
heatmap.corr.old <- function(data, rows, levels.lab, ...) {
    require(ggplot2)
    require(reshape2)
    p.df <- data

    cor.matrix <- round(cor(p.df, use = "pairwise.complete.obs",
                            method = "spearman"), digits = 2)
    for (i in 1:rows) {
        cor.matrix[i, 1:i] <- NA
    }
    
    cor.dat <- melt(cor.matrix)
    cor.dat <- cor.dat[-which(is.na(cor.dat[, 3])),]
    levels(cor.dat$Var1) <- levels.lab
    levels(cor.dat$Var2) <- rev(levels.lab)

    theme_white()
    p <- ggplot(cor.dat, aes(Var2, Var1, fill = value))
    p <- p + geom_tile()
    p <- p + geom_text(aes(Var2, Var1, label = value),
                       color = "#073642", size = 7)
    p <- p + scale_fill_gradient(name=expression("Spearman" * ~ rho),
                                 low = "white", high = "skyblue4",
                                 breaks=seq(0, 1, by = 0.2),
                                 limits = c(0, 1))
    p <- p + scale_x_discrete(expand = c(0, 0))
    p <- p + scale_y_discrete(expand = c(0, 0))
    p <- p + labs(x = "", y = "")
    p <- p + theme(axis.text.x = element_text(angle = 45,
                       vjust = 1, hjust = 1, size = 20),
                   axis.text.y = element_text(size = 20),
                   panel.grid.major = element_blank(),
                   panel.border = element_blank(),
                   panel.background = element_blank(),
                   axis.ticks = element_blank(),
                   legend.justification = c(1, 0),
                   legend.position = c(0.9, 0.7),
                   legend.title = element_text(size = 14),
                   legend.text = element_text(size = 13),
                   legend.direction = "horizontal")
    p <- p + guides(fill = guide_colorbar(barwidth = 10,
                        barheight = 1.5, title.position = "top", 
                        title.hjust = 0.5))
    return(p)
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Generate a non-matrix style heatmap of correlation coefficients
#' (i.e. the number of columns and rows can be different).
#'
#' This function takes two arguments, the x variables and the y
#' variables, and generates a heatmap from the variables.  A
#' correlation matrix is computed from the data, melted (reshape
#' package), and input into ggplot2 to generate a heatmap.  The output
#' is the correlations and the plot object.
#'
#' Dependencies are: reshape2 and ggplot2, as well as the function
#' theme_white()
#'
#' @param data The dataframe (dataset) with the variables of interest
#' @param x A numerical value that represents the columns in a
#' dataset, which will be placed on the x-axis
#' @param y As with `x`, it is a numerical value or range for the
#' columns in the dataset and will be placed on the y-axis
#' @param leg.range The range of (two) values that represents the max
#' and min of the legend fill color
#' @param levels.xlab Custom specified names for each variable along
#' the x axis (is a list)
#' @param levels.ylab Custom specified names for each variable along
#' the y axis (is a list)
#' @param xlab The x-axis label
#' @param ylab The y-axis label
#' @param ... For additional options that I may add in the future
#' @keywords heatmap correlations ggplot2 reshape2
#' @export 
#' @author Luke Johnston
#' @examples
#'
#' xlabs <- list("X Name" = "x1", "X Name Two!" = "x2" ... )
#' ylabs <- list("Y Name" = "y1", "Y Name Two!" = "y2" ... )
#' df <- data.frame(replicate(10,sample(0:1,1000,rep=TRUE)))
#' plot <- heatmap.corr(data = df, x = 1:3, y = 4:10,
#'                      levels.xlab=xlabs, levels.ylab=ylabs)
#' print(plot)
#'
heatmap.corr <- function(data, x, y, leg.range=c(-1.0, 1.0),
                         levels.xlab=NULL, levels.ylab=NULL,
                         xlab="", ylab="", lo.color="darkorange2",
                         hi.color="skyblue4", rm.legend=FALSE,
                         matrix.sty=FALSE, ...) {

    ## Require the needed library packages
    require(ggplot2)
    require(reshape2)

    ## Import data into the function
    p.df <- data

    ## Run the correlation analysis, rounding to 2 digits. I used
    ## Spearman. If another test is run using Pearson, change the
    ## title on the legend!
    cor <- round(cor(p.df[,y], p.df[,x],
                     use = "pairwise.complete.obs",
                     method = "spearman"), digits = 2)

    ## Take the absolute to make the correlations all be
    ## positive. Having negative correlations may make the plot look
    ## weird. You can always determine which are negative from the
    ## results of the cor above.
    ##cor <- abs(cor)

    if (matrix.sty == TRUE) {
        rows <- length(x)
        for (i in 1:rows) {
            cor[i, 1:i] <- NA
        }
        print(cor)
    } else if (matrix.sty == FALSE) {
        cor[cor == 1] <- NA
        print(cor)
    } else {
        stop("Specify either TRUE or FALSE for the matrix.sty arg")
    }
    
    ## Melt base
    cor.dat <- melt(cor)
    cor.dat <- cor.dat[-which(is.na(cor.dat[, 3])), ]
    print(cor.dat)


    ## If specified, provide custom levels for the x and y vars.
    if (!is.null(levels.xlab)) {
        levels(cor.dat$Var1) <- levels.ylab
        if (matrix.sty == TRUE) {
            levels(cor.dat$Var2) <- levels.ylab
           
            print(cor.dat)
        } else if (matrix.sty == FALSE) {
            levels(cor.dat$Var2) <- levels.xlab
        } else {
            stop("Specify either TRUE or FALSE for the matrix.sty arg")
        }
    }

    ## Start the plot
    p <- ggplot(cor.dat, aes(Var2, Var1, fill = value))

    ## Tiles is the thing that makes it into a heatmap.
    p <- p + geom_tile()

    ## Place the correlation values within the tiles. Colour is the
    ## colour of the text and size is the size of the font.
    p <- p + geom_text(aes(Var2, Var1, label = value),
                       color = "#073642", size = 5)

    ## Specify color used for the correlation values
    ltom <- colorRampPalette(c(lo.color, "white"))
    mtoh <- colorRampPalette(c("white", hi.color))

    ## Max and min for the legend range
    max <- round(leg.range[2], 2)
    min <- round(leg.range[1], 2)

    ## Make the legend if the legend is set. Name = title of legend.
    if (rm.legend == FALSE) {
        p <- p + scale_fill_gradient(name=expression("Spearman" * ~ rho),

                                     ## low means the colour for low
                                     ## values high means the colour of
                                     ## the high values
                                     low = ltom(100), high = mtoh(100),
                                     
                                     ## Breaks means where the legend
                                     ## starts (at 0) and ends (at 1),
                                     ## increasing by 0.2 each time.
                                     breaks = seq(min, max, by = 0.2),

                                     ## limits is the limits to the data
                                     ## values (i.e. between 0 and 1)
                                     limits = c(min, max))
    }

    ## Plots data close to the axes
    p <- p + scale_x_discrete(expand = c(0, 0))
    p <- p + scale_y_discrete(expand = c(0, 0))

    ## Remove the x and y axis labels. If you want labels, just type
    ## in text between the quotes
    p <- p + labs(x = xlab, y = ylab)

    ## The theme command customizes the plot. Axis.text.x customizes
    ## the text on the x-axis. Angle is the angle of the text, vjust
    ## and hjust are the justification/where the text is located. Size
    ## is font size
    p <- p + theme(axis.text.x = element_text(angle = 45,
                       vjust = 1, hjust = 1, size = 14),

                   ## As with the axis.text.x above.
                   axis.text.y = element_text(size = 14),

                   ## Remove the grid lines
                   panel.grid.major = element_blank(),

                   ## Remove the borders
                   panel.border = element_blank(),

                   ## Remove the panel background
                   panel.background = element_blank(),

                   ## Remove the axes ticks.
                   axis.ticks = element_blank(),
                   
                   ## Size of legend title font 
                   legend.title = element_text(size = 8),

                   ## Size of numbers on legend
                   legend.text = element_text(size = 7))

    ## If the default option for keeping the legend remains, develop
    ## legend more
    if (rm.legend == FALSE) {
        p <- p + theme(
                   ## Direction that the legend will run. Other option
                   ## is "horizontal"
                   legend.direction = "vertical")
    
    ## Customize the legend more.
    p <- p + guides(fill = guide_colorbar(barwidth = 1.5,
                        barheight = 10, title.position = "top", 
                        title.hjust = 0.5))
    }

    ## Output the plot
    return(p)
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Need to develop this more.

smooth.bivar.plot <- function(x, y, data, ...){
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


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Need to develop this.
smooth.plot <- function(x, y, data, ...){
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




##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Need to develop this.

bivar.plot <- function(x, y, data, ...) {
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


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Generate regression diagnostic plots and tests
#'
#' This function runs a linear regression on the specified variables
#' and generates diagnostics based on the regression.  Basic
#' diagnostics include checking the normality of the residuals,
#' assessing outliers, influence and Cook's D, and multicollinearity.
#' Several tests have been commented out, though they can be
#' uncommented if desired (edit the function to output these if
#' desired).  Some of the tests I don't fully understand how to
#' interpret them, but as I learn more I will probably know.
#'
#' Dependencies are: MASS and gplots
#'
#' @param data The dataframe that contains the variables of interest
#' @param y The dependent or outcome variable in the regression
#' @param x The independent or exposure variable in the regression
#' @param covar Confounders (variables being adjusted for)
#' @keywords diagnostics linear regression
#' @export 
#' @author Luke Johnston
#' @examples
#'
diagnosticPlots <- function(data, y, x, covar){

    require(MASS)
    require(gplots)

    ## Linear regression
    p.df <- data[,c(y, x)]
    p.df <- as.data.frame(append(p.df, data[,covar]))
    fit <- lm(p.df[,1] ~ ., data=p.df[,-1], na.action=na.omit)

    y <- names(p.df[1])
    x <- names(p.df[2])
    covar <- paste(names(p.df[,3:ncol(p.df)]), collapse = " ", sep = " ")

    ## Identify the variables being examined
    cat("Starting plots for:", y, x, covar, "\n")
    textplot(paste("Plots for regression using:", y, x, "\n", covar))

    ## General diagnostics plot
    par(mfrow=c(2,2))
    plot(fit, ask=FALSE, main=paste("Plot for", y, x, collapse = " ", sep = " "))
    par(mfrow=c(1,1))

    ## Assessing Outliers
    ## QQ plot for studentized resid
    ## qqPlot(fit, ask=FALSE, main=paste("QQ Plot:", y, x), id.n=3) 
    
    ## Leverage plots
    leveragePlots(fit, id.n=3, ask=FALSE,
                  main=paste("Leverage plot:", y, x))

    ## Influential Observations
    ## Added Variable plots
    ##avPlots(fit, ask=FALSE, lwd=1, id.n=3, 
    ##        col=c("black","black"), 
    ##        main=paste("A-V plot:", y, x))

    ## Cook's D plot
    ## D values > 4/(n-k-1)
    cutoff <- 4/((nrow(p.df)-length(fit$coefficients)-2))
    plot(fit, which=4, cook.levels=cutoff, ask=FALSE,
         main=paste("Cook's D plot:", y, x))

    ## Influence Plot
    influencePlot(fit, main=paste("Influence Plot:", y, x), 
                  sub="Circle size is proportial to Cook's Distance",
                  ask=FALSE, id.n=3)

    ## Checking normality of studentized residuals
    hist(studres(fit), freq=FALSE, ask=FALSE,
         main=paste("Distribution of Studentized Residuals:", y, x))
    lines(seq(min(studres(fit)),max(studres(fit)),length=40), 
          dnorm(seq(min(studres(fit)),max(studres(fit)),length=40)))

    ## Evaluate homoscedasticity
    ## Studentized residuals vs. fitted values
    ##spreadLevelPlot(fit, id.n=3, ask=FALSE,
    ##                main=paste("SpreadPlot for", y, x))

    ## Significance test
    ##textplot(ncvTest(fit))

    ## Evaluate Nonlinearity
    ## Component + residual plot
    ##crPlots(fit, col=c("black","black"), 
    ##        ask=FALSE,id.n=3, 
    ##        lwd=1, main=paste("Plot for", y, x))

    ## Multicollinearity
    textplot(vif(fit))
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Visualize linear regression relationships
#'
#' This function runs a linear regression on the specified variables
#' and plots the partial residuals.  This allows for visualizing the
#' relationship between the outcome and the exposure, after adjusting
#' for confounders.  A linear slope is plotted through the partial
#' residuals, with a confidence interval band around it.  The output
#' is a plot.
#'
#' Dependencies are: visreg
#''
#' @param data The dataframe that contains the variables of interest
#' @param y The dependent or outcome variable in the regression
#' @param x The independent or exposure variable in the regression
#' @param covar Confounders (variables being adjusted for)
#' @param ylabel Y-axis label
#' @param xlabel X-axis label
#' @param ... Additional options (e.g. ylabs)
#' @keywords visualize linear regression partial residual plot
#' @export 
#' @author Luke Johnston
#' @examples
#'
plot.visreg <- function(data, y, x, covar, ylabel = x, xlabel = y, ...) {

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

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Univariate jittered boxplots
#' 
#' Creates a univariate distribution plot of a series of shared unit
#' variables with a horizontal boxplot layered over top of jittered
#' dots for the values.
#'
#' This function is useful for exploring the distribution of a series
#' of variables that share a common unit, such as kilogram.  The
#' values for each variable are plotted as jittered dots with a
#' boxplot of the distribution layered on top of the dots.  The
#' function takes a subsetted dataset that contains only the series of
#' variables that share a common unit.  The output object is the plot.
#'
#' The dependencies are: \pkg{reshape2} and \pkg{ggplot2}
#'
#' @param subset.ds The dataset (dataframe) that only contains the
#' series of variables that will be plotted
#' @param dot.size The size of the dots for the
#' \code{\link{geom_jitter}} function
#' @param dot.colour The colour of the dots for the
#' \code{\link{geom_jitter}} function
#' @param custom.var.names This is a list object that contains alternative names for the variables you are passing into the function
#' @param xlab The label for the x axis
#' @param ylab The label for the y axis
#' @return Prints off the names within the supplied \code{subset.ds},
#' as a reminder of what you put into the function
#' @keywords univariate jitter boxplot \pkg{ggplot2} \pkg{reshape2}
#' exploration
#' @export
#' @author Luke W. Johnston
#' @examples
#' 
#' varnames <- list("Name of X1" = "X1", "Name of X2" = "X2", ...)
#' df <- subset(ds, VN == 0, select=nefa)
#' names(df)
#' jitter.boxplot(df, xlab="Concentration (nmol/mL)",
#'                ylab="Fatty acid", custom.var.names=varnames)
#'
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

