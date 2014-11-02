##' Generate a matrix or non-matrix style heatmap of correlation
##' coefficients (i.e. the number of columns and rows can be
##' different).
##'
##' This function takes two arguments, the x variables and the y
##' variables, and generates a heatmap from the variables.  A
##' correlation matrix is computed from the data, melted (reshape
##' package), and input into ggplot2 to generate a heatmap.  The output
##' is the correlations and the plot object.
##'
##' Dependencies are: \pkg{reshape2} and \pkg{ggplot2}
##'
##' @title Correlation heatmap
##' @param data Dataset that contains the variables of interest
##' @param x Vector of variables that will run along the x-axis
##' @param y Same as the \code{x} arg, but for the y-axis
##' @param leg.range Range in values for the legend, between -1 and 1
##' @param levels.xlab Specifies custom variable names for the x-axis
##' (it needs to be a list object)
##' @param levels.ylab Same as the \code{levels.xlab} arg, but for the y-axis
##' @param xlab Sets the label for the x-axis
##' @param ylab Same as \code{xlab}, but for the y-axis
##' @param lo.color Color of negative correlation coefficients
##' @param hi.color Color of positive correlation coefficients
##' @param rm.legend In development. Goal is it will remove the legend
##' @param matrix.sty In development. Goal is to select whether the
##' heatmap will be a matrix style (the same variable on the x- and y-
##' axis) or non-matrix
##' @param ... Potential options to add. In development
##' @return Outputs a plot object
##' @author Luke Johnston
##' @export
##' @examples
##' 
##' xlabs <- list("X Name" = "x1", "X Name Two!" = "x2" ... )
##' ylabs <- list("Y Name" = "y1", "Y Name Two!" = "y2" ... )
##' df <- data.frame(replicate(10,sample(0:1,1000,rep=TRUE)))
##' plot <- heatmap.corr(data = df, x = 1:3, y = 4:10,
##'                      levels.xlab=xlabs, levels.ylab=ylabs)
##' print(plot)
##'
heatmapCorr <- function(data, x, y, leg.range=c(-1.0, 1.0),
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

    ## Depending on whether you want only one set of variables of both
    ## the y and x axis (matrix.sty = TRUE) or if you want different
    ## variables on each axis (matrix.sty = FALSE), run this code to
    ## set the data accordingly.
    if (matrix.sty == TRUE) {
        rows <- length(x)
        for (i in 1:rows) {
            ## Remove values that will be repeated as in a matrix.
            cor[i, 1:i] <- NA
        }
    } else if (matrix.sty == FALSE) {
        ## Set values = to 1 to be removed. This only happens when the
        ## same variable is correlated to itself.
        cor[cor == 1] <- NA
    } else {
        ## A test to make sure that the matrix.sty arg is set properly.
        stop("Specify either TRUE or FALSE for the matrix.sty arg")
    }
    
    ## Melt base
    cor.dat <- melt(cor)
    cor.dat <- cor.dat[-which(is.na(cor.dat[, 3])), ]

    ## If specified, provide custom levels for the x and y vars. Set
    ## the custom variable depending on whether matrix is TRUE or
    ## FALSE.
    if (!is.null(levels.xlab)) {
        levels(cor.dat$Var1) <- levels.ylab
        if (matrix.sty == TRUE) {
            levels(cor.dat$Var2) <- levels.ylab
            cor.dat$Var2 <- factor(cor.dat$Var2,
                                   levels = rev(levels(cor.dat$Var2)))
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
heatmapCorr <- function(data, x, y, leg.range=c(-1.0, 1.0),
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
