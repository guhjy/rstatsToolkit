##' Lay out multiple ggplots on one frame or page.
##' 
##' This function, which was from
##' \url{http://www.cookbook-r.com/Graphs}, is used to lay out several
##' ggplot objects onto one frame or pdf page.  For instance, you can
##' have 3 plots on a page, one going vertically across the top, the
##' other two in each corner on the bottom.  This function makes up
##' for the difficulty \pkg{ggplot2} has with outputting multiple
##' plots on one grid.  This function depends on \pkg{grid}.
##' 
##' @title Multiple plots on page
##' @param ... Where the ggplot objects are placed to be laid out on
##' the graph grid
##' @param plotlist Can be used in place of the ... argument by
##' specifying the ggplot objects as a list object
##' @param file Not sure what this is used for
##' @param cols Number of columns for the layout.  For example cols=2
##' provides two columns and with four ggplot objects, the resulting
##' output would be a 2 by 2 graphic
##' @param layout A matrix that indicates the plot grid layout.  For
##' example, if layout = matrix(c(1, 2, 3, 3), nrow = 2, byrow = TRUE)
##' the result would have plot 1 in the upper left, plot 2 in the upper
##' right, and plot 3 would be go across the bottom
##' @export
##' @author Cookbook R
multiPlot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    ## Uses grid

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
