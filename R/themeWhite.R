##' Creates a white, simple theme for \pkg{ggplot2} objects
##' 
##' The default \pkg{ggplot2} theme is decent for most purposes, but
##' is visually unappealing.  This function aims to correct that by
##' setting the theme to something more similar to the default in the
##' base R plot package.  The function dependes on \pkg{ggplot2}.
##' 
##' @title Custom white ggplot theme
##' @author Luke Johnston
##' @export
##' @examples
##' 
##' ## This creates a white theme
##' themeWhite()
##' 
themeWhite <- function() {
    theme_update(panel.background = element_blank(),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.border = element_blank(),
                 axis.line = element_line(colour = "black"),
                 axis.text = element_text(colour = "black"),
                 axis.ticks = element_line(colour = "black")
                 )
}
