#' Creates a white, simple ggplot theme
#' 
#' The default ggplot theme is decent for most purposes, but is
#' visually unappealing.  This function aims to correct that by
#' setting the theme to something more similar to the default in the
#' base R plot package.
#' 
#' @keywords ggplot2 themes customizing graphics
#' @export
#' @author Luke Johnston
#' @example
#' 
#' ## This creates a white theme
#' theme_white()
#' 
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
