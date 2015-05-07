##' Plot subjects in a longitudinal dataset, making a 'spaghetti'
##' plot.
##'
##' @title Spaghetti plot
##' @param data The dataset to plot.
##' @param y The variable to go on the y-axis.
##' @param x The variable to go on the x-axis.
##' @param groups The unique ID variable to differentiate subjects in
##' a longitudinal dataset.
##' @export
##' @author Luke W. Johnston
##' @examples
##' \dontrun{
##' ## A pretend case (not a real example)
##' plotSpaghetti(dataset, 'Height', 'Year', 'SubjectID')
##' }
plotSpaghetti <- function(data, y, x, groups = 'SID') {
    ## Uses ggplot
    ggplot(data, aes_string(y = y, x = x, group = groups)) +
        geom_line(position = position_jitter(w = 0.1, h = 0.1))
}
