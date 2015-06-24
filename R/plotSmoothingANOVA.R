##' Generate a bivariate plot of a factor variable (x-axis) and a
##' continuous variable (y-axis) that overlays a smoothing line
##' (loess), which also prints an ANOVA p-value.
##'
##' Generally I use this for plotting longitudinal data, where the
##' x-axis is the timepoints and the ANOVA is testing the significance
##' across time (which may be arguably inappropriate... FIXME).
##' 
##' @title Smoothing line plot, with ANOVA
##' @param data Dataset with the variables of interest.
##' @param x The factor variable on the x-axis.  Generally is the time
##' variable in a longitudinal setting.  Must be a string/character.
##' @param y The continuous variable on the y-axis.  Must be a
##' string/character.
##' @param id The grouping variable, generally the ID for the
##' participant in a longitudinal setting.
##' @param y.axis.limits Limits of the y-axis.  Must be two numbers.
##' @author Luke W. Johnston
##' @export
##' @examples
##'
##' data(state)
##' cbind(state.region, state.x77) %>%
##'   as.data.frame() %>%
##'   mutate(f.Pop = Population %>%
##'       quantile(., c(0, .333, .666, 1), na.rm = TRUE) %>%
##'       cut(Population, ., include.lowest = TRUE)) %>%
##'   plotSmoothingANOVA(., 'f.Pop', 'Illiteracy', id = 'state.region')
##' 
plotSmoothingANOVA <- function(data, x, y, id, y.axis.limits = c(0.20, 0.80)) {
    ## Uses dplyr, ggplot2, and broom packages.

    if ( length(y.axis.limits) != 2 ) {
        stop('The y.axis.limits variable needs to contain two numbers using c()',
             call. = FALSE)
    }

    if ( !is.character(x) | !is.character(y) | !is.character(id) ) {
        stop('The x, y, or id variables need to be as a character or string.',
             call. = FALSE)
    }

    p <- data %>%
      ggplot(., aes_string(x, y, group = id)) +
      stat_smooth(aes(group = 1), method = 'loess', colour = 'black') +
      coord_cartesian(ylim = c(quantile(data[[y]], y.axis.limits[1], na.rm = TRUE),
                               quantile(data[[y]], y.axis.limits[2], na.rm = TRUE))) +
      annotate('text', label =
                 data %>%
                 select(contains(y), contains(x)) %>% 
                 aov(paste(y, paste('as.factor(', x, ')', sep = ''), sep = '~') %>%
                       as.formula(), data = .) %>%
                 tidy() %>%
                 head(1)['p.value'] %>%
                 round(3) %>%
                 { ifelse(. < 0.001, paste0('p < 0.001'), paste0('p = ', .)) },
               x = Inf, y = quantile(data[[y]], 0.65, na.rm = TRUE),
               hjust = 1, vjust = 1)

    return(p)
}
