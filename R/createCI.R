##' Generate confidence intervals (upper and lower) and p-values from
##' a regression (e.g. GEE or GLM).
##'
##' @title Compute confidence interval for GEE
##' @param data The results output from a GEE analysis from the
##' \code{\link[gee]{gee}} package.
##' @param test.distribution,dist The column that contains the test
##' statistic distribution, e.g. z-score or t-score.
##' @param standard.error,se The column that contains the standard
##' error.
##' @param sig.level The significance level for calculating the
##' confidence interval.
##' @return Columns with the confidence interval and p-value.
##' @export
##' @author Luke W. Johnston
##' @examples
##'
##' data(state)
##' 
##' ## Very simple test example.  Merely to show how the function is used.
##' outcomes <- c('Income', 'Population')
##' exposures <- c('Frost', 'Illiteracy')
##' covariates <- c('Murder', 'LifeExp')
##' interaction <- 'LifeExp'
##' 
##' ## This uses the dplyr package.
##' ds <- cbind(state.region, state.x77) %>%
##'   as.data.frame() %>%
##'   rename(LifeExp = `Life Exp`,
##'          ## Need to rename the id variable to SID (see description
##'          ## above)
##'          SID = state.region) %>%
##'   arrange(SID)
##' 
##' ## Without interaction
##' loopOutputToListGEE(ds, outcomes, exposures, covariates,
##'                     corstr = 'exchangeable') %>%
##'   extractBetaFromListGEE() %>%
##'   unlistAndFilterIndep(., exposures) %>%
##'   createCI()
##' 
##' ## With interaction
##' loopOutputToListGEE(ds, outcomes, exposures, covariates,
##'                     interaction, corstr = 'exchangeable') %>%
##'   extractBetaFromListGEE() %>%
##'   unlistAndFilterIndep(., ':', pattern = TRUE) %>%
##'        ## The ':' represents an interaction
##'   createCI()
##' 
createCI <- function(data, dist = test.distribution,
                     se = standard.error, test.distribution = "Naive.z",
                     standard.error = "Naive.S.E.", sig.level = 0.95) {
    ## Uses dplyr and lazyeval (only load, but dplyr loads it).
    
    ## Test if the two variables are the same (the short form vs long form)
    if (dist != test.distribution | se != standard.error) {
        test.distribution <- dist
        standard.error <- se
    }
    
    ## Create a pvalue and confidence interval from the 
    data %>%
      mutate_(pvalue = lazyeval::interp( ~ 2 * pnorm(-abs(distrib)),
                                        distrib = as.name(test.distribution)),
             upperCI = lazyeval::interp(~ Estimate + (std.err * qnorm((1 + sig.level)/2)),
                                        std.err = as.name(standard.error)),
             lowerCI = lazyeval::interp(~ Estimate - (std.err * qnorm((1 + sig.level)/2)),
                                        std.err= as.name(standard.error))
             ) %>%
      mutate(f.pvalue = cut(pvalue, breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
                             labels = c('<0.001', '<0.01', '<0.05', '>0.05'),
                             ordered_result = TRUE) %>% factor(., levels = rev(levels(.))))
}
