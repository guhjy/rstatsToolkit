##' Run a GEE analysis on a list of formulas and create a list of GEE
##' objects.
##'
##' This function is merely a wrapper around \code{\link[gee]{gee}},
##' and therefore all inquiries into GEE should start there.  As a
##' note, I don't know how (yet) to provide an option to use a custom
##' \code{id} variable for the GEE analysis, so \strong{make sure to
##' rename your \code{id} variable to 'SID'}.  Also, according to the
##' GEE documentations, the \strong{ordering of the ID variables
##' matters}!  Make sure to sort the ID variables as per how you want
##' them!  For documentation on the function to create the formula
##' list, see \code{\link{createFormulaList}}.
##' 
##' @title Loop through GEE analyses.
##' @param data The dataset with the variables of interest
##' @inheritParams createFormulaList
##' @param corstr The working correlation structure to use in the
##' \code{\link[gee]{gee}} function call.  Options can be found in the
##' gee package, but include 'exchangeable', 'unstructured', 'AR-M' (M = 
##' @return Creates a list of GEE objects
##' @author Luke W. Johnston
##' @export
##' @examples
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
##'                     corstr = 'exchangeable')
##' 
##' ## With interaction
##' loopOutputToListGEE(ds, outcomes, exposures, covariates,
##'                     interaction, corstr = 'exchangeable')
##' 
loopOutputToListGEE <- function(data, dependent, independent, covariates,
                          interactions = NULL, corstr = 'exchangeable') {
    ## Uses the gee package.

    ## Call this function to allow GEE to loop through all formulas.
    formulaList <- createFormulaList(dependent, independent, covariates, interactions)

    ## This loops through each of the formulas and runs a GEE
    ## analysis.  This creates a list object.  The benefit of this is
    ## that it can be used as part of a chain to extract parts of the
    ## GEE.  Also, suppress messages to screen using capture.output().
    capture.output(
    geeList <- lapply(formulaList, function(x) {
        suppressMessages(gee(x, id = SID, data = data, corstr = corstr,
            na.action = na.omit))
    }), file = '/dev/null')

    return(geeList)
}
