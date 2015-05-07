##' \code{createFormulaList} returns a list of formulas with all
##' combinations of dependent and independent variables.
##'
##' This function creates a list of formulas for use in a chain of
##' processes.  This function's goal is to create all combinations of
##' a set of dependent, or outcome, variables with a set of
##' independent, or exposure variables.  Covariates and an interaction
##' term can also be specified and included into the formula.
##'
##' @title Create a list of formulas
##' @param dependent A single or a vector of variables names that the
##' user wishes to use as dependent variables in an analysis;
##' \code{dependent} must be as a string/character.
##' @param independent Similar to \code{dependent}, except that the
##' variables would be the independent, or exposure, variables of
##' interest.
##' @param covariates Confounders or covariates that would be included
##' in \strong{all} formulas and hence \strong{all} models.  Also must
##' be a string/character.
##' @param interactions Optional, include \emph{one} variable from the
##' \code{covariates} set that will be assigned as an interaction term
##' with the \code{independent} variable.  Must be a string/character.
##' @return Outputs a list of formulas.
##' @author Luke W. Johnston
##' @examples
##'
##' outcomes <- c('Income', 'Education', 'Job')
##' exposures <- c('Age', 'Sex', 'Height', 'Race', 'IQ')
##' covariates <- c('ParentEdu', 'Country', 'City')
##' interactions <- 'City'
##'
##' createFormulaList(outcomes, exposures, covariates)
##' createFormulaList(outcomes, exposures, covariates, interactions)
##' 
createFormulaList <- function(dependent, independent,
                              covariates, interactions = NULL) {
    ## Uses dplyr pipes and functions.

    ## Confirm that each variable is a character.
    if (!is.character(dependent) | !is.character(independent) | !is.character(covariates)) {
        stop('The variables to be included into the formula list must be strings/characters.')
    }

    ## At this time, I can only use one interaction term at a time. I
    ## don't know how programmatically to include more than one
    ## interaction terms.  I'll have to try to figure it out later.
    if (length(interactions) > 1) {
        stop('At this time, only one interaction can be specified')
    }

    ## This is the initial preparation to get all combinations of the
    ## dependent and independent variables, eg. if there are 2 y's and
    ## 2 x's, the end result is 4 variables (y1x1, y2x1, x2y1...).
    ## This allows regression analyses to loop through each combination.
    preFormulaList <-
        expand.grid(dep = as.list(dependent),
                    indep = as.list(independent)) %>%
          mutate(covars = paste(covariates, collapse = ' + ',
                                sep = ' + '),
                 pre = paste(indep, interactions, sep = ':'))

    ## Combine the variables on the 'right side' of the
    ## equation/formula (ie. the independent, or predictor,
    ## variables).  If an interaction term is specified, include it as
    ## well.  This step prepares the variables for final formula list
    ## creation.
    if (!is.null(interactions) & is.character(interactions)) {
        preFormulaList <- preFormulaList %>%
          mutate(indep = paste(indep, covars, pre, sep = ' + ')) %>%
          select(-pre, -covars)
    } else if (is.null(interactions)) {
        preFormulaList <- preFormulaList %>%
          mutate(indep = paste(indep, covars, sep = ' + ')) %>%
          select(-pre, -covars)
    } else {
        stop('Interaction should either be a string or NULL')
    }

    ## Create a list object of formulas so that the list can be
    ## processed by other functions to loop through analyses
    ## (eg. linear regression or GEE)
    formulaList <- with(preFormulaList,
                        Map(function(y, x) as.formula(paste0(y, ' ~ ', x)),
                            dep, indep))

    return(formulaList)
}
