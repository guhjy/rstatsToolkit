##' Used as part of a chain, \code{extractBetaFromListGEE} grabs the
##' beta estimate from a list of GEE objects.
##'
##' This is used after \code{\link{loopOutputToListGEE}} as part of a
##' chain, preferably using dplyr/magrittr's pipe command (%>%).
##' After generating the list of GEE objects,
##' \code{extractBetaFromListGEE} loops through each GEE object and
##' converts the beta estimate and associated statistics into list of
##' dataframes.
##' 
##' @title Extract beta coefficients from a GEE list object
##' @param data This is variable that contains the list of GEE objects.
##' @return Outputs the beta coefficients.
##' @export
##' @author Luke W. Johnston
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
##'                     corstr = 'exchangeable') %>%
##'   extractBetaFromListGEE()
##' 
##' ## With interaction
##' loopOutputToListGEE(ds, outcomes, exposures, covariates,
##'                     interaction, corstr = 'exchangeable') %>%
##'   extractBetaFromListGEE()
##' 
extractBetaFromListGEE <- function(data) {
    ## Uses dplyr packages.
    
    ## Loop the extractBetaGEE function through the list of GEE
    ## objects.
    geeBeta <- lapply(data, extractBetaGEE)
    return(geeBeta)
}

##' Extract the beta object from a GEE object.
##'
##' @title Extract beta coefficients from a GEE object
##' @param data The variable that contains the GEE object.
##' @author Luke W. Johnston
extractBetaGEE <- function(data) {
    ## Uses the dplyr package. 

    ## Extract the beta estimates, convert the result into a
    ## dataframe, and include variables and columns to make
    ## identifying the output easier (eg. add the dependent variable
    ## name to the output dataset).
    out <- data %>%
      summary() %>%
      coef() %>%
      as.data.frame() %>%
      add_rownames() %>%
      mutate(dep =
               ## Extract the dependent variable name by grabbing the formula in
               ## the GEE object, convert the formula into a vector of strings,
               ## and finally take the first variable, which is the dependent
               ## variable.
               data %>%
               formula() %>%
               all.vars() %>%
               .[1]) %>%
      rename(indep = rowname)

    ## Remove any spaces from the column names.
    names(out) <- gsub(' ', '.', names(out))

    return(out)
}
