##' Unlist a list of dataframes, convert into a single dataframe, and
##' filter out a string or pattern from the 'indep' column.
##'
##' This function is generally used within a chain of other commands
##' that creates a list of dataframes, generally from a regression.
##' The list gets unlisted and converted into a single dataset.
##' Afterward, the dataset gets filtered by the variables of interest
##' (eg. the exposures) that are contained within the 'indep' column.
##' 
##' @title Unlist a list of dataframes and filter a pattern or variable.
##' @param data The list object with the dataframes
##' @param x The variables of interest (eg. exposures) that are within
##' the 'indep' column.
##' @param pattern Logical: TRUE if \code{x} is a pattern rather than
##' an explicit list of variables and FALSE if \code{x} is an explicit
##' list of variables (eg. exposures).
##' @return Outputs a single dataframe with only the rows with the
##' variables from \code{x}.
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
##'   unlistAndFilterIndep(., exposures)
##' 
##' ## With interaction
##' loopOutputToListGEE(ds, outcomes, exposures, covariates,
##'                     interaction, corstr = 'exchangeable') %>%
##'   extractBetaFromListGEE() %>%
##'   unlistAndFilterIndep(., ':', pattern = TRUE)
##'        ## The ':' represents an interaction
##' 
unlistAndFilterIndep <- function(data, x, pattern = FALSE) {
    ## Uses the dplyr and data.table packages.

    message('The dataset must contain an "indep" column name,\nwhere the "x" selects from.')

    data %>%
      ## Unlist the list of dataframes and combine them into a single
      ## dataframe.
      data.table::rbindlist() %>%
      ## Convert the dataframe into a dplyr datatable, for easier
      ## viewing on screen.
      tbl_df() %>%
      ## This allows patterns to be used for filtering out the other
      ## variables.
      { if (pattern == TRUE) {
          filter(., grepl(x, indep))
      } else if (pattern == FALSE) {
          filter(., indep %in% c(x))
      } }
}
