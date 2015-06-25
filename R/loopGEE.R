##' Loop through each combination of dependent and independent
##' variables and generate a dataframe of the results.
##'
##' @title Loop through multiple GEE analyses.
##' @param data Dataset to run GEE on
##' @param dependent The dependent (aka outcome or response)
##' variables.  Must be quoted and can have several.
##' @param independent Like \code{dependent}, except the explanatory
##' (aka predictor or exposure) variables.
##' @param id The variable to cluster on for GEE, for instance the
##' 'ID' variable for a person in a longitudinal cohort.
##' @param covariates The covariate variables.  Can be multiple
##' covariates.
##' @param interaction A single interaction variable.
##' @param na.rm Remove missing values from the dataset before running
##' GEE.  \code{geeglm} can't handle any missingness, so sometimes
##' it's necessary to remove missingness.
##' @inheritParams geepack::geeglm
##' @inheritParams broom::lm_tidiers
##' @export
##' @return A dataframe of all the GEE analyses, with estimates,
##' confidence intervals, and p-values.
##'
##' @seealso \code{\link[broom]{tidy}}
##' @author Luke W. Johnston
##'
##' @examples
##'
##' data(state)
##' ds <- data.frame(state.region, state.x77)
##' loopGEE(ds, c('Income', 'Frost'), c('Population', 'Murder'), 'state.region')
##' loopGEE(ds, 'Income', 'Population', 'state.region',
##' covariates = c('Frost', 'Area'))
##' loopGEE(ds, 'Income', 'Population', 'state.region',
##' covariates = 'Frost', interaction = 'Frost')
##' loopGEE(ds, 'Income', 'Population', 'state.region', corstr = 'ar1',
##' conf.int = FALSE)
##'
##' @import geepack
##' @import tidyr
##' @import dplyr
##' 
loopGEE <- function(data, dependent, independent, id, covariates = NULL, interaction = NULL,
                    corstr = 'exchangeable', family = gaussian, conf.int = TRUE,
                    conf.level = 0.95, na.rm = TRUE) {
    if (length(interaction) > 1) {
        stop("At this time, testing for interactions can only have one variable.")
    }
    
    if (is.null(covariates) & is.null(interaction)) {
        geeFormula <- as.formula(paste('Yvalue ~ independent'))
    } else if (is.character(covariates) & is.null(interaction)) {
        geeFormula <- as.formula(paste('Yvalue ~ independent',
                                       paste(covariates, collapse = ' + '),
                                       sep = ' + '))
    } else if (is.character(covariates) & is.character(interaction)) {
        geeFormula <- as.formula(paste('Yvalue ~ independent',
                                       paste(covariates, collapse = ' + '),
                                       paste('independent', interaction, sep = ':'),
                                       sep = ' + '))
    } else {
        stop('Covariates and interaction varibles should be quoted variable names.')
    }

    prep.ds <- data %>%
      ungroup() %>%
      select_(.dots = c(dependent, independent, id, covariates)) %>%
      gather_('dep', 'Yvalue', dependent) %>%
      gather_('indep', 'independent', independent) %>%
      rename_('SID' = id)

    if (na.rm) prep.ds <- na.omit(prep.ds)

    prep.ds %>%
      group_by(dep, indep) %>%
      do(geepack::geeglm(geeFormula, data = ., id = SID, corstr = corstr,
                    family = family) %>%
           broom::tidy(., conf.int = conf.int, conf.level = conf.level)) %>%
      ungroup() %>%
      mutate(f.pvalue = cut(p.value, breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
                            labels = c('<0.001', '<0.01', '<0.05', '>0.05'),
                            ordered_result = TRUE) %>%
               factor(., levels = rev(levels(.))))
}

