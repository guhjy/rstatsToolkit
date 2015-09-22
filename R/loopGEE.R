##' Loop through each combination of dependent and independent
##' variables, running a generalized estimating equations model on
##' each combination, and generate a dataframe of the results.
##'
##' @title Loop through multiple generalized estimating equations
##' (GEE) analyses.
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
##' @param filter.indep Logical; Keep only the rows that have the
##' \code{independent} variables.
##' @param filter.interact Logical; Keep only the rows that have the
##' \code{interaction} variable.
##' @param adjust.p.value Logical; Adjust for multiple comparisons
##' using the False Discovery Rate.
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
##' names(ds)
##' outcome <- c('Income', 'Murder')
##' exposure <- c('Population', 'Life.Exp', 'Illiteracy')
##' covar <- c('Area', 'Frost')
##' cid <- 'state.region'
##' 
##' loopGEE(ds, outcome, exposure, cid)
##' loopGEE(ds, outcome, exposure, cid, covariates = covar)
##' loopGEE(ds, outcome, exposure, cid, covariates = covar, interaction = 'HS.Grad')
##' loopGEE(ds, outcome, exposure, cid, covariates = covar, interaction = 'Area')
##' loopGEE(ds, outcome, exposure, cid, corstr = 'ar1', conf.level = 0.99)
##' loopGEE(ds, outcome, exposure, cid, corstr = 'ar1', conf.int = FALSE)
##' loopGEE(ds, outcome, exposure, cid, corstr = 'ar1', adjust.p.value = TRUE)
##' loopGEE(ds, outcome, exposure, cid, covariates = covar, adjust.p.value = TRUE)
##' loopGEE(ds, outcome, exposure, cid, covariates = covar, adjust.p.value = TRUE,
##'         filter.indep = TRUE)
##' loopGEE(ds, outcome, exposure, cid, interaction = 'Area', covariates = covar,
##'         adjust.p.value = TRUE, filter.interact = TRUE)
##'
loopGEE <- function(data,
                    dependent,
                    independent,
                    id,
                    covariates = NULL,
                    interaction = NULL,
                    corstr = 'exchangeable',
                    family = gaussian,
                    conf.int = TRUE,
                    conf.level = 0.95,
                    na.rm = TRUE,
                    filter.indep = FALSE,
                    filter.interact = FALSE,
                    adjust.p.value = FALSE) {

    ## Test if the interaction input is more than 1 interaction. I
    ## haven't developed that yet... FIXME
    if (length(interaction) > 1) stop("At this time, testing for interactions can only have one variable.")

    if (!is.null(interaction)) {
        if (interaction %in% independent) stop('Interaction term must be also in the covariate list')
        if (! interaction %in% covariates) stop('Interaction term must be also in the covariate list')
    }

    if (filter.indep & filter.interact) stop('Only one filter can be TRUE')
    
    ## Create the formulas for the GEE analysis.
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

    ## Prepare the dataset for the analysis
    prep.ds <- data %>%
      dplyr::ungroup() %>%
      dplyr::select_(.dots = c(dependent, independent, id, covariates)) %>%
      tidyr::gather_('dep', 'Yvalue', dependent) %>%
      tidyr::gather_('indep', 'independent', independent) %>%
      dplyr::rename_('SID' = id)

    ## Because geeglm doesn't handle missingness, just remove if the option is given.
    if (na.rm) prep.ds <- na.omit(prep.ds)

    ## Run the GEE on each of the dep and indep variables
    gee.ds <- prep.ds %>%
      dplyr::group_by(dep, indep) %>%
      dplyr::do(geepack::geeglm(geeFormula, data = ., id = SID, corstr = corstr,
                    family = family) %>%
           broom::tidy(., conf.int = conf.int, conf.level = conf.level)) %>%
      dplyr::ungroup()

    
    ## Filter the independent variables dataset if need be.
    if (filter.indep) gee.ds <- dplyr::filter(gee.ds, term == 'independent')
    
    ## Filter the interaction variables dataset if need be.
    if (filter.interact) gee.ds <- dplyr::filter(gee.ds, grepl(':', term))

    ## Make multiple testing comparison corrections, if need be
    if (adjust.p.value) gee.ds <- dplyr::mutate(gee.ds, p.value = p.adjust(p.value, 'fdr'))

    ## Create a p.value factor variable and output.
    gee.ds %>%
      dplyr::mutate(f.pvalue = cut(p.value, breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
                            labels = c('<0.001', '<0.01', '<0.05', '>0.05'),
                            ordered_result = TRUE) %>%
               factor(., levels = rev(levels(.))))
}

