##' Calculates the sample size, mean, standard deviation, standard
##' error of the mean, and the confidence interval of specified
##' variables.
##'
##' I took this function on 2014-01-21 from the website
##' \url{http://www.cookbook-r.com/Graphs}.  It basically summarizes
##' the provided data by giving count, mean, standard deviation,
##' standard error of the mean, and confidence interval (default 95%).
##' The dependencies are \pkg{plyr}
##'
##' @title Summarize means and standard errors of the mean
##' @param data A dataset (dataframe) that contains the values to be summarized
##' @param measurevar The name of a column that contains the variable to be summarized
##' @param groupvars A vector containing names of columns that contain grouping variables
##' @param na.rm A binary (boolean) response that indicates whether to
##' ignore missing (NA) data
##' @param conf.interval Percent range of the confidence interval
##' @return Outputs a dataframe that contains the summarized statistics (means, etc.)
##' @export
##' @author Cookbook R
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
    require(plyr)

    ## New version of length which can handle NA's: if na.rm==T, don't count them
    length2 <- function (x, na.rm=FALSE) {
        if (na.rm) sum(!is.na(x))
        else       length(x)
    }

    ## This does the summary. For each group's data frame, return a vector with
    ## N, mean, and sd
    datac <- ddply(data, groupvars, .drop=.drop,
      .fun = function(xx, col) {
        c(N    = length2(xx[[col]], na.rm=na.rm),
          mean = mean   (xx[[col]], na.rm=na.rm),
          sd   = sd     (xx[[col]], na.rm=na.rm)
        )
      },
      measurevar
    )

    ## Rename the "mean" column    
    datac <- rename(datac, c("mean" = measurevar))

    datac$se <- datac$sd / sqrt(datac$N)  ## Calculate standard error of the mean

    ## Confidence interval multiplier for standard error
    ## Calculate t-statistic for confidence interval: 
    ## e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    ciMult <- qt(conf.interval/2 + .5, datac$N-1)
    datac$ci <- datac$se * ciMult

    return(datac)
}
