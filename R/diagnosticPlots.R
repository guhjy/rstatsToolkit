##' Generate regression diagnostic plots and tests for linear
##' regression models.
##' 
##' This function runs a linear regression on the specified variables
##' and generates diagnostics based on the regression.  Basic
##' diagnostics include checking the normality of the residuals,
##' assessing outliers, influence and Cook's D, and multicollinearity.
##' Several tests have been commented out, though they can be
##' uncommented if desired (edit the function to output these if
##' desired).  Some of the tests I don't fully understand how to
##' interpret them, but as I learn more I will probably know.  This
##' function relies on \pkg{MASS} and \pkg{gplots}.
##' 
##' @title Regression diagnostic plots and tests
##' @param data The dataset with the variables of interest
##' @param y The dependent or outcome variable (that is, the \code{y}
##' in the regression equation)
##' @param x The independent, exposure, or predictor variable (that
##' is, the \code{x} in the regression equation)
##' @param covar The variables selected as to condition or adjust for
##' the \code{y} and \code{x} relationship, also known as the
##' confounding variables
##' @return Outputs multiple plots and textplots with diagnostic
##' information
##' @export
##' @author Luke Johnston
diagnosticPlots <- function(data, y, x, covar){

    require(MASS)
    require(gplots)

    ## Linear regression
    p.df <- data[,c(y, x)]
    p.df <- as.data.frame(append(p.df, data[,covar]))
    fit <- lm(p.df[,1] ~ ., data=p.df[,-1], na.action=na.omit)

    y <- names(p.df[1])
    x <- names(p.df[2])
    covar <- paste(names(p.df[,3:ncol(p.df)]), collapse = " ", sep = " ")

    ## Identify the variables being examined
    cat("Starting plots for:", y, x, covar, "\n")
    textplot(paste("Plots for regression using:", y, x, "\n", covar))

    ## General diagnostics plot
    par(mfrow=c(2,2))
    plot(fit, ask=FALSE, main=paste("Plot for", y, x, collapse = " ", sep = " "))
    par(mfrow=c(1,1))

    ## Assessing Outliers
    ## QQ plot for studentized resid
    ## qqPlot(fit, ask=FALSE, main=paste("QQ Plot:", y, x), id.n=3) 
    
    ## Leverage plots
    leveragePlots(fit, id.n=3, ask=FALSE,
                  main=paste("Leverage plot:", y, x))

    ## Influential Observations
    ## Added Variable plots
    ##avPlots(fit, ask=FALSE, lwd=1, id.n=3, 
    ##        col=c("black","black"), 
    ##        main=paste("A-V plot:", y, x))

    ## Cook's D plot
    ## D values > 4/(n-k-1)
    cutoff <- 4/((nrow(p.df)-length(fit$coefficients)-2))
    plot(fit, which=4, cook.levels=cutoff, ask=FALSE,
         main=paste("Cook's D plot:", y, x))

    ## Influence Plot
    influencePlot(fit, main=paste("Influence Plot:", y, x), 
                  sub="Circle size is proportial to Cook's Distance",
                  ask=FALSE, id.n=3)

    ## Checking normality of studentized residuals
    hist(studres(fit), freq=FALSE, ask=FALSE,
         main=paste("Distribution of Studentized Residuals:", y, x))
    lines(seq(min(studres(fit)),max(studres(fit)),length=40), 
          dnorm(seq(min(studres(fit)),max(studres(fit)),length=40)))

    ## Evaluate homoscedasticity
    ## Studentized residuals vs. fitted values
    ##spreadLevelPlot(fit, id.n=3, ask=FALSE,
    ##                main=paste("SpreadPlot for", y, x))

    ## Significance test
    ##textplot(ncvTest(fit))

    ## Evaluate Nonlinearity
    ## Component + residual plot
    ##crPlots(fit, col=c("black","black"), 
    ##        ask=FALSE,id.n=3, 
    ##        lwd=1, main=paste("Plot for", y, x))

    ## Multicollinearity
    textplot(vif(fit))
}
