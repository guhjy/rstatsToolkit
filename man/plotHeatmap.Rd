% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/plotHeatmapCorr.R
\name{plotHeatmap}
\alias{plotHeatmap}
\title{Heatmap}
\usage{
plotHeatmap(data, x = "Var1", y = "Var2", heat.colours = c("darkorange2",
  "skyblue4"), show.corr.values = TRUE, ylab = y.axis.label,
  xlab = x.axis.label, y.axis.label = NULL, x.axis.label = NULL,
  corr.text.size = 5, legend.position = "left")
}
\arguments{
\item{data}{The dataset to plot.}

\item{x}{The x axis variables.}

\item{y}{The y axis variables.}

\item{heat.colours}{The spectrum of colours for the heat map, as a
vector between the lowest (negative) correlation and the highest
(positive) correlation.}

\item{show.corr.values}{Logical; add the correlation values to the
heatmap.}

\item{ylab}{}

\item{xlab}{}

\item{y.axis.label}{}

\item{x.axis.label}{}

\item{corr.text.size}{Size of the text that show the correlation
values.}
}
\value{
Heatmap
}
\description{
Create a heatmap.
}
\details{
Used with \code{\link{plotHeatmapCorr}}.
}
\author{
Luke W. Johnston
}

