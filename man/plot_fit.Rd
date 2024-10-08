% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_fit.R
\name{plot_fit}
\alias{plot_fit}
\title{Plot the fitted values and sample quantiles for a walker object}
\usage{
plot_fit(object, level = 0.05, alpha = 0.33, ...)
}
\arguments{
\item{object}{An output from \code{\link[=walker]{walker()}} or \code{\link[=walker_glm]{walker_glm()}}.}

\item{level}{Level for intervals. Default is 0.05, leading to 90\% intervals.}

\item{alpha}{Transparency level for \code{\link[ggplot2:geom_ribbon]{ggplot2::geom_ribbon()}}.}

\item{...}{Further arguments to \code{\link[bayesplot:PPC-intervals]{bayesplot::ppc_ribbon()}}.}
}
\description{
Plot the fitted values and sample quantiles for a walker object
}
