% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/autoplot.R
\name{autoplot.glex_vi}
\alias{autoplot.glex_vi}
\title{Plot glex Variable Importances}
\usage{
\method{autoplot}{glex_vi}(
  object,
  by_degree = FALSE,
  threshold = 0,
  max_interaction = NULL,
  scale = "absolute",
  ...
)
}
\arguments{
\item{object}{Object of class \code{glex_vi}, see \code{\link[=glex_vi]{glex_vi()}}.}

\item{by_degree}{(\code{logical(1): FALSE}) Optionally sum values by degree of interaction, resulting in one contribution score
for all main effects, all second-order interactions, etc.}

\item{threshold}{(\code{numeric(1)}: 0) Optional threshold to filter output to include only importance scores greater
than this value. Refers to the chosen \code{scale}.}

\item{max_interaction}{(\code{integer(1): NULL}) Optionally filter plot to show terms up to the specified
degree of interaction. Similar to \code{threshold}, all other terms will be aggregated under a
\code{"Remaining terms"} label.}

\item{scale}{(\code{"absolute"}) Plot average absolute contributions (default) or the same value but scaled by the
average prediction (\code{"relative"}).}

\item{...}{(Unused)}
}
\value{
A \code{\link[ggplot2:ggplot]{ggplot}} object.
}
\description{
Plot glex Variable Importances
}
\seealso{
\link{glex_vi}

Other Visualization functions: 
\code{\link{autoplot.glex}()},
\code{\link{glex_explain}()},
\code{\link{plot_pdp}()}
}
\concept{Visualization functions}
