% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mdc.plot.R
\name{mdc.plot}
\alias{mdc.plot}
\title{Mean based dot chart with error bars.}
\usage{
mdc.plot(
  x,
  y,
  pch = 19,
  col = 1,
  ylim = c(-3, 3),
  xlab,
  ylab,
  xlas = 1,
  ord,
  std = TRUE,
  add = FALSE,
  lg = 0,
  ug = 1,
  xjit = 0
)
}
\arguments{
\item{x}{Categorical predictor.}

\item{y}{Continuous response.}

\item{pch}{Shape definition.}

\item{col}{Color definition.}

\item{ylim}{Y-axis limits.}

\item{xlab}{X-axis label.}

\item{ylab}{Y-axis label.}

\item{xlas}{X-axis label orientation.}

\item{ord}{Re-ordering vector for the predictor levels.}

\item{std}{LOGICAL: should y be standardized?}

\item{add}{LOGICAL: should the new values be added to the current plot?}

\item{lg}{Lower guide for the x-axis.}

\item{ug}{Upper guide for the x-axis.}
}
\description{
Creates a plot of means with error bars for a categorical predictor
and continuous response variable..
}
\note{
%% ~~further notes~~
}
\examples{

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

}
\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\author{
Matthew K. Lau
}
\keyword{~kwd1}
\keyword{~kwd2}
