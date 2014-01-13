\name{ch.plot}
\alias{ch.plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Cross-hair plotting functions for 2D NMDS plots. 
}
\description{
  This is a high level plotting function that produces a "cross-hair"
  plot of centroids and standard deviations for groups of ordinated points,
  as is often desired with community genetics data.
}
\usage{
ch.plot(x = "ordination matrix", g = "groupings", cex = 1, plot.legend =
FALSE, loc = "topleft", mu.pch = 19)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    Ordination matrix.
}
  \item{g}{
    Grouping (i.e. genotype) vector.
}
  \item{cex}{
    Value to scale centroids.
}
  \item{plot.legend}{
    Logical: should a legend be generated?
}
  \item{loc}{
    Location of the legend (e.g. top, top-right, right, bottom-right,
    bottom, bottom-left,left,top-left).
  }
  \item{mu.pch}{
    Input to define the shape of the centroids.
  }
}
\details{
  Simple way to plot centroids of groups of ordinated points with
  standard error values. 
}
% \value{
% %%  ~Describe the value returned
% %%  If it is a LIST, use
% %%  \item{comp1 }{Description of 'comp1'}
% %%  \item{comp2 }{Description of 'comp2'}
% %% ...
% }
% \references{
% %% ~put references to the literature/web site here ~
% }
\author{
  Matthew K. Lau
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x = "ordination matrix", g = "groupings", cex = 1, 
    buffer = 0.1, plot.legend = TRUE, loc = "topleft") 
{
    mu <- apply(x, 2, function(x, g) tapply(x, g, mean), g = g)
    se <- apply(x, 2, function(x, g) tapply(x, g, function(x) sd(x)/sqrt(length(x))), 
        g = g)
    mu <- na.omit(mu)
    se <- na.omit(se)
    mu.col <- "black"
    mu.pch <- 19
    cl.xu <- mu[, 1] + se[, 1]
    cl.xl <- mu[, 1] - se[, 1]
    cl.yu <- mu[, 2] + se[, 2]
    cl.yl <- mu[, 2] - se[, 2]
    plot(mu, pch = mu.pch, cex = cex, xlim = c(min(cl.xl), max(cl.xu)), 
        ylim = c(min(cl.yl), max(cl.yu)), col = mu.col)
    for (i in 1:nrow(mu)) {
        lines(x = c(cl.xl[i], cl.xu[i]), y = c(mu[i, 2], mu[i, 
            2]))
        lines(x = c(mu[i, 1], mu[i, 1]), y = c(cl.yl[i], cl.yu[i]))
    }
    if (plot.legend) {
        legend(loc, legend = rownames(se), cex = cex * 0.5, pch = mu.pch, 
            col = mu.col, border = "grey")
    }
    else {
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
