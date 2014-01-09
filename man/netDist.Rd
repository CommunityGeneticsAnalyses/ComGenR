\name{netDist}
\alias{netDist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Network distance for a set of networks.
}
\description{
  Takes a list of networks and returns a distance matrix using Euclidean distance.
}
\usage{
netDist(dn.t)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dn.t}{
    A list of networks to be used for calculating distances.
}
}
\details{
  This function computes the Euclidean (i.e. straight line) distance
  using all edges for all pairs of networks in a list of networks. The
  mathematics of using Euclidean distance to summarize the difference
  between two networks has not been explored fully, so use with caution.

}
\value{
  Returns a matrix of Euclidean distances for all pairs of networks in
  the list.
}
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
function (dn.t) 
{
    net.d <- matrix(0, nrow = length(dn.t), ncol = length(dn.t))
    rownames(net.d) <- colnames(net.d) <- names(dn.t)
    for (i in 1:nrow(net.d)) {
        for (j in 1:ncol(net.d)) {
            net.d[i, j] <- sum(abs(dn.t[[i]] - dn.t[[j]])^2)
        }
    }
    net.d <- as.dist(net.d)
    return(net.d)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
