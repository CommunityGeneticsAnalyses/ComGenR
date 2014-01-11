\name{cnm.test}
\alias{cnm.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Co-occurrence Null Model test.
}
\description{
Provides high level user access to the null modeling tools in the
vegan package.
}
\usage{
cnm.test(com, method = "r1", nits = 5000, burn = 500, thin = 10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{com}{
Community matrix with species in columns.
}
  \item{method}{
    Permutation method. Options include: r00 = fully random, r0 = maintain
    site frequencies, r1 = maintain both site and species frequencies. For
    more options see the oecosimu function in the vegan package.
  }
  \item{nits}{
    Number of iterations to conduct. 
  }
  \item{burn}{
    The number permutations to conduct prior to recording matrices.
  }
  \item{thin}{
    The number of discarded matrices between iterations.
  }
}
\details{
  
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
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
function (com, method = "r1", nits = 5000, burn = 500, thin = 10) 
{
    com.nul <- nullCom(com, method = method, nits = nits, burn = burn, 
        thin = thin)
    cs.nul <- unlist(pblapply(com.nul, cscore))
    cs.obs <- cscore(com)
    ses <- (cs.obs - mean(cs.nul))/sd(cs.nul)
    return(c(SES = ses, lower.p = length(cs.nul[cs.nul <= cs.obs])/length(cs.nul), 
        upper.p = length(cs.nul[cs.nul >= cs.obs])/length(cs.nul)))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
