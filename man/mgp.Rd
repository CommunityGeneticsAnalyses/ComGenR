\name{mgp}
\alias{mgp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Modified graph plotting for network models.
}
\description{
High level access to the gplot function in the sna package.
}
\usage{
mgp(net='species network',com='community matrix',my.coord='',loc=TRUE,v.scale=3,v.min=0.1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{net}{
    A network.
  }
  \item{com}{
    The community matrix used to generate the network with species in
    the same order as the network.
  }
  \item{my.coord}{
    A set of point coordinates to coordinates the nodes in the network.
  }
  \item{loc}{
    Logical: should the location (i.e. coordinates) of the plotted nodes
    be returned?
  }
  \item{v.scale}{
    Value to scale the node (i.e. vertex) size.
}
  \item{v.min}{
    Minumum size for a node (i.e. vertex). 
  }

}
\details{
  This is a high level function for plotting networks using the sna
  package specifically tailored for community data based network models.
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
function (scn, com, loc = TRUE, my.coord = "") 
{
    e.col <- sign(scn)
    e.col[e.col == 1] <- "grey"
    e.col[e.col == -1] <- "red"
    v.cex <- apply(com, 2, sum)
    v.cex <- log(v.cex, 10)
    v.cex <- v.cex * (1/min(v.cex))
    v.cex <- v.cex/2
    if (length(my.coord) == 1) {
        coord <- gplot(abs(scn), displaylabels = TRUE, gmode = "graph", 
            pad = 1.5, edge.col = e.col, edge.lwd = abs(scn), 
            vertex.cex = v.cex, vertex.col = "darkgrey", vertex.border = "darkgrey")
    }
    else {
        coord <- gplot(abs(scn), displaylabels = TRUE, gmode = "graph", 
            pad = 1.5, edge.col = e.col, edge.lwd = abs(scn), 
            vertex.cex = v.cex, vertex.col = "darkgrey", vertex.border = "darkgrey", 
            coord = my.coord)
    }
    if (loc) {
        return(coord)
    }
    else {
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
