#' Cross-hair plotting functions for 2D NMDS plots.
#' 
#' This is a high level plotting function that produces a "cross-hair" plot of
#' centroids and standard deviations for groups of ordinated points, as is
#' often desired with community genetics data.
#' 
#' Simple way to plot centroids of groups of ordinated points with standard
#' error values.
#' 
#' @param x Ordination matrix.
#' @param g Grouping (i.e. genotype) vector.
#' @param cex Value to scale centroids.
#' @param plot.legend Logical: should a legend be generated?
#' @param loc Location of the legend (e.g. top, top-right, right, bottom-right,
#' bottom, bottom-left,left,top-left).
#' @param mu.pch Input to define the shape of the centroids.
#' @note %% ~~further notes~~
#' @author Matthew K. Lau
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' ##---- Should be DIRECTLY executable !! ----
#' ##-- ==>  Define data, use random,
#' ##--	or do  help(data=index)  for the standard data sets.
#' 
ch.plot <- function(x = 'ordination matrix', g = 'groupings', cex = 1, plot.legend = FALSE, loc = 'topleft', mu.pch=19){
  mu <- apply(x,2,function(x,g) tapply(x,g,mean),g=g)
  se <- apply(x,2,function(x,g) tapply(x,g,function(x) sd(x)/sqrt(length(x))),g=g)
  mu <- na.omit(mu)
  se <- na.omit(se)
                                        #error bars
  cl.xu <- mu[,1] + se[,1]
  cl.xl <- mu[,1] - se[,1]
  cl.yu <- mu[,2] + se[,2]
  cl.yl <- mu[,2] - se[,2]
    if (plot.legend){
                                        #coloring
      mu.col <- rainbow(length(unique(g)))[as.numeric(unique(g))]
      plot(mu,pch=mu.pch,cex=cex,xlim=c(min(cl.xl),max(cl.xu)),ylim=c(min(cl.yl),max(cl.yu)),col=mu.col)
      for (i in 1:nrow(mu)){
        lines(x=c(cl.xl[i],cl.xu[i]),y=c(mu[i,2],mu[i,2]))
        lines(x=c(mu[i,1],mu[i,1]),y=c(cl.yl[i],cl.yu[i]))
      }    
      legend(loc,legend=rownames(se),cex=cex*0.5,pch=mu.pch,col=mu.col,border='grey')
  }else{
                                        #coloring
    mu.col <- 'black'
    plot(mu,pch=mu.pch,cex=cex,xlim=c(min(cl.xl),max(cl.xu)),ylim=c(min(cl.yl),max(cl.yu)),col=mu.col)
    for (i in 1:nrow(mu)){
      lines(x=c(cl.xl[i],cl.xu[i]),y=c(mu[i,2],mu[i,2]))
      lines(x=c(mu[i,1],mu[i,1]),y=c(cl.yl[i],cl.yu[i]))
    }
  }
}
