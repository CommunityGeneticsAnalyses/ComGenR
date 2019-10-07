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
#' @param lwd Line width scaler. 
#' @param mu.pch Centroid shape.
#' @param pt.col Centroid color.
#' @param car.col Error bar color.
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
ch.plot <- function(x = 'ordination matrix', 
                    g = 'groupings', 
                    cex = 1,
                    lwd = 1, 
                    mu.pch = 19,
                    pt.col = 1, 
                    bar.col = 1){
    mu <- apply(x, 2, function(x, g) tapply(x, g, mean), g = g) 
    se <- apply(x, 2, function(x, g) tapply(x, g, function(x)
            sd(x)/sqrt(length(x))), g = g) 
    mu <- na.omit(mu) 
    se <- na.omit(se)
                                        #error bars
    cl.xu <- mu[, 1] +  se[, 1]
    cl.xl <- mu[, 1] -  se[, 1]
    cl.yu <- mu[, 2] + se[, 2]
    cl.yl <-  mu[, 2] -  se[, 2]
                                        # plotting
    plot(mu, pch = 0, cex = 0, 
         xlim = c(min(cl.xl), max(cl.xu)), 
         ylim = c(min(cl.yl), max(cl.yu)))
    for (i in 1:nrow(mu)){
        lines(x = c(cl.xl[i], cl.xu[i]), 
              y = c(mu[i, 2], mu[i, 2]), 
              col = bar.col, lwd = lwd)
        lines(x = c(mu[i, 1], mu[i, 1]), 
              y = c(cl.yl[i], cl.yu[i]), 
              col = bar.col, lwd = lwd)
        }
    points(mu, cex = cex, col = pt.col, pch = mu.pch)
                                        # Return
    mu
}
