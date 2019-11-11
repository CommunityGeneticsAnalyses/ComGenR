#' Mean based dot chart with error bars.
#' 
#' Creates a plot of means with error bars for a categorical predictor
#' and continuous response variable..
#' 
#' @param x Categorical predictor.
#' @param y Continuous response.
#' @param pch Shape definition.
#' @param col Color definition.
#' @param ylim Y-axis limits.
#' @param xlab X-axis label.
#' @param ylab Y-axis label.
#' @param xlas X-axis label orientation.
#' @param ord Re-ordering vector for the predictor levels.
#' @param std LOGICAL: should y be standardized?
#' @param add LOGICAL: should the new values be added to the current plot?
#' @param lg Lower guide for the x-axis.
#' @param ug Upper guide for the x-axis.
#' @export mdc.plot
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
mdc.plot <- function(x, y, pch = 19, col = 1, 
                     ylim = c(-3, 3), xlab, ylab, xlas = 1,
                     ord, std = TRUE, add = FALSE, lg = 0, ug = 1,
                     xjit = 0){
    if (std){y <- (y - mean(y)) / sd(y)}
    x <- factor(x)
    mu <- tapply(y, x, mean)
    se <- tapply(y, x, function(x) sd(x) / sqrt(length(x)))
    if (exists("ord")){
        se <- se[ord]
        mu <- mu[ord]
    }
    se.bars <- matrix(c(mu + se, mu - se), nrow = 2, byrow = TRUE)
    n <- nlevels(x)
    x.grid <- seq(lg, ug, by = 1 / (length(unique(x)) + 1))
    if (!exists("xlab")){xlab = ""}
    if (!exists("ylab")){ylab = ""}
    if (!add){
        plot(x.grid, rep(0, length(x.grid)), pch = "", 
             ylab = ylab, xlab = xlab, 
             las = xlas, xaxt = "none", ylim = ylim)
    }
    axis(1, at = x.grid[(1:n + 1)], labels = names(mu), las = xlas)
    points(x.grid[(1:n + 1)] + xjit, mu, pch = pch)
    for (i in 1:length(mu)){
        lines(rep(x.grid[(1:n + 1)][i], 2) + xjit, 
              c(mu[i] - se[i], mu[i] + se[i]) + xjit)
    }
}
