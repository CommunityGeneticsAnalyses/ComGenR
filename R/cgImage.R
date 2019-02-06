#' Plot a binary representation of a matrix.
#' 
#' Generates an "image" of the input matrix in binary form.
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param x Matrix to be plotted.
#' @param xlab Label for the x-axis.
#' @param ylab Label for the y-axis.
#' @param col Vector of length two defining the two levels of color in the
#' plot.
#' @return Plot.
#' @note %% ~~further notes~~
#' @author Matthew K. Lau
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% ~put references to the literature/web site here ~
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' ##---- Should be DIRECTLY executable !! ----
#' ##-- ==>  Define data, use random,
#' ##--	or do  help(data=index)  for the standard data sets.
#' 
#' ## The function is currently defined as
#' 
#' 
cgImage <- function(x,xlab='',ylab='',col=c(0,1)){
  x <- x[order(apply(x,1,function(x) sum(sign(x))),decreasing=TRUE),
         order(apply(x,2,function(x) sum(sign(x))),decreasing=TRUE)]
  x[x!=0] <- 1
  image(t(x),col=col,xaxt='n',yaxt='n')
  mtext(side=c(1,2),c(xlab,ylab),font=2)
}
