#' Plot a genotype-community bipartite network.
#' 
#' High level function for using the plotweb function in the bipartite package.
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param x Bipartite network to be plotted.
#' @param g Grouping or genotype vector.
#' @param col.low Color of the "lower" part of the network.
#' @param col.high Color of the "higher" part of the network.
#' @param lab.cex Expansion factor for the node labels.
#' @return %% ~Describe the value returned %% If it is a LIST, use %%
#' \item{comp1 }{Description of 'comp1'} %% \item{comp2 }{Description of
#' 'comp2'} %% ...
#' @note %% ~~further notes~~
#' @author %% ~~who you are~~
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
cgPlotweb <- function(x,g='genotypes',col.geno=TRUE,lab.cex=1,mean.geno=TRUE){
  if (col.geno){
    col.geno <- rainbow(length(unique(g)))[as.numeric(factor(g))]
    col.geno <- col.geno[apply(x,1,function(x)sum(sign(x)))]
  }else{col.geno <- 'black'}
  if (mean.geno){x <- mean.g(x,g);col.geno <- 'black'}
  x <- x[order(apply(x,1,function(x)sum(sign(x))),decreasing=TRUE),
         order(apply(x,2,function(x)sum(sign(x))),decreasing=TRUE)]
  plotweb(x,method='normal',col.low=col.geno,text.rot=90,labsize=lab.cex)
}
