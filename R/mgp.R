#' Modified graph plotting for network models.
#' 
#' High level access to the gplot function in the sna package.
#' 
#' This is a high level function for plotting networks using the sna package
#' specifically tailored for community data based network models.
#' 
#' @param net A network.
#' @param com The community matrix used to generate the network with species in
#' the same order as the network.
#' @param my.coord A set of point coordinates to coordinates the nodes in the
#' network.
#' @param loc Logical: should the location (i.e. coordinates) of the plotted
#' nodes be returned?
#' @param v.scale Value to scale the node (i.e. vertex) size.
#' @param v.min Minumum size for a node (i.e. vertex).
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
#' function (scn, com, loc = TRUE, my.coord = "") 
#' {
#'     e.col <- sign(scn)
#'     e.col[e.col == 1] <- "grey"
#'     e.col[e.col == -1] <- "red"
#'     v.cex <- apply(com, 2, sum)
#'     v.cex <- log(v.cex, 10)
#'     v.cex <- v.cex * (1/min(v.cex))
#'     v.cex <- v.cex/2
#'     if (length(my.coord) == 1) {
#'         coord <- gplot(abs(scn), displaylabels = TRUE, gmode = "graph", 
#'             pad = 1.5, edge.col = e.col, edge.lwd = abs(scn), 
#'             vertex.cex = v.cex, vertex.col = "darkgrey", vertex.border = "darkgrey")
#'     }
#'     else {
#'         coord <- gplot(abs(scn), displaylabels = TRUE, gmode = "graph", 
#'             pad = 1.5, edge.col = e.col, edge.lwd = abs(scn), 
#'             vertex.cex = v.cex, vertex.col = "darkgrey", vertex.border = "darkgrey", 
#'             coord = my.coord)
#'     }
#'     if (loc) {
#'         return(coord)
#'     }
#'     else {
#'     }
#'   }
#' 
mgp <- function(net='species network',com='community matrix',my.coord='',loc=TRUE,v.scale=3,v.min=0.1,displaylabels=FALSE){

v.cex <- apply(com[,apply(com,2,sum)!=0],2,sum) #scaling node size by the log of species frequencies
v.cex <- (((v.cex/sum(v.cex))/max((v.cex/sum(v.cex))))*v.scale)+v.min
e.col <- net
e.col[net>0.5] <- 'red'
e.col[net<0.5] <- 'black'
e.col[net==0.5] <- 'grey'

if (length(my.coord)==1){
  coord <- gplot(abs(net),displaylabels=displaylabels,gmode='graph',pad=1.5,
        edge.lwd=(abs(net)),vertex.cex=v.cex,vertex.col='grey',
        edge.col=e.col,label.lty=1)
}else{
  gplot(abs(net),displaylabels=displaylabels,gmode='graph',pad=1.5,
        edge.lwd=(abs(net)),vertex.cex=v.cex,vertex.col='grey',
        edge.col=e.col,coord=my.coord,label.lty=1)
}
if (loc){return(coord)}else{}
}
