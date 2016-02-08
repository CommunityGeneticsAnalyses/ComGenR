### Minimize a network using any network generating function and an abundance threshold
### MKLau 7 Feb 2015

minNet <- function(x,FUN='cdNet',alpha=0.05){
    thresh <- start
    g <- get(FUN)(m,alpha=alpha)
    no.c <- no.clusters(graph.adjacency(g,weighted=TRUE))
    while (no.c == 1){
        thresh <- thresh + 1
        m[m < thresh] <- 0
        g <- get(FUN)(m,alpha=alpha)
        w <- c(w,components(graph.adjacency(g,weighted=TRUE),mode='weak'))
        s <- c(s,components(graph.adjacency(g,weighted=TRUE),mode='strong'))
        no.c <- no.clusters(graph.adjacency(g,weighted=TRUE))
    }
    m <- x
    m[m < (thresh - 1)] <- 0
    get(FUN)(m,alpha=alpha)
}
