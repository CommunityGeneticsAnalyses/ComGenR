cgPlotweb <- function(x,g){
  x <- x[order(apply(x,1,function(x) sum(sign(x))),decreasing=TRUE),
           order(apply(x,2,function(x) sum(sign(x))),decreasing=TRUE)]
  plotweb(apply(x,2,function(x,g) tapply(x,g,mean),g=g),method='normal')
}
