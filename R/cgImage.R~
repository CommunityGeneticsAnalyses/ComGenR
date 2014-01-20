cgImage <- function(x,xlab='',ylab='',col=c(0,1)){
  x <- x[order(apply(x,1,function(x) sum(sign(x))),decreasing=TRUE),
         order(apply(x,2,function(x) sum(sign(x))),decreasing=TRUE)]
  x[x!=0] <- 1
  image(t(x),col=col,xaxt='n',yaxt='n')
  mtext(side=c(1,2),c(xlab,ylab),font=2)
}
