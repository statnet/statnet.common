## Compress a data frame by eliminating duplicate rows while keeping
## track of their frequency.
compress.data.frame<-function(x){
  x<-sort(x)
  firsts<-which(!duplicated(x))
  freqs<-diff(c(firsts,nrow(x)+1))
  x<-x[firsts,]
  list(rows=x,frequencies=freqs)
}

## Sorts rows of a data frame in lexicographic order.
sort.data.frame<-function(x, decreasing=FALSE, ...){
  x[do.call(order,c(sapply(seq_along(x),function(i)x[[i]],simplify=FALSE), decreasing=decreasing)),]
}

## Return the first non-NULL argument. If all arguments are NULL, return NULL.
NVL <- function(...){
  for(x in list(...))
    if(!is.null(x)) break
  x
}
