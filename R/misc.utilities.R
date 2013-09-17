#  File R/misc.utilities.R in package statnet.common, part of the Statnet suite
#  of packages for network analysis, http://statnet.org .
#
#  This software is distributed under the GPL-3 license.  It is free,
#  open source, and has the attribution requirements (GPL Section 7) at
#  http://statnet.org/attribution
#
#  Copyright 2003-2013 Statnet Commons
#######################################################################
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

## Only run expr if environment variable testvar is set to specified values. Otherwise, skip them and optionally print a message documenting this.
opttest <- function(expr, testname=NULL, testvar="ENABLE_statnet_TESTS", yesvals=c("y","yes","t","true","1"), lowercase=TRUE){
  testval <- Sys.getenv(testvar)
  if(lowercase) testval <- tolower(testval)
  if(testval %in% yesvals)
    eval.parent(expr)
  else
    if(!is.null(testname))
      message(testname," test(s) skipped. Set ",testvar," environment variable to run.")
}
