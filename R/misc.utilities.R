## A helper function to reorder vector v (if named) into order
## specified by names. Copied from ergm::ergm.model.utils.R.
vector.namesmatch<-function(v,names,errname=NULL){
  if(is.null(errname)) errname <- deparse(substitute(v))

  if (is.null(names(v))){
    if(length(v) == length(names)){
      names(v) <- names
    }else stop('Length of "', errname, '" is ', length(v), " but should be ", length(names),".")
  }else{
    if(length(v) == length(names)
       && length(unique(names(v)))==length(v)
       && length(unique(names))==length(names)
       && all(sort(names(v)) == sort(names))){
      namesmatch <- match(names, names(v))
      v <- v[namesmatch]
    }else stop('Name missmatch in "', errname,'". Specify by position.')
  }
  v
}

## Compress a data frame by eliminating duplicate rows while keeping
## track of their frequency and keeping track of the original ordering.
compress.data.frame<-function(x){
  r <- rownames(x)
  o <- order.data.frame(x)
  x <- x[o, , drop=FALSE]
  firsts<-which(!duplicated(x))
  freqs<-diff(c(firsts,nrow(x)+1))
  x<-x[firsts, , drop=FALSE]
  list(rows=x, frequencies=freqs, ordering=order(o), rownames=r) # Note that x[order(x)][order(order(x))]==x.
}

decompress.data.frame<-function(x){
  r <- x$rows
  rn <- x$rownames
  f <- x$frequencies
  o <- x$ordering

  out <- r[rep.int(seq_along(f), f),, drop=FALSE][o,, drop=FALSE]
  rownames(out) <- rn
  out
}

order <- function(..., na.last = TRUE, decreasing = FALSE) UseMethod("order")

order.default <- function(..., na.last = TRUE, decreasing = FALSE) base::order(..., na.last=na.last, decreasing=decreasing)

## order method for a data frame in lexicographic order.
order.data.frame<-function(..., na.last = TRUE, decreasing=FALSE){
  x <- list(...)[[1]]
  do.call(base::order,c(sapply(seq_along(x),function(i)x[[i]],simplify=FALSE), na.last=na.last, decreasing=decreasing))
}


## Sorts rows of a data frame in lexicographic order.
sort.data.frame<-function(x, decreasing=FALSE, ...){
  x[order(x,decreasing=decreasing),,drop=FALSE]
}

## Return the first non-NULL argument. If all arguments are NULL, return NULL.
NVL <- function(...){
  for(x in list(...))
    if(!is.null(x)) break
  x
}

## Return the first non-try-error argument. If all arguments are try-errors, stop with an error.
ERRVL <- function(...){
  for(x in list(...))
    if(!inherits(x, "try-error")) return(x)
  stop("No non-error expressions passed.")
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
