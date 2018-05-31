#  File R/misc.utilities.R in package statnet.common, part of the Statnet suite
#  of packages for network analysis, http://statnet.org .
#
#  This software is distributed under the GPL-3 license.  It is free,
#  open source, and has the attribution requirements (GPL Section 7) at
#  http://statnet.org/attribution
#
#  Copyright 2007-2018 Statnet Commons
#######################################################################
#' reorder vector v into order determined by matching the names of its elements
#' to a vector of names
#' 
#' A helper function to reorder vector \code{v} (if named) into order specified
#' by matching its names to the argument \code{names}
#' 
#' does some checking of appropriateness of arguments, and reorders v by
#' matching its names to character vector \code{names}
#' 
#' @param v a vector (or list) with named elements, to be reorderd
#' @param names a character vector of element names, corresponding to names of
#' \code{v}, specificying desired orering of \code{v}
#' @param errname optional, name to be reported in any error messages. default
#' to \code{deparse(substitute(v))}
#' @return returns \code{v}, with elements reordered
#' @note earlier versions of this function did not order as advertiased
#' @examples
#' 
#' test<-list(c=1,b=2,a=3)
#' vector.namesmatch(test,names=c('a','c','b'))
#' @export
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


#' "Compress" a data frame.
#' 
#' \code{compress.data.frame} "compresses" a data frame, returning unique rows
#' and a tally of the number of times each row is repeated, as well as a
#' permutation vector that can reconstruct the original data frame.
#' \code{decompress.data.frame} reconstructs the original data frame.
#' 
#' 
#' @param x For \code{compress.data.frame} a \code{\link{data.frame}} to be
#' compressed. For \code{decompress.data.frame} a \code{\link{list}} as
#' returned by \code{compress.data.frame}.
#' @return For \code{compress.data.frame}, a \code{\link{list}} with three
#' elements: \item{rows }{Unique rows of \code{x}} \item{frequencies }{A vector
#' of the same length as the number or rows, giving the number of times the
#' corresponding row is repeated } \item{ordering}{A vector such that if
#' \code{c} is the compressed data frame, \code{c$rows[c$ordering,,drop=FALSE]}
#' equals the original data frame, except for row names} \item{rownames}{Row
#' names of \code{x}}
#' 
#' For \code{decompress.data.frame}, the original data frame.
#' @seealso \code{\link{data.frame}}
#' @keywords manip
#' @examples
#' 
#' (x <- data.frame(V1=sample.int(3,30,replace=TRUE),
#'                  V2=sample.int(2,30,replace=TRUE),
#'                  V3=sample.int(4,30,replace=TRUE)))
#' 
#' (c <- compress.data.frame(x))
#' 
#' stopifnot(all(decompress.data.frame(c)==x))
#' @export
compress.data.frame<-function(x){
  r <- rownames(x)
  o <- order.data.frame(x)
  x <- x[o, , drop=FALSE]
  firsts<-which(!duplicated(x))
  freqs<-diff(c(firsts,nrow(x)+1))
  x<-x[firsts, , drop=FALSE]
  list(rows=x, frequencies=freqs, ordering=order(o), rownames=r) # Note that x[order(x)][order(order(x))]==x.
}

#' @rdname compress.data.frame
#' @export
decompress.data.frame<-function(x){
  r <- x$rows
  rn <- x$rownames
  f <- x$frequencies
  o <- x$ordering

  out <- r[rep.int(seq_along(f), f),, drop=FALSE][o,, drop=FALSE]
  rownames(out) <- rn
  out
}

#' @rdname sort.data.frame
#' @export
order <- function(..., na.last = TRUE, decreasing = FALSE) UseMethod("order")

#' @rdname sort.data.frame
#' @export
order.default <- function(..., na.last = TRUE, decreasing = FALSE) base::order(..., na.last=na.last, decreasing=decreasing)

#' @rdname sort.data.frame
#' @export
order.data.frame<-function(..., na.last = TRUE, decreasing=FALSE){
  x <- list(...)[[1]]
  do.call(base::order,c(unname(x), na.last=na.last, decreasing=decreasing))
}

#' @rdname sort.data.frame
#' @export
order.matrix<-function(..., na.last = TRUE, decreasing=FALSE){
  x <- list(...)[[1]]
  do.call(base::order,c(lapply(seq_len(ncol(x)), function(i) x[,i]), na.last=na.last, decreasing=decreasing))
}



#' Implement the \code{\link{sort}} and \code{\link{order}} methods for
#' \code{\link{data.frame}} and \code{\link{matrix}}, sorting it in
#' lexicographic order.
#' 
#' These function return a data frame sorted in lexcographic order or a
#' permutation that will rearrange it into lexicographic order: first by the
#' first column, ties broken by the second, remaining ties by the third, etc..
#' 
#' 
#' @param x A \code{\link{data.frame}} to sort.
#' @param \dots Ignored for \code{sort}. For \code{order}, first argument is
#' the data frame to be ordered. (This is needed for compatibility with
#' \code{\link[base]{order}}.)
#' @param decreasing Whether to sort in decreasing order.
#' @param na.last See \code{\link[base]{order}} documentation.
#' @return For \code{sort}, a data frame, sorted lexicographically. For
#' \code{order}, a permutation \code{I} (of a vector \code{1:nrow(x)}) such
#' that \code{x[I,,drop=FALSE]} equals \code{x} ordered lexicographically.
#' @seealso \code{\link{data.frame}}, \code{\link{sort}}, \code{\link{order}},
#' \code{\link{matrix}}
#' @keywords manip
#' @examples
#' 
#' data(iris)
#' 
#' head(iris)
#' 
#' head(order(iris))
#' 
#' head(sort(iris))
#' 
#' stopifnot(identical(sort(iris),iris[order(iris),]))
#' @export
sort.data.frame<-function(x, decreasing=FALSE, ...){
  x[order(x,decreasing=decreasing),,drop=FALSE]
}


#' Convenience functions for handling [`NULL`] objects.
#'
#' 
#' @param \dots,test expressions to be tested.
#' 
#' @name NVL
#'
#' @note Whenever possible, these functions use lazy evaluation, so,
#'   for example `NVL(1, stop("Error!"))` will never evaluate the
#'   [`stop`] call and will not produce an error, whereas `NVL(NULL, stop("Error!"))` would.
#'
#' @seealso [`NULL`], \code{\link[base]{is.null}}, \code{\link[base]{if}}
#' @keywords utilities
#'
NULL

#' @describeIn NVL
#'
#' Inspired by SQL function \code{NVL}, returns the first argument
#' that is not \code{NULL}, or \code{NULL} if all arguments are
#' `NULL`.
#'
#' @examples
#' a <- NULL
#' 
#' a # NULL
#' NVL(a,0) # 0
#' 
#' b <- 1
#' 
#' b # 1
#' NVL(b,0) # 1
#' 
#' # Here, object x does not exist, but since b is not NULL, x is
#' # never evaluated, so the statement finishes.
#' NVL(b,x) # 1
#' 
#' # Also,
#' NVL(NULL,1,0) # 1
#' NVL(NULL,0,1) # 0
#' NVL(NULL,NULL,0) # 0
#' NVL(NULL,NULL,NULL) # NULL
#' @export
NVL <- function(...){
  for(e in eval(substitute(alist(...)))){ # Lazy evaluate. (See http://adv-r.had.co.nz/Computing-on-the-language.html .)
    x <- eval(e, parent.frame())
    if(!is.null(x)) break
  }
  x
}

#' @describeIn NVL
#'
#' Inspired by Oracle SQL function `NVL2`, returns the second argument
#' if the first argument is not `NULL` and the third argument if the
#' first argument is `NULL`. The third argument defaults to `NULL`, so
#' `NVL2(a, b)` can serve as shorthand for `(if(!is.null(a)) b)`.
#'
#' @param notnull expression to be returned if `test` is not `NULL`.
#' @param null expression to be returned if `test` is `NULL`.
#'
#' @examples
#' 
#' NVL2(a, "not null!", "null!") # "null!"
#' NVL2(b, "not null!", "null!") # "not null!"
#' @export
NVL2 <- function(test, notnull, null = NULL){
  if(is.null(test)) null else notnull
}


#' @describeIn NVL
#'
#' Inspired by Oracle SQL `NVL2` function and `magittr` \code{\%>\%}
#' operator, behaves as `NVL2` but `.`s in the second argument are
#' substituted with the first argument.
#'
#' @examples
#' 
#' NVL3(a, "not null!", "null!") # "null!"
#' NVL3(b, .+1, "null!") # 2
#' @export
NVL3 <- function(test, notnull, null = NULL){
  if(is.null(test)) null
  else{
    e <- substitute(notnull)
    eval(do.call(substitute, list(e, list(.=test))),
         parent.frame())
  }
}


#' @describeIn NVL
#'
#' Assigning to `NVL` overwrites its first argument if that argument
#' is [`NULL`]. Note that it will *always* return the right-hand-side
#' of the assignment (`value`), regardless of what `x` is.
#'
#' @param x an object to be overwritten if [`NULL`].
#' @param value new value for `x`.
#'
#' @examples
#' 
#' NVL(a) <- 2
#' a # 2
#' NVL(b) <- 2
#' b # still 1
#' @export
`NVL<-` <- function(x, value){
  if(is.null(x)) value
  else x
}


#' Return the first argument passed (out of any number) that is not a
#' \code{try-error} (result of \code{\link[base]{try}} encountering an error.
#' 
#' This function is inspired by \code{\link{NVL}}, and simply returns the first
#' argument that is not a \code{try-error}, raising an error if all arguments
#' are \code{try-error}s.
#' 
#' 
#' @param \dots Expressions to be tested; usually outputs of
#'   \code{\link[base]{try}}.
#' @return The first argument that is not a \code{try-error}. Stops
#'   with an error if all are.
#' @note This function uses lazy evaluation, so, for example `ERRVL(1,
#'   stop("Error!"))` will never evaluate the [`stop`] call and will
#'   not produce an error, whereas `ERRVL(try(solve(0)),
#'   stop("Error!"))` would.
#'
#' In addition, all expressions after the first may contain a `.`,
#' which is substituted with the `try-error` object returned by the
#' previous expression.
#' 
#' @seealso \code{\link[base]{try}}, \code{\link[base]{inherits}}
#' @keywords utilities
#' @examples
#' 
#' print(ERRVL(1,2,3)) # 1
#' print(ERRVL(try(solve(0)),2,3)) # 2
#' print(ERRVL(1, stop("Error!"))) # No error
#' 
#' \dontrun{
#' # Error:
#' print(ERRVL(try(solve(0), silent=TRUE),
#'             stop("Error!")))
#' 
#' # Error with an elaborate message:
#' print(ERRVL(try(solve(0), silent=TRUE),
#'             stop("Stopped with an error: ", .)))
#' }
#' @export
ERRVL <- function(...){
  x <- NULL
  for(e in eval(substitute(alist(...)))){ # Lazy evaluate. (See http://adv-r.had.co.nz/Computing-on-the-language.html .)
    x <- eval(if(inherits(x, "try-error")) do.call(substitute, list(e, list(.=x))) else e, parent.frame())
    if(!inherits(x, "try-error")) return(x)
  }
  stop("No non-error expressions passed.")
}


#' Optionally test code depending on environment variable.
#' 
#' A convenience wrapper to run code based on whether an environment variable
#' is defined.
#' 
#' 
#' @param expr An expression to be evaluated only if \code{testvar} is set to a
#' non-empty value.
#' @param testname Optional name of the test. If given, and the test is
#' skipped, will print a message to that end, including the name of the test,
#' and instructions on how to enable it.
#' @param testvar Environment variable name. If set to one of the
#' \code{yesvals}, \code{expr} is run. Otherwise, an optional message is
#' printed.
#' @param yesvals A character vector of strings considered affirmative values
#' for \code{testvar}.
#' @param lowercase Whether to convert the value of \code{testvar} to lower
#' case before comparing it to \code{yesvals}.
#' @keywords utilities environment debugging
#' @export
opttest <- function(expr, testname=NULL, testvar="ENABLE_statnet_TESTS", yesvals=c("y","yes","t","true","1"), lowercase=TRUE){
  testval <- Sys.getenv(testvar)
  if(lowercase) testval <- tolower(testval)
  if(testval %in% yesvals)
    eval.parent(expr)
  else
    if(!is.null(testname))
      message(testname," test(s) skipped. Set ",testvar," environment variable to run.")
}

#' Test if all items in a vector or a list are identical.
#'
#' @param x a vector or a list
#'
#' @return `TRUE` if all elements of `x` are identical to each other.
#'
#' @seealso [`identical`]
#'
#' @examples
#'
#' stopifnot(!all_identical(1:3))
#'
#' stopifnot(all_identical(list("a", "a", "a")))
#' @export
all_identical <- function(x){
  if(length(x)==0) return(TRUE)
  v0 <- x[[1]]
  for(v in x[-1]) if(!identical(v0,v)) return(FALSE)
  return(TRUE)
}

#' Construct a logical vector with `TRUE` in specified positions.
#'
#' This function is basically an inverse of [`which`].
#'
#' @param which a numeric vector of indices to set to `TRUE`.
#' @param n total length of the output vector.
#'
#' @return A logical vector of length `n` whose elements listed in
#'   `which` are set to `TRUE`, and all other elements set to `FALSE`.
#'
#' @examples
#'
#' x <- as.logical(rbinom(10,1,0.5))
#' stopifnot(all(x == unwhich(which(x), 10)))
#' @export
unwhich <- function(which, n){
  o <- logical(n)
  if(length(which)) o[which] <- TRUE
  o
}
