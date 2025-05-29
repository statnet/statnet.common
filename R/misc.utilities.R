#  File R/misc.utilities.R in package statnet.common, part of the Statnet suite
#  of packages for network analysis, https://statnet.org .
#
#  This software is distributed under the GPL-3 license.  It is free, open
#  source, and has the attribution requirements (GPL Section 7) at
#  https://statnet.org/attribution .
#
#  Copyright 2007-2025 Statnet Commons
################################################################################
#' reorder vector v into order determined by matching the names of its elements
#' to a vector of names
#' 
#' This function is deprecated in favor of [match_names()] and will be
#' removed in a future release.
#' 
#' @param v a vector (or list) with named elements, to be reorderd
#' @param names a character vector of element names, corresponding to names of
#' \code{v}, specificying desired orering of \code{v}
#' @param errname optional, name to be reported in any error messages. default
#' to \code{deparse(substitute(v))}
#' @return returns \code{v}, with elements reordered
#' @note earlier versions of this function did not order as advertised
#' @examples
#' 
#' test<-list(c=1,b=2,a=3)
#' vector.namesmatch(test,names=c('a','c','b'))
#' @export
vector.namesmatch<-function(v,names,errname=NULL){
  if(is.null(errname)) errname <- deparse(substitute(v))

  if(is.null(names(v))){
    if(length(v) == length(names)){
      names(v) <- names
    }else stop('Length of ', sQuote(errname), ' is ', length(v), " but should be ", length(names),".")
  }else{
    if(length(v) == length(names)
       && length(unique(names(v)))==length(v)
       && length(unique(names))==length(names)
       && all(sort(names(v)) == sort(names))){
      namesmatch <- match(names, names(v))
      v <- v[namesmatch]
    }else stop('Name mismatch in ', sQuote(errname),'. Specify by position.')
  }
  v
}

#' Construct a named vector with semantics useful for parameter vectors
#'
#' This is a helper function that constructs a named vector with names
#' in `names` with values taken from `v` and optionally `default`,
#' performing various checks. It supersedes [vector.namesmatch()].
#'
#' If `v` is not named, it is required to be the same length as
#' `names` and is simply given the corresponding names. If it is
#' named, nonempty names are matched to the corresponding elements of
#' `names`, with partial matching supported.
#'
#' Default values can be specified by the caller in `default` or by
#' the end-user by adding an element with an empty (`""`) name in
#' addition to the others. If given, the latter overrides the former.
#'
#' Duplicated names in `v` or `names` are resolved sequentially,
#' though note the example below for caveat about partial matching.
#'
#' Zero-length `v` is handled as follows:
#'
#' * If length of `names` is empty, return `v` unchanged.
#'
#' * If it is not and `default` is not `NULL`, return the `default` vector.
#'
#' * Otherwise, raise an error.
#'
#' An informative error is raised under any of the following conditions:
#'
#' * `v` is not named but has length that differs from that of `names`.
#'
#' * More than one element of `v` has an empty name.
#'
#' * Not all elements of `names` are matched by an element of `v`, and
#'   no default is specified.
#'
#' * Not all elements of `v` are used up for elements of `names`.
#'
#' * There is ambiguity that [pmatch()] cannot resolve.
#'
#' @note At this time, passing `partial=FALSE` will use a crude
#'   sentinel to prevent partial matching, which in some, extremely
#'   improbable, circumstances might not work.
#'
#' @param v a vector
#' @param names a character vector of element names
#' @param default value to be used for elements of `names` not found in `v`
#' @param partial whether partial matching is allowed
#' @param errname optional, name to be reported in any error messages;
#'   defaults to `deparse1(substitute(v))`
#' @return A named vector with names `names` (in that order). See
#'   Details.
#' @examples
#'
#' # Unnamed:
#' test <- as.numeric(1:3)
#' stopifnot(identical(
#'   match_names(test, c('a', 'c', 'b')),
#'   c(a = 1, c = 2, b = 3)
#' ))
#'
#' # Named, reordered:
#' test <- c(c = 1, b = 2, a = 3)
#' stopifnot(identical(
#'   match_names(test, c('a', 'c', 'b')),
#'   c(a = 3, c = 1, b = 2)
#' ))
#'
#' # Default value specified by default= assigned to a
#' test <- c(c = 1, b = 2)
#' stopifnot(identical(
#'   match_names(test, c('a', 'c', 'b'), NA),
#'   c(a = NA, c = 1, b = 2)
#' ))
#'
#' # Default value specified in v assigned to a and b:
#' test <- c(c = 1, 2)
#' stopifnot(identical(
#'   match_names(test, c('a', 'c', 'b')),
#'   c(a = 2, c = 1, b = 2)
#' ))
#'
#' # Partial matching
#' test <- c(c = 1, 2)
#' stopifnot(identical(
#'   match_names(test, c('a', 'cab', 'b')),
#'   c(a = 2, cab = 1, b = 2)
#' ))
#'
#' # Multiple matching
#' test <- c(c = 1, 2, c = 3)
#' stopifnot(identical(
#'   match_names(test, c('a', 'c', 'c')),
#'   c(a = 2, c = 1, c = 3)
#' ))
#'
#' # Partial + multiple matching caveat: exact match will match first.
#' test <- c(c = 1, a = 2, ca = 3)
#' stopifnot(identical(
#'   match_names(test, c('a', 'ca', 'ca')),
#'   c(a = 2, ca = 3, ca = 1)
#' ))
#'
#' @importFrom stats na.omit setNames
#' @export
match_names <- function(v, names, default = NULL, partial = TRUE, errname = NULL) {
  if(is.null(errname)) errname <- deparse1(substitute(v))

  if(length(v) == 0) {
    if(length(names) == 0) v
    else if(!is.null(default)) setNames(rep_len(default, length(names)), names)
    else stop(sQuote(errname), ' is ', sQuote(deparse1(v)), " but should have ", length(names)," element(s):\n", paste(strwrap(paste(sQuote(names), collapse = ", "), indent = 2, exdent = 2), collapse = "\n"), call. = FALSE)
  }else if(is.null(names(v))){
    if(length(v) == length(names)){
      setNames(v, names)
    }else stop('Length of ', sQuote(errname), ' is ', length(v), " but should be ", length(names),":\n", paste(strwrap(paste(sQuote(names), collapse = ", "), indent = 2, exdent = 2), collapse = "\n"), call. = FALSE)
  }else{
    blanks <- names(v) == ""
    if(any(blanks)){
      if(sum(blanks) > 1L) stop("Named vector ", sQuote(errname), " may have at most one unnamed element.", call. = FALSE)

      default <- unname(v[blanks])
      v <- v[!blanks]
    }

    # partial == FALSE -> add a sentinel string at the end of all strings to prevent partial matching.
    namesmatch <- if(partial) pmatch(names(v), names)
                  else pmatch(paste(names(v), "\n\xf5\xdc\n"), paste(names, "\n\xf5\xdc\n"))
    used <- !is.na(namesmatch)
    found <- unwhich(na.omit(namesmatch), length(names))

    if(is.null(default) && !all(found)) stop("Named vector ", sQuote(errname), " is missing values for the following elements: ", paste.and(sQuote(names[!found])), ".", call. = FALSE)
    if(!all(used)) stop("In named vector ", sQuote(errname), " unused or not uniquely matched elements: ", substr(s <- deparse1(v[!used]), 3, nchar(s)-1L), call. = FALSE)

    numeric(length(names)) |>
      replace(na.omit(namesmatch), v[used]) |>
      replace(!found, default) |>
      setNames(names)
  }
}

#' "Compress" a data frame.
#' 
#' \code{compress_rows.data.frame} "compresses" a data frame, returning unique rows
#' and a tally of the number of times each row is repeated, as well as a
#' permutation vector that can reconstruct the original data frame.
#' \code{decompress_rows.compressed_rows_df} reconstructs the original data frame.
#' 
#' 
#' @param x For \code{compress_rows.data.frame} a \code{\link{data.frame}} to be
#' compressed. For \code{decompress_rows.compress_rows_df} a \code{\link{list}} as
#' returned by \code{compress_rows.data.frame}.
#' @param ... Additional arguments, currently unused.
#' @return For \code{compress_rows.data.frame}, a \code{\link{list}} with three
#' elements: \item{rows }{Unique rows of \code{x}} \item{frequencies }{A vector
#' of the same length as the number or rows, giving the number of times the
#' corresponding row is repeated } \item{ordering}{A vector such that if
#' \code{c} is the compressed data frame, \code{c$rows[c$ordering,,drop=FALSE]}
#' equals the original data frame, except for row names} \item{rownames}{Row
#' names of \code{x}}
#' 
#' For \code{decompress_rows.compressed_rows_df}, the original data frame.
#' @seealso \code{\link{data.frame}}
#' @keywords manip
#' @examples
#' 
#' (x <- data.frame(V1=sample.int(3,30,replace=TRUE),
#'                  V2=sample.int(2,30,replace=TRUE),
#'                  V3=sample.int(4,30,replace=TRUE)))
#' 
#' (c <- compress_rows(x))
#' 
#' stopifnot(all(decompress_rows(c)==x))
#'
#' @export
compress_rows.data.frame<-function(x, ...){
  r <- rownames(x)
  o <- order.data.frame(x)
  x <- x[o, , drop=FALSE]
  firsts<-which(!duplicated(x))
  freqs<-diff(c(firsts,nrow(x)+1))
  x<-x[firsts, , drop=FALSE]
  structure(x, frequencies=freqs, ordering=order(o), rownames=r, class=c("compressed_rows_df", class(x))) # Note that x[order(x)][order(order(x))]==x.
}

#' @rdname compress_rows.data.frame
#' @export
decompress_rows.compressed_rows_df<-function(x, ...){
  r <- x
  rn <- attr(x, "rownames")
  f <- attr(x, "frequencies")
  o <- attr(x, "ordering")

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
  x <- list(...)[[1L]]
  do.call(base::order,c(unname(x), na.last=na.last, decreasing=decreasing))
}

#' @rdname sort.data.frame
#' @export
order.matrix<-function(..., na.last = TRUE, decreasing=FALSE){
  x <- list(...)[[1L]]
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
#' As `NVL`, but for any objects of length 0 (*E*mpty) rather than just `NULL`. Note that if no non-zero-length arguments are given, `NULL` is returned.
#'
#' @examples
#'
#' NVL(NULL*2, 1) # numeric(0) is not NULL
#' EVL(NULL*2, 1) # 1
#'
#' @export
EVL <- function(...){
  o <- NULL
  for(e in eval(substitute(alist(...)))){ # Lazy evaluate. (See http://adv-r.had.co.nz/Computing-on-the-language.html .)
    x <- eval(e, parent.frame())
    if(length(x)){ o <- x;  break }
  }
  o
}

#' @describeIn NVL
#'
#' As `NVL2`, but for any objects of length 0 (*E*mpty) rather than just `NULL`.
#'
#' @export
EVL2 <- function(test, notnull, null = NULL){
  if(length(test)) notnull else null
}

#' @describeIn NVL
#'
#' As `NVL3`, but for any objects of length 0 (*E*mpty) rather than just `NULL`.
#'
#' @export
EVL3 <- function(test, notnull, null = NULL){
  if(length(test)==0) null
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

#' @describeIn NVL
#'
#' As assignment to `NVL`, but for any objects of length 0 (*E*mpty) rather than just `NULL`.
#'
#' @export
`EVL<-` <- function(x, value){
  if(length(x)) x
  else value
}


#' Attempt a series of statements and return the first one that is not an error.
#'
#' `ERRVL()` expects the potentially erring statements to be wrapped
#' in [try()]. In addition, all expressions after the first may
#' contain a `.`, which is substituted with the `try-error` object
#' returned by the previous expression.
#' 
#' @note This family of functions behave similarly to the [NVL()] and the [EVL()] families.
#' 
#' @param \dots Expressions to be attempted; for `ERRVL()`, should be
#'   wrapped in [try()].
#' @return The first argument that is not an error. Stops with an
#'   error if all are.
#' @note These functions use lazy evaluation, so, for example
#'   `ERRVL(1, stop("Error!"))` will never evaluate the [stop()] call
#'   and will not produce an error, whereas `ERRVL2(solve(0),
#'   stop("Error!"))` would.
#'
#' @seealso [try()], [inherits()], [tryCatch()]
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
#' }
#'
#' # Capture and print the try-error object:
#' ERRVL(try(solve(0), silent=TRUE),
#'       print(paste0("Stopped with an error: ", .)))
#' @export
ERRVL <- function(...){
  x <- NULL
  for(e in eval(substitute(alist(...)))){ # Lazy evaluate. (See http://adv-r.had.co.nz/Computing-on-the-language.html .)
    x <- eval(if(inherits(x, "try-error")) do.call(substitute, list(e, list(.=x))) else e, parent.frame())
    if(!inherits(x, "try-error")) return(x)
  }
  stop("No non-error expressions passed.")
}

#' @rdname ERRVL
#' @description `ERRVL2()` does *not* require the potentially erring
#'   statements to be wrapped in [try()] and will, in fact, treat them
#'   as non-erring; it does not perform dot substitution.
#'
#' @examples
#'
#' print(ERRVL2(1,2,3)) # 1
#' print(ERRVL2(solve(0),2,3)) # 2
#' print(ERRVL2(1, stop("Error!"))) # No error
#'
#' @export
ERRVL2 <- function(...){
  for(e in eval(substitute(alist(...)))) # Lazy evaluate. (See http://adv-r.had.co.nz/Computing-on-the-language.html .)
    tryCatch(return(eval(e, parent.frame())),
             error = function(err){})
  stop("No non-error expressions passed.")
}

#' @rdname ERRVL
#' @description `ERRVL3()` behaves as `ERRVL2()`, but it does perform
#'   dot-substitution with the [`condition`] object.
#'
#' @examples
#'
#' \dontrun{
#' # Error:
#' ERRVL3(solve(0), stop("Error!"))
#' }
#'
#' # Capture and print the error object:
#' ERRVL3(solve(0), print(paste0("Stopped with an error: ", .)))
#'
#' # Shorthand for tryCatch(expr, error = function(e) e):
#' ERRVL3(solve(0), .)
#'
#' @export
ERRVL3 <- function(...){
  x <- NULL
  for(e in eval(substitute(alist(...)))) # Lazy evaluate. (See http://adv-r.had.co.nz/Computing-on-the-language.html .)
    x <- tryCatch(
      return(eval(if(!is.null(x)) do.call(substitute, list(e, list(.=x)))
                  else e,
                  parent.frame())),
      error = function(err) err)
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
#' @param .p a predicate function of two arguments returning a logical.
#'   Defaults to [identical()].
#' @param .ref integer; index of element of `x` to which all the remaining
#'   ones will be compared. Defaults to 1.
#' @param ... additional arguments passed to `.p()`
#'
#' @return By default `TRUE` if all elements of `x` are identical to each
#'   other, `FALSE` otherwise. In the general case, `all_identical()`
#'   returns `TRUE` if and only if `.p()` returns `TRUE` for all the pairs
#'   involving the first element and the remaining elements.
#'
#' @seealso [identical()], [all.equal()]
#'
#' @examples
#'
#' stopifnot(!all_identical(1:3))
#'
#' stopifnot(all_identical(list("a", "a", "a")))
#' 
#' # Using with `all.equal()` has its quirks 
#' # because of numerical tolerance:
#' x <- seq(
#'   .Machine$double.eps, 
#'   .Machine$double.eps + 1.1 * sqrt(.Machine$double.eps), 
#'   length = 3
#' )
#' # Results with `all.equal()` are affected by ordering
#' all_identical(x, all.equal) # FALSE
#' all_identical(x[c(2,3,1)], all.equal) # TRUE
#' # ... because `all.equal()` is intransitive
#' all_identical(x[-3], all.equal) # is TRUE and
#' all_identical(x[-1], all.equal) # is TRUE, but
#' all_identical(x[-2], all.equal) # is FALSE
#' 
#' @export
all_identical <- function(x, .p = identical, .ref = 1L, ...){
  if(length(x) == 0) return(TRUE)
  stopifnot(is.function(.p))
  stopifnot(length(.ref) == 1)
  v0 <- x[[.ref]]
  for(v in x[- .ref]) if(!isTRUE(.p(v0, v, ...))) return(FALSE)
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
#'   `which` are set to `TRUE`, and whose other elements are set to
#'   `FALSE`.
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

#' Evaluate an \R expression with a hard time limit by forking a process
#'
#' This function uses
#' #ifndef windows
#' [parallel::mcparallel()],
#' #endif
#' #ifdef windows
#' `parallel::mcparallel()`,
#' #endif
#' so the time limit is not
#' enforced on Windows. However, unlike functions using [setTimeLimit()], the time
#' limit is enforced even on native code.
#'
#' @param expr expression to be evaluated.
#' @param timeout number of seconds to wait for the expression to
#'   evaluate.
#' @param unsupported a character vector of length 1 specifying how to
#'   handle a platform that does not support
#' #ifndef windows
#' [parallel::mcparallel()],
#' #endif
#' #ifdef windows
#' `parallel::mcparallel()`,
#' #endif
#'   \describe{
#'
#'   \item{`"warning"` or `"message"`}{Issue a warning or a message,
#'   respectively, then evaluate the expression without the time limit
#'   enforced.}
#'
#'   \item{`"error"`}{Stop with an error.}
#'
#'   \item{`"silent"`}{Evaluate the expression without the time limit
#'   enforced, without any notice.}
#'
#'   } Partial matching is used.
#' @param onTimeout Value to be returned on time-out.
#'
#' @return Result of evaluating `expr` if completed, `onTimeout`
#'   otherwise.
#'
#' @note `onTimeout` can itself be an expression, so it is, for
#'   example, possible to stop with an error by passing
#'   `onTimeout=stop()`.
#'
#' @note Note that this function is not completely transparent:
#'   side-effects may behave in unexpected ways. In particular, RNG
#'   state will not be updated.
#'
#' @examples
#'
#' forkTimeout({Sys.sleep(1); TRUE}, 2) # TRUE
#' forkTimeout({Sys.sleep(1); TRUE}, 0.5) # NULL (except on Windows)
#' @export
forkTimeout <- function(expr, timeout, unsupported = c("warning","error","message","silent"), onTimeout = NULL){
  loadNamespace("parallel")
  loadNamespace("tools")
  env <- parent.frame()
  if(!exists("mcparallel", where=asNamespace("parallel"), mode="function")){ # fork() is not available on the system.
    unsupported <- match.arg(unsupported)
    warnmsg <- "Your platform (probably Windows) does not have fork() capabilities. Time limit will not be enforced."
    errmsg <- "Your platform (probably Windows) does not have fork() capabilities."
    switch(unsupported,
           message = message(warnmsg),
           warning = warning(warnmsg),
           error = stop(errmsg))

    out <- eval(expr, env)
  }else{ # fork() is available on the system.

    child <- parallel::mcparallel(eval(expr, env), mc.interactive=NA)
    out <- parallel::mccollect(child, wait=FALSE, timeout=timeout)

    if(is.null(out)){ # Timed out with no result: kill.
      tools::pskill(child$pid)
      out <- onTimeout
      suppressWarnings(parallel::mccollect(child)) # Clean up.
    }else{
      out <- out[[1L]]
    }

  }
  out
}

#' Extract or replace the *ult*imate (last) element of a vector or a list, or an element counting from the end.
#'
#' @param x a vector or a list.
#' @param i index from the end of the list to extract or replace (where 1 is the last element, 2 is the penultimate element, etc.).
#'
#' @return An element of `x`.
#'
#' @examples
#' x <- 1:5
#' (last <- ult(x))
#' (penultimate <- ult(x, 2)) # 2nd last.
#'
#' \dontshow{
#' stopifnot(last==5)
#' stopifnot(penultimate==4)
#' }
#'
#' @export
ult <- function(x, i=1L){
  x[[length(x)-i+1L]]
}

#' @rdname ult
#'
#' @param value Replacement value for the `i`th element from the end.
#'
#' @note Due to the way in which assigning to a function is
#'   implemented in R, `ult(x) <- e` may be less efficient than
#'   `x[[length(x)]] <- e`.
#' 
#' @examples
#' (ult(x) <- 6)
#' (ult(x, 2) <- 7) # 2nd last.
#' x
#'
#' \dontshow{
#' stopifnot(all(x == c(1:3, 7L, 6L)))
#' }
#'
#' @export
`ult<-` <- function(x, i=1L, value){
  x[[length(x)-i+1L]] <- value
  x
}

#' Evaluate a function once for a given input.
#'
#' This is a `purrr`-style adverb that checks if a given function has
#' already been called with a given configuration of arguments and
#' skips it if it has.
#'
#' @param f A function to modify.
#' @param expire_after The number of seconds since it was added to the
#'   database before a particular configuration is "forgotten". This
#'   can be used to periodically remind the user without overwhelming
#'   them.
#' @param max_entries The number of distinct configurations to
#'   remember. If not `Inf`, *earliest-inserted* configurations will
#'   be removed from the database when capacity is exceeded. (This
#'   exact behavior may change in the future.)
#'
#' @details Each modified function instance returned by `once()`
#'   maintains a database of previous argument configurations. They
#'   are not in any way compressed, so this database may grow over
#'   time. Thus, this wrapper should be used with caution if arguments
#'   are large objects. This may be replaced with hashing in the
#'   future. In the meantime, you may want to set the `max_entries`
#'   argument to be safe.
#'
#'   Different instances of a modified function do not share
#'   databases, even if the function is the same. This means that if
#'   you, say, modify a function within another function, the modified
#'   function will call once per call to the outer function. Modified
#'   functions defined at package level count as the same "instance",
#'   however. See example.
#'
#' @note Because the function needs to test whether a particular
#'   configuration of arguments have already been used, do not rely on
#'   lazy evaluation behaviour.
#'
#' @examples
#' msg <- once(message)
#' msg("abc") # Prints.
#' msg("abc") # Silent.
#'
#' msg <- once(message) # Starts over.
#' msg("abc") # Prints.
#'
#' f <- function(){
#'   innermsg  <- once(message)
#'   innermsg("efg") # Prints once per call to f().
#'   innermsg("efg") # Silent.
#'   msg("abcd") # Prints only the first time f() is called.
#'   msg("abcd") # Silent.
#' }
#' f() # Prints "efg" and "abcd".
#' f() # Prints only "efg".
#'
#' msg3 <- once(message, max_entries=3)
#' msg3("a") # 1 remembered.
#' msg3("a") # Silent.
#' msg3("b") # 2 remembered.
#' msg3("a") # Silent.
#' msg3("c") # 3 remembered.
#' msg3("a") # Silent.
#' msg3("d") # "a" forgotten.
#' msg3("a") # Printed.
#'
#' msg2s <- once(message, expire_after=2)
#' msg2s("abc") # Prints.
#' msg2s("abc") # Silent.
#' Sys.sleep(1)
#' msg2s("abc") # Silent after 1 sec.
#' Sys.sleep(1.1)
#' msg2s("abc") # Prints after 2.1 sec.
#'
#' @export
once <- function(f, expire_after=Inf, max_entries=Inf){
  local({
    prev <- list()
    prev.time <- c()
    function(...){
      # If using expire_after, expire old entries.
      if(is.finite(expire_after)){
        expired <- Sys.time() - prev.time > expire_after
        prev <<- prev[!expired]
        prev.time <<- prev.time[!expired]
      }
      sig <- list(...)
      if(! list(sig)%in%prev){
        prev <<- c(prev, list(sig))
        prev.time <<- c(prev.time, Sys.time())
        if(length(prev) > max_entries){
          prev <<- prev[-1]
          prev.time <<- prev.time[-1]
        }
        f(...)
      }
    }
  })
}

#' Evaluate an expression, restarting on error
#'
#' A pair of functions paralleling [eval()] and [evalq()] that make
#' multiple attempts at evaluating an expression, retrying on error up
#' to a specified number of attempts, and optionally evaluating
#' another expression before restarting.
#'
#' @param expr an expression to be retried; note the difference
#'   between [eval()] and [evalq()].
#' @param retries number of retries to make; defaults to
#'   `"eval.retries"` option, or 5.
#' @param beforeRetry if given, an expression that will be evaluated
#'   before each retry if the initial attempt fails; it is evaluated
#'   in the same environment and with the same quoting semantics as
#'   `expr`, but its errors are not handled.
#' @param envir,enclos see [eval()].
#' @param verbose Whether to output retries.
#'
#' @note If `expr` returns a `"try-error"` object (returned by
#'   [try()]), it will be treated as an error. This behavior may
#'   change in the future.
#'
#' @return Results of evaluating `expr`, including side-effects such
#'   as variable assignments, if successful in `retries` retries.
#'
#' @examples
#' x <- 0
#' persistEvalQ({if((x<-x+1)<3) stop("x < 3") else x},
#'              beforeRetry = {cat("Will try incrementing...\n")})
#'
#' x <- 0
#' e <- quote(if((x<-x+1)<3) stop("x < 3") else x)
#' persistEval(e,
#'             beforeRetry = quote(cat("Will try incrementing...\n")))
#' @export
persistEval <- function(expr, retries=NVL(getOption("eval.retries"), 5), beforeRetry,
                        envir = parent.frame(),
                        enclos = if (is.list(envir) ||
                                     is.pairlist(envir)) parent.frame() else baseenv(), verbose=FALSE){
  for(attempt in seq_len(retries)){
    out <- try(eval(expr, envir=envir, enclos=enclos), silent=TRUE)
    #' @importFrom methods is
    if(!is(out, "try-error")) return(out)
    else{
      if(!missing(beforeRetry)) eval(beforeRetry, envir=envir, enclos=enclos)
      if(verbose) message("Retrying: retry ", attempt, ".")
    }
  }
  out <- eval(expr, envir=envir, enclos=enclos)
}

#' @rdname persistEval
#' @export
persistEvalQ <- function(expr, retries=NVL(getOption("eval.retries"), 5), beforeRetry,
                         envir = parent.frame(),
                         enclos = if (is.list(envir) ||
                                      is.pairlist(envir)) parent.frame() else baseenv(), verbose=FALSE){
  expr <- substitute(expr)
  beforeRetry <- substitute(beforeRetry)
  envir <- force(envir)
  enclos <- force(enclos)

  persistEval(expr=expr, retries=retries, beforeRetry=beforeRetry, envir=envir, enclos=enclos, verbose=verbose)
}

#' Truncate values of high magnitude in a vector.
#'
#' @param x a numeric or integer vector.
#' @param replace a number or a string `"maxint"` or `"intmax"`.
#'
#' @return Returns `x` with elements whose magnitudes exceed `replace`
#'   replaced replaced by `replace` (or its negation). If `replace` is
#'   `"maxint"` or `"intmax"`, `.Machine$integer.max` is used instead.
#'
#' `NA` and `NAN` values are preserved.
#'
#' @export
deInf <- function(x, replace=1/.Machine$double.eps){
  NVL(x) <- integer(0)
  if(tolower(replace) %in% c("maxint","intmax")) replace <- .Machine$integer.max
  ifelse(is.nan(x) | abs(x)<replace, x, sign(x)*replace)
}

#' A [split()] method for [`array`] and [`matrix`] types on a margin.
#'
#' These methods split an [`array`] and [`matrix`] into a list of
#' arrays or matrices with the same number of dimensions
#' according to the specified margin.
#'
#' @param x A [`matrix`] or an [`array`].
#' @param f,drop See help for [split()]. Note that `drop` here is
#'   *not* for array dimensions: these are always preserved.
#' @param margin Which margin of the array to split along. `NULL`
#'   splits as [`split.default`], dropping dimensions.
#' @param ... Additional arguments to [split()].
#'
#' @examples
#'
#' x <- diag(5)
#' f <- rep(1:2, c(2,3))
#' split(x, f, margin=1) # Split rows.
#' split(x, f, margin=2) # Split columns.
#'
#' # This is similar to how data frames are split:
#' stopifnot(identical(split(x, f, margin=1),
#'           lapply(lapply(split(as.data.frame(x), f), as.matrix), unname)))
#'
#' @export
split.array <- function(x, f, drop = FALSE, margin = NULL, ...){
  if(is.null(margin)) return(NextMethod("split"))
  d <- dim(x)
  margin <- as.integer(margin)
  if(margin < 1L || margin > length(d)) stop(sQuote("margin"), " must be between 1 and the dimensionality of ", sQuote("x"), ".")

  args <- c(list(x), rep(TRUE, length(d)), list(drop=FALSE))
  ind_call <- function(ind){
    args[[margin+1L]] <- ind
    do.call(`[`, args)
  }
  lapply(split(x = seq_len(dim(x)[margin]), f = f, drop = drop, ...), ind_call)
}

#' @rdname split.array
#' @export
split.matrix <- split.array

#' Convert a list to an atomic vector if it consists solely of atomic elements of length 1.
#'
#' This behaviour is not dissimilar to that of [simplify2array()], but
#' it offers more robust handling of empty or NULL elements and never
#' promotes to a matrix or an array, making it suitable to be a column
#' of a [`data.frame`].
#'
#' @param x an R [`list`] to be simplified.
#' @param toNA a character string indicating whether `NULL` entries
#'   (if `"null"`) or 0-length entries including `NULL` (if `"empty"`)
#'   should be replaced with `NA`s before attempting conversion;
#'   specifying `keep` or `FALSE` leaves them alone (typically
#'   preventing conversion).
#' @param empty a character string indicating how empty lists should
#'   be handled: either `"keep"`, in which case they are unchanged or
#'   `"unlist"`, in which cases they are unlisted (typically to
#'   `NULL`).
#' @param ... additional arguments passed to [unlist()].
#'
#' @return an atomic vector or a list of the same length as `x`.
#' @examples
#'
#' (x <- as.list(1:5))
#' stopifnot(identical(simplify_simple(x), 1:5))
#'
#' x[3] <- list(NULL) # Put a NULL in place of 3.
#' x
#' stopifnot(identical(simplify_simple(x, FALSE), x)) # Can't be simplified without replacing the NULL.
#'
#' stopifnot(identical(simplify_simple(x), c(1L,2L,NA,4L,5L))) # NULL replaced by NA and simplified.
#'
#' x[[3]] <- integer(0)
#' x
#' stopifnot(identical(simplify_simple(x), x)) # A 0-length vector is not replaced by default,
#' stopifnot(identical(simplify_simple(x, "empty"), c(1L,2L,NA,4L,5L))) # but can be.
#'
#' (x <- lapply(1:5, function(i) c(i,i+1L))) # Elements are vectors of equal length.
#' simplify2array(x) # simplify2array() creates a matrix,
#' stopifnot(identical(simplify_simple(x), x)) # but simplify_simple() returns a list.
#'
#' @export
simplify_simple <- function(x, toNA = c("null","empty","keep"), empty = c("keep", "unlist"), ...){
  if(isFALSE(toNA)) toNA <- "keep"
  toNA <- match.arg(toNA)
  empty <- match.arg(empty)

  if(is.atomic(x)) return(x)

  x <- switch(toNA,
              keep = x,
              null = lapply(x, NVL, NA),
              empty = lapply(x, EVL, NA))

  if(length(x)==0) switch(empty, keep=x, unlist=unlist(x, recursive=FALSE, ...))
  else if(all(lengths(x)==1L) && all(vapply(x, is.atomic, logical(1)))) unlist(x, recursive=FALSE, ...)
  else x
}

#' A wrapper for base::attr which defaults to exact matching.
#'
#' @param x,which,exact as in \code{base::attr}, but with \code{exact}
#'   defaulting to \code{TRUE} in this implementation
#'
#' @return as in \code{base::attr}
#' @examples
#'
#' x <- list()
#' attr(x, "name") <- 10
#'
#' base::attr(x, "n")
#'
#' stopifnot(is.null(attr(x, "n")))
#'
#' base::attr(x, "n", exact = TRUE)
#' @export
attr <- function(x, which, exact = TRUE) {
  base::attr(x, which, exact)
}

#' An error handler for [rlang::check_dots_used()] that issues a
#' warning that only lists argument names.
#'
#' This handler parses the error message produced by
#' [rlang::check_dots_used()], extracting the names of the unused
#' arguments, and formats them into a more gentle warning message. It
#' relies on \CRANpkg{rlang} maintaining its current format.
#'
#' @param e a [condition][condition] object, typically not passed by
#'   the end-user; see example below.
#'
#' @examples
#'
#' \dontshow{
#' o <- options(warn=1, useFancyQuotes=FALSE)
#' }
#'
#' g <- function(b=NULL, ...){
#'   invisible(force(b))
#' }
#'
#' f <- function(...){
#'   rlang::check_dots_used(error = unused_dots_warning)
#'   g(...)
#' }
#'
#' f() # OK
#' f(b=2) # OK
#' f(a=1, b=2, c=3) # Warning about a and c but not about b
#'
#' \dontshow{
#' # Test:
#' stopifnot(grepl("Argument(s) 'a' and 'c' were not recognized or used. Did you mistype an argument name?", tryCatch(f(a=1, b=2, c=3), warning = function(e) e$message), fixed=TRUE))
#' options(o)
#' }
#' @export
unused_dots_warning <- function(e){
  v <- lapply(parse(text = e$body[names(e$body)=="*"]), `[[`, 2)
  rlang::warn(sprintf("Argument(s) %s were not recognized or used. Did you mistype an argument name?",
               paste.and(sQuote(v))))
}

#' Modify the argument in the calling environment of the calling function
#'
#' This is a helper function that enables a function to modify its argument in place, emulating behavior of \CRANpkg{R6} classes and methods in the \CRANpkg{network}. It should typically be the last line of the calling function.
#'
#' This function determines whether the argument can be assigned to by actually attempting to do so. If this results in an error, for example, because the argument is anonymous, the error is silently ignored.
#'
#' It can be called multiple times by the same function to modify multiple arguments. It uses the [on.exit()] mechanism, adding to the list. Thus, if some other function calls `on.exit(..., add = FALSE)` (the default) afterwards, `modify_in_place()` will fail silently.
#'
#' @param x the argument (not its name!) to be modified
#' @param value the value to assign (defaulting to the current value of `x`)
#'
#' @return `value`, invisibly, while attempting to modify `x` in place
#'
#' @examples
#' ## A function that increments its argument in place:
#' inc <- function(x){
#'   modify_in_place(x, x+1)
#' }
#'
#' y <- 1
#' z <- 1
#'
#' stopifnot(inc(z) == 2)
#' stopifnot(z == 2)
#' stopifnot(inc(y) == 2)
#' stopifnot(y == 2)
#' stopifnot(inc(z) == 3)
#' stopifnot(z == 3)
#'
#' stopifnot(inc(identity(z)) == 4)
#' stopifnot(z == 3) # Not updated!
#'
#' ## Modify an argument that's been updated in place:
#' inc2 <- function(y){
#'   y <- y + 1
#'   modify_in_place(y)
#' }
#'
#' z
#' stopifnot(inc2(z) == 4)
#' stopifnot(z == 4)
#'
#' ## Decrement the first argument, increment the second:
#' incdec <- function(x,y){
#'   modify_in_place(x, x-1)
#'   modify_in_place(y, y+1)
#' }
#'
#' c(y,z)
#' incdec(y,z)
#' stopifnot(all(c(y,z) == c(1,5)))
#' @export
modify_in_place <- function(x, value = x){
  xn <- substitute(x) # Grab the name of the argument to be updated.
  xnn <- match.call(sys.function(-1), sys.call(-1))[[xn]] # Grab the expression that was passed into its argument.

  eval.parent(on.exit( # As the calling function exits...
    tryCatch( # try to...
    eval.parent(call("<-", xnn, value), n = 2), # Assign `value` to whatever `xnn` stands for in the caller's calling environment.
    error = identity # and do nothing if it fails.
    ), add = TRUE))

  invisible(value) # Return invisibly.
}

#' Replace values in a vector according to functions
#'
#' This is a thin wrapper around [base::replace()] that allows `list`
#' and/or `values` to be functions that are evaluated on `x` to obtain
#' the replacement indices and values. The assignment version replaces
#' `x`.
#'
#' `list` function is passed the whole vector `x` at once (not
#' elementwise) and any additional arguments to `replace()`, and must
#' return an indexing vector (numeric, logical, character,
#' etc.). `values`/`value` function is passed `x` after subsetting it by the
#' result of calling `list()`.
#'
#' If passing named arguments, `x`, `list`, and `values` may cause a
#' conflict.
#'
#' @param x a vector.
#' @param list either an index vector or a function (*not* a function
#'   name).
#' @param values,value either a vector of replacement values or a function
#'   (*not* a function name).
#' @param ... additional arguments to `list` if it is a function;
#'   otherwise ignored.
#'
#' @return A vector with the values replaced.
#'
#' @seealso [purrr::modify()] family of functions.
#'
#' @examples
#'
#' (x <- rnorm(10))
#'
#' ### Replace elements of x that are < 1/4 with 0.
#'
#' # Note that this code is pipeable.
#' x |> replace(`<`, 0, 1/4)
#' # More readable, using lambda notation.
#' x |> replace(\(.x) .x < 1/4, 0)
#' # base equivalent.
#' stopifnot(identical(replace(x, `<`, 0, 1/4),
#'                     base::replace(x, x < 1/4, 0)))
#'
#' ### Multiply negative elements of x by 1i.
#'
#' x |> replace(\(.x) .x < 0, \(.x) .x * 1i)
#' stopifnot(identical(replace(x, \(.x) .x < 0, \(.x) .x * 1i),
#'                     base::replace(x, x < 0, x[x < 0] * 1i)))
#'
#' ### Modify the list in place.
#'
#' y <- x
#' replace(x, `<`, 1/4) <- 0
#' x
#' stopifnot(identical(x, replace(y, `<`, 0, 1/4)))
#'
#' @export
replace <- function(x, list, values, ...) {
  if (is.function(list)) list <- list(x, ...)
  if (is.function(values)) values <- values(x[list], ...)
  base::replace(x, list, values)
}

#' @rdname replace
#' @export
`replace<-` <- function(x, list, ..., value) replace(x, list, value, ...)
