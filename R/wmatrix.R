#  File R/wmatrix.R in package statnet.common, part of the Statnet suite
#  of packages for network analysis, http://statnet.org .
#
#  This software is distributed under the GPL-3 license.  It is free,
#  open source, and has the attribution requirements (GPL Section 7) at
#  http://statnet.org/attribution
#
#  Copyright 2007-2018 Statnet Commons
#######################################################################
#' A data matrix with row weights
#'
#' A representation of a numeric matrix with row weights, represented
#' on either linear (`linwmatrix`) or logarithmic (`logwmatrix`)
#' scale.
#'
#' @param x an object to be coerced or tested.
#' @param data,nrow,ncol,byrow,dimnames passed to [`matrix`].
#' @param w row weights on the appropriate scale.
#' @param target.nrows see [`decompress_rows`].
#' @param i,j,value rows and columns and values for extraction or
#'   replacement; as [`matrix`].
#' @param drop Used for consistency with the generic. Ignored, and
#'   always treated as `FALSE`.
#' @param ... extra arguments, currently unused.
#'
#' @note Note that `wmatrix` itself is an "abstract" class: you cannot
#'   instantiate it.
#'
#' @note Note that at this time, `wmatrix` is designed as, first and
#'   foremost, as class for storing compressed data matrices, so most
#'   methods that operate on matrices may not handle the weights
#'   correctly and may even cause them to be lost.
#' 
#' @return An object of class `linwmatrix`/`logwmatrix` and `wmatrix`,
#'   which is a [`matrix`] but also has an attribute `w` containing
#'   row weights on the linear or the natural-log-transformed scale.
#'
#' @seealso [`rowweights`], [`lrowweights`], [`compress_rows`]
#' 
#' @name wmatrix
#'
#' @examples
#' (m <- matrix(1:3, 2, 3, byrow=TRUE))
#' (m <- rbind(m, 3*m, 2*m, m))
#' (mlog <- as.logwmatrix(m))
#' (mlin <- as.linwmatrix(m))
#' (cmlog <- compress_rows(mlog))
#' (cmlin <- compress_rows(mlin))
#'
#' stopifnot(all.equal(as.linwmatrix(cmlog),cmlin))
#'
#' cmlog[2,] <- 1:3
#' (cmlog <- compress_rows(cmlog))
#' stopifnot(sum(rowweights(cmlog))==nrow(m))
#'
#' (m3 <- matrix(c(1:3,(1:3)*2,(1:3)*3), 3, 3, byrow=TRUE))
#' (rowweights(m3) <- c(4, 2, 2))
#'
#' stopifnot(all.equal(compress_rows(as.logwmatrix(m)), as.logwmatrix(m3),check.attributes=FALSE))
#' stopifnot(all.equal(rowweights(compress_rows(as.logwmatrix(m))),
#'                     rowweights(as.logwmatrix(m3)),check.attributes=FALSE))
NULL

#' @rdname wmatrix
#' @export
logwmatrix <- function(data = NA, nrow = 1, ncol = 1, byrow = FALSE, dimnames = NULL, w = NULL){
  x <- matrix(data, nrow, ncol, byrow, dimnames)
  as.logwmatrix(x, w)
}

#' @rdname wmatrix
#' @export
linwmatrix <- function(data = NA, nrow = 1, ncol = 1, byrow = FALSE, dimnames = NULL, w = NULL){
  x <- matrix(data, nrow, ncol, byrow, dimnames)
  as.linwmatrix(x, w)
}

#' @rdname wmatrix
#' @export
is.wmatrix <- function(x) inherits(x, "wmatrix")
#' @rdname wmatrix
#' @export
is.logwmatrix <- function(x) inherits(x, "logwmatrix")
#' @rdname wmatrix
#' @export
is.linwmatrix <- function(x) inherits(x, "linwmatrix")

#' @rdname wmatrix
#' @export
as.linwmatrix <- function(x, ...) UseMethod("as.linwmatrix")
#' @rdname wmatrix
#' @export
as.logwmatrix <- function(x, ...) UseMethod("as.logwmatrix")

#' @rdname wmatrix
#' @export
as.linwmatrix.linwmatrix <- function(x, ...) x
#' @rdname wmatrix
#' @export
as.linwmatrix.logwmatrix <- function(x, ...){
  attr(x, "w") <- exp(attr(x, "w"))
  class(x)[class(x)=="logwmatrix"] <- "linwmatrix" 
  x
}

#' @rdname wmatrix
#' @export
as.logwmatrix.logwmatrix <- function(x, ...) x
#' @rdname wmatrix
#' @export
as.logwmatrix.linwmatrix <- function(x, ...){
  attr(x, "w") <- log(attr(x, "w"))
  class(x)[class(x)=="linwmatrix"] <- "logwmatrix" 
  x
}

#' @rdname wmatrix
#' @export
as.linwmatrix.matrix <- function(x, w=NULL, ...){
  attr(x, "w") <- NVL(w, rep(1, nrow(x)))
  class(x) <- c("linwmatrix", "wmatrix", class(x))
  x
}
#' @rdname wmatrix
#' @export
as.logwmatrix.matrix <- function(x, w=NULL, ...){
  attr(x, "w") <- NVL(w, rep(0, nrow(x)))
  class(x) <- c("logwmatrix", "wmatrix", class(x))
  x
}

#' @rdname wmatrix
#' @export
print.wmatrix <- function(x, ...){
  x <- cbind(unclass(x), Weight = attr(x, "w"))
  print(x, ...)
}

#' @rdname wmatrix
#' @export
print.logwmatrix <- function(x, ...){
  cat("A row-weighted matrix with natural-log-scaled weights:\n")
  NextMethod("print")
}

#' @rdname wmatrix
#' @export
print.linwmatrix <- function(x, ...){
  cat("A row-weighted matrix with linear-scaled weights:\n")
  NextMethod("print")
}

#' Set or extract weighted matrix row weights
#'
#' @param x a [`linwmatrix`], a [`logwmatrix`], or a [`matrix`]; a
#'   [`matrix`] is coerced to a weighted matrix of an appropriate
#'   type.
#' @param value weights to set, on the appropriate scale.
#' @param update if `TRUE` (the default), the old weights are updated
#'   with the new weights (i.e., corresponding weights are multiplied
#'   on linear scale or added on on log scale); otherwise, they are
#'   overwritten.
#' @param ... extra arguments for methods.
#'
#' @return For the accessor functions, the row weights or the row
#'   log-weights; otherwise, a weighted matrix with modified
#'   weights. The type of weight (linear or logarithmic) is converted
#'   to the required type and the type of weighting of the matrix is
#'   preserved.
#' 
#' @name wmatrix_weights
NULL

#' @rdname wmatrix_weights
#' @export
rowweights <- function(x, ...) UseMethod("rowweights")
#' @rdname wmatrix_weights
#' @export
rowweights.linwmatrix <- function(x, ...) attr(x, "w")
#' @rdname wmatrix_weights
#' @export
rowweights.logwmatrix <- function(x, ...) exp(attr(x, "w"))

#' @rdname wmatrix_weights
#' @export
lrowweights <- function(x, ...) UseMethod("lrowweights")
#' @rdname wmatrix_weights
#' @export
lrowweights.logwmatrix <- function(x, ...) attr(x, "w")
#' @rdname wmatrix_weights
#' @export
lrowweights.linwmatrix <- function(x, ...) log(attr(x, "w"))

#' @rdname wmatrix_weights
#' @export
`rowweights<-` <- function(x, ..., value) UseMethod("rowweights<-")
#' @rdname wmatrix_weights
#' @export
`rowweights<-.linwmatrix` <- function(x, update=TRUE, ..., value){
  attr(x, "w") <- value * if(update) attr(x, "w") else 1
  x
}
#' @rdname wmatrix_weights
#' @export
`rowweights<-.logwmatrix` <- function(x, update=TRUE,..., value){
  attr(x, "w") <- log(value) + if(update) log(attr(x, "w")) else 0
  x
}

#' @rdname wmatrix_weights
#' @export
`lrowweights<-` <- function(x, ..., value) UseMethod("lrowweights<-")
#' @rdname wmatrix_weights
#' @export
`lrowweights<-.linwmatrix` <- function(x, update=TRUE, ..., value){
  attr(x, "w") <- exp(value + if(update) log(attr(x, "w")) else 0)
  x
}
#' @rdname wmatrix_weights
#' @export
`lrowweights<-.logwmatrix` <- function(x, update=TRUE,..., value){
  attr(x, "w") <- value + if(update) attr(x, "w") else 0
  x
}

#' @rdname wmatrix_weights
#' @export
`rowweights<-.matrix` <- function(x, ..., value){
  attr(x, "w") <- value
  class(x) <- c("linwmatrix", "wmatrix", class(x))
  x
}

#' @rdname wmatrix_weights
#' @export
`lrowweights<-.matrix` <- function(x, ..., value){
  attr(x, "w") <- value
  class(x) <- c("logwmatrix", "wmatrix", class(x))
  x
} 



#' A generic function to compress a row-weighted table
#' 
#' Compress a matrix or a data frame with duplicated rows, updating row weights
#' to reflect frequencies, or reverse the process, reconstructing a matrix like
#' the one compressed (subject to permutation of rows and weights not adding up
#' to an integer).
#' 
#' @param x a weighted matrix or data frame.
#' @param ... extra arguments for methods.
#' @param target.nrows the approximate number of rows the uncompressed matrix
#' should have; if not achievable exactly while respecting proportionality, a
#' matrix with a slightly different number of rows will be constructed.
#' @return For \code{compress_rows} A weighted matrix or data frame of the same
#' type with duplicated rows removed and weights updated appropriately.
#' @export
compress_rows <- function(x, ...) UseMethod("compress_rows")

#' @rdname wmatrix
#' @export
compress_rows.logwmatrix <- function(x, ...){
  o <- order.matrix(x)
  x <- x[o, , drop=FALSE]
  firsts <- !duplicated(x)
  groups <- cumsum(firsts)
  cx <- x[firsts, , drop=FALSE]
  attr(cx, "w") <- c(tapply(attr(x, "w"), list(groups), log_sum_exp), use.names=FALSE)

  cx
}

#' @rdname wmatrix
#' @export
compress_rows.linwmatrix <- function(x, ...){
  o <- order.matrix(x)
  x <- x[o, , drop=FALSE]
  firsts <- !duplicated(x)
  groups <- cumsum(firsts)
  cx<-x[firsts, , drop=FALSE]
  attr(cx, "w") <- c(tapply(attr(x, "w"), list(groups), sum), use.names=FALSE)

  cx
}

#' @rdname compress_rows
#' @export
decompress_rows <- function(x, target.nrows=NULL, ...) UseMethod("decompress_rows")

#' @rdname wmatrix
#' @export
decompress_rows.wmatrix <- function(x, target.nrows=NULL, ...){
  w <- rowweights(x)
  if(is.null(target.nrows)) target.nrows <- sum(w) 
  n <- round(w/sum(w)*target.nrows) # Number of replications of each row

  rowweights(x) <- 1/n
  x[rep(seq_along(n), n),]
}

#' @rdname wmatrix
#' @export
`[.wmatrix` <- function(x, i, j, ..., drop=FALSE){
  if(drop) warning("Row-weighted matrices cannot drop dimensions.")
  o <- unclass(x)[i,j,...,drop=FALSE]
  attr(o, "w") <- attr(x, "w")[i]
  class(o) <- class(x)
  o
}

#' @rdname wmatrix
#' @export
`[<-.wmatrix` <- function(x, i, j, ..., value){
  o <- unclass(x)
  o[i,j,...] <- value
  attr(o, "w") <- attr(x, "w")
  class(o) <- class(x)
  o
}
