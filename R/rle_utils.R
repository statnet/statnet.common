.check_lengths <- function(rle1, rle2){
  if(sum(rle1$lengths)!=sum(rle2$lengths))
    stop("At this time, binary rle operators require the vectors represented by the encoding to have equal lengths.")
}

#' RLE utilities
#'
#' Simple utilities for operations on RLE-encoded vectors.
#'
#' @param x,e1,e2 Arguments to unary (`x`) and binary (`e1` and `e2`)
#'   operators.
#'
#' @param FUN A binary function or operator or a name of one. It is
#'   assumed to be vectorized: it expects two vectors of equal lengths
#'   and outputs a vector of the same length.
#' 
#' @param ... Objects to be concatenated. The first object must be of
#'   class [rle()].
#' 
#' @name rle.utils
#'
#' @return All functions return an [rle()] object. By default, the
#'   functions and the operators do not merge adjacent runs with
#'   the same value. This must be done explicitly with [compact.rle()].
#' 
#' @examples
#'
#' x <- rle(as.logical(rbinom(10,1,.7)))
#' y <- rle(as.logical(rbinom(10,1,.3)))
#'
NULL

#' @rdname rle.utils
#'
#' @examples
#' stopifnot(c(inverse.rle(x),inverse.rle(y))==inverse.rle(c(x,y)))
#' 
#' @importFrom methods is
#' @export
c.rle <- function(...){
  l <- list(...)
  o <- l[[1]]
  for(x in l[-1]){
    if(!is(x, "rle")) x <- rle(x)
    o$lengths <- c(o$lengths, x$lengths)
    o$values <- c(o$values, x$values)
  }
  o
}

#' @rdname rle.utils
#' @examples
#' stopifnot((!inverse.rle(x))==inverse.rle(!x))
#' @export
`!.rle` <- function(x){
  x$values <- !x$values
  x
}

#' @describeIn rle.utils
#'
#' Perform an arbitrary binary operation on the pair of vectors
#' represented by the [rle()] objects.
#' 
#' @export
binop.rle <- function(e1, e2, FUN){
  .check_lengths(e1, e2)
  f <- match.fun(FUN)
  if(!is(e2, "rle")) e2 <- rle(e2)
  syncinfo <- .Call("sync_RLEs", e1$lengths, e2$lengths)
  structure(list(lengths = syncinfo$lengths[seq_len(syncinfo$nruns)],
                 values = FUN(e1$values[syncinfo$val1i[seq_len(syncinfo$nruns)]],
                              e2$values[syncinfo$val2i[seq_len(syncinfo$nruns)]])),
            class="rle")
}

#' @rdname rle.utils
#' @examples
#' stopifnot((inverse.rle(x)|inverse.rle(y))==inverse.rle(x|y))
#' @export
`|.rle` <- function(e1, e2){
  binop.rle(e1, e2, `|`)
}

#' @rdname rle.utils
#' @examples
#' stopifnot((inverse.rle(x)&inverse.rle(y))==inverse.rle(x&y))
#' @export
`&.rle` <- function(e1, e2){
  binop.rle(e1, e2, `&`)
}

#' @describeIn rle.utils
#'
#' Compact the [rle()] object by merging adjacent runs.
#'
#' @note [compact.rle()] may not be safe if the combined run lenth
#'   exceeds the maximum value representable by a 32-bit signed
#'   integer (\Sexpr{.Machine$integer.max}).
#' 
#' @examples
#' stopifnot(identical(rle(inverse.rle(x)&inverse.rle(y)),compact.rle(x&y)))
#' @export
compact.rle <- function(x){
  tmp <- rle(x$values)
  l <- tmp$lengths
  v <- tmp$values
  i <- rep(seq_along(v), l)
  l <- as.vector(tapply(x$lengths, list(i), sum))
  structure(list(lengths = l,
                 values = v),
            class = "rle")
}
