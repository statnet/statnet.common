#  File R/rle_utils.R in package statnet.common, part of the Statnet suite
#  of packages for network analysis, http://statnet.org .
#
#  This software is distributed under the GPL-3 license.  It is free,
#  open source, and has the attribution requirements (GPL Section 7) at
#  http://statnet.org/attribution
#
#  Copyright 2007-2018 Statnet Commons
#######################################################################
.check_lengths <- function(rle1, rle2){
  if(sum(as.numeric(rle1$lengths))!=sum(as.numeric(rle2$lengths)))
    stop("At this time, binary rle operators require the vectors represented by the encoding to have equal lengths.")
}

#' Safe multiplication of integer run lengths.
#'
#' Return a vector of run lengths each no larger than maximum
#' representable integer that sum to the product of the arguments. If
#' the product is 0, an empty integer vector is returned.
#'
#' @param e1,e2 arguments to multiply, both `<=.Machine$integer.max`.
#' 
#' @noRd
.run_mul <- function(e1, e2){
  o <- suppressWarnings(as.integer(e1)*as.integer(e2))
  if(is.na(o)){ # Integer overflow.
    do <- as.numeric(e1)*as.numeric(e2)
    c(as.integer(rep.int(.Machine$integer.max, do %/% .Machine$integer.max)), as.integer(do %% .Machine$integer.max))
  }else if(o==0){
    integer(0)
  }else o
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
#' @param ... For `c`, objects to be concatenated. The first object
#'   must be of class [`rle`]. For `rep`, see documentation for
#'   [`rep`]. For `sum`, objects to be summed.
#' 
#' @name rle.utils
#'
#' @return Unless otherwise stated, all functions return an [`rle`]
#'   object. By default, the functions and the operators do not merge
#'   adjacent runs with the same value. This must be done explicitly
#'   with [`compact.rle`].
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
#' stopifnot(isTRUE(all.equal(c(inverse.rle(x),inverse.rle(y)),inverse.rle(c(x,y)))))
#' 
#' @export
c.rle <- function(...){
  l <- list(...)
  o <- l[[1]]
  # This might be suboptimal.
  for(x in l[-1]){
    x <- as.rle(x)
    o$lengths <- c(o$lengths, x$lengths)
    o$values <- c(o$values, x$values)
  }
  o
}

#' @rdname rle.utils
#' @examples
#' stopifnot(isTRUE(all.equal((!inverse.rle(x)),inverse.rle(!x))))
#' @export
`!.rle` <- function(x){
  x$values <- !x$values
  x
}

#' @describeIn rle.utils
#'
#' Perform an arbitrary binary operation on the pair of vectors
#' represented by the [`rle`] objects.
#' 
#' @export
binop.rle <- function(e1, e2, FUN){
  .check_lengths(e1, e2)
  f <- match.fun(FUN)
  e2 <- as.rle(e2)
  syncinfo <- .Call("sync_RLEs", e1$lengths, e2$lengths)
  structure(list(lengths = syncinfo$lengths[seq_len(syncinfo$nruns)],
                 values = FUN(e1$values[syncinfo$val1i[seq_len(syncinfo$nruns)]],
                              e2$values[syncinfo$val2i[seq_len(syncinfo$nruns)]])),
            class="rle")
}

#' @rdname rle.utils
#' @examples
#' stopifnot(isTRUE(all.equal((inverse.rle(x)|inverse.rle(y)),inverse.rle(x|y))))
#' @export
`|.rle` <- function(e1, e2){
  binop.rle(e1, e2, `|`)
}

#' @rdname rle.utils
#' @examples
#' stopifnot(isTRUE(all.equal((inverse.rle(x)&inverse.rle(y)),inverse.rle(x&y))))
#' @export
`&.rle` <- function(e1, e2){
  binop.rle(e1, e2, `&`)
}

#' @describeIn rle.utils
#'
#' Compact the [`rle`] object by merging adjacent runs.
#'
#' @note Since [`rle`] stores run lengths as integers, [`compact.rle`]
#'   will not merge runs that add up to lengths greater than what can
#'   be represented by a 32-bit signed integer
#'   (\Sexpr{.Machine$integer.max}).
#' 
#' @examples
#' stopifnot(identical(rle(inverse.rle(x)&inverse.rle(y)),compact.rle(x&y)))
#'
#' big <- structure(list(lengths=as.integer(rep(.Machine$integer.max/4,6)),
#'                       values=rep(TRUE,6)), class="rle")
#'
#' stopifnot(all(aggregate(as.numeric(lengths)~values,
#'                         data=as.data.frame(unclass(big)),FUN=sum)
#'               ==
#'               aggregate(as.numeric(lengths)~values,
#'                         data=as.data.frame(unclass(compact.rle(big))),
#'                         FUN=sum)))
#' @export
compact.rle <- function(x){
  # First, strip the 0-length runs.
  x$values <- x$values[x$lengths!=0]
  x$lengths <- x$lengths[x$lengths!=0]
  # Second, code distinct values as integers.
  vf <- as.integer(as.factor(x$values))
  vf[is.na(vf)] <- 0L # NA runs get coded 0.
  # Third, call the C code to produce the mapping onto the compacted vector.
  compinfo <- .Call("compact_RLE", x$lengths, vf)
  # Lastly, rebuild the rle with the combined lengths and remapped values.
  structure(list(lengths = compinfo$lengths[seq_len(compinfo$nruns)],
                 values = x$values[compinfo$vali[seq_len(compinfo$nruns)]]),
            class = "rle")
}

#' @rdname rle.utils
#' 
#' @param na.rm see documentation for [`any`], [`all`], and [`sum`].
#'
#' @return [`any`], [`all`], [`sum`], and [`length`] return logical, logical, numeric, and numeric vectors, respectively.
#' 
#' @examples
#'
#' x <- rle(as.logical(rbinom(10,1,.9)))
#' y <- rle(as.logical(rbinom(10,1,.1)))
#' 
#' stopifnot(isTRUE(all.equal(any(x),any(inverse.rle(x)))))
#' stopifnot(isTRUE(all.equal(any(y),any(inverse.rle(y)))))
#' 
#' @export
any.rle <- function(..., na.rm = FALSE){
  inl <- list(...)
  inl <- lapply(inl, as.rle)
  if(length(inl)==1){
    any(inl[[1]]$values, na.rm = na.rm)
  }else{
    any(sapply(inl, any, na.rm = na.rm))
  }
}

#' @rdname rle.utils
#' @examples
#' 
#' stopifnot(isTRUE(all.equal(all(x),all(inverse.rle(x)))))
#' stopifnot(isTRUE(all.equal(all(y),all(inverse.rle(y)))))
#' 
#' @export
all.rle <- function(..., na.rm = FALSE){
  inl <- list(...)
  inl <- lapply(inl, as.rle)
  if(length(inl)==1){
    all(inl[[1]]$values, na.rm = na.rm)
  }else{
    all(sapply(inl, all, na.rm = na.rm))
  }
}

#' @rdname rle.utils
#' @examples
#'
#' x <- rle(sample(c(-1,+1), 10, c(.7,.3), replace=TRUE))
#' y <- rle(sample(c(-1,+1), 10, c(.3,.7), replace=TRUE))
#' 
#' stopifnot(isTRUE(all.equal((inverse.rle(x)*inverse.rle(y)),inverse.rle(x*y))))
#' @export
`*.rle` <- function(e1, e2){
  binop.rle(e1, e2, `*`)
}

#' @rdname rle.utils
#' @examples
#' stopifnot(isTRUE(all.equal((inverse.rle(x)/inverse.rle(y)),inverse.rle(x/y))))
#' @export
`/.rle` <- function(e1, e2){
  binop.rle(e1, e2, `/`)
}

#' @rdname rle.utils
#' @examples
#' stopifnot(isTRUE(all.equal((-inverse.rle(y)),inverse.rle(-y))))
#' stopifnot(isTRUE(all.equal((inverse.rle(x)-inverse.rle(y)),inverse.rle(x-y))))
#' @export
`-.rle` <- function(e1, e2){
  if(missing(e2)){
    e1$values <- -e1$values
    e1
  }else
    binop.rle(e1, e2, `-`)
}

#' @rdname rle.utils
#' @examples
#' stopifnot(isTRUE(all.equal((+inverse.rle(y)),inverse.rle(+y))))
#' stopifnot(isTRUE(all.equal((inverse.rle(x)+inverse.rle(y)),inverse.rle(x+y))))
#' @export
`+.rle` <- function(e1, e2){
  if(missing(e2)){
    e1$values <- +e1$values
    e1
  }else
    binop.rle(e1, e2, `+`)
}

#' @rdname rle.utils
#' @examples
#' stopifnot(isTRUE(all.equal((inverse.rle(x)^inverse.rle(y)),inverse.rle(x^y))))
#' @export
`^.rle` <- function(e1, e2){
  binop.rle(e1, e2, `^`)
}

#' @rdname rle.utils
#' @examples
#' stopifnot(isTRUE(all.equal((inverse.rle(x)%%inverse.rle(y)),inverse.rle(x%%y))))
#' @export
`%%.rle` <- function(e1, e2){
  binop.rle(e1, e2, `%%`)
}

#' @rdname rle.utils
#' @examples
#' stopifnot(isTRUE(all.equal((inverse.rle(x)%/%inverse.rle(y)),inverse.rle(x%/%y))))
#' @export
`%/%.rle` <- function(e1, e2){
  binop.rle(e1, e2, `%/%`)
}

#' @rdname rle.utils
#' @examples
#' stopifnot(isTRUE(all.equal(inverse.rle(x)==inverse.rle(y),inverse.rle(x==y))))
#' @export
`==.rle` <- function(e1, e2){
  binop.rle(e1, e2, `==`)
}

#' @rdname rle.utils
#' @examples
#' stopifnot(isTRUE(all.equal((inverse.rle(x)>inverse.rle(y)),inverse.rle(x>y))))
#' @export
`>.rle` <- function(e1, e2){
  binop.rle(e1, e2, `>`)
}

#' @rdname rle.utils
#' @examples
#' stopifnot(isTRUE(all.equal((inverse.rle(x)<inverse.rle(y)),inverse.rle(x<y))))
#' @export
`<.rle` <- function(e1, e2){
  binop.rle(e1, e2, `<`)
}

#' @rdname rle.utils
#' @examples
#' stopifnot(isTRUE(all.equal((inverse.rle(x)!=inverse.rle(y)),inverse.rle(x!=y))))
#' @export
`!=.rle` <- function(e1, e2){
  binop.rle(e1, e2, `!=`)
}

#' @rdname rle.utils
#' @examples
#' stopifnot(isTRUE(all.equal((inverse.rle(x)<=inverse.rle(y)),inverse.rle(x<=y))))
#' @export
`<=.rle` <- function(e1, e2){
  binop.rle(e1, e2, `<=`)
}

#' @rdname rle.utils
#' @examples
#' stopifnot(isTRUE(all.equal((inverse.rle(x)>=inverse.rle(y)),inverse.rle(x>=y))))
#' @export
`>=.rle` <- function(e1, e2){
  binop.rle(e1, e2, `>=`)
}


#' @rdname rle.utils
#'
#' @examples
#' 
#' stopifnot(isTRUE(all.equal(sum(inverse.rle(x)),sum(x))))
#' stopifnot(isTRUE(all.equal(sum(inverse.rle(y)),sum(y))))
#' 
#' @export
sum.rle <- function(..., na.rm = FALSE){
  inl <- list(...)
  inl <- lapply(inl, as.rle)
  if(length(inl)==1){
    sum(inl[[1]]$values*as.numeric(inl[[1]]$lengths), na.rm = na.rm)
  }else{
    sum(sapply(inl, sum, na.rm = na.rm))
  }
}



#' @rdname rle.utils
#'
#' @examples
#' 
#' stopifnot(isTRUE(all.equal(mean(inverse.rle(x)),mean(x))))
#' stopifnot(isTRUE(all.equal(mean(inverse.rle(y)),mean(y))))
#' 
#' @export
mean.rle <- function(x, na.rm = FALSE, ...){
  if(na.rm) sum(x$values*as.numeric(x$lengths), na.rm = TRUE, ...)/sum(!is.na(x))
  else sum(x$values*as.numeric(x$lengths), na.rm = FALSE, ...)/length(x)
}

#' @rdname rle.utils
#'
#' @note The [`length`] method returns the length of the vector
#'   represented by the object, obtained by summing the lengths of
#'   individual runs.
#'
#' @examples
#'
#' stopifnot(isTRUE(all.equal(length(inverse.rle(x)),length(x))))
#' stopifnot(isTRUE(all.equal(length(inverse.rle(y)),length(y))))
#'
#' @export
length.rle <- function(x){
  sum(as.numeric(x$lengths))
}

#' @rdname rle.utils
#'
#' @examples
#' x$values[1] <- NA
#' y$values[1] <- NA
#' stopifnot(isTRUE(all.equal(is.na(inverse.rle(x)),inverse.rle(is.na(x)))))
#' stopifnot(isTRUE(all.equal(is.na(inverse.rle(y)),inverse.rle(is.na(y)))))
#' 
#' @export
is.na.rle <- function(x){
  x$values <- is.na(x$values)
  x
}

#' @rdname rle.utils
#'
#' @param scale whether to replicate the elements of the
#'   RLE-compressed vector or the runs.
#'
#' @param doNotCompact whether the method should call [`compact.rle`]
#'   the results before returning. Methods liable to produce very long
#'   output vectors, like [`rep`], have this set `FALSE` by default.
#' 
#' @note The [`rep`] method for [`rle`] objects is very limited at
#'   this time. Even though the default setting is to replicate
#'   elements of the vector, only the run-replicating functionality is
#'   implemented at this time except for the simplest case (scalar
#'   `times` argument).
#'
#' @examples
#' 
#' x <- rle(sample(c(-1,+1), 10, c(.7,.3), replace=TRUE))
#' y <- rpois(length(x$lengths), 2)
#' 
#' stopifnot(isTRUE(all.equal(rep(inverse.rle(x), rep(y, x$lengths)),
#'                                inverse.rle(rep(x, y, scale="run")))))
#'
#' stopifnot(isTRUE(all.equal(rep(inverse.rle(x), max(y)),
#'                                inverse.rle(rep(x, max(y), scale="element")))))
#' 
#' @export
rep.rle <- function(x, ..., scale = c("element", "run"), doNotCompact = FALSE){
  scale <- match.arg(scale)
  ddd <- list(...)

  if(is.null(names(ddd)) && length(ddd)==1) names(ddd) <- "times"
  
  if(scale=="element" && length(ddd$times)!=1) stop("RLE on element scale is not supported at this time for vector ",sQuote("times")," argument.")

  if(length(x$lengths)==length(ddd$times)){ # This handles the specific scale="run" AND times is vector of appropriate length case.
    tmp <- mapply(function(v, l, times){
      newl <- .run_mul(l, times)
      newv <- rep(v, length(newl))
      list(l = newl, v = newv)
    },
    x$values, x$lengths, ddd$times, SIMPLIFY=FALSE)
    
    x$values <- as.vector(unlist(sapply(tmp, `[[`, "v")))
    x$lengths <- as.integer(unlist(sapply(tmp, `[[`, "l")))
  }else{  # This handles the scale="run" OR times is scalar case.
    x$values <- rep(x$values, ...)
    x$lengths <- rep(x$lengths, ...)
  }
  
  if(doNotCompact) x else compact.rle(x)
}

#' Coerce to [`rle`] if not already an [`rle`] object.
#'
#' @param x the object to be coerced.
#' 
#' @export
as.rle <- function(x){
  UseMethod("as.rle")
}

#' @rdname as.rle
#' @export
as.rle.rle <- function(x) x

#' @rdname as.rle
#' @export
as.rle.default <- function(x){
  #' @importFrom methods is
  if(is(x, "rle")) x else rle(x)
}
