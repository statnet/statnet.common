#  File R/rle_reexport.R in package statnet.common, part of the Statnet suite
#  of packages for network analysis, https://statnet.org .
#
#  This software is distributed under the GPL-3 license.  It is free,
#  open source, and has the attribution requirements (GPL Section 7) at
#  https://statnet.org/attribution
#
#  Copyright 2007-2020 Statnet Commons
#######################################################################
#' @name rle-reexport
#'
#' @title Functions reexported from `rle`
#'
#' They will no longer be reexported after the next release.
#'
#' Some [Ops] group methods are exported as well, since as of version
#' 4.0.2, R's [NextMethod()] does not appear to be able to dispatch
#' from a group member method to a group method.
#'
#' @param x,e1,e2,na.rm,scale,doNotCompact,doNotCompress,object,... Arguments to the corresponding functions in `rle`.
#'
#' @import rle
NULL

#' @rdname rle-reexport
#' @export
c.rle <- getS3method("c","rle",envir=getNamespace("rle"))
#' @rdname rle-reexport
#' @export
Ops.rle <- getS3method("Ops","rle",envir=getNamespace("rle"))
#' @rdname rle-reexport
#' @export
`!.rle` <- function(e1) structure(list(lengths = e1$lengths,
                                       values = !e1$values),
                                  class = "rle")
#' @rdname rle-reexport
#' @export
`|.rle` <- Ops.rle
#' @rdname rle-reexport
#' @export
`&.rle` <- Ops.rle
#' @rdname rle-reexport
#' @export
`<.rle` <- Ops.rle
#' @rdname rle-reexport
#' @export
`>.rle` <- Ops.rle
#' @rdname rle-reexport
#' @export
`<=.rle` <- Ops.rle
#' @rdname rle-reexport
#' @export
`>=.rle` <- Ops.rle
#' @rdname rle-reexport
#' @export
`==.rle` <- Ops.rle
#' @rdname rle-reexport
#' @export
`!=.rle` <- Ops.rle
#' @rdname rle-reexport
#' @export
Math.rle <- getS3method("Math","rle",envir=getNamespace("rle"))
#' @rdname rle-reexport
#' @export
Summary.rle <- getS3method("Summary","rle",envir=getNamespace("rle"))
#' @rdname rle-reexport
#' @export
compress <- rle::compress
#' @rdname rle-reexport
#' @export
compress.rle <- getS3method("compress","rle",envir=getNamespace("rle"))
#' @rdname rle-reexport
#' @export
mean.rle <- getS3method("mean","rle",envir=getNamespace("rle"))
#' @rdname rle-reexport
#' @export
length.rle <- getS3method("length","rle",envir=getNamespace("rle"))
#' @rdname rle-reexport
#' @export
is.na.rle <- getS3method("is.na","rle",envir=getNamespace("rle"))
#' @rdname rle-reexport
#' @export
rep.rle <- getS3method("rep","rle",envir=getNamespace("rle"))
#' @rdname rle-reexport
#' @export
as.rle <- rle::as.rle
#' @rdname rle-reexport
#' @export
as.rle.rle <- getS3method("as.rle","rle",envir=getNamespace("rle"))
#' @rdname rle-reexport
#' @export
as.rle.default <- getS3method("as.rle","default",envir=getNamespace("rle"))
#' @rdname rle-reexport
#' @export
str.rle <- getS3method("str","rle",envir=getNamespace("rle"))
#' @rdname rle-reexport
#' @export
compact.rle <- rle::compact.rle
