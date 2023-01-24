#  File R/mcmc-utils.R in package statnet.common, part of the
#  Statnet suite of packages for network analysis, https://statnet.org .
#
#  This software is distributed under the GPL-3 license.  It is free,
#  open source, and has the attribution requirements (GPL Section 7) at
#  https://statnet.org/attribution .
#
#  Copyright 2007-2023 Statnet Commons
################################################################################
#' @name mcmc-utilities
#' @title Utility operations for [`mcmc.list`] objects
#' 
#' @description \code{colMeans.mcmc.list} is a "method" for (non-generic) [`colMeans`] applicable to [`mcmc.list`] objects.
#' 
#' @param x a \code{\link{mcmc.list}} object.
#' @param \dots additional arguments to \code{\link{colMeans}} or
#'   \code{\link{sweep}}.
#' @return \code{colMeans.mcmc} returns a vector with length equal to
#'   the number of mcmc chains in \code{x} with the mean value for
#'   each chain.
#' @seealso [`colMeans`], [`mcmc.list`]
#' @examples
#' data(line, package="coda")
#' summary(line) # coda
#' colMeans.mcmc.list(line) # "Method"
#' \dontshow{
#' stopifnot(isTRUE(all.equal(summary(line)$statistics[,"Mean"],colMeans.mcmc.list(line))))
#' }
#' @export colMeans.mcmc.list
colMeans.mcmc.list<-function(x,...) colMeans(as.matrix(x),...)

#' @rdname mcmc-utilities
#'
#' @description \code{sweep.mcmc.list} is a "method" for (non-generic)
#'   [`sweep`] applicable to [`mcmc.list`] objects.
#' 
#' @param STATS,FUN,check.margin See help for [`sweep`].
#' @return \code{sweep.mcmc.list} returns an appropriately modified
#'   version of \code{x}
#' @seealso [`sweep`]
#' @examples
#' data(line, package="coda")
#' colMeans.mcmc.list(line)-1:3
#' colMeans.mcmc.list(sweep.mcmc.list(line, 1:3))
#' \dontshow{
#' stopifnot(isTRUE(all.equal(colMeans.mcmc.list(sweep.mcmc.list(line, 1:3)), colMeans.mcmc.list(line)-1:3)))
#' }
#' @export sweep.mcmc.list
sweep.mcmc.list<-function(x, STATS, FUN="-", check.margin=TRUE, ...){
  for(chain in seq_along(x)){
    x[[chain]] <- sweep(x[[chain]], 2, STATS, FUN, check.margin, ...)
  }
  x
}

#' @rdname mcmc-utilities
#'
#' @description \code{lapply.mcmc.list} is a "method" for (non-generic)
#'   [`lapply`] applicable to [`mcmc.list`] objects.
#' 
#' @param X An [`mcmc.list`] object.
#' @return `lapply.mcmc.list` returns an [`mcmc.list`] each of
#'   whose chains had been passed through `FUN`.
#' @seealso [`lapply`]
#' @examples
#' data(line, package="coda")
#' colMeans.mcmc.list(line)[c(2,3,1)]
#' colMeans.mcmc.list(lapply.mcmc.list(line, `[`,,c(2,3,1)))
#' \dontshow{
#' stopifnot(isTRUE(all.equal(colMeans.mcmc.list(line)[c(2,3,1)],colMeans.mcmc.list(lapply.mcmc.list(line, `[`,,c(2,3,1))))))
#' }
#' @importFrom coda as.mcmc.list as.mcmc
#' @export lapply.mcmc.list
lapply.mcmc.list<-function(X, FUN, ...){
  as.mcmc.list(lapply(lapply(X, FUN, ...), as.mcmc))
}
