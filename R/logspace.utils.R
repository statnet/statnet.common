#  File R/logspace.utils.R in package statnet.common, part of the
#  Statnet suite of packages for network analysis, https://statnet.org .
#
#  This software is distributed under the GPL-3 license.  It is free,
#  open source, and has the attribution requirements (GPL Section 7) at
#  https://statnet.org/attribution .
#
#  Copyright 2007-2024 Statnet Commons
################################################################################
#' Utilities for performing calculations on logarithmic scale.
#'
#' A small suite of functions to compute sums, means, and weighted means on
#' logarithmic scale, minimizing loss of precision.
#'
#' @param logx Numeric vector of \eqn{\log(x)}, the natural logarithms of the
#' values to be summed or averaged.
#'
#' @param x,y Numeric vectors or matrices of \eqn{x} and \eqn{y}, the (raw) values
#'   to be summed, averaged, or whose variances and covariances are to
#'   be calculated.
#'
#' @param logw Numeric vector of \eqn{\log(w)}, the natural logarithms of the
#' weights.
#' @param use_ldouble Whether to use \code{long double} precision in the
#' calculation. If \code{TRUE}, 's C built-in \code{logspace_sum()} is used. If
#' \code{FALSE}, the package's own implementation based on it is used, using
#' \code{double} precision, which is (on most systems) several times faster, at
#' the cost of precision.
#'
#' @param onerow If given a matrix or matrices with only one row
#'   (i.e., sample size 1), [var()] and [cov()] will return `NA`. But,
#'   since weighted matrices are often a product of compression, the
#'   same could be interpreted as a variance of variables that do not
#'   vary, i.e., 0. This argument controls what value should be
#'   returned.
#'
#' @return The functions return the equivalents of the R expressions given below,
#' but faster and with less loss of precision.
#' @author Pavel N. Krivitsky
#' @keywords arith
#' @examples
#' x <- rnorm(1000)
#' stopifnot(all.equal(log_sum_exp(x), log(sum(exp(x))), check.attributes=FALSE))
#' stopifnot(all.equal(log_mean_exp(x), log(mean(exp(x))), check.attributes=FALSE))
#'
#' logw <- rnorm(1000)
#' stopifnot(all.equal(m <- sum(x*exp(logw))/sum(exp(logw)),lweighted.mean(x, logw)))
#' stopifnot(all.equal(sum((x-m)^2*exp(logw))/sum(exp(logw)),
#'                     lweighted.var(x, logw), check.attributes=FALSE))
#'
#' x <- cbind(x, rnorm(1000))
#' stopifnot(all.equal(mx <- colSums(x*exp(logw))/sum(exp(logw)),
#'                     lweighted.mean(x, logw), check.attributes=FALSE))
#' stopifnot(all.equal(crossprod(t(t(x)-mx)*exp(logw/2))/sum(exp(logw)),
#'                     lweighted.var(x, logw), check.attributes=FALSE))
#'
#'
#' y <- cbind(x, rnorm(1000))
#' my <- colSums(y*exp(logw))/sum(exp(logw))
#' stopifnot(all.equal(crossprod(t(t(x)-mx)*exp(logw/2), t(t(y)-my)*exp(logw/2))/sum(exp(logw)),
#'                     lweighted.cov(x, y, logw), check.attributes=FALSE))
#' stopifnot(all.equal(crossprod(t(t(y)-my)*exp(logw/2), t(t(x)-mx)*exp(logw/2))/sum(exp(logw)),
#'                     lweighted.cov(y, x, logw), check.attributes=FALSE))
#' @name logspace.utils
#' @useDynLib statnet.common
NULL

#' @describeIn logspace.utils `log(sum(exp(logx)))`
#' @export
log_sum_exp <- function(logx, use_ldouble=FALSE){
  if(length(logx)==0) -Inf
  else .Call("log_sum_exp_wrapper", logx, use_ldouble, PACKAGE="statnet.common")
}

#' @describeIn logspace.utils `log(mean(exp(logx)))`
#' @export
log_mean_exp <- function(logx, use_ldouble=FALSE){
  if(length(logx)==0) NaN
  else .Call("log_sum_exp_wrapper", logx, use_ldouble, PACKAGE="statnet.common") - log(length(logx))
}

#' @describeIn logspace.utils weighted mean of `x`:
#'   `sum(x*exp(logw))/sum(exp(logw))` for `x` scalar and
#'   `colSums(x*exp(logw))/sum(exp(logw))` for `x` matrix
#' @export
lweighted.mean <- function(x, logw){
  d <- dim(x)
  if(is.null(d)){ # Vector
    if(length(x)==0) NaN
    else if(length(x)!=length(logw)) stop("x and logw must have the same length")
    else .Call("logspace_wmean_wrapper", x, logw, PACKAGE="statnet.common")
  }else if(length(d)>2){
    stop("Arrays of 3 or more dimensions are not supported at this time.")
  }else{ # Matrix
    if(d[1L]==0) rep(NaN, d[2L])
    else if(d[1L]!=length(logw)) stop("logw must have the same length as the number of rows in x")
    else .Call("logspace_wmeans_wrapper", x, logw, PACKAGE="statnet.common")
  }
}

#' @describeIn logspace.utils weighted variance of `x`: `crossprod(x-lweighted.mean(x,logw)*exp(logw/2))/sum(exp(logw))`
#' @export
lweighted.var <- function(x, logw, onerow = NA){
  E <- lweighted.mean(x, logw)
  if(is.null(dim(x))){
    if(length(x)<2) return(onerow)
    x <- x - E
    lweighted.mean(x*x, logw)
  }else{
    if(nrow(x)<2) return(matrix(onerow, ncol(x), ncol(x)))
    .Call("logspace_wmean2_wrapper", sweep_cols.matrix(x, E), logw, PACKAGE="statnet.common")
  }
}

#' @describeIn logspace.utils weighted covariance between `x` and `y`: `crossprod(x-lweighted.mean(x,logw)*exp(logw/2), y-lweighted.mean(y,logw)*exp(logw/2))/sum(exp(logw))`
#' @export
lweighted.cov <- function(x, y, logw, onerow = NA){
  xdim <- dim(x)
  E <- lweighted.mean(x, logw)
  x <- if(is.null(xdim)) x - E else sweep_cols.matrix(x, E)

  ydim <- dim(y)
  E <- lweighted.mean(y, logw)
  y <- if(is.null(ydim)) y - E else sweep_cols.matrix(y, E)

  if(is.null(xdim) || is.null(ydim)){
    if(length(x)<2) return(onerow)
    o <- lweighted.mean(x*y, logw)
    if(!is.null(xdim)) cbind(o)
    else if(!is.null(xdim)) rbind(o)
    else o
  }else{
    if(nrow(x)<2) matrix(onerow, ncol(x), ncol(y))
    else .Call("logspace_wxmean_wrapper", x, y, logw, PACKAGE="statnet.common")
  }
}

#' @describeIn logspace.utils `log(1-exp(-x))` for `x >= 0` (a wrapper for the eponymous C macro provided by R)
#' @examples
#'
#' x <- rexp(1000)
#' stopifnot(isTRUE(all.equal(log1mexp(x), log(1-exp(-x)))))
#'
#' @export
log1mexp <- function(x) .Call("log1mexp_wrapper", x)

#' Suptract a elements of a vector from respective columns of a matrix
#' 
#' An optimized function equivalent to \code{sweep(x, 2, STATS)} for a matrix
#' \code{x}.
#' 
#' 
#' @param x a numeric matrix;
#' @param STATS a numeric vector whose length equals to the number of columns
#' of \code{x}.
#' @param disable_checks if \code{TRUE}, do not check that \code{x} is a
#' numeric matrix and its number of columns matches the length of \code{STATS};
#' set in production code for a significant speed-up.
#' @return A matrix of the same attributes as \code{x}.
#' @examples
#' 
#' x <- matrix(runif(1000), ncol=4)
#' s <- 1:4
#' 
#' stopifnot(all.equal(sweep_cols.matrix(x, s), sweep(x, 2, s)))
#' 
#' @export 
sweep_cols.matrix <- function(x, STATS, disable_checks=FALSE){
  if(!disable_checks)
    if(!is.matrix(x) || mode(x)!="numeric" || ncol(x)!=length(STATS)) stop("Argument ",sQuote("x")," must be a numeric matrix variable (not an expression that evaluates to a numeric matrix).")
  o <- .Call("sweep2m", x, STATS, PACKAGE="statnet.common")
  attributes(o) <- attributes(x)
  o
}
