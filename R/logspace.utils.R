log_sum_exp <- function(logx, use_ldouble=FALSE){
  if(length(logx)==0) -Inf
  else .Call("log_sum_exp_wrapper", logx, use_ldouble, PACKAGE="statnet.common")
}

log_mean_exp <- function(logx, use_ldouble=FALSE){
  if(length(logx)==0) NaN
  else .Call("log_sum_exp_wrapper", logx, use_ldouble, PACKAGE="statnet.common") - log(length(logx))
}

lweighted.mean <- function(x, logw){
  d <- dim(x)
  if(is.null(d)){ # Vector
    if(length(x)==0) NaN
    else if(length(x)!=length(logw)) stop("x and logw must have the same length")
    else .Call("logspace_wmean_wrapper", x, logw, PACKAGE="statnet.common")
  }else if(length(d)>2){
    stop("Arrays of 3 or more dimensions are not supported at this time.")
  }else{ # Matrix
    if(d[1]==0) rep(NaN, d[2])
    else if(d[1]!=length(logw)) stop("logw must have the same length as the number of rows in x")
    else .Call("logspace_wmeans_wrapper", x, logw, PACKAGE="statnet.common")
  }
}

lweighted.var <- function(x, logw){
  E <- lweighted.mean(x, logw)
  if(is.null(dim(x))){
    if(length(x)<2) return(NA)
    x <- x - E
    lweighted.mean(x*x, logw)
  }else{
    if(nrow(x)<2) return(matrix(NA, 1, ncol(x)))
    .Call("logspace_wmean2_wrapper", sweep_cols.matrix(x, E), logw, PACKAGE="statnet.common")
  }
}

#' Suptract a elements of a vector from respective columns of a matrix
#'
#' An optimized function equivalent to `sweep(x, 2, STATS)` for a
#' matrix `x`.
#'
#' @param x a numeric matrix;
#' @param STATS a numeric vector whose length equals to the number of
#'   columns of `x`.
#' @param disable_checks if `TRUE`, do not check that `x` is a numeric
#'   matrix and its number of columns matches the length of `STATS`;
#'   set in production code for a significant speed-up.
#'
#' @return A matrix of the same attributes as `x`.
#'
#' @examples
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
