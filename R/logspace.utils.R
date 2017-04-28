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
    tmp <- x
    sweep_cols.matrix(tmp, E)
    .Call("logspace_wmean2_wrapper", tmp, logw, PACKAGE="statnet.common")
  }
}

#' Suptract a elements of a vector from respective columns of a matrix
#' in place
#'
#' An optimized function equivalent to `x <- sweep(x, 2, STATS)` for a
#' matrix `x`.
#'
#' @param x a variable containing a numeric matrix; is overwritten
#'   with the result.
#' @param STATS a numeric vector whose length equals to the number of
#'   columns of `x`.
#' @param disable_checks if `TRUE`, do not check that `x` is a
#'   variable containing a numeric matrix and its number of columns
#'   matches the length of `STATS`; set in production code for a
#'   significant speed-up.
#'
#' @return Always returns `NULL`, as a reminder that the operation
#'   occurs in place. If you need to keep `x`, see the example below.
#'
#' @examples
#' x <- y <- matrix(runif(1000), ncol=4)
#' s <- 1:4
#'
#' sweep_cols.matrix(y, s) # Note that y is overwritten.
#' stopifnot(any(y != sweep(x, 2, s)))
#'
#' # Invalid input: x+y is an expression that evaluates to a matrix,
#' # not a matrix variable.
#' try(sweep_cols.matrix(x+y, s))
#'
#' @export
sweep_cols.matrix <- function(x, STATS, disable_checks=FALSE){
  if(!disable_checks)
    if(!exists(deparse(substitute(x))) || !is.matrix(x) || mode(x)!="numeric" || ncol(x)!=length(STATS)) stop("Argument ",sQuote("x")," must be a numeric matrix variable (not an expression that evaluates to a numeric matrix).")
  invisible(.Call("sweep2m", x, STATS, PACKAGE="statnet.common"))
}
