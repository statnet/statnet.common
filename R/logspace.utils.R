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
    .sweep2m(tmp, E)
    .Call("logspace_wmean2_wrapper", tmp, logw, PACKAGE="statnet.common")
  }
}

.sweep2m <- function(x, STATS){
  .Call("sweep2m", x, STATS, PACKAGE="statnet.common")
}
