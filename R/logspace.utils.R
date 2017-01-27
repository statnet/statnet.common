log_sum_exp <- function(logx, use_ldouble=FALSE){
  if(length(logx)==0) -Inf
  else .Call("log_sum_exp_wrapper", logx, use_ldouble, PACKAGE="statnet.common")
}

log_mean_exp <- function(logx, use_ldouble=FALSE){
  if(length(logx)==0) NaN
  else .Call("log_sum_exp_wrapper", logx, use_ldouble, PACKAGE="statnet.common") - log(length(logx))
}

lweighted.mean <- function(x, logw){
  if(length(x)==0) NaN
  else if(length(x)!=length(logw)) stop("x and logw must have the same length")
  else .Call("logspace_wmean_wrapper", x, logw, PACKAGE="statnet.common")
}
