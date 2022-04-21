/*  File src/logspace_utils.c in package statnet.common, part of the
 *  Statnet suite of packages for network analysis, https://statnet.org .
 *
 *  This software is distributed under the GPL-3 license.  It is free,
 *  open source, and has the attribution requirements (GPL Section 7) at
 *  https://statnet.org/attribution .
 *
 *  Copyright 2007-2022 Statnet Commons
 */
#include<R.h>
#include<Rmath.h>
#include<Rinternals.h>

/*
 * Compute log sum x given log(x) values logx
 *
 *     log (sum_i  exp (logx[i]) ) =
 *     log (e^M * sum_i  e^(logx[i] - M) ) =
 *     M + log( sum_i  e^(logx[i] - M)
 *
 * without causing overflows or throwing much accuracy.
 *
 * Based on logspace_sum in pgamma.c in R; unlike that implementation,
 * it does not use the long double type, sacrificing precision for a
 * speed gain.
 */
double log_sum_exp(const double *logx, int n){
  if(n == 1) return logx[0];
  if(n == 2) return logspace_add(logx[0], logx[1]);
  // else (n >= 3) :
  int i;
  // Mx := max_i log(x_i)
  double Mx = logx[0];
  for(i = 1; i < n; i++) if(Mx < logx[i]) Mx = logx[i];
  double s = 0.;
  for(i = 0; i < n; i++) s += exp(logx[i] - Mx);
  return Mx + log(s);
}


SEXP log_sum_exp_wrapper(SEXP logx, SEXP long_double){
  long_double = PROTECT(coerceVector(long_double, LGLSXP));
  logx = PROTECT(coerceVector(logx, REALSXP));
  int n = length(logx);
  SEXP out = PROTECT(allocVector(REALSXP, 1));
  if(LOGICAL(long_double)[0])
    REAL(out)[0] = logspace_sum(REAL(logx), n);
  else
    REAL(out)[0] = log_sum_exp(REAL(logx), n);
  UNPROTECT(3);
  return(out);
}


/*
 * Compute a weighted mean of x given log-weights lw
 *
 *     log (sum_i  exp (logx[i]) ) =
 *     log (e^M * sum_i  e^(logx[i] - M) ) =
 *     M + log( sum_i  e^(logx[i] - M)
 *
 * without causing overflows or throwing much accuracy.
 * Based on logspace_sum in pgamma.c in R.
 */
double logspace_wmean (const double *x, const double* logw, int n){
  if(n == 1) return x[0];
  // else (n >= 2) :
  int i;
  // Mw := max_i log(w_i)
  double Mw = logw[0];
  for(i = 1; i < n; i++) if(Mw < logw[i]) Mw = logw[i];
  double sw = 0., sxw = 0.;
  for(i = 0; i < n; i++){
    double w = exp(logw[i] - Mw);
    sw += w;
    sxw += w*x[i];
  }
  return (double) sxw/sw;
}


SEXP logspace_wmean_wrapper(SEXP x, SEXP logw){
  x = PROTECT(coerceVector(x, REALSXP));
  logw = PROTECT(coerceVector(logw, REALSXP));
  int n = length(x);
  if(n != length(logw)) error("Lengths of value and log-weight vectors differ.");
  SEXP out = PROTECT(allocVector(REALSXP, 1));
  REAL(out)[0] = logspace_wmean(REAL(x), REAL(logw), n);
  UNPROTECT(3);
  return(out);
}

/*
  Matrix version of logspace_wmean
 */
void logspace_wmeans (const double *xm, const double* logw, int n, int p, double *out){
  if(n == 1){
    memcpy(out, xm, p*sizeof(double));
    return;
  }
  // else (n >= 2) :
  int i;
  // Mw := max_i log(w_i)
  double Mw = logw[0];
  for(i = 1; i < n; i++) if(Mw < logw[i]) Mw = logw[i];
  memset(out, 0, p*sizeof(double));
  double sw = 0.;
  for(i = 0; i < n; i++){
    double w = exp(logw[i] - Mw);
    sw += w;
    for(unsigned int j = 0; j < p; j++)
      out[j] += w*xm[i + j*n];
  }
  for(unsigned int j = 0; j < p; j++) out[j] /= sw;
}



SEXP logspace_wmeans_wrapper(SEXP xm, SEXP logw){
  SEXP xdim = PROTECT(getAttrib(xm, R_DimSymbol));
  int n = INTEGER(xdim)[0], p = INTEGER(xdim)[1];
  xm = PROTECT(coerceVector(xm, REALSXP));
  logw = PROTECT(coerceVector(logw, REALSXP));
  if(n != length(logw)) error("Number of rows in the value matrix differs from the length of the log-weights vector.");
  SEXP out = PROTECT(allocVector(REALSXP, p));
  logspace_wmeans(REAL(xm), REAL(logw), n, p, REAL(out));
  UNPROTECT(4);
  return(out);
}

SEXP sweep2m(SEXP xm, SEXP stats){
  SEXP xdim = PROTECT(getAttrib(xm, R_DimSymbol));
  int n = INTEGER(xdim)[0], p = INTEGER(xdim)[1];
  SEXP out = PROTECT(allocMatrix(REALSXP, n, p));
  xm = PROTECT(coerceVector(xm, REALSXP));
  stats = PROTECT(coerceVector(stats, REALSXP));
  /* if(p != length(stats)) error("Number of columns in the value matrix differs from the length of the STATS vector."); */
  unsigned int pos = 0;
  for(unsigned int i=0; i<p; i++){
    double s = REAL(stats)[i];
    for(unsigned int j=0; j<n; j++, pos++){
      REAL(out)[pos] = REAL(xm)[pos] - s;
    }
  }
  UNPROTECT(4);
  return(out);
}


void logspace_wmean2 (const double *xm, const double* logw, int n, int p, double *out)
{
  // else (n >= 2) :
  int i;
  // Mw := max_i log(w_i)
  double Mw = logw[0];
  for(i = 1; i < n; i++) if(Mw < logw[i]) Mw = logw[i];
  memset(out, 0, p*p*sizeof(double));
  double sw = 0.;
  for(i = 0; i < n; i++){
    double w = exp(logw[i] - Mw);
    sw += w;
    for(unsigned int j = 0; j < p; j++)
      for(unsigned int k = 0; k <= j; k++)
	out[j + k*p] += w*xm[i + j*n]*xm[i + k*n];
  }
  for(unsigned int j = 0; j < p; j++)
    for(unsigned int k = 0; k <= j; k++){
      out[j + k*p] /= sw;
      out[k + j*p] = out[j + k*p];
    }
}

SEXP logspace_wmean2_wrapper(SEXP xm, SEXP logw){
  SEXP xdim = PROTECT(getAttrib(xm, R_DimSymbol));
  int n = INTEGER(xdim)[0], p = INTEGER(xdim)[1];
  xm = PROTECT(coerceVector(xm, REALSXP));
  logw = PROTECT(coerceVector(logw, REALSXP));
  if(n != length(logw)) error("Number of rows in the value matrix differs from the length of the log-weights vector.");
  SEXP out = PROTECT(allocMatrix(REALSXP, p, p));
  logspace_wmean2(REAL(xm), REAL(logw), n, p, REAL(out));
  UNPROTECT(4);
  return(out);
}
