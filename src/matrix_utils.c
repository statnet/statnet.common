/*  File src/logspace_utils.c in package statnet.common, part of the Statnet
 *  suite of packages for network analysis, https://statnet.org .
 *
 *  This software is distributed under the GPL-3 license.  It is free, open
 *  source, and has the attribution requirements (GPL Section 7) at
 *  https://statnet.org/attribution .
 *
 *  Copyright 2007-2025 Statnet Commons
 */
#include<R.h>
#include<Rmath.h>
#include<Rinternals.h>

SEXP sweep2m(SEXP xmR, SEXP statsR){
  int *xdim = INTEGER(PROTECT(getAttrib(xmR, R_DimSymbol)));
  unsigned int n = xdim[0], p = xdim[1];
  SEXP outR = PROTECT(allocMatrix(REALSXP, n, p));
  double *out = REAL(outR);
  double *xm = REAL(PROTECT(coerceVector(xmR, REALSXP)));
  double *stats = REAL(PROTECT(coerceVector(statsR, REALSXP)));
  /* if(p != length(statsR)) error("Number of columns in the value matrix differs from the length of the STATS vector."); */
  unsigned int pos = 0;
  for(unsigned int i = 0; i < p; i++){
    double s = stats[i];
    for(unsigned int j = 0; j < n; j++, pos++){
      out[pos] = xm[pos] - s;
    }
  }
  UNPROTECT(4);
  return(outR);
}
