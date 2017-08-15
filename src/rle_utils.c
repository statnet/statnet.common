/*  File src/rle_utils.c in package statnet.common, part of the Statnet suite
 *  of packages for network analysis, http://statnet.org .
 *
 *  This software is distributed under the GPL-3 license.  It is free,
 *  open source, and has the attribution requirements (GPL Section 7) at
 *  http://statnet.org/attribution
 *
 *  Copyright 2007-2017 Statnet Commons
 */
#include<R.h>
#include<Rmath.h>
#include<Rinternals.h>

SEXP sync_RLEs(SEXP lens1, SEXP lens2){
  const char *names[] = {"lengths", "val1i", "val2i", "nruns", ""};
  SEXP out = PROTECT(mkNamed(VECSXP, names));
  SEXP lengths = PROTECT(allocVector(INTSXP, length(lens1)+length(lens2)));
  SEXP v1i = PROTECT(allocVector(INTSXP, length(lens1)+length(lens2)));
  SEXP v2i = PROTECT(allocVector(INTSXP, length(lens1)+length(lens2)));
  SEXP nruns = PROTECT(allocVector(INTSXP, 1));

  // Leftover lengths of the current run in each vector:
  unsigned int left1 = 0, left2 = 0;
  // Positions in the RLE length vectors:
  unsigned int opos = 0, pos1 = 0, pos2 = 0;

  /*
    Iteratate through the length vectors. Note that we increment the
    position as soon as we read from a vector, so it's already in "R"
    indexing (from 1).
*/
  while(pos1 < length(lens1)){
    left1 += INTEGER(lens1)[pos1];
    pos1++;
    
    while(left1 > left2){
      /*
	Now, we know that left1 > left2.

	So, we flush from both rle1 and rle2 by the left2's amount,
	and add on to left2 until left2 is the one "sticking out".
      */      
      
      if(left2 != 0){
	INTEGER(lengths)[opos] = left2;
	INTEGER(v1i)[opos] = pos1;
	INTEGER(v2i)[opos] = pos2;
	left1 -= left2;
	left2 = 0; // I.e., subtract left2 from left2.
	opos++;
      }

      if(pos2 >= length(lens2)) break;
      left2 += INTEGER(lens2)[pos2];
      pos2++;
    }

    /* 
       Now, left1 <= left2. (If lens2 has run out, then this should
       still hold, since the uncompressed lengths of the input vectors
       must be equal.)

       So, we flush from both by the left1's amount.
    */
      
    if(left1 != 0){
      INTEGER(lengths)[opos] = left1;
      INTEGER(v1i)[opos] = pos1;
      INTEGER(v2i)[opos] = pos2;
      left2 -= left1;
      left1 = 0; // I.e., subtract left1 from left1.
      opos++;
    }
  }

  *INTEGER(nruns) = opos;
  
  SET_VECTOR_ELT(out, 0, lengths);
  SET_VECTOR_ELT(out, 1, v1i);
  SET_VECTOR_ELT(out, 2, v2i);
  SET_VECTOR_ELT(out, 3, nruns);

  UNPROTECT(5);
  return(out);
}


SEXP compact_RLE(SEXP l, SEXP v){
  const char *names[] = {"lengths", "vali", "nruns", ""};
  SEXP out = PROTECT(mkNamed(VECSXP, names));
  SEXP lengths = PROTECT(allocVector(INTSXP, length(l)));
  SEXP vi = PROTECT(allocVector(INTSXP, length(l)));
  SEXP nruns = PROTECT(allocVector(INTSXP, 1));

  INTEGER(lengths)[0] = INTEGER(l)[0];
  INTEGER(vi)[0] = 1;
  unsigned int o = 0;
  for(unsigned int i = 1; i < length(l); i++){
    unsigned int nextl = INTEGER(l)[i];
    if(INTEGER(lengths)[o] > INT_MAX-nextl || // If cumulative run is too long or
       INTEGER(v)[INTEGER(vi)[o]-1] != INTEGER(v)[i]){ // the value is not the same as the previous one...
      // advance the output vector;
      o++;
      INTEGER(lengths)[o] = nextl;
      INTEGER(vi)[o] = i+1;
    }else{ // otherwise...
      // stay put and add to the run length.
      INTEGER(lengths)[o] += nextl;
    }
  }
  
  *INTEGER(nruns) = o+1;
  
  SET_VECTOR_ELT(out, 0, lengths);
  SET_VECTOR_ELT(out, 1, vi);
  SET_VECTOR_ELT(out, 2, nruns);

  UNPROTECT(4);
  return(out);
}
