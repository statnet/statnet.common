#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP log_sum_exp_wrapper(SEXP, SEXP);
extern SEXP logspace_wmean_wrapper(SEXP, SEXP);
extern SEXP logspace_wmean2_wrapper(SEXP, SEXP);
extern SEXP logspace_wmeans_wrapper(SEXP, SEXP);
extern SEXP logspace_wxmean_wrapper(SEXP, SEXP, SEXP);
extern SEXP sweep2m(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"log_sum_exp_wrapper",     (DL_FUNC) &log_sum_exp_wrapper,     2},
    {"logspace_wmean_wrapper",  (DL_FUNC) &logspace_wmean_wrapper,  2},
    {"logspace_wmean2_wrapper", (DL_FUNC) &logspace_wmean2_wrapper, 2},
    {"logspace_wmeans_wrapper", (DL_FUNC) &logspace_wmeans_wrapper, 2},
    {"logspace_wxmean_wrapper", (DL_FUNC) &logspace_wxmean_wrapper, 3},
    {"sweep2m",                 (DL_FUNC) &sweep2m,                 2},
    {NULL, NULL, 0}
};

void R_init_statnet_common(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
