#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP eulerr_bit_index(SEXP);
extern SEXP eulerr_choose_two(SEXP);
extern SEXP eulerr_discdisc(SEXP, SEXP, SEXP);
extern SEXP eulerr_loss_final(SEXP, SEXP);
extern SEXP eulerr_return_intersections(SEXP);
extern SEXP eulerr_venneuler_stress(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"eulerr_bit_index",            (DL_FUNC) &eulerr_bit_index,            1},
  {"eulerr_choose_two",           (DL_FUNC) &eulerr_choose_two,           1},
  {"eulerr_discdisc",             (DL_FUNC) &eulerr_discdisc,             3},
  {"eulerr_loss_final",           (DL_FUNC) &eulerr_loss_final,           2},
  {"eulerr_return_intersections", (DL_FUNC) &eulerr_return_intersections, 1},
  {"eulerr_venneuler_stress",     (DL_FUNC) &eulerr_venneuler_stress,     2},
  {NULL, NULL, 0}
};

void R_init_eulerr(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
