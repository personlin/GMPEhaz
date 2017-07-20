#include <R_ext/RS.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
Check these declarations against the C/Fortran source code.
*/

/* .Fortran calls */
extern void F77_NAME(ab03)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(ac_2010)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(aga16_tw_c01)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(am09_cas)(void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(arroyo2010)(void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(asb_2013)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(asb14_tw_b01)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(asb14_tw_c01)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(ask_ngawest2_2013)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(ask14_tw_b01)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(ask14_tw_c01)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(bchydrosub_v3)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(bindi_hor_2013)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(bindi14_tw_b01)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(bindi14_tw_c01)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(bssa_ngawest2_2013)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(bssa14_tw_b01)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(bssa14_tw_c01)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(camp03_h)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(cb_ngawest2_2013)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(cb14_tw_c01)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(chao2017)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(cy_nga_2008)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(cy_ngawest2_2013)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(cy14_tw_b01)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(cy14_tw_c01)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(garciah05)(void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(gk_nov2012)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(gregor06cas)(void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(i_ngawest2_2013)(void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(i14_tw_b01)(void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(i14_tw_c01)(void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(kaah_2015)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(kanno2006)(void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(lin_fw_rock)(void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(lin_fw_soil)(void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(lin_hw_rock)(void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(lin_hw_soil)(void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(lin2009)(void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(linlee08rock)(void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(linlee08soil)(void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(loh96)(void *, void *, void *, void *, void *, void *);
extern void F77_NAME(meaninten)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(montalva2017)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(s02_mcverry_subduction_2006)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(tg09221_2012)(void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(youngs97_rock)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(youngs97_soil)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(zhaoetal2006)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(zhaoetal2016_cru)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(zhaoetal2016_int)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(zhaoetal2016_slab)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);

static const R_FortranMethodDef FortranEntries[] = {
  {"ab03",                        (DL_FUNC) &F77_NAME(ab03),                        13},
  {"ac_2010",                     (DL_FUNC) &F77_NAME(ac_2010),                     10},
  {"aga16_tw_c01",                (DL_FUNC) &F77_NAME(aga16_tw_c01),                12},
  {"am09_cas",                    (DL_FUNC) &F77_NAME(am09_cas),                     7},
  {"arroyo2010",                  (DL_FUNC) &F77_NAME(arroyo2010),                   8},
  {"asb_2013",                    (DL_FUNC) &F77_NAME(asb_2013),                    11},
  {"asb14_tw_b01",                (DL_FUNC) &F77_NAME(asb14_tw_b01),                11},
  {"asb14_tw_c01",                (DL_FUNC) &F77_NAME(asb14_tw_c01),                11},
  {"ask_ngawest2_2013",           (DL_FUNC) &F77_NAME(ask_ngawest2_2013),           22},
  {"ask14_tw_b01",                (DL_FUNC) &F77_NAME(ask14_tw_b01),                22},
  {"ask14_tw_c01",                (DL_FUNC) &F77_NAME(ask14_tw_c01),                22},
  {"bchydrosub_v3",               (DL_FUNC) &F77_NAME(bchydrosub_v3),               13},
  {"bindi_hor_2013",              (DL_FUNC) &F77_NAME(bindi_hor_2013),              11},
  {"bindi14_tw_b01",              (DL_FUNC) &F77_NAME(bindi14_tw_b01),              11},
  {"bindi14_tw_c01",              (DL_FUNC) &F77_NAME(bindi14_tw_c01),              11},
  {"bssa_ngawest2_2013",          (DL_FUNC) &F77_NAME(bssa_ngawest2_2013),          15},
  {"bssa14_tw_b01",               (DL_FUNC) &F77_NAME(bssa14_tw_b01),               15},
  {"bssa14_tw_c01",               (DL_FUNC) &F77_NAME(bssa14_tw_c01),               15},
  {"camp03_h",                    (DL_FUNC) &F77_NAME(camp03_h),                    14},
  {"cb_ngawest2_2013",            (DL_FUNC) &F77_NAME(cb_ngawest2_2013),            20},
  {"cb14_tw_c01",                 (DL_FUNC) &F77_NAME(cb14_tw_c01),                 20},
  {"chao2017",                    (DL_FUNC) &F77_NAME(chao2017),                    17},
  {"cy_nga_2008",                 (DL_FUNC) &F77_NAME(cy_nga_2008),                 19},
  {"cy_ngawest2_2013",            (DL_FUNC) &F77_NAME(cy_ngawest2_2013),            19},
  {"cy14_tw_b01",                 (DL_FUNC) &F77_NAME(cy14_tw_b01),                 19},
  {"cy14_tw_c01",                 (DL_FUNC) &F77_NAME(cy14_tw_c01),                 19},
  {"garciah05",                   (DL_FUNC) &F77_NAME(garciah05),                    8},
  {"gk_nov2012",                  (DL_FUNC) &F77_NAME(gk_nov2012),                  11},
  {"gregor06cas",                 (DL_FUNC) &F77_NAME(gregor06cas),                  9},
  {"i_ngawest2_2013",             (DL_FUNC) &F77_NAME(i_ngawest2_2013),              9},
  {"i14_tw_b01",                  (DL_FUNC) &F77_NAME(i14_tw_b01),                   9},
  {"i14_tw_c01",                  (DL_FUNC) &F77_NAME(i14_tw_c01),                   9},
  {"kaah_2015",                   (DL_FUNC) &F77_NAME(kaah_2015),                   11},
  {"kanno2006",                   (DL_FUNC) &F77_NAME(kanno2006),                    9},
  {"lin_fw_rock",                 (DL_FUNC) &F77_NAME(lin_fw_rock),                  7},
  {"lin_fw_soil",                 (DL_FUNC) &F77_NAME(lin_fw_soil),                  7},
  {"lin_hw_rock",                 (DL_FUNC) &F77_NAME(lin_hw_rock),                  7},
  {"lin_hw_soil",                 (DL_FUNC) &F77_NAME(lin_hw_soil),                  7},
  {"lin2009",                     (DL_FUNC) &F77_NAME(lin2009),                      9},
  {"linlee08rock",                (DL_FUNC) &F77_NAME(linlee08rock),                 9},
  {"linlee08soil",                (DL_FUNC) &F77_NAME(linlee08soil),                 9},
  {"loh96",                       (DL_FUNC) &F77_NAME(loh96),                        6},
  {"meaninten",                   (DL_FUNC) &F77_NAME(meaninten),                   35},
  {"montalva2017",                (DL_FUNC) &F77_NAME(montalva2017),                12},
  {"s02_mcverry_subduction_2006", (DL_FUNC) &F77_NAME(s02_mcverry_subduction_2006), 14},
  {"tg09221_2012",                (DL_FUNC) &F77_NAME(tg09221_2012),                 9},
  {"youngs97_rock",               (DL_FUNC) &F77_NAME(youngs97_rock),               10},
  {"youngs97_soil",               (DL_FUNC) &F77_NAME(youngs97_soil),               10},
  {"zhaoetal2006",                (DL_FUNC) &F77_NAME(zhaoetal2006),                14},
  {"zhaoetal2016_cru",            (DL_FUNC) &F77_NAME(zhaoetal2016_cru),            14},
  {"zhaoetal2016_int",            (DL_FUNC) &F77_NAME(zhaoetal2016_int),            14},
  {"zhaoetal2016_slab",           (DL_FUNC) &F77_NAME(zhaoetal2016_slab),           14},
  {NULL, NULL, 0}
};

void R_init_GMPEhaz(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, NULL, FortranEntries, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
