//#include "conf.h"

//#ifdef ESMC_HAVE_FORTRAN_UNDERSCORE
#ifdef IRIX64
#define FTN(func) func##_
#elif ABSOFT
#define FTN(func) func##__
#elif LAHEY
#define FTN(func) func##_
#elif OSF1
#define FTN(func) func##_
#else
#define FTN(func) func
#endif


