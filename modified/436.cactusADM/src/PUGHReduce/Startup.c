#ifdef SPEC_CPU
# define THORN_IS_PUGHReduce
#endif /* SPEC_CPU */
 /*@@
   @file      Startup.c
   @date      Wed Feb  3 23:10:19 1999
   @author    Tom Goodale
   @desc
              Startup routines for PUGHReduce.
   @enddesc
   @version $Header: /cactus/CactusPUGH/PUGHReduce/src/Startup.c,v 1.5 2001/08/27 16:30:51 tradke Exp $
@@*/

#include "cctk.h"
#include "pugh_reductions.h"

static const char *rcsid="$Header: /cactus/CactusPUGH/PUGHReduce/src/Startup.c,v 1.5 2001/08/27 16:30:51 tradke Exp $";

CCTK_FILEVERSION(CactusPUGH_PUGHReduce_Startup_c)


/* prototypes of routines defined in this source file */
int PUGHReduce_Startup(void);


 /*@@
   @routine    PUGHReduce_Startup
   @date       Wed Feb  3 23:14:38 1999
   @author     Tom Goodale
   @desc
               The startup registration routine for PUGHReduce.
   @enddesc

   @calls      CCTK_RegisterReductionOperator
               CCTK_RegisterReductionArrayOperator

   @returntype void
   @returndesc
   @endreturndesc
@@*/
int PUGHReduce_Startup (void)
{
  /* Register the reduction operators provided by PUGH */
  CCTK_RegisterReductionOperator (PUGH_ReductionMinValGVs,  "minimum");
  CCTK_RegisterReductionOperator (PUGH_ReductionMaxValGVs,  "maximum");
  CCTK_RegisterReductionOperator (PUGH_ReductionSumGVs,     "sum");
  CCTK_RegisterReductionOperator (PUGH_ReductionNorm1GVs,   "norm1");
  CCTK_RegisterReductionOperator (PUGH_ReductionNorm2GVs,   "norm2");
  CCTK_RegisterReductionOperator (PUGH_ReductionNormInfGVs, "norm_inf");

  CCTK_RegisterReductionArrayOperator (PUGH_ReductionMinValArrays,  "minimum");
  CCTK_RegisterReductionArrayOperator (PUGH_ReductionMaxValArrays,  "maximum");
  CCTK_RegisterReductionArrayOperator (PUGH_ReductionSumArrays,     "sum");
  CCTK_RegisterReductionArrayOperator (PUGH_ReductionNorm1Arrays,   "norm1");
  CCTK_RegisterReductionArrayOperator (PUGH_ReductionNorm2Arrays,   "norm2");
  CCTK_RegisterReductionArrayOperator (PUGH_ReductionNormInfArrays, "norm_inf");

  return (0);
}
