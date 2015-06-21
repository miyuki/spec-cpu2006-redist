 /*@@
   @header    iobasicGH.h
   @date      Friday 18th September 1999
   @author    Gabrielle Allen
   @desc
              The extensions to the GH structure from IOBasic.
   @enddesc
   @version $Header: /cactus/CactusBase/IOBasic/src/iobasicGH.h,v 1.7 2001/12/28 21:22:52 tradke Exp $
 @@*/

#include "StoreNamedData.h"


typedef struct IOBASIC_REDUCTION
{
  int handle;
  char *name;
  char is_valid;
  CCTK_REAL value;
  struct IOBASIC_REDUCTION *next;
} iobasic_reduction_t;

typedef struct IOBASIC_REDUCTIONLIST
{
  unsigned int num_reductions;
  iobasic_reduction_t *reductions;
} iobasic_reductionlist_t;

typedef struct IOBASIC_GH
{
  /* how often to output */
  int  outScalar_every;
  int  outInfo_every;
  char info_reductions_changed;

  /* flags indicating output for variable i */
  char   *do_outScalar;

  /* directory in which to place scalar output */
  char  *outdirScalar;

  /* The last iteration output */
  int   *outInfo_last;
  int   *outScalar_last;

  /* The reduction lists for info output for all variables */
  iobasic_reductionlist_t *info_reductions;

  /* database for names of output files that were already created */
  pNamedData *filenameListScalar;

} iobasicGH;


/* prototypes of functions to be registered */
int IOBasic_InfoOutputGH (const cGH *GH);
int IOBasic_TriggerInfoOutput (const cGH *GH, int vindex);
int IOBasic_TimeForInfoOutput (const cGH *GH, int vindex);
int IOBasic_ScalarOutputGH (const cGH *GH);
int IOBasic_TriggerScalarOutput (const cGH *GH, int vindex);
int IOBasic_TimeForScalarOutput (const cGH *GH, int vindex);
int IOBasic_ScalarOutputVarAs (const cGH *GH, const char *vname, const char *alias);

/* other function prototypes */
int IOBasic_WriteInfo (const cGH *GH, int vindex);
int IOBasic_WriteScalarGS (const cGH *GH, int vindex, const char *alias);
int IOBasic_WriteScalarGA (const cGH *GH, int vindex, const char *alias);
