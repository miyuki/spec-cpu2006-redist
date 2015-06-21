#ifdef SPEC_CPU
# define THORN_IS_CactusBindings
#endif /* SPEC_CPU */
/*@@
  @header    PUGHReduce_Register.h
  @desc
  Register aliased functions for PUGHReduce
  @enddesc 
  @@*/

#include "cctk_Flesh.h"

int CCTKBindings_PUGHReduceAliases(void);

int CCTKBindings_PUGHReduceAliases(void)
{
  int retval=0; /* returns minus number of failed overloads */
  int ierr=0;

  retval = ierr; /* use ierr to prevent warnings */

  return retval;
}
