#ifdef SPEC_CPU
# define THORN_IS_CactusBindings
#endif /* SPEC_CPU */
/*@@
  @header    BenchADM_Register.h
  @desc
  Register aliased functions for BenchADM
  @enddesc 
  @@*/

#include "cctk_Flesh.h"

int CCTKBindings_BenchADMAliases(void);

int CCTKBindings_BenchADMAliases(void)
{
  int retval=0; /* returns minus number of failed overloads */
  int ierr=0;

  retval = ierr; /* use ierr to prevent warnings */

  return retval;
}
