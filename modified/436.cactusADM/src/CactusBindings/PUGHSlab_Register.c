#ifdef SPEC_CPU
# define THORN_IS_CactusBindings
#endif /* SPEC_CPU */
/*@@
  @header    PUGHSlab_Register.h
  @desc
  Register aliased functions for PUGHSlab
  @enddesc 
  @@*/

#include "cctk_Flesh.h"

int CCTKBindings_PUGHSlabAliases(void);

int CCTKBindings_PUGHSlabAliases(void)
{
  int retval=0; /* returns minus number of failed overloads */
  int ierr=0;

  retval = ierr; /* use ierr to prevent warnings */

  return retval;
}
