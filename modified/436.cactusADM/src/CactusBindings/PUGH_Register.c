#ifdef SPEC_CPU
# define THORN_IS_CactusBindings
#endif /* SPEC_CPU */
/*@@
  @header    PUGH_Register.h
  @desc
  Register aliased functions for PUGH
  @enddesc 
  @@*/

#include "cctk_Flesh.h"

int CCTKBindings_PUGHAliases(void);

int CCTKBindings_PUGHAliases(void)
{
  int retval=0; /* returns minus number of failed overloads */
  int ierr=0;

  retval = ierr; /* use ierr to prevent warnings */

  return retval;
}
