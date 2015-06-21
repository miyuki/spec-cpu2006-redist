#ifdef SPEC_CPU
# define THORN_IS_CactusBindings
#endif /* SPEC_CPU */
/*@@
  @header    Einstein_Register.h
  @desc
  Register aliased functions for Einstein
  @enddesc 
  @@*/

#include "cctk_Flesh.h"

int CCTKBindings_EinsteinAliases(void);

int CCTKBindings_EinsteinAliases(void)
{
  int retval=0; /* returns minus number of failed overloads */
  int ierr=0;

  retval = ierr; /* use ierr to prevent warnings */

  return retval;
}
