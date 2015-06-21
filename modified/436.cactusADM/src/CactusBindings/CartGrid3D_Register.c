#ifdef SPEC_CPU
# define THORN_IS_CactusBindings
#endif /* SPEC_CPU */
/*@@
  @header    CartGrid3D_Register.h
  @desc
  Register aliased functions for CartGrid3D
  @enddesc 
  @@*/

#include "cctk_Flesh.h"

int CCTKBindings_CartGrid3DAliases(void);

int CCTKBindings_CartGrid3DAliases(void)
{
  int retval=0; /* returns minus number of failed overloads */
  int ierr=0;

  retval = ierr; /* use ierr to prevent warnings */

  return retval;
}
