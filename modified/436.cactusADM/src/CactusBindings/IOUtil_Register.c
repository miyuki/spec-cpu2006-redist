#ifdef SPEC_CPU
# define THORN_IS_CactusBindings
#endif /* SPEC_CPU */
/*@@
  @header    IOUtil_Register.h
  @desc
  Register aliased functions for IOUtil
  @enddesc 
  @@*/

#include "cctk_Flesh.h"

int CCTKBindings_IOUtilAliases(void);

int CCTKBindings_IOUtilAliases(void)
{
  int retval=0; /* returns minus number of failed overloads */
  int ierr=0;

  retval = ierr; /* use ierr to prevent warnings */

  return retval;
}
