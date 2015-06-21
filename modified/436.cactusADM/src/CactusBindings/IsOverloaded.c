#ifdef SPEC_CPU
# define THORN_IS_CactusBindings
#endif /* SPEC_CPU */
/*@@
  @header    IsOverloaded.c
  @desc
  Query how many times a function is overloaded
  @enddesc 
  @@*/

#include <stdlib.h>

#include "cctk_Flesh.h"

#include "cctk_FortranString.h"

int CCTK_IsOverloaded(const char *function);
int CCTK_IsOverloaded(const char *function)
{
  int retval=0;

  const char *cctk_dummy_string; /* avoid warnings */

  cctk_dummy_string=function; /* avoid warnings */

  return retval;
}


void CCTK_FCALL cctk_isoverloaded_
  (int *ret, ONE_FORTSTRING_ARG);
void CCTK_FCALL cctk_isoverloaded_
  (int *ret, ONE_FORTSTRING_ARG)
{
  ONE_FORTSTRING_CREATE(name);
  *ret = CCTK_IsOverloaded(name);
  free(name);
}
