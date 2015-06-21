#ifdef SPEC_CPU
# define THORN_IS_CactusBindings
#endif /* SPEC_CPU */
/*@@
  @header    FortranThornFunctions.h
  @desc
  Fortran wrappers for overloaded thorn functions
  @enddesc 
  @@*/

#include <stdlib.h>

#include "cctk_Flesh.h"
#include "cctk_WarnLevel.h"

#include "cctk_FortranString.h"
