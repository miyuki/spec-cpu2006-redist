#ifdef SPEC_CPU
# define THORN_IS_CactusBindings
#endif /* SPEC_CPU */
#define THORN_IS_IOUtil

#include <stdarg.h>

#include "cctk.h"
#include "cctk_Parameters.h"
#include "cctki_ScheduleBindings.h"

/* Prototypes for functions to be registered. */


/*@@
  @routine    CCTKi_BindingsParameterRecovery_IOUtil
  @date       
  @author     
  @desc 
  Creates the parameter recovery bindings for thorn IOUtil
  @enddesc 
  @calls     
  @calledby   
  @history 

  @endhistory

@@*/
int CCTKi_BindingsParameterRecovery_IOUtil(void);
int CCTKi_BindingsParameterRecovery_IOUtil(void)
{
  DECLARE_CCTK_PARAMETERS
  int result = 0;


if (! CCTK_Equals (recover, "no"))

{


}

if (*recover_ID_files)

{


}

if (! CCTK_Equals (parfile_write, "no") && parfile_update_every)

{


}


  USE_CCTK_PARAMETERS;   return (result);
}
