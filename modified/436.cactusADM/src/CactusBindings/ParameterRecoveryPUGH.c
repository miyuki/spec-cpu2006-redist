#ifdef SPEC_CPU
# define THORN_IS_CactusBindings
#endif /* SPEC_CPU */
#define THORN_IS_PUGH

#include <stdarg.h>

#include "cctk.h"
#include "cctk_Parameters.h"
#include "cctki_ScheduleBindings.h"

/* Prototypes for functions to be registered. */


/*@@
  @routine    CCTKi_BindingsParameterRecovery_PUGH
  @date       
  @author     
  @desc 
  Creates the parameter recovery bindings for thorn PUGH
  @enddesc 
  @calls     
  @calledby   
  @history 

  @endhistory

@@*/
int CCTKi_BindingsParameterRecovery_PUGH(void);
int CCTKi_BindingsParameterRecovery_PUGH(void)
{
  DECLARE_CCTK_PARAMETERS
  int result = 0;



if (timer_output)

{


}

if (CCTK_Equals(storage_verbose,"yes") || CCTK_Equals(storage_verbose,"report") )

{


}

if (CCTK_Equals(storage_verbose,"yes") || CCTK_Equals(storage_verbose,"report"))

{


}



  USE_CCTK_PARAMETERS;   return (result);
}
