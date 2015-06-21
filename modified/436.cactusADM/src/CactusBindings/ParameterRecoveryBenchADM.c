#ifdef SPEC_CPU
# define THORN_IS_CactusBindings
#endif /* SPEC_CPU */
#define THORN_IS_BenchADM

#include <stdarg.h>

#include "cctk.h"
#include "cctk_Parameters.h"
#include "cctki_ScheduleBindings.h"

/* Prototypes for functions to be registered. */


/*@@
  @routine    CCTKi_BindingsParameterRecovery_BenchADM
  @date       
  @author     
  @desc 
  Creates the parameter recovery bindings for thorn BenchADM
  @enddesc 
  @calls     
  @calledby   
  @history 

  @endhistory

@@*/
int CCTKi_BindingsParameterRecovery_BenchADM(void);
int CCTKi_BindingsParameterRecovery_BenchADM(void)
{
  DECLARE_CCTK_PARAMETERS
  int result = 0;


if (CCTK_Equals(evolution_system,"ADM"))

{





  if (time_symmetric)

  {


  }

  else

  {


  }


}


  USE_CCTK_PARAMETERS;   return (result);
}
