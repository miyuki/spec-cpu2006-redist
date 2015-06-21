#ifdef SPEC_CPU
# define THORN_IS_CactusBindings
#endif /* SPEC_CPU */
#define THORN_IS_Time

#include <stdarg.h>

#include "cctk.h"
#include "cctk_Parameters.h"
#include "cctki_ScheduleBindings.h"

/* Prototypes for functions to be registered. */


/*@@
  @routine    CCTKi_BindingsParameterRecovery_Time
  @date       
  @author     
  @desc 
  Creates the parameter recovery bindings for thorn Time
  @enddesc 
  @calls     
  @calledby   
  @history 

  @endhistory

@@*/
int CCTKi_BindingsParameterRecovery_Time(void);
int CCTKi_BindingsParameterRecovery_Time(void)
{
  DECLARE_CCTK_PARAMETERS
  int result = 0;



if (CCTK_Equals (timestep_method, "courant_static"))

{


}

else if (CCTK_Equals (timestep_method, "courant_speed") ||

         CCTK_Equals (timestep_method, "courant_time"))

{



}

else

{


}


  USE_CCTK_PARAMETERS;   return (result);
}
