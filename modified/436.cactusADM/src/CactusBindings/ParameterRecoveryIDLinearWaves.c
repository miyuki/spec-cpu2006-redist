#ifdef SPEC_CPU
# define THORN_IS_CactusBindings
#endif /* SPEC_CPU */
#define THORN_IS_IDLinearWaves

#include <stdarg.h>

#include "cctk.h"
#include "cctk_Parameters.h"
#include "cctki_ScheduleBindings.h"

/* Prototypes for functions to be registered. */


/*@@
  @routine    CCTKi_BindingsParameterRecovery_IDLinearWaves
  @date       
  @author     
  @desc 
  Creates the parameter recovery bindings for thorn IDLinearWaves
  @enddesc 
  @calls     
  @calledby   
  @history 

  @endhistory

@@*/
int CCTKi_BindingsParameterRecovery_IDLinearWaves(void);
int CCTKi_BindingsParameterRecovery_IDLinearWaves(void)
{
  DECLARE_CCTK_PARAMETERS
  int result = 0;

if (CCTK_Equals(initial_data,"planewaves")) 

{


}

if (CCTK_Equals(initial_data,"teukwaves")) 

{


}


  USE_CCTK_PARAMETERS;   return (result);
}
