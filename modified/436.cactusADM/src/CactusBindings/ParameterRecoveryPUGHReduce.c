#ifdef SPEC_CPU
# define THORN_IS_CactusBindings
#endif /* SPEC_CPU */
#define THORN_IS_PUGHReduce

#include <stdarg.h>

#include "cctk.h"
#include "cctk_Parameters.h"
#include "cctki_ScheduleBindings.h"

/* Prototypes for functions to be registered. */


/*@@
  @routine    CCTKi_BindingsParameterRecovery_PUGHReduce
  @date       
  @author     
  @desc 
  Creates the parameter recovery bindings for thorn PUGHReduce
  @enddesc 
  @calls     
  @calledby   
  @history 

  @endhistory

@@*/
int CCTKi_BindingsParameterRecovery_PUGHReduce(void);
int CCTKi_BindingsParameterRecovery_PUGHReduce(void)
{
  DECLARE_CCTK_PARAMETERS
  int result = 0;



  USE_CCTK_PARAMETERS;   return (result);
}
