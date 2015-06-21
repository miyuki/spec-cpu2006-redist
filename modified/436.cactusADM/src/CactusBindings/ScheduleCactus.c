#ifdef SPEC_CPU
# define THORN_IS_CactusBindings
#endif /* SPEC_CPU */
#define THORN_IS_Cactus

#include <stdarg.h>

#include "cctk.h"
#include "cctk_Parameters.h"
#include "cctki_ScheduleBindings.h"

/* Prototypes for functions to be registered. */


/*@@
  @routine    CCTKi_BindingsSchedule_Cactus
  @date       
  @author     
  @desc 
  Creates the schedule bindings for thorn Cactus
  @enddesc 
  @calls     
  @calledby   
  @history 

  @endhistory

@@*/
void CCTKi_BindingsSchedule_Cactus(void);
void CCTKi_BindingsSchedule_Cactus(void)
{
  DECLARE_CCTK_PARAMETERS

USE_CCTK_PARAMETERS; return;
}
