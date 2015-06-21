#ifdef SPEC_CPU
# define THORN_IS_CactusBindings
#endif /* SPEC_CPU */
#define THORN_IS_PUGHSlab

#include <stdarg.h>

#include "cctk.h"
#include "cctk_Parameters.h"
#include "cctki_ScheduleBindings.h"

/* Prototypes for functions to be registered. */


/*@@
  @routine    CCTKi_BindingsSchedule_PUGHSlab
  @date       
  @author     
  @desc 
  Creates the schedule bindings for thorn PUGHSlab
  @enddesc 
  @calls     
  @calledby   
  @history 

  @endhistory

@@*/
void CCTKi_BindingsSchedule_PUGHSlab(void);
void CCTKi_BindingsSchedule_PUGHSlab(void)
{
  DECLARE_CCTK_PARAMETERS

USE_CCTK_PARAMETERS; return;
}
