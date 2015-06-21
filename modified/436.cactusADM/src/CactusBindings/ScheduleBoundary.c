#ifdef SPEC_CPU
# define THORN_IS_CactusBindings
#endif /* SPEC_CPU */
#define THORN_IS_Boundary

#include <stdarg.h>

#include "cctk.h"
#include "cctk_Parameters.h"
#include "cctki_ScheduleBindings.h"

/* Prototypes for functions to be registered. */


/*@@
  @routine    CCTKi_BindingsSchedule_Boundary
  @date       
  @author     
  @desc 
  Creates the schedule bindings for thorn Boundary
  @enddesc 
  @calls     
  @calledby   
  @history 

  @endhistory

@@*/
void CCTKi_BindingsSchedule_Boundary(void);
void CCTKi_BindingsSchedule_Boundary(void)
{
  DECLARE_CCTK_PARAMETERS

USE_CCTK_PARAMETERS; return;
}
