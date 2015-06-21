#ifdef SPEC_CPU
# define THORN_IS_CactusBindings
#endif /* SPEC_CPU */
#define THORN_IS_PUGHReduce

#include <stdarg.h>

#include "cctk.h"
#include "cctk_Parameters.h"
#include "cctki_ScheduleBindings.h"

/* Prototypes for functions to be registered. */
extern int PUGHReduce_Startup(void); /* Note that this is a cheat, we just need a function pointer. */


/*@@
  @routine    CCTKi_BindingsSchedule_PUGHReduce
  @date       
  @author     
  @desc 
  Creates the schedule bindings for thorn PUGHReduce
  @enddesc 
  @calls     
  @calledby   
  @history 

  @endhistory

@@*/
void CCTKi_BindingsSchedule_PUGHReduce(void);
void CCTKi_BindingsSchedule_PUGHReduce(void)
{
  DECLARE_CCTK_PARAMETERS
  CCTKi_ScheduleFunction((void *)PUGHReduce_Startup,
                        "PUGHReduce_Startup",
                        "PUGHReduce",
                        "reduce",
                        "Startup routine.",
                        "CCTK_STARTUP",
                        "C",
                        0,                       /* Number of STORAGE  groups   */
                        0,                      /* Number of COMM     groups   */
                        0,                   /* Number of TRIGGERS groups   */
                        0,                      /* Number of SYNC     groups   */
                        0,                          /* Number of Options           */
                        0,                      /* Number of BEFORE  routines  */
                        0,                       /* Number of AFTER   routines  */
                        0                        /* Number of WHILE   variables */);



USE_CCTK_PARAMETERS; return;
}
