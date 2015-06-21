#ifdef SPEC_CPU
# define THORN_IS_CactusBindings
#endif /* SPEC_CPU */
#define THORN_IS_IOASCII

#include <stdarg.h>

#include "cctk.h"
#include "cctk_Parameters.h"
#include "cctki_ScheduleBindings.h"

/* Prototypes for functions to be registered. */
extern int IOASCII_Startup(void); /* Note that this is a cheat, we just need a function pointer. */
extern int IOASCII_Choose1D(void); /* Note that this is a cheat, we just need a function pointer. */
extern int IOASCII_Choose2D(void); /* Note that this is a cheat, we just need a function pointer. */


/*@@
  @routine    CCTKi_BindingsSchedule_IOASCII
  @date       
  @author     
  @desc 
  Creates the schedule bindings for thorn IOASCII
  @enddesc 
  @calls     
  @calledby   
  @history 

  @endhistory

@@*/
void CCTKi_BindingsSchedule_IOASCII(void);
void CCTKi_BindingsSchedule_IOASCII(void)
{
  DECLARE_CCTK_PARAMETERS
  CCTKi_ScheduleFunction((void *)IOASCII_Startup,
                        "IOASCII_Startup",
                        "IOASCII",
                        "IOASCII",
                        "Startup routine",
                        "CCTK_STARTUP",
                        "C",
                        0,                       /* Number of STORAGE  groups   */
                        0,                      /* Number of COMM     groups   */
                        0,                   /* Number of TRIGGERS groups   */
                        0,                      /* Number of SYNC     groups   */
                        0,                          /* Number of Options           */
                        0,                      /* Number of BEFORE  routines  */
                        1,                       /* Number of AFTER   routines  */
                        0                        /* Number of WHILE   variables */,
                        "IOUtil_Startup");


  CCTKi_ScheduleFunction((void *)IOASCII_Choose1D,
                        "IOASCII_Choose1D",
                        "IOASCII",
                        "IOASCII",
                        "Choose 1D output lines",
                        "CCTK_BASEGRID",
                        "C",
                        0,                       /* Number of STORAGE  groups   */
                        0,                      /* Number of COMM     groups   */
                        0,                   /* Number of TRIGGERS groups   */
                        0,                      /* Number of SYNC     groups   */
                        0,                          /* Number of Options           */
                        0,                      /* Number of BEFORE  routines  */
                        1,                       /* Number of AFTER   routines  */
                        0                        /* Number of WHILE   variables */,
                        "SpatialCoordinates");


  CCTKi_ScheduleFunction((void *)IOASCII_Choose2D,
                        "IOASCII_Choose2D",
                        "IOASCII",
                        "IOASCII",
                        "Choose 2D output planes",
                        "CCTK_BASEGRID",
                        "C",
                        0,                       /* Number of STORAGE  groups   */
                        0,                      /* Number of COMM     groups   */
                        0,                   /* Number of TRIGGERS groups   */
                        0,                      /* Number of SYNC     groups   */
                        0,                          /* Number of Options           */
                        0,                      /* Number of BEFORE  routines  */
                        1,                       /* Number of AFTER   routines  */
                        0                        /* Number of WHILE   variables */,
                        "SpatialCoordinates");



USE_CCTK_PARAMETERS; return;
}
