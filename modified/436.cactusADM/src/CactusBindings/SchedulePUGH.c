#ifdef SPEC_CPU
# define THORN_IS_CactusBindings
#endif /* SPEC_CPU */
#define THORN_IS_PUGH

#include <stdarg.h>

#include "cctk.h"
#include "cctk_Parameters.h"
#include "cctki_ScheduleBindings.h"

/* Prototypes for functions to be registered. */
extern int PUGH_Startup(void); /* Note that this is a cheat, we just need a function pointer. */
extern int PUGH_Report(void); /* Note that this is a cheat, we just need a function pointer. */
extern int PUGH_PrintTimingInfo(void); /* Note that this is a cheat, we just need a function pointer. */
extern int PUGH_PrintFinalStorageReport(void); /* Note that this is a cheat, we just need a function pointer. */
extern int PUGH_PrintStorageReport(void); /* Note that this is a cheat, we just need a function pointer. */
extern int PUGH_Terminate(void); /* Note that this is a cheat, we just need a function pointer. */


/*@@
  @routine    CCTKi_BindingsSchedule_PUGH
  @date       
  @author     
  @desc 
  Creates the schedule bindings for thorn PUGH
  @enddesc 
  @calls     
  @calledby   
  @history 

  @endhistory

@@*/
void CCTKi_BindingsSchedule_PUGH(void);
void CCTKi_BindingsSchedule_PUGH(void)
{
  DECLARE_CCTK_PARAMETERS
  CCTKi_ScheduleFunction((void *)PUGH_Startup,
                        "Driver_Startup",
                        "PUGH",
                        "driver",
                        "Startup routine",
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


  CCTKi_ScheduleFunction((void *)PUGH_Report,
                        "PUGH_Report",
                        "PUGH",
                        "driver",
                        "Report on PUGH set up",
                        "CCTK_BASEGRID",
                        "C",
                        0,                       /* Number of STORAGE  groups   */
                        0,                      /* Number of COMM     groups   */
                        0,                   /* Number of TRIGGERS groups   */
                        0,                      /* Number of SYNC     groups   */
                        0,                          /* Number of Options           */
                        0,                      /* Number of BEFORE  routines  */
                        0,                       /* Number of AFTER   routines  */
                        0                        /* Number of WHILE   variables */);


if (timer_output)

{

  CCTKi_ScheduleFunction((void *)PUGH_PrintTimingInfo,
                        "PUGH_PrintTimingInfo",
                        "PUGH",
                        "driver",
                        "Print time spent in communication",
                        "CCTK_TERMINATE",
                        "C",
                        0,                       /* Number of STORAGE  groups   */
                        0,                      /* Number of COMM     groups   */
                        0,                   /* Number of TRIGGERS groups   */
                        0,                      /* Number of SYNC     groups   */
                        0,                          /* Number of Options           */
                        0,                      /* Number of BEFORE  routines  */
                        0,                       /* Number of AFTER   routines  */
                        0                        /* Number of WHILE   variables */);


}

if (CCTK_Equals(storage_verbose,"yes") || CCTK_Equals(storage_verbose,"report") )

{

  CCTKi_ScheduleFunction((void *)PUGH_PrintFinalStorageReport,
                        "PUGH_PrintFinalStorageReport",
                        "PUGH",
                        "driver",
                        "Print storage information",
                        "CCTK_TERMINATE",
                        "C",
                        0,                       /* Number of STORAGE  groups   */
                        0,                      /* Number of COMM     groups   */
                        0,                   /* Number of TRIGGERS groups   */
                        0,                      /* Number of SYNC     groups   */
                        0,                          /* Number of Options           */
                        0,                      /* Number of BEFORE  routines  */
                        0,                       /* Number of AFTER   routines  */
                        0                        /* Number of WHILE   variables */);


}

if (CCTK_Equals(storage_verbose,"yes") || CCTK_Equals(storage_verbose,"report"))

{

  CCTKi_ScheduleFunction((void *)PUGH_PrintStorageReport,
                        "PUGH_PrintStorageReport",
                        "PUGH",
                        "driver",
                        "Print storage information",
                        "CCTK_POSTSTEP",
                        "C",
                        0,                       /* Number of STORAGE  groups   */
                        0,                      /* Number of COMM     groups   */
                        0,                   /* Number of TRIGGERS groups   */
                        0,                      /* Number of SYNC     groups   */
                        0,                          /* Number of Options           */
                        0,                      /* Number of BEFORE  routines  */
                        0,                       /* Number of AFTER   routines  */
                        0                        /* Number of WHILE   variables */);


}

  CCTKi_ScheduleFunction((void *)PUGH_Terminate,
                        "Driver_Terminate",
                        "PUGH",
                        "driver",
                        "Termination routine",
                        "CCTK_TERMINATE",
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
