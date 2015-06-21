#ifdef SPEC_CPU
# define THORN_IS_CactusBindings
#endif /* SPEC_CPU */
#define THORN_IS_IOUtil

#include <stdarg.h>

#include "cctk.h"
#include "cctk_Parameters.h"
#include "cctki_ScheduleBindings.h"

/* Prototypes for functions to be registered. */
extern int IOUtil_Startup(void); /* Note that this is a cheat, we just need a function pointer. */
extern int IOUtil_RecoverGH(void); /* Note that this is a cheat, we just need a function pointer. */
extern int IOUtil_RecoverIDFromDatafiles(void); /* Note that this is a cheat, we just need a function pointer. */
extern int IOUtil_UpdateParFile(void); /* Note that this is a cheat, we just need a function pointer. */


/*@@
  @routine    CCTKi_BindingsSchedule_IOUtil
  @date       
  @author     
  @desc 
  Creates the schedule bindings for thorn IOUtil
  @enddesc 
  @calls     
  @calledby   
  @history 

  @endhistory

@@*/
void CCTKi_BindingsSchedule_IOUtil(void);
void CCTKi_BindingsSchedule_IOUtil(void)
{
  DECLARE_CCTK_PARAMETERS
  CCTKi_ScheduleFunction((void *)IOUtil_Startup,
                        "IOUtil_Startup",
                        "IOUtil",
                        "IO",
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
                        "Driver_Startup");


if (! CCTK_Equals (recover, "no"))

{

  CCTKi_ScheduleFunction((void *)IOUtil_RecoverGH,
                        "IOUtil_RecoverGH",
                        "IOUtil",
                        "IO",
                        "Checkpoint recovery routine",
                        "CCTK_RECOVER_VARIABLES",
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

if (*recover_ID_files)

{

  CCTKi_ScheduleFunction((void *)IOUtil_RecoverIDFromDatafiles,
                        "IOUtil_RecoverIDFromDatafiles",
                        "IOUtil",
                        "IO",
                        "Initial data recovery routine",
                        "CCTK_RECOVER_VARIABLES",
                        "C",
                        0,                       /* Number of STORAGE  groups   */
                        0,                      /* Number of COMM     groups   */
                        0,                   /* Number of TRIGGERS groups   */
                        0,                      /* Number of SYNC     groups   */
                        0,                          /* Number of Options           */
                        0,                      /* Number of BEFORE  routines  */
                        1,                       /* Number of AFTER   routines  */
                        0                        /* Number of WHILE   variables */,
                        "IOUtil_RecoverGH");


}

if (! CCTK_Equals (parfile_write, "no") && parfile_update_every)

{

  CCTKi_ScheduleFunction((void *)IOUtil_UpdateParFile,
                        "IOUtil_UpdateParFile",
                        "IOUtil",
                        "IO",
                        "Append steered parameters to parameter file",
                        "CCTK_POSTSTEP",
                        "C",
                        0,                       /* Number of STORAGE  groups   */
                        0,                      /* Number of COMM     groups   */
                        0,                   /* Number of TRIGGERS groups   */
                        0,                      /* Number of SYNC     groups   */
                        0,                          /* Number of Options           */
                        0,                      /* Number of BEFORE  routines  */
                        1,                       /* Number of AFTER   routines  */
                        0                        /* Number of WHILE   variables */,
                        "HTTP_Work");


}


USE_CCTK_PARAMETERS; return;
}
