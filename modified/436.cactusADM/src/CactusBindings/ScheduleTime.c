#ifdef SPEC_CPU
# define THORN_IS_CactusBindings
#endif /* SPEC_CPU */
#define THORN_IS_Time

#include <stdarg.h>

#include "cctk.h"
#include "cctk_Parameters.h"
#include "cctki_ScheduleBindings.h"

/* Prototypes for functions to be registered. */
extern int Time_Initialise(void); /* Note that this is a cheat, we just need a function pointer. */
extern int Time_Simple(void); /* Note that this is a cheat, we just need a function pointer. */
extern int Time_Simple(void); /* Note that this is a cheat, we just need a function pointer. */
extern int Time_Courant(void); /* Note that this is a cheat, we just need a function pointer. */
extern int Time_Given(void); /* Note that this is a cheat, we just need a function pointer. */


/*@@
  @routine    CCTKi_BindingsSchedule_Time
  @date       
  @author     
  @desc 
  Creates the schedule bindings for thorn Time
  @enddesc 
  @calls     
  @calledby   
  @history 

  @endhistory

@@*/
void CCTKi_BindingsSchedule_Time(void);
void CCTKi_BindingsSchedule_Time(void)
{
  DECLARE_CCTK_PARAMETERS
  CCTKi_ScheduleGroupStorage( "time::speedvars");
  CCTKi_ScheduleGroupStorage( "Time::couranttemps");
  CCTKi_ScheduleGroupComm( "time::speedvars");
  CCTKi_ScheduleGroupComm( "Time::couranttemps");

  CCTKi_ScheduleFunction((void *)Time_Initialise,
                        "Time_Initialise",
                        "Time",
                        "time",
                        "Initialise Time variables",
                        "CCTK_BASEGRID",
                        "C",
                        0,                       /* Number of STORAGE  groups   */
                        0,                      /* Number of COMM     groups   */
                        0,                   /* Number of TRIGGERS groups   */
                        0,                      /* Number of SYNC     groups   */
                        0,                          /* Number of Options           */
                        2,                      /* Number of BEFORE  routines  */
                        0,                       /* Number of AFTER   routines  */
                        0                        /* Number of WHILE   variables */,
                        "Time_Simple",
                        "Time_Given");


if (CCTK_Equals (timestep_method, "courant_static"))

{

  CCTKi_ScheduleFunction((void *)Time_Simple,
                        "Time_Simple",
                        "Time",
                        "time",
                        "Set timestep based on Courant condition",
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


}

else if (CCTK_Equals (timestep_method, "courant_speed") ||

         CCTK_Equals (timestep_method, "courant_time"))

{

  CCTKi_ScheduleFunction((void *)Time_Simple,
                        "Time_Simple",
                        "Time",
                        "time",
                        "Set timestep based on Courant condition",
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


  CCTKi_ScheduleFunction((void *)Time_Courant,
                        "Time_Courant",
                        "Time",
                        "time",
                        "Reset timestep each iteration",
                        "CCTK_PRESTEP",
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

else

{

  CCTKi_ScheduleFunction((void *)Time_Given,
                        "Time_Given",
                        "Time",
                        "time",
                        "Set fixed timestep",
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


}


USE_CCTK_PARAMETERS; return;
}
