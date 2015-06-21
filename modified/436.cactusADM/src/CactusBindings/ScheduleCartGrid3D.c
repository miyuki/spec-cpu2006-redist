#ifdef SPEC_CPU
# define THORN_IS_CactusBindings
#endif /* SPEC_CPU */
#define THORN_IS_CartGrid3D

#include <stdarg.h>

#include "cctk.h"
#include "cctk_Parameters.h"
#include "cctki_ScheduleBindings.h"

/* Prototypes for functions to be registered. */
extern int SymmetryStartup(void); /* Note that this is a cheat, we just need a function pointer. */
extern int RegisterCartGrid3DCoords(void); /* Note that this is a cheat, we just need a function pointer. */
extern int ParamCheck_CartGrid3D(void); /* Note that this is a cheat, we just need a function pointer. */
extern int CartGrid3D(void); /* Note that this is a cheat, we just need a function pointer. */


/*@@
  @routine    CCTKi_BindingsSchedule_CartGrid3D
  @date       
  @author     
  @desc 
  Creates the schedule bindings for thorn CartGrid3D
  @enddesc 
  @calls     
  @calledby   
  @history 

  @endhistory

@@*/
void CCTKi_BindingsSchedule_CartGrid3D(void);
void CCTKi_BindingsSchedule_CartGrid3D(void)
{
  DECLARE_CCTK_PARAMETERS
  CCTKi_ScheduleGroupStorage( "grid::coordinates");
  CCTKi_ScheduleGroupStorage( "grid::gridspacings");
  CCTKi_ScheduleGroupComm( "grid::coordinates");
  CCTKi_ScheduleGroupComm( "grid::gridspacings");

  CCTKi_ScheduleFunction((void *)SymmetryStartup,
                        "SymmetryStartup",
                        "CartGrid3D",
                        "grid",
                        "Register GH Extension for GridSymmetry",
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


  CCTKi_ScheduleFunction((void *)RegisterCartGrid3DCoords,
                        "RegisterCartGrid3DCoords",
                        "CartGrid3D",
                        "grid",
                        "Register coordinates for the Cartesian grid",
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


  CCTKi_ScheduleFunction((void *)ParamCheck_CartGrid3D,
                        "ParamCheck_CartGrid3D",
                        "CartGrid3D",
                        "grid",
                        "Check coordinates for CartGrid3D",
                        "CCTK_PARAMCHECK",
                        "C",
                        0,                       /* Number of STORAGE  groups   */
                        0,                      /* Number of COMM     groups   */
                        0,                   /* Number of TRIGGERS groups   */
                        0,                      /* Number of SYNC     groups   */
                        0,                          /* Number of Options           */
                        0,                      /* Number of BEFORE  routines  */
                        0,                       /* Number of AFTER   routines  */
                        0                        /* Number of WHILE   variables */);


  CCTKi_ScheduleFunction((void *)CartGrid3D,
                        "SpatialCoordinates",
                        "CartGrid3D",
                        "grid",
                        "Set up spatial 3D Cartesian coordinates on the GH",
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



USE_CCTK_PARAMETERS; return;
}
