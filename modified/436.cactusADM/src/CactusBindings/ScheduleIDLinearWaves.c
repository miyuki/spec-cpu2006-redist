#ifdef SPEC_CPU
# define THORN_IS_CactusBindings
#endif /* SPEC_CPU */
#define THORN_IS_IDLinearWaves

#include <stdarg.h>

#include "cctk.h"
#include "cctk_Parameters.h"
#include "cctki_ScheduleBindings.h"

/* Prototypes for functions to be registered. */
extern int planewaves_(void); /* Note that this is a cheat, we just need a function pointer. */
extern int teukwaves_(void); /* Note that this is a cheat, we just need a function pointer. */


/*@@
  @routine    CCTKi_BindingsSchedule_IDLinearWaves
  @date       
  @author     
  @desc 
  Creates the schedule bindings for thorn IDLinearWaves
  @enddesc 
  @calls     
  @calledby   
  @history 

  @endhistory

@@*/
void CCTKi_BindingsSchedule_IDLinearWaves(void);
void CCTKi_BindingsSchedule_IDLinearWaves(void)
{
  DECLARE_CCTK_PARAMETERS
if (CCTK_Equals(initial_data,"planewaves")) 

{

  CCTKi_ScheduleFunction((void *)planewaves_,
                        "planewaves",
                        "IDLinearWaves",
                        "idlinearwaves",
                        "Construct linear planewave initial data",
                        "CCTK_INITIAL",
                        "Fortran",
                        0,                       /* Number of STORAGE  groups   */
                        0,                      /* Number of COMM     groups   */
                        0,                   /* Number of TRIGGERS groups   */
                        0,                      /* Number of SYNC     groups   */
                        0,                          /* Number of Options           */
                        0,                      /* Number of BEFORE  routines  */
                        0,                       /* Number of AFTER   routines  */
                        0                        /* Number of WHILE   variables */);


}

if (CCTK_Equals(initial_data,"teukwaves")) 

{

  CCTKi_ScheduleFunction((void *)teukwaves_,
                        "teukwaves",
                        "IDLinearWaves",
                        "idlinearwaves",
                        "Construct linear Teukolsky wave initial data",
                        "CCTK_INITIAL",
                        "Fortran",
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
