#ifdef SPEC_CPU
# define THORN_IS_CactusBindings
#endif /* SPEC_CPU */
#define THORN_IS_BenchADM

#include <stdarg.h>

#include "cctk.h"
#include "cctk_Parameters.h"
#include "cctki_ScheduleBindings.h"

/* Prototypes for functions to be registered. */
extern int Bench_ParamCheck(void); /* Note that this is a cheat, we just need a function pointer. */
extern int BenchRegisterSlicing(void); /* Note that this is a cheat, we just need a function pointer. */
extern int bench_preloop_(void); /* Note that this is a cheat, we just need a function pointer. */
extern int bench_staggeredleapfrog1a_ts_(void); /* Note that this is a cheat, we just need a function pointer. */
extern int bench_staggeredleapfrog1a_(void); /* Note that this is a cheat, we just need a function pointer. */
extern int bench_staggeredleapfrog2_(void); /* Note that this is a cheat, we just need a function pointer. */


/*@@
  @routine    CCTKi_BindingsSchedule_BenchADM
  @date       
  @author     
  @desc 
  Creates the schedule bindings for thorn BenchADM
  @enddesc 
  @calls     
  @calledby   
  @history 

  @endhistory

@@*/
void CCTKi_BindingsSchedule_BenchADM(void);
void CCTKi_BindingsSchedule_BenchADM(void)
{
  DECLARE_CCTK_PARAMETERS
  CCTKi_ScheduleFunction((void *)Bench_ParamCheck,
                        "Bench_ParamCheck",
                        "BenchADM",
                        "benchadm",
                        "Check parameters",
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


if (CCTK_Equals(evolution_system,"ADM"))

{

  CCTKi_ScheduleGroupStorage( "BenchADM::ADM_metric_prev");
  CCTKi_ScheduleGroupComm( "BenchADM::ADM_metric_prev");

  CCTKi_ScheduleGroupStorage( "BenchADM::ADM_curv_stag");
  CCTKi_ScheduleGroupComm( "BenchADM::ADM_curv_stag");

  CCTKi_ScheduleFunction((void *)BenchRegisterSlicing,
                        "BenchRegisterSlicing",
                        "BenchADM",
                        "benchadm",
                        "Register slicings",
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


  CCTKi_ScheduleFunction((void *)bench_preloop_,
                        "Bench_PreLoop",
                        "BenchADM",
                        "benchadm",
                        "Setup for ADM",
                        "CCTK_INITIAL",
                        "Fortran",
                        0,                       /* Number of STORAGE  groups   */
                        0,                      /* Number of COMM     groups   */
                        0,                   /* Number of TRIGGERS groups   */
                        0,                      /* Number of SYNC     groups   */
                        0,                          /* Number of Options           */
                        0,                      /* Number of BEFORE  routines  */
                        1,                       /* Number of AFTER   routines  */
                        0                        /* Number of WHILE   variables */,
                        "InitialEinstein");


  if (time_symmetric)

  {

  CCTKi_ScheduleFunction((void *)bench_staggeredleapfrog1a_ts_,
                        "Bench_StaggeredLeapfrog1a_TS",
                        "BenchADM",
                        "benchadm",
                        "Time symmetric initial data for staggered leapfrog",
                        "CCTK_INITIAL",
                        "Fortran",
                        0,                       /* Number of STORAGE  groups   */
                        0,                      /* Number of COMM     groups   */
                        0,                   /* Number of TRIGGERS groups   */
                        0,                      /* Number of SYNC     groups   */
                        0,                          /* Number of Options           */
                        0,                      /* Number of BEFORE  routines  */
                        5,                       /* Number of AFTER   routines  */
                        0                        /* Number of WHILE   variables */,
                        "planewaves",
                        "teukwaves",
                        "InitialFlat",
                        "LapseOne",
                        "LapseGaussian");


  }

  else

  {

  CCTKi_ScheduleFunction((void *)bench_staggeredleapfrog1a_,
                        "Bench_StaggeredLeapfrog1a",
                        "BenchADM",
                        "benchadm",
                        "Time asymmetric initial data for staggered leapfrog",
                        "CCTK_INITIAL",
                        "Fortran",
                        1,                       /* Number of STORAGE  groups   */
                        1,                      /* Number of COMM     groups   */
                        0,                   /* Number of TRIGGERS groups   */
                        0,                      /* Number of SYNC     groups   */
                        0,                          /* Number of Options           */
                        0,                      /* Number of BEFORE  routines  */
                        5,                       /* Number of AFTER   routines  */
                        0                        /* Number of WHILE   variables */,
                        "BenchADM::ADM_sources",
                        "BenchADM::ADM_sources",
                        "planewaves",
                        "teukwaves",
                        "InitialFlat",
                        "LapseOne",
                        "LapseGaussian");


  }

  CCTKi_ScheduleFunction((void *)bench_staggeredleapfrog2_,
                        "Bench_StaggeredLeapfrog2",
                        "BenchADM",
                        "benchadm",
                        "Evolve using Staggered Leapfrog",
                        "CCTK_EVOL",
                        "Fortran",
                        0,                       /* Number of STORAGE  groups   */
                        0,                      /* Number of COMM     groups   */
                        0,                   /* Number of TRIGGERS groups   */
                        1,                      /* Number of SYNC     groups   */
                        0,                          /* Number of Options           */
                        0,                      /* Number of BEFORE  routines  */
                        0,                       /* Number of AFTER   routines  */
                        0                        /* Number of WHILE   variables */,
                        "BenchADM::ADM_curv_stag");


}


USE_CCTK_PARAMETERS; return;
}
