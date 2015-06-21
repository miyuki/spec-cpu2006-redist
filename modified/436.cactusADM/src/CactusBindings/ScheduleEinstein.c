#ifdef SPEC_CPU
# define THORN_IS_CactusBindings
#endif /* SPEC_CPU */
#define THORN_IS_Einstein

#include <stdarg.h>

#include "cctk.h"
#include "cctk_Parameters.h"
#include "cctki_ScheduleBindings.h"

/* Prototypes for functions to be registered. */
extern int MaskOne(void); /* Note that this is a cheat, we just need a function pointer. */
extern int Einstein_InitSymBound(void); /* Note that this is a cheat, we just need a function pointer. */
extern int Einstein_ActivateSlicing(void); /* Note that this is a cheat, we just need a function pointer. */
extern int InitialEinstein(void); /* Note that this is a cheat, we just need a function pointer. */
extern int InitialFlat(void); /* Note that this is a cheat, we just need a function pointer. */
extern int LapseOne(void); /* Note that this is a cheat, we just need a function pointer. */
extern int LapseGaussian(void); /* Note that this is a cheat, we just need a function pointer. */
extern int LapsePsiMinusTwo(void); /* Note that this is a cheat, we just need a function pointer. */
extern int LapseIsotropic(void); /* Note that this is a cheat, we just need a function pointer. */
extern int ShiftZero(void); /* Note that this is a cheat, we just need a function pointer. */
extern int ShiftRotation(void); /* Note that this is a cheat, we just need a function pointer. */
extern int Einstein_SetNextSlicing(void); /* Note that this is a cheat, we just need a function pointer. */
extern int evaltrK(void); /* Note that this is a cheat, we just need a function pointer. */
extern int metric_carttosphere(void); /* Note that this is a cheat, we just need a function pointer. */
extern int curv_carttosphere(void); /* Note that this is a cheat, we just need a function pointer. */
extern int CalcCourant(void); /* Note that this is a cheat, we just need a function pointer. */


/*@@
  @routine    CCTKi_BindingsSchedule_Einstein
  @date       
  @author     
  @desc 
  Creates the schedule bindings for thorn Einstein
  @enddesc 
  @calls     
  @calledby   
  @history 

  @endhistory

@@*/
void CCTKi_BindingsSchedule_Einstein(void);
void CCTKi_BindingsSchedule_Einstein(void)
{
  DECLARE_CCTK_PARAMETERS
  CCTKi_ScheduleGroupStorage( "einstein::metric");
  CCTKi_ScheduleGroupStorage( "einstein::curv");
  CCTKi_ScheduleGroupStorage( "einstein::lapse");
  CCTKi_ScheduleGroupStorage( "einstein::flags");
  CCTKi_ScheduleGroupStorage( "einstein::slicing_flags");
  CCTKi_ScheduleGroupComm( "einstein::metric");
  CCTKi_ScheduleGroupComm( "einstein::curv");
  CCTKi_ScheduleGroupComm( "einstein::lapse");
  CCTKi_ScheduleGroupComm( "einstein::flags");
  CCTKi_ScheduleGroupComm( "einstein::slicing_flags");

if (!CCTK_Equals(shift,"none")) 

{

  CCTKi_ScheduleGroupStorage( "einstein::shift");
  CCTKi_ScheduleGroupComm( "einstein::shift");

}

if (use_conformal) {

  CCTKi_ScheduleGroupStorage( "einstein::confac");
  CCTKi_ScheduleGroupComm( "einstein::confac");

}

if (use_conformal_derivs) {

  CCTKi_ScheduleGroupStorage( "einstein::confac_1derivs");
  CCTKi_ScheduleGroupStorage( "einstein::confac_2derivs");
  CCTKi_ScheduleGroupComm( "einstein::confac_1derivs");
  CCTKi_ScheduleGroupComm( "einstein::confac_2derivs");

}

if (use_mask) {

  CCTKi_ScheduleGroupStorage( "einstein::mask");
  CCTKi_ScheduleGroupComm( "einstein::mask");

  CCTKi_ScheduleFunction((void *)MaskOne,
                        "MaskOne",
                        "Einstein",
                        "einstein",
                        "Set mask to one",
                        "CCTK_INITIAL",
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

  CCTKi_ScheduleFunction((void *)Einstein_InitSymBound,
                        "Einstein_InitSymBound",
                        "Einstein",
                        "einstein",
                        "Set up GF symmetries",
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


if (einstein_register_slicing)

{

  CCTKi_ScheduleFunction((void *)Einstein_ActivateSlicing,
                        "Einstein_ActivateSlicing",
                        "Einstein",
                        "einstein",
                        "Initialize slicing, setup priorities for mixed slicings",
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

  CCTKi_ScheduleFunction((void *)InitialEinstein,
                        "InitialEinstein",
                        "Einstein",
                        "einstein",
                        "Initialisation for Einstein methods",
                        "CCTK_INITIAL",
                        "C",
                        0,                       /* Number of STORAGE  groups   */
                        0,                      /* Number of COMM     groups   */
                        0,                   /* Number of TRIGGERS groups   */
                        0,                      /* Number of SYNC     groups   */
                        0,                          /* Number of Options           */
                        0,                      /* Number of BEFORE  routines  */
                        0,                       /* Number of AFTER   routines  */
                        0                        /* Number of WHILE   variables */);


if (CCTK_Equals(initial_data,"flat")) 

{

  CCTKi_ScheduleFunction((void *)InitialFlat,
                        "InitialFlat",
                        "Einstein",
                        "einstein",
                        "Flat initial data",
                        "CCTK_INITIAL",
                        "C",
                        0,                       /* Number of STORAGE  groups   */
                        0,                      /* Number of COMM     groups   */
                        0,                   /* Number of TRIGGERS groups   */
                        0,                      /* Number of SYNC     groups   */
                        0,                          /* Number of Options           */
                        0,                      /* Number of BEFORE  routines  */
                        1,                       /* Number of AFTER   routines  */
                        0                        /* Number of WHILE   variables */,
                        "InitialEinstein");


}

if (CCTK_Equals(initial_lapse,"one") || CCTK_Equals(slicing,"geodesic"))

{

  CCTKi_ScheduleFunction((void *)LapseOne,
                        "LapseOne",
                        "Einstein",
                        "einstein",
                        "Set initial lapse to one",
                        "CCTK_INITIAL",
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

if (CCTK_Equals(initial_lapse,"gaussian"))

{

  CCTKi_ScheduleFunction((void *)LapseGaussian,
                        "LapseGaussian",
                        "Einstein",
                        "einstein",
                        "Set initial lapse to a gaussian",
                        "CCTK_INITIAL",
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

if (CCTK_Equals(initial_lapse,"psiminustwo"))

{

  CCTKi_ScheduleFunction((void *)LapsePsiMinusTwo,
                        "LapsePsiMinusTwo",
                        "Einstein",
                        "einstein",
                        "Set initial lapse to psi to the minus two",
                        "CCTK_INITIAL",
                        "C",
                        0,                       /* Number of STORAGE  groups   */
                        0,                      /* Number of COMM     groups   */
                        0,                   /* Number of TRIGGERS groups   */
                        0,                      /* Number of SYNC     groups   */
                        0,                          /* Number of Options           */
                        0,                      /* Number of BEFORE  routines  */
                        3,                       /* Number of AFTER   routines  */
                        0                        /* Number of WHILE   variables */,
                        "BiBBAM_InitialWrapper",
                        "IDAxiBrillBH",
                        "Schwarzschild");


}

if (CCTK_Equals(initial_lapse,"isotropic"))

{

  CCTKi_ScheduleFunction((void *)LapseIsotropic,
                        "LapseIsotropic",
                        "Einstein",
                        "einstein",
                        "Set initial lapse to isotropic lapse",
                        "CCTK_INITIAL",
                        "C",
                        0,                       /* Number of STORAGE  groups   */
                        0,                      /* Number of COMM     groups   */
                        0,                   /* Number of TRIGGERS groups   */
                        0,                      /* Number of SYNC     groups   */
                        0,                          /* Number of Options           */
                        0,                      /* Number of BEFORE  routines  */
                        3,                       /* Number of AFTER   routines  */
                        0                        /* Number of WHILE   variables */,
                        "BiBBAM_InitialWrapper",
                        "IDAxiBrillBH",
                        "Schwarzschild");


}

if (!CCTK_Equals(shift,"none"))

{

   if (CCTK_Equals(initial_shift,"zero"))

   {

  CCTKi_ScheduleFunction((void *)ShiftZero,
                        "ShiftZero",
                        "Einstein",
                        "einstein",
                        "Set initial shift to zero",
                        "CCTK_INITIAL",
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

   if (CCTK_Equals(initial_shift,"rotation"))

   {

  CCTKi_ScheduleFunction((void *)ShiftRotation,
                        "ShiftRotation",
                        "Einstein",
                        "einstein",
                        "Set initial shift to rigid rotation",
                        "CCTK_INITIAL",
                        "C",
                        0,                       /* Number of STORAGE  groups   */
                        0,                      /* Number of COMM     groups   */
                        0,                   /* Number of TRIGGERS groups   */
                        0,                      /* Number of SYNC     groups   */
                        0,                          /* Number of Options           */
                        0,                      /* Number of BEFORE  routines  */
                        3,                       /* Number of AFTER   routines  */
                        0                        /* Number of WHILE   variables */,
                        "BiBBAM_InitialWrapper",
                        "IDAxiBrillBH",
                        "Schwarzschild");


   }

}

if (einstein_register_slicing)

{

  CCTKi_ScheduleFunction((void *)Einstein_SetNextSlicing,
                        "Einstein_SetNextSlicing",
                        "Einstein",
                        "einstein",
                        "Identify the slicing for the next iteration",
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

  CCTKi_ScheduleFunction((void *)evaltrK,
                        "evaltrK",
                        "Einstein",
                        "einstein",
                        "Compute the trace of the extrinsic curvature",
                        "CCTK_ANALYSIS",
                        "C",
                        2,                       /* Number of STORAGE  groups   */
                        2,                      /* Number of COMM     groups   */
                        2,                   /* Number of TRIGGERS groups   */
                        0,                      /* Number of SYNC     groups   */
                        0,                          /* Number of Options           */
                        0,                      /* Number of BEFORE  routines  */
                        0,                       /* Number of AFTER   routines  */
                        0                        /* Number of WHILE   variables */,
                        "Einstein::trace_of_K",
                        "Einstein::detofg",
                        "Einstein::trace_of_K",
                        "Einstein::detofg",
                        "Einstein::trace_of_K",
                        "Einstein::detofg");


  CCTKi_ScheduleFunction((void *)metric_carttosphere,
                        "metric_carttosphere",
                        "Einstein",
                        "einstein",
                        "Calculate the spherical metric in r,theta(q), phi(p)",
                        "CCTK_ANALYSIS",
                        "C",
                        1,                       /* Number of STORAGE  groups   */
                        1,                      /* Number of COMM     groups   */
                        1,                   /* Number of TRIGGERS groups   */
                        0,                      /* Number of SYNC     groups   */
                        0,                          /* Number of Options           */
                        0,                      /* Number of BEFORE  routines  */
                        0,                       /* Number of AFTER   routines  */
                        0                        /* Number of WHILE   variables */,
                        "Einstein::spherical_metric",
                        "Einstein::spherical_metric",
                        "Einstein::spherical_metric");


  CCTKi_ScheduleFunction((void *)curv_carttosphere,
                        "curv_carttosphere",
                        "Einstein",
                        "einstein",
                        "Calculate the spherical ex. curvature in r, theta(q), phi(p)",
                        "CCTK_ANALYSIS",
                        "C",
                        1,                       /* Number of STORAGE  groups   */
                        1,                      /* Number of COMM     groups   */
                        1,                   /* Number of TRIGGERS groups   */
                        0,                      /* Number of SYNC     groups   */
                        0,                          /* Number of Options           */
                        0,                      /* Number of BEFORE  routines  */
                        0,                       /* Number of AFTER   routines  */
                        0                        /* Number of WHILE   variables */,
                        "Einstein::spherical_curv",
                        "Einstein::spherical_curv",
                        "Einstein::spherical_curv");


if (CCTK_Equals(timestep_method,"courant") || CCTK_Equals(timestep_method,"courant_time")) 

{

  CCTKi_ScheduleFunction((void *)CalcCourant,
                        "CalcCourant",
                        "Einstein",
                        "einstein",
                        "Calculate the wavespeed for the Courant condition",
                        "CCTK_PRESTEP",
                        "C",
                        0,                       /* Number of STORAGE  groups   */
                        0,                      /* Number of COMM     groups   */
                        0,                   /* Number of TRIGGERS groups   */
                        0,                      /* Number of SYNC     groups   */
                        0,                          /* Number of Options           */
                        1,                      /* Number of BEFORE  routines  */
                        0,                       /* Number of AFTER   routines  */
                        0                        /* Number of WHILE   variables */,
                        "Time_Simple");


}


USE_CCTK_PARAMETERS; return;
}
