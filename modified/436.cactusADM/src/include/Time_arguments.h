#ifdef FCODE
#define DECLARE_TIME_PRIVATE_FARGUMENTS \
CCTK_REAL courant_dt&&\


#define TIME_PRIVATE_FARGUMENTS \
courant_dt


#endif /*FCODE*/


#ifdef CCODE
#define DECLARE_TIME_PRIVATE_CARGUMENTS \
CCTK_REAL *courant_dt=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("Time::courant_dt")][0]); \


#define USE_TIME_PRIVATE_CARGUMENTS \
cctk_dummy_pointer = &courant_dt; \


#define DECLARE_TIME_PRIVATE_C2F \
static int CCTKARGNUM_courant_dt = -1; \
static int CCTKGROUPNUM_couranttemps = -1; \


#define INITIALISE_TIME_PRIVATE_C2F \
if(CCTKARGNUM_courant_dt == -1) CCTKARGNUM_courant_dt = CCTK_VarIndex("Time::courant_dt"); \
if(CCTKGROUPNUM_couranttemps == -1) CCTKGROUPNUM_couranttemps = CCTK_GroupIndex("Time::couranttemps"); \


#define TIME_PRIVATE_C2F_PROTO \
CCTK_REAL *


#define PASS_TIME_PRIVATE_C2F(xGH) \
(CCTK_REAL *)(CCTKARGNUM_courant_dt<0 ? NULL : (xGH)->data[CCTKARGNUM_courant_dt][1-1])


#endif /*CCODE*/


#ifdef FCODE
#define DECLARE_TIME_PROTECTED_FARGUMENTS \


#define TIME_PROTECTED_FARGUMENTS \



#endif /*FCODE*/


#ifdef CCODE
#define DECLARE_TIME_PROTECTED_CARGUMENTS \


#define USE_TIME_PROTECTED_CARGUMENTS \


#define DECLARE_TIME_PROTECTED_C2F \


#define INITIALISE_TIME_PROTECTED_C2F \


#define TIME_PROTECTED_C2F_PROTO \



#define PASS_TIME_PROTECTED_C2F(xGH) \



#endif /*CCODE*/


#ifdef FCODE
#define DECLARE_TIME_PUBLIC_FARGUMENTS \
CCTK_REAL courant_min_time&&\
CCTK_REAL courant_wave_speed&&\


#define TIME_PUBLIC_FARGUMENTS \
courant_min_time,courant_wave_speed


#endif /*FCODE*/


#ifdef CCODE
#define DECLARE_TIME_PUBLIC_CARGUMENTS \
CCTK_REAL *courant_min_time=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("time::courant_min_time")][0]); \
CCTK_REAL *courant_wave_speed=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("time::courant_wave_speed")][0]); \


#define USE_TIME_PUBLIC_CARGUMENTS \
cctk_dummy_pointer = &courant_min_time; \
cctk_dummy_pointer = &courant_wave_speed; \


#define DECLARE_TIME_PUBLIC_C2F \
static int CCTKARGNUM_courant_min_time = -1; \
static int CCTKGROUPNUM_speedvars = -1; \
static int CCTKARGNUM_courant_wave_speed = -1; \


#define INITIALISE_TIME_PUBLIC_C2F \
if(CCTKARGNUM_courant_min_time == -1) CCTKARGNUM_courant_min_time = CCTK_VarIndex("time::courant_min_time"); \
if(CCTKGROUPNUM_speedvars == -1) CCTKGROUPNUM_speedvars = CCTK_GroupIndex("time::speedvars"); \
if(CCTKARGNUM_courant_wave_speed == -1) CCTKARGNUM_courant_wave_speed = CCTK_VarIndex("time::courant_wave_speed"); \


#define TIME_PUBLIC_C2F_PROTO \
CCTK_REAL *,CCTK_REAL *


#define PASS_TIME_PUBLIC_C2F(xGH) \
(CCTK_REAL *)(CCTKARGNUM_courant_min_time<0 ? NULL : (xGH)->data[CCTKARGNUM_courant_min_time][1-1]),(CCTK_REAL *)(CCTKARGNUM_courant_wave_speed<0 ? NULL : (xGH)->data[CCTKARGNUM_courant_wave_speed][1-1])


#endif /*CCODE*/


#ifdef FCODE
#define TIME_FARGUMENTS _CCTK_FARGUMENTS\
,TIME_PRIVATE_FARGUMENTS\
,TIME_PUBLIC_FARGUMENTS\


#define DECLARE_TIME_FARGUMENTS _DECLARE_CCTK_FARGUMENTS \
DECLARE_TIME_PRIVATE_FARGUMENTS \
DECLARE_TIME_PUBLIC_FARGUMENTS \


#endif /*FCODE*/


#ifdef CCODE
#define DECLARE_TIME_CARGUMENTS _DECLARE_CCTK_CARGUMENTS \
DECLARE_TIME_PRIVATE_CARGUMENTS \
DECLARE_TIME_PUBLIC_CARGUMENTS \


#define USE_TIME_CARGUMENTS _USE_CCTK_CARGUMENTS \
USE_TIME_PRIVATE_CARGUMENTS \
USE_TIME_PUBLIC_CARGUMENTS \


#endif /*CCODE*/


#ifdef CCODE
#define TIME_C2F_PROTO _CCTK_C2F_PROTO\
,TIME_PRIVATE_C2F_PROTO\
,TIME_PUBLIC_C2F_PROTO\


#define PASS_TIME_C2F(xGH) _PASS_CCTK_C2F(xGH)\
,PASS_TIME_PRIVATE_C2F(xGH)\
,PASS_TIME_PUBLIC_C2F(xGH)\


#define DECLARE_TIME_C2F _DECLARE_CCTK_C2F \
DECLARE_TIME_PRIVATE_C2F \
DECLARE_TIME_PUBLIC_C2F \


#define INITIALISE_TIME_C2F _INITIALISE_CCTK_C2F \
INITIALISE_TIME_PRIVATE_C2F \
INITIALISE_TIME_PUBLIC_C2F \


#define TIME_CARGUMENTS cGH *cctkGH 


#endif /*CCODE*/


