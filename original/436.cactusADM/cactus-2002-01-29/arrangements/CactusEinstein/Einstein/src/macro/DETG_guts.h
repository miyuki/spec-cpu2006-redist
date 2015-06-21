/*
DETG_guts.h

Macro to calculate the determinants of the conformal and
physical 3-metric

Gabrielle Allen, 5th November 1998

*/

#ifndef DETG_GUTS
#define DETG_GUTS

#ifdef FCODE

      IF (conformal_state /= CONFORMAL_METRIC) THEN
         DETG_PSI4 = 1.0d0
      ELSE
         DETG_PSI4 = DETG_PSI**4
      ENDIF

      DETG_TEMPXX =  DETG_GYY*DETG_GZZ - DETG_GYZ*DETG_GYZ
      DETG_TEMPXY =  DETG_GXZ*DETG_GYZ - DETG_GXY*DETG_GZZ
      DETG_TEMPXZ = -DETG_GXZ*DETG_GYY + DETG_GXY*DETG_GYZ
      DETG_TEMPYY =  DETG_GXX*DETG_GZZ - DETG_GXZ*DETG_GXZ
      DETG_TEMPYZ =  DETG_GXY*DETG_GXZ - DETG_GXX*DETG_GYZ
      DETG_TEMPZZ =  DETG_GXX*DETG_GYY - DETG_GXY*DETG_GXY

/* This is the conformal determinant */
      DETG_DETCG = (DETG_TEMPXX*DETG_GXX + 
     &    DETG_TEMPXY*DETG_GXY + DETG_TEMPXZ*DETG_GXZ)

/* This is the physical determinat */
      DETG_DETG = DETG_PSI4**3*DETG_DETCG

#endif

#ifdef CCODE

#define Cal(x,y,z) ((x)?(y):(z))
#define Power(x,y) (pow((CCTK_REAL) (x), (CCTK_REAL) (y)))

DETG_PSI4 = ((*conformal_state != CONFORMAL_METRIC)?1:DETG_PSI*DETG_PSI*DETG_PSI*DETG_PSI);

DETG_TEMPXX =  DETG_GYY*DETG_GZZ - DETG_GYZ*DETG_GYZ;
DETG_TEMPXY =  DETG_GXZ*DETG_GYZ - DETG_GXY*DETG_GZZ;
DETG_TEMPXZ = -DETG_GXZ*DETG_GYY + DETG_GXY*DETG_GYZ;
DETG_TEMPYY =  DETG_GXX*DETG_GZZ - DETG_GXZ*DETG_GXZ;
DETG_TEMPYZ =  DETG_GXY*DETG_GXZ - DETG_GXX*DETG_GYZ;
DETG_TEMPZZ =  DETG_GXX*DETG_GYY - DETG_GXY*DETG_GXY;

/* This is the conformal determinant */
DETG_DETCG = (DETG_TEMPXX*DETG_GXX + 
       DETG_TEMPXY*DETG_GXY + DETG_TEMPXZ*DETG_GXZ);

/* This is the physical determinat */
DETG_DETG = DETG_PSI4*DETG_PSI4*DETG_PSI4*DETG_DETCG;

#endif

#endif
