#ifdef SPEC_CPU
# define THORN_IS_Boundary
#endif /* SPEC_CPU */
 /*@@
   @file      RadiationBoundary.c
   @date      Mon Mar 15 15:09:00 1999
   @author    Miguel Alcubierre, Gabrielle Allen, Gerd Lanfermann
   @desc
              <PRE>
              Routines for applying radiation boundary conditions

              The radiative boundary condition that is implemented is:

                f  =  f0  +  u(r - v*t) / r  +  h(r + v*t) / r

              That is, I assume outgoing radial waves with a 1/r
              fall off, and the correct asymptotic value f0, plus
              I include the possibility of incoming waves as well
              (these incoming waves should be modeled somehow).

              The condition above leads to the differential equation:

                (x / r) d f  +  v d f  + v x (f - f0) / r^2  =  v x H / r^2
                  i      t         i        i                      i

              where x_i is the normal direction to the given boundaries,
              and H = 2 dh(s)/ds.

              So at a given boundary I only worry about derivatives in
              the normal direction.  Notice that u(r-v*t) has dissapeared,
              but we still do not know the value of H.

              To get H what I do is the following:  I evaluate the
              expression one point in from the boundary and solve for H
              there.  We now need a way of extrapolation H to the boundary.
              For this I assume that H falls off as a power law:

                H = k/r**n   =>  d H  =  - n H/r
                                  i

              The value of n is is defined by the parameter "radpower".
              If this parameter is negative, H is forced to be zero (this
              corresponds to pure outgoing waves and is the default).
              <P>
              The behaviour I have observed is the following:  Using H=0
              is very stable, but has a very bad initial transient. Taking
              n to be 0 or positive improves the initial behaviour considerably,
              but introduces a drift that can kill the evolution at very late
              times.  Empirically, the best value I have found is n=2, for
              which the initial behaviour is very nice, and the late time drift
              is quite small.

              Another problem with this condition is that it does not
              use the physical characteristic speed, but rather it assumes
              a wave speed of v, so the boundaries should be out in
              the region where the characteristic speed is constant.
              Notice that this speed does not have to be 1.  For gauge
              quantities {alpha,phi,trK} we can have a different asymptotic
              speed, which is why the value of v is passed as a parameter.
              </PRE>
   @enddesc
   @history
   @hdate     unknown
   @hauthor   Gerd Lanfermann
   @hdesc     Ported to Cactus 4.0
   @hdate     Fri 6 Apr 2001
   @hauthor   Thomas Radke
   @hdesc     BC routines generalized for applying to arbitrary CCTK data types
   @endhistory
   @version   $Id: RadiationBoundary.c,v 1.27 2001/10/10 10:06:28 miguel Exp $
 @@*/

#include <stdlib.h>
#include <string.h>

#include "cctk.h"
#include "cctk_FortranString.h"
#include "cctk_Parameters.h"

#include "Symmetry.h"
#include "Boundary.h"

/* the rcs ID and its dummy function to use it */
static const char *rcsid = "$Header: /cactus/CactusBase/Boundary/src/RadiationBoundary.c,v 1.27 2001/10/10 10:06:28 miguel Exp $";
CCTK_FILEVERSION(CactusBase_Boundary_RadiationBoundary_c)


/********************************************************************
 ********************    External Routines   ************************
 ********************************************************************/
/* prototypes for external C routines are declared in header Boundary.h
   here only follow the fortran wrapper prototypes */
void CCTK_FCALL bndradiativedirgi_
                           (int *ierr,
                            const cGH *GH,
                            const int *stencil_size,
                            const int *dir,
                            const CCTK_REAL *var0,
                            const CCTK_REAL *speed,
                            const int *gi_to,
                            const int *gi_from);
void CCTK_FCALL bndradiativegi_
                           (int *ierr,
                            const cGH *GH,
                            const int *stencil,
                            const CCTK_REAL *var0,
                            const CCTK_REAL *speed,
                            const int *gi_to,
                            const int *gi_from);
void CCTK_FCALL bndradiativedirgn_
                           (int *ierr,
                            const cGH *GH,
                            const int *stencil_size,
                            const int *dir,
                            const CCTK_REAL *var0,
                            const CCTK_REAL *speed,
                            TWO_FORTSTRING_ARG);
void CCTK_FCALL bndradiativegn_
                           (int *ierr,
                            const cGH *GH,
                            const int *stencil,
                            const CCTK_REAL *var0,
                            const CCTK_REAL *speed,
                            TWO_FORTSTRING_ARG);
void CCTK_FCALL bndradiativedirvi_
                           (int *ierr,
                            const cGH *GH,
                            const int *stencil_size,
                            const int *dir,
                            const CCTK_REAL *var0,
                            const CCTK_REAL *speed,
                            const int *vi_to,
                            const int *vi_from);
void CCTK_FCALL bndradiativevi_
                           (int *ierr,
                            const cGH *GH,
                            const int *stencil,
                            const CCTK_REAL *var0,
                            const CCTK_REAL *speed,
                            const int *vi_to,
                            const int *vi_from);
void CCTK_FCALL bndradiativedirvn_
                           (int *ierr,
                            const cGH *GH,
                            const int *stencil_size,
                            const int *dir,
                            const CCTK_REAL *var0,
                            const CCTK_REAL *speed,
                            TWO_FORTSTRING_ARG);
void CCTK_FCALL bndradiativevn_
                           (int *ierr,
                            const cGH *GH,
                            const int *stencil,
                            const CCTK_REAL *var0,
                            const CCTK_REAL *speed,
                            TWO_FORTSTRING_ARG);


/********************************************************************
 ********************    Internal Routines   ************************
 ********************************************************************/
static int ApplyBndRadiative (const cGH *GH,
                              int stencil_dir,
                              const int *stencil_alldirs,
                              int dir,
                              CCTK_REAL var0,
                              CCTK_REAL speed,
                              int first_var_to,
                              int first_var_from,
                              int num_vars);


/*@@
   @routine    BndRadiativeDirGI
   @date       Sat Jan 20 2001
   @author     Gabrielle Allen
   @desc
               Aply radiative BC's by group index in given direction
   @enddesc
   @calls      ApplyBndRadiative

   @var        GH
   @vdesc      Pointer to CCTK grid hierarchy
   @vtype      const cGH *
   @vio        in
   @endvar
   @var        stencil_size
   @vdesc      stencil size in this direction
   @vtype      int
   @vio        in
   @endvar
   @var        dir
   @vdesc      direction to apply BC
   @vtype      int
   @vio        in
   @endvar
   @var        var0
   @vdesc      asymptotic value of function at infinity
   @vtype      CCTK_REAL
   @vio        in
   @endvar
   @var        speed
   @vdesc      wave speed
   @vtype      CCTK_REAL
   @vio        in
   @endvar
   @var        gi_to
   @vdesc      index of group to apply BC to
   @vtype      int
   @vio        in
   @endvar
   @var        gi_from
   @vdesc      index of group to apply BC from
   @vtype      int
   @vio        in
   @endvar

   @returntype int
   @returndesc
               return code of @seeroutine ApplyBndRadiative <BR>
               -1 if invalid group indices are given
   @endreturndesc
@@*/
int BndRadiativeDirGI (const cGH *GH,
                       int stencil_size,
                       int dir,
                       CCTK_REAL var0,
                       CCTK_REAL speed,
                       int gi_to,
                       int gi_from)
{
  int first_vi_to, first_vi_from, retval;


  first_vi_to   = CCTK_FirstVarIndexI (gi_to);
  first_vi_from = CCTK_FirstVarIndexI (gi_from);
  if (first_vi_to >= 0 && first_vi_from >= 0)
  {
    retval = ApplyBndRadiative (GH, stencil_size, NULL, dir, var0, speed,
                                first_vi_to, first_vi_from,
                                CCTK_NumVarsInGroupI (gi_to));
  }
  else
  {
    CCTK_VWarn (2, __LINE__, __FILE__, CCTK_THORNSTRING,
                "Invalid group indices %d and/or %d in BndRadiativeDirGI",
                gi_to, gi_from);
    retval = -1;
  }

  return (retval);
}

void CCTK_FCALL bndradiativedirgi_
                           (int *ierr,
                            const cGH *GH,
                            const int *stencil_size,
                            const int *dir,
                            const CCTK_REAL *var0,
                            const CCTK_REAL *speed,
                            const int *gi_to,
                            const int *gi_from)
{
  *ierr = BndRadiativeDirGI (GH, *stencil_size, *dir, *var0, *speed,
                             *gi_to, *gi_from);
}


/*@@
   @routine    BndRadiativeGI
   @date       Tue Jul 18 18:04:07 2000
   @author     Gerd Lanfermann
   @desc
               Aply radiative BC's by group index
   @enddesc
   @calls      ApplyBndRadiative

   @var        GH
   @vdesc      Pointer to CCTK grid hierarchy
   @vtype      const cGH *
   @vio        in
   @endvar
   @var        stencil
   @vdesc      stencil width
   @vtype      int [ dimension of group ]
   @vio        in
   @endvar
   @var        var0
   @vdesc      asymptotic value of function at infinity
   @vtype      CCTK_REAL
   @vio        in
   @endvar
   @var        speed
   @vdesc      wave speed
   @vtype      CCTK_REAL
   @vio        in
   @endvar
   @var        gi_to
   @vdesc      index of group to apply BC to
   @vtype      int
   @vio        in
   @endvar
   @var        gi_from
   @vdesc      index of group to apply BC from
   @vtype      int
   @vio        in
   @endvar

   @returntype int
   @returndesc
               return code of @seeroutine ApplyBndRadiative <BR>
               -1 if invalid group indices are given
   @endreturndesc
@@*/
int BndRadiativeGI (const cGH *GH,
                    const int *stencil,
                    CCTK_REAL var0,
                    CCTK_REAL speed,
                    int gi_to,
                    int gi_from)
{
  int first_vi_to, first_vi_from, retval;


  first_vi_to   = CCTK_FirstVarIndexI (gi_to);
  first_vi_from = CCTK_FirstVarIndexI (gi_from);
  if (first_vi_to >= 0 && first_vi_from >= 0)
  {
    retval = ApplyBndRadiative (GH, -1, stencil, 0, var0, speed,
                                first_vi_to, first_vi_from,
                                CCTK_NumVarsInGroupI (gi_to));
  }
  else
  {
    CCTK_VWarn (2, __LINE__, __FILE__, CCTK_THORNSTRING,
                "Invalid group indices %d and/or %d in BndRadiativeGI",
                gi_to, gi_from);
    retval = -1;
  }

  return (retval);
}

void CCTK_FCALL bndradiativegi_
                           (int *ierr,
                            const cGH *GH,
                            const int *stencil,
                            const CCTK_REAL *var0,
                            const CCTK_REAL *speed,
                            const int *gi_to,
                            const int *gi_from)
{
  *ierr = BndRadiativeGI (GH, stencil, *var0, *speed, *gi_to, *gi_from);
}


/* ===================================================================== */

/*@@
   @routine    BndRadiativeDirGN
   @date       Sat Jan 20 2001
   @author     Gabrielle Allen
   @desc
               Apply radiative BC's by group name in given direction
   @enddesc
   @calls      BndRadiativeDirGI

   @var        GH
   @vdesc      Pointer to CCTK grid hierarchy
   @vtype      const cGH *
   @vio        in
   @endvar
   @var        stencil_size
   @vdesc      stencil size in this direction
   @vtype      int
   @vio        in
   @endvar
   @var        dir
   @vdesc      direction to apply BC
   @vtype      int
   @vio        in
   @endvar
   @var        var0
   @vdesc      asymptotic value of function at infinity
   @vtype      CCTK_REAL
   @vio        in
   @endvar
   @var        speed
   @vdesc      wave speed
   @vtype      CCTK_REAL
   @vio        in
   @endvar
   @var        gname_to
   @vdesc      name of group to apply BC to
   @vtype      const char *
   @vio        in
   @endvar
   @var        gname_from
   @vdesc      name of group to apply BC from
   @vtype      const char *
   @vio        in
   @endvar

   @returntype int
   @returndesc
               return code of @seeroutine BndRadiativeDirGI <BR>
               -1 if invalid group names are given
   @endreturndesc
@@*/
int BndRadiativeDirGN (const cGH *GH,
                       int stencil_size,
                       int dir,
                       CCTK_REAL var0,
                       CCTK_REAL speed,
                       const char *gname_to,
                       const char *gname_from)
{
  int gi_to, gi_from, retval;


  gi_to   = CCTK_GroupIndex (gname_to);
  gi_from = CCTK_GroupIndex (gname_from);
  if (gi_to >= 0 && gi_from >= 0)
  {
    retval = BndRadiativeDirGI (GH, stencil_size, dir, var0, speed,
                                gi_to, gi_from);
  }
  else
  {
    CCTK_VWarn (2, __LINE__, __FILE__, CCTK_THORNSTRING,
                "Invalid group names '%s' and/or '%s' in BndRadiativeDirGN",
                gname_to, gname_from);
    retval = -1;
  }

  return (retval);
}

void CCTK_FCALL bndradiativedirgn_
                           (int *ierr,
                            const cGH *GH,
                            const int *stencil_size,
                            const int *dir,
                            const CCTK_REAL *var0,
                            const CCTK_REAL *speed,
                            TWO_FORTSTRING_ARG)
{
  TWO_FORTSTRINGS_CREATE (gname_to, gname_from)
  *ierr = BndRadiativeDirGN (GH, *stencil_size, *dir, *var0, *speed,
                             gname_to, gname_from);
  free (gname_to);
  free (gname_from);
}


/*@@
   @routine    BndRadiativeGN
   @date       Tue Jul 18 18:04:07 2000
   @author     Gerd Lanfermann
   @desc
               Aply radiative BC's by group name
   @enddesc
   @calls      BndRadiativeGI

   @var        GH
   @vdesc      Pointer to CCTK grid hierarchy
   @vtype      const cGH *
   @vio        in
   @endvar
   @var        stencil
   @vdesc      stencil width
   @vtype      int [ dimension of group ]
   @vio        in
   @endvar
   @var        var0
   @vdesc      asymptotic value of function at infinity
   @vtype      CCTK_REAL
   @vio        in
   @endvar
   @var        speed
   @vdesc      wave speed
   @vtype      CCTK_REAL
   @vio        in
   @endvar
   @var        gname_to
   @vdesc      name of group to apply BC to
   @vtype      const char *
   @vio        in
   @endvar
   @var        gname_from
   @vdesc      name of group to apply BC from
   @vtype      const char *
   @vio        in
   @endvar

   @returntype int
   @returndesc
               return code of @seeroutine BndRadiativeGI <BR>
               -1 if invalid group names are given
   @endreturndesc
@@*/
int BndRadiativeGN (const cGH *GH,
                    const int *stencil,
                    CCTK_REAL var0,
                    CCTK_REAL speed,
                    const char *gname_to,
                    const char *gname_from)
{
  int gi_to, gi_from, retval;


  gi_to   = CCTK_GroupIndex (gname_to);
  gi_from = CCTK_GroupIndex (gname_from);
  if (gi_to >= 0 && gi_from >= 0)
  {
    retval = BndRadiativeGI (GH, stencil, var0, speed, gi_to, gi_from);
  }
  else
  {
    CCTK_VWarn (2, __LINE__, __FILE__, CCTK_THORNSTRING,
                "Invalid group names '%s' and/or '%s' in BndRadiativeGN",
                gname_to, gname_from);
    retval = -1;
  }

  return (retval);
}

void CCTK_FCALL bndradiativegn_
                           (int *ierr,
                            const cGH *GH,
                            const int *stencil,
                            const CCTK_REAL *var0,
                            const CCTK_REAL *speed,
                            TWO_FORTSTRING_ARG)
{
  TWO_FORTSTRINGS_CREATE (gname_to, gname_from)
  *ierr = BndRadiativeGN (GH, stencil, *var0, *speed, gname_to, gname_from);
  free (gname_to);
  free (gname_from);
}


/* ===================================================================== */

/*@@
   @routine    BndRadiativeDirVI
   @date       Sat Jan 20 2001
   @author     Gabrielle Allen
   @desc
               Apply radiative BC's by variable index in given direction
   @enddesc
   @calls      ApplyBndRadiative

   @var        GH
   @vdesc      Pointer to CCTK grid hierarchy
   @vtype      const cGH *
   @vio        in
   @endvar
   @var        stencil_size
   @vdesc      stencil size in this direction
   @vtype      int
   @vio        in
   @endvar
   @var        dir
   @vdesc      direction to apply BC
   @vtype      int
   @vio        in
   @endvar
   @var        var0
   @vdesc      asymptotic value of function at infinity
   @vtype      CCTK_REAL
   @vio        in
   @endvar
   @var        speed
   @vdesc      wave speed
   @vtype      CCTK_REAL
   @vio        in
   @endvar
   @var        vi_to
   @vdesc      index of variable to apply BC to
   @vtype      int
   @vio        in
   @endvar
   @var        vi_from
   @vdesc      index of variable to apply BC from
   @vtype      int
   @vio        in
   @endvar

   @returntype int
   @returndesc
               return code of @seeroutine ApplyBndRadiative <BR>
               -1 if invalid variable indices are given
   @endreturndesc
@@*/
int BndRadiativeDirVI (const cGH *GH,
                       int stencil_size,
                       int dir,
                       CCTK_REAL var0,
                       CCTK_REAL speed,
                       int vi_to,
                       int vi_from)
{
  int retval, num_vars;


  num_vars = CCTK_NumVars ();
  if (vi_to >= 0 && vi_to < num_vars && vi_from >= 0 && vi_from < num_vars)
  {
    retval = ApplyBndRadiative (GH, stencil_size, NULL, dir, var0, speed,
                                vi_to, vi_from, 1);
  }
  else
  {
    CCTK_VWarn (2, __LINE__, __FILE__, CCTK_THORNSTRING,
                "Invalid variable indices %d and/or %d in BndRadiativeDirVI",
                vi_to, vi_from);
    retval = -1;
  }

  return (retval);
}

void CCTK_FCALL bndradiativedirvi_
                           (int *ierr,
                            const cGH *GH,
                            const int *stencil_size,
                            const int *dir,
                            const CCTK_REAL *var0,
                            const CCTK_REAL *speed,
                            const int *vi_to,
                            const int *vi_from)
{
  *ierr = BndRadiativeDirVI (GH, *stencil_size, *dir, *var0, *speed,
                             *vi_to, *vi_from);
}


/*@@
   @routine    BndRadiativeVI
   @date       Tue Jul 18 18:04:07 2000
   @author     Gerd Lanfermann
   @desc
               Apply radiative BC's by variable index
   @enddesc
   @calls      ApplyBndRadiative

   @var        GH
   @vdesc      Pointer to CCTK grid hierarchy
   @vtype      const cGH *
   @vio        in
   @endvar
   @var        stencil
   @vdesc      stencil width
   @vtype      int [ dimension of variable ]
   @vio        in
   @endvar
   @var        var0
   @vdesc      asymptotic value of function at infinity
   @vtype      CCTK_REAL
   @vio        in
   @endvar
   @var        speed
   @vdesc      wave speed
   @vtype      CCTK_REAL
   @vio        in
   @endvar
   @var        vi_to
   @vdesc      index of variable to apply BC to
   @vtype      int
   @vio        in
   @endvar
   @var        vi_from
   @vdesc      index of variable to apply BC from
   @vtype      int
   @vio        in
   @endvar

   @returntype int
   @returndesc
               return code of @seeroutine ApplyBndRadiative <BR>
               -1 if invalid variable indices are given
   @endreturndesc
@@*/
int BndRadiativeVI (const cGH *GH,
                    const int *stencil,
                    CCTK_REAL var0,
                    CCTK_REAL speed,
                    int vi_to,
                    int vi_from)
{
  int retval, num_vars;


  num_vars = CCTK_NumVars ();
  if (vi_to >= 0 && vi_to < num_vars && vi_from >= 0 && vi_from < num_vars)
  {
    retval = ApplyBndRadiative (GH, -1, stencil, 0, var0, speed,
                                vi_to, vi_from, 1);
  }
  else
  {
    CCTK_VWarn (2, __LINE__, __FILE__, CCTK_THORNSTRING,
                "Invalid variable indices %d and/or %d in BndRadiativeVI",
                vi_to, vi_from);
    retval = -1;
  }

  return (retval);
}

void CCTK_FCALL bndradiativevi_
                           (int *ierr,
                            const cGH *GH,
                            const int *stencil,
                            const CCTK_REAL *var0,
                            const CCTK_REAL *speed,
                            const int *vi_to,
                            const int *vi_from)
{
  *ierr = BndRadiativeVI (GH, stencil, *var0, *speed, *vi_to, *vi_from);
}


/* ======================================================================= */

/*@@
   @routine    BndRadiativeDirVN
   @date       Sat Jan 20 2001
   @author     Gabrielle Allen
   @desc
               Apply radiative BC's by variable name in given direction
   @enddesc
   @calls      BndRadiativeDirVI

   @var        GH
   @vdesc      Pointer to CCTK grid hierarchy
   @vtype      const cGH *
   @vio        in
   @endvar
   @var        stencil_size
   @vdesc      stencil size in this direction
   @vtype      int
   @vio        in
   @endvar
   @var        dir
   @vdesc      direction to apply BC
   @vtype      int
   @vio        in
   @endvar
   @var        var0
   @vdesc      asymptotic value of function at infinity
   @vtype      CCTK_REAL
   @vio        in
   @endvar
   @var        speed
   @vdesc      wave speed
   @vtype      CCTK_REAL
   @vio        in
   @endvar
   @var        vname_to
   @vdesc      name of variable to apply BC to
   @vtype      const char *
   @vio        in
   @endvar
   @var        vname_from
   @vdesc      name of variable to apply BC from
   @vtype      const char *
   @vio        in
   @endvar

   @returntype int
   @returndesc
               return code of @seeroutine BndRadiativeDirVI <BR>
               -1 if invalid variable names are given
   @endreturndesc
@@*/
int BndRadiativeDirVN (const cGH *GH,
                       int stencil_size,
                       int dir,
                       CCTK_REAL var0,
                       CCTK_REAL speed,
                       const char *vname_to,
                       const char *vname_from)
{
  int vi_to, vi_from, num_vars, retval;


  vi_to    = CCTK_VarIndex (vname_to);
  vi_from  = CCTK_VarIndex (vname_from);
  num_vars = CCTK_NumVars ();

  if (vi_to >= 0 && vi_to < num_vars && vi_from >= 0 && vi_from < num_vars)
  {
    retval = BndRadiativeDirVI (GH, stencil_size, dir, var0, speed,
                                vi_to, vi_from);
  }
  else
  {
    CCTK_VWarn (2, __LINE__, __FILE__, CCTK_THORNSTRING,
                "Invalid variable names '%s' and/or '%s' in BndRadiativeDirVN",
                vname_to, vname_from);
    retval = -1;
  }

  return (retval);
}

void CCTK_FCALL bndradiativedirvn_
                           (int *ierr,
                            const cGH *GH,
                            const int *stencil_size,
                            const int *dir,
                            const CCTK_REAL *var0,
                            const CCTK_REAL *speed,
                            TWO_FORTSTRING_ARG)
{
  TWO_FORTSTRINGS_CREATE (vname_to, vname_from)
  *ierr = BndRadiativeDirVN (GH, *stencil_size, *dir, *var0, *speed,
                             vname_to, vname_from);
  free (vname_to);
  free (vname_from);
}


/*@@
   @routine    BndRadiativeVN
   @date       Tue Jul 18 18:04:07 2000
   @author     Gerd Lanfermann
   @desc
               Apply radiative BC's by variable name
   @enddesc
   @calls      BndRadiativeVI

   @var        GH
   @vdesc      Pointer to CCTK grid hierarchy
   @vtype      const cGH *
   @vio        in
   @endvar
   @var        stencil
   @vdesc      stencil width
   @vtype      int [ dimension of variable ]
   @vio        in
   @endvar
   @var        var0
   @vdesc      asymptotic value of function at infinity
   @vtype      CCTK_REAL
   @vio        in
   @endvar
   @var        speed
   @vdesc      wave speed
   @vtype      CCTK_REAL
   @vio        in
   @endvar
   @var        vname_to
   @vdesc      name of variable to apply BC to
   @vtype      const char *
   @vio        in
   @endvar
   @var        vname_from
   @vdesc      name of variable to apply BC from
   @vtype      const char *
   @vio        in
   @endvar

   @returntype int
   @returndesc
               return code of @seeroutine BndRadiativeVI <BR>
               -1 if invalid variable names are given
   @endreturndesc
@@*/
int BndRadiativeVN (const cGH *GH,
                    const int *stencil,
                    CCTK_REAL var0,
                    CCTK_REAL speed,
                    const char *vname_to,
                    const char *vname_from)
{
  int vi_to, vi_from, num_vars, retval;


  vi_to    = CCTK_VarIndex (vname_to);
  vi_from  = CCTK_VarIndex (vname_from);
  num_vars = CCTK_NumVars ();

  if (vi_to >= 0 && vi_to < num_vars && vi_from >= 0 && vi_from < num_vars)
  {
    retval = BndRadiativeVI (GH, stencil, var0, speed, vi_to, vi_from);
  }
  else
  {
    CCTK_VWarn (2, __LINE__, __FILE__, CCTK_THORNSTRING,
                "Invalid variable names '%s' and/or '%s' in BndRadiativeVN",
                vname_to, vname_from);
    retval = -1;
  }

  return (retval);
}

void CCTK_FCALL bndradiativevn_
                           (int *ierr,
                            const cGH *GH,
                            const int *stencil,
                            const CCTK_REAL *var0,
                            const CCTK_REAL *speed,
                            TWO_FORTSTRING_ARG)
{
  TWO_FORTSTRINGS_CREATE (vname_to, vname_from)
  *ierr = BndRadiativeVN (GH, stencil, *var0, *speed, vname_to, vname_from);
  free (vname_to);
  free (vname_from);
}


/********************************************************************
 *********************     Local Routines   *************************
 ********************************************************************/

/* shortcut for multiplying a variable with itself */
#define SQR(a) ((a) * (a))

/* the maximum dimension we can deal with */
#define MAXDIM 3


/*@@
   @routine    LOWER_RADIATIVE_BOUNDARY_3D
   @date       Mon 9 Apr 2001
   @author     Thomas Radke
   @desc
               Macro to apply radiative BC to a lower bound of a 3D variable
   @enddesc

   @var        istart, jstart, kstart
   @vdesc      start index for the x,y,z direction
   @vtype      int
   @vio        in
   @endvar
   @var        xyz
   @vdesc      coordinate arrays for x, y, and z
   @vtype      CCTK_REAL [ MAXDIM ]
   @vio        in
   @endvar
   @var        radius
   @vdesc      radius coordinate array
   @vtype      CCTK_REAL []
   @vio        in
   @endvar
   @var        offset
   @vdesc      offset to the next element in this direction
   @vtype      int
   @vio        in
   @endvar
   @var        var_to
   @vdesc      target variable array
   @vtype      cctk_type []
   @vio        in
   @endvar
   @var        var_from
   @vdesc      source variable array
   @vtype      cctk_type []
   @vio        in
   @endvar
   @var        cctk_type
   @vdesc      CCTK datatypes of the source and target variable
   @vtype      <cctk_type>
   @vio        in
   @endvar
@@*/
#define LOWER_RADIATIVE_BOUNDARY_3D(istart, jstart, kstart,                   \
                                    xyz,                                      \
                                    radius,                                   \
                                    offset,                                   \
                                    var_to,                                   \
                                    var_from,                                 \
                                    cctk_type)                                \
{                                                                             \
  int _i, _j, _k;                                                             \
                                                                              \
                                                                              \
  for (_k = kstart-1; _k >= 0; _k--)                                          \
  {                                                                           \
    for (_j = jstart-1; _j >= 0; _j--)                                        \
    {                                                                         \
      int _0 = 0*offset,                                                      \
          _1 = 1*offset,                                                      \
          _2 = 2*offset;                                                      \
      int _index = CCTK_GFINDEX3D (GH, istart-1, _j, _k);                     \
      CCTK_REAL *_radius = radius   + _index,                                 \
                *_xyz    = xyz      + _index;                                 \
      cctk_type *_to     = (cctk_type *) (var_to)   + _index,                 \
                *_from   = (cctk_type *) (var_from) + _index;                 \
                                                                              \
                                                                              \
      for (_i = istart-1; _i >= 0; _i--)                                      \
      {                                                                       \
        CCTK_REAL _radius0_inv = 1/_radius[_0],                               \
                  _radius1_inv = 1/_radius[_1];                               \
                                                                              \
        if (radpower >= 0)                                                    \
        {                                                                     \
          CCTK_REAL H;                                                        \
                                                                              \
          H  = 0.25 * radpower * dxyz[0]                                      \
               * (_xyz[_0]*SQR (_radius0_inv) + _xyz[_1]*SQR (_radius1_inv)); \
          H  = (1 + H) / (1 - H);                                             \
          H *= dtv * (0.25*(_to[_1] + _to[_2] + _from[_1] + _from[_2]) - var0)\
               + 0.5*(  _radius[_1] * (_to[_1] - _from[_1])                   \
                      + _radius[_2] * (_to[_2] - _from[_2]))                  \
               + 0.25*(_to[_2] - _to[_1] + _from[_2] - _from[_1]) * rho[0]    \
                     *(  SQR (_radius[_1]) / _xyz[_1]                         \
                       + SQR (_radius[_2]) / _xyz[_2]);                       \
          dtvvar0H = dtvvar0 + H;                                             \
        }                                                                     \
                                                                              \
        _to[_0] = (cctk_type) (                                               \
                   (dtvvar0H * (  _xyz[_0] * SQR (_radius0_inv)               \
                                + _xyz[_1] * SQR (_radius1_inv))              \
                    - _to[_1]   * ( rho[0] + _xyz[_1]*_radius1_inv            \
                                                     *(1 + dtvh*_radius1_inv))\
                    + _from[_0] * ( rho[0] + _xyz[_0]*_radius0_inv            \
                                                     *(1 - dtvh*_radius0_inv))\
                    - _from[_1] * ( rho[0] - _xyz[_1]*_radius1_inv            \
                                                     *(1 - dtvh*_radius1_inv))\
                   )                                                          \
                   /              (-rho[0] + _xyz[_0]*_radius0_inv            \
                                                     *(1 + dtvh*_radius0_inv))\
                  );                                                          \
        _radius--; _xyz--; _to--; _from--;                                    \
      }                                                                       \
    }                                                                         \
  }                                                                           \
}


/*@@
   @routine    UPPER_RADIATIVE_BOUNDARY_3D
   @date       Mon 9 Apr 2001
   @author     Thomas Radke
   @desc
               Macro to apply radiative BC to an upper bound of a 3D variable
   @enddesc

   @var        istart, jstart, kstart
   @vdesc      start index for the x,y,z direction
   @vtype      int
   @vio        in
   @endvar
   @var        xyz
   @vdesc      coordinate arrays for x, y, and z
   @vtype      CCTK_REAL [ MAXDIM ]
   @vio        in
   @endvar
   @var        radius
   @vdesc      radius coordinate array
   @vtype      CCTK_REAL []
   @vio        in
   @endvar
   @var        offset
   @vdesc      offset to the next element in this direction
   @vtype      int
   @vio        in
   @endvar
   @var        var_to
   @vdesc      target variable array
   @vtype      cctk_type []
   @vio        in
   @endvar
   @var        var_from
   @vdesc      source variable array
   @vtype      cctk_type []
   @vio        in
   @endvar
   @var        cctk_type
   @vdesc      CCTK datatypes of the source and target variable
   @vtype      <cctk_type>
   @vio        in
   @endvar
@@*/
#define UPPER_RADIATIVE_BOUNDARY_3D(istart, jstart, kstart,                   \
                                    xyz,                                      \
                                    radius,                                   \
                                    offset,                                   \
                                    var_to,                                   \
                                    var_from,                                 \
                                    cctk_type)                                \
{                                                                             \
  int _i, _j, _k;                                                             \
                                                                              \
                                                                              \
  for (_k = kstart; _k < GH->cctk_lsh[2]; _k++)                               \
  {                                                                           \
    for (_j = jstart; _j < GH->cctk_lsh[1]; _j++)                             \
    {                                                                         \
      int _0 = -0*offset,                                                     \
          _1 = -1*offset,                                                     \
          _2 = -2*offset;                                                     \
      int _index = CCTK_GFINDEX3D (GH, istart, _j, _k);                       \
      CCTK_REAL *_radius = radius   + _index,                                 \
                *_xyz    = xyz      + _index;                                 \
      cctk_type *_to     = (cctk_type *) (var_to)   + _index,                 \
                *_from   = (cctk_type *) (var_from) + _index;                 \
                                                                              \
                                                                              \
      for (_i = istart; _i < GH->cctk_lsh[0]; _i++)                           \
      {                                                                       \
        CCTK_REAL _radius0_inv = 1/_radius[_0],                               \
                  _radius1_inv = 1/_radius[_1];                               \
                                                                              \
        if (radpower >= 0)                                                    \
        {                                                                     \
          CCTK_REAL H;                                                        \
                                                                              \
          H  = 0.25 * radpower * dxyz[0]                                      \
               * (_xyz[_0]*SQR (_radius0_inv) + _xyz[_1]*SQR (_radius1_inv)); \
          H  = (1 - H) / (1 + H);                                             \
          H *= dtv * (0.25*(_to[_1] + _to[_2] + _from[_1] + _from[_2]) - var0)\
               + 0.5 *(  _radius[_1] * (_to[_1] - _from[_1])                  \
                       + _radius[_2] * (_to[_2] - _from[_2]))                 \
               + 0.25*(_to[_1] - _to[_2] + _from[_1] - _from[_2]) * rho[0]    \
                     * (  SQR (_radius[_1]) / _xyz[_1]                        \
                        + SQR (_radius[_2]) / _xyz[_2]);                      \
          dtvvar0H = dtvvar0 + H;                                             \
        }                                                                     \
                                                                              \
        _to[_0] = (cctk_type) (                                               \
                   (dtvvar0H * (  _xyz[_0] * (SQR (_radius0_inv))             \
                                + _xyz[_1] * (SQR (_radius1_inv)))            \
                    + _to[_1]   * ( rho[0] - _xyz[_1]*_radius1_inv            \
                                                     *(1 + dtvh*_radius1_inv))\
                    + _from[_0] * (-rho[0] + _xyz[_0]*_radius0_inv            \
                                                     *(1 - dtvh*_radius0_inv))\
                    + _from[_1] * ( rho[0] + _xyz[_1]*_radius1_inv            \
                                                     *(1 - dtvh*_radius1_inv))\
                   )                                                          \
                   /              ( rho[0] + _xyz[_0]*_radius0_inv            \
                                                     *(1 + dtvh*_radius0_inv))\
                  );                                                          \
        _radius++; _xyz++; _to++; _from++;                                    \
      }                                                                       \
    }                                                                         \
  }                                                                           \
}


/*@@
   @routine    RADIATIVE_BOUNDARY
   @date       Mon 9 Apr 2001
   @author     Thomas Radke
   @desc
               Macro to apply radiative BC to a variable
               Currently it is limited to 3D variables only.
   @enddesc
   @calls      LOWER_RADIATIVE_BOUNDARY_3D
               UPPER_RADIATIVE_BOUNDARY_3D

   @var        lsh
   @vdesc      local shape of the variable
   @vtype      int [ dim ]
   @vio        in
   @endvar
   @var        doBC
   @vdesc      flags telling whether to apply BC in a given direction or not
   @vtype      int [ 2*dim ]
   @vio        in
   @endvar
   @var        stencil
   @vdesc      stencils in every direction
   @vtype      int [ 2*dim ]
   @vio        in
   @endvar
   @var        coords
   @vdesc      coordinate arrays for x, y, z, and the radius
   @vtype      CCTK_REAL [ MAXDIM+1 ]
   @vio        in
   @endvar
   @var        offset
   @vdesc      offsets to the next element in each dimension
   @vtype      int [ dim ]
   @vio        in
   @endvar
   @var        var_to
   @vdesc      target variable array
   @vtype      cctk_type []
   @vio        in
   @endvar
   @var        var_from
   @vdesc      source variable array
   @vtype      cctk_type []
   @vio        in
   @endvar
   @var        dim
   @vdesc      dimension of the source and target variable
   @vtype      int
   @vio        in
   @endvar
   @var        cctk_type
   @vdesc      CCTK datatypes of the source and target variable
   @vtype      <cctk_type>
   @vio        in
   @endvar
@@*/
#define RADIATIVE_BOUNDARY(lsh,                                               \
                           doBC,                                              \
                           stencil,                                           \
                           coords,                                            \
                           offset,                                            \
                           var_to,                                            \
                           var_from,                                          \
                           dim,                                               \
                           cctk_type)                                         \
{                                                                             \
  /* check the dimensionality */                                              \
  if (dim != 3)                                                               \
  {                                                                           \
    CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,                      \
                "ApplyBndRadiative: variable dimension of %d not supported",  \
                dim);                                                         \
    return (-5);                                                              \
  }                                                                           \
                                                                              \
  /* Lower x-bound */                                                         \
  if (doBC[0])                                                                \
  {                                                                           \
    LOWER_RADIATIVE_BOUNDARY_3D (stencil[0], lsh[1], lsh[2],                  \
                                 coords[0], coords[MAXDIM], offset[0],        \
                                 var_to, var_from, cctk_type);                \
  }                                                                           \
                                                                              \
  /* Upper x-bound */                                                         \
  if (doBC[1])                                                                \
  {                                                                           \
    UPPER_RADIATIVE_BOUNDARY_3D (lsh[0] - stencil[0], 0, 0,                   \
                                 coords[0], coords[MAXDIM], offset[0],        \
                                 var_to, var_from, cctk_type);                \
  }                                                                           \
                                                                              \
  /* Lower y-bound */                                                         \
  if (doBC[2])                                                                \
  {                                                                           \
    LOWER_RADIATIVE_BOUNDARY_3D (lsh[0], stencil[1], lsh[2],                  \
                                 coords[1], coords[MAXDIM], offset[1],        \
                                 var_to, var_from, cctk_type);                \
  }                                                                           \
                                                                              \
  /* Upper y-bound */                                                         \
  if (doBC[3])                                                                \
  {                                                                           \
    UPPER_RADIATIVE_BOUNDARY_3D (0, lsh[1] - stencil[1], 0,                   \
                                 coords[1], coords[MAXDIM], offset[1],        \
                                 var_to, var_from, cctk_type);                \
  }                                                                           \
                                                                              \
  /* Lower z-bound */                                                         \
  if (doBC[4])                                                                \
  {                                                                           \
    LOWER_RADIATIVE_BOUNDARY_3D (lsh[0], lsh[1], stencil[2],                  \
                                 coords[2], coords[MAXDIM], offset[2],        \
                                 var_to, var_from, cctk_type);                \
  }                                                                           \
                                                                              \
  /* Upper z-bound */                                                         \
  if (doBC[5])                                                                \
  {                                                                           \
    UPPER_RADIATIVE_BOUNDARY_3D (0, 0, lsh[2] - stencil[2],                   \
                                 coords[2], coords[MAXDIM], offset[2],        \
                                 var_to, var_from, cctk_type);                \
  }                                                                           \
}


/*@@
   @routine    ApplyBndRadiative
   @date       Tue Jul 18 18:04:07 2000
   @author     Gerd Lanfermann
   @desc
               Apply radiation boundary conditions to a group of grid functions
               given by their indices
               This routine is called by the various BndRadiativeXXX wrappers.

               Although it is currently limited to handle 3D variables only
               it can easily be extended for other dimensions
               by adapting the appropriate macros.
   @enddesc

   @var        GH
   @vdesc      Pointer to CCTK grid hierarchy
   @vtype      const cGH *
   @vio        in
   @endvar
   @var        stencil_dir
   @vdesc      stencil width in direction dir
   @vtype      int
   @vio        in
   @endvar
   @var        stencil_alldirs
   @vdesc      stencil widths for all directions
   @vtype      int [ dimension of variable(s) ]
   @vio        in
   @endvar
   @var        dir
   @vdesc      direction to copy boundaries (0 for copying all directions)
   @vtype      int
   @vio        in
   @endvar
   @var        var0
   @vdesc      asymptotic value of function at infinity
   @vtype      CCTK_REAL
   @vio        in
   @endvar
   @var        speed
   @vdesc      wave speed
   @vtype      CCTK_REAL
   @vio        in
   @endvar
   @var        first_var_to
   @vdesc      index of first variable to copy boundaries to
   @vtype      int
   @vio        in
   @endvar
   @var        first_var_from
   @vdesc      index of first variable to copy boundaries from
   @vtype      int
   @vio        in
   @endvar
   @var        num_vars
   @vdesc      number of variables
   @vtype      int
   @vio        in
   @endvar
               CCTK_VarTypeI
               CCTK_GroupDimFromVarI
               RADIATIVE_BOUNDARY
   @history
   @hdate      Mon 9 Apr 2001
   @hauthor    Thomas Radke
   @hdesc      Merged separate routines for 1D, 2D, and 3D
               into a single generic routine
   @endhistory

   @returntype int
   @returndesc
                0 for success
               -1 if abs(direction) is greater than variables' dimension
               -2 if variable dimension is not supported
               -3 if NULL pointer passed as stencil array
               -4 if variable type is not supported
               -5 if variable dimension is other than 3D
   @endreturndesc
@@*/
static int ApplyBndRadiative (const cGH *GH,
                              int stencil_dir,
                              const int *stencil_alldirs,
                              int dir,
                              CCTK_REAL var0,
                              CCTK_REAL speed,
                              int first_var_to,
                              int first_var_from,
                              int num_vars)
{
  DECLARE_CCTK_PARAMETERS
  int i, gdim;
  int var_to, var_from;
  int timelvl_to, timelvl_from;
  SymmetryGHex *sGHex;
  char coord_system_name[10];
  CCTK_REAL dxyz[MAXDIM], rho[MAXDIM], *coords[MAXDIM+1];
  int doBC[2*MAXDIM], stencil[MAXDIM], offset[MAXDIM];
  CCTK_REAL dtv, dtvh, dtvvar0, dtvvar0H;


  /* check the direction parameter */
  if (abs (dir) > MAXDIM)
  {
    CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,
                "ApplyBndRadiative: direction %d is greater than maximum "
                "dimension %d", dir, MAXDIM);
    return (-1);
  }

  /* get the dimensionality */
  gdim = CCTK_GroupDimFromVarI (first_var_to);

  /* check the dimensionality */
  if (gdim > MAXDIM)
  {
    CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,
                "ApplyBndRadiative: variable dimension of %d not supported",
                gdim);
    return (-2);
  }

  /* set up stencil width array */
  if (dir)
  {
    stencil[abs (dir) - 1] = stencil_dir;
  }
  else if (stencil_alldirs)
  {
    memcpy (stencil, stencil_alldirs, gdim * sizeof (int));
  }
  else
  {
    CCTK_WARN (1, "ApplyBndRadiative: NULL pointer passed "
                  "for stencil width array");
    return (-3);
  }

  /* Use next time level, if available */
  timelvl_to = 0;
  /*  if (timelvl_to < 0)
  {
    timelvl_to = 0;
    }*/
  /* Use current time level, if available */
  if (CCTK_NumTimeLevelsFromVarI(first_var_from) > 1)
  {
    timelvl_from = 1;
  }
  else
  {
    timelvl_from = 0;
  }

  /*if (timelvl_from < 0)
  {
    timelvl_from = 0;
    }*/

  /* Find Courant parameters. */
  dtv      = speed * GH->cctk_delta_time;
  dtvh     = 0.5 * dtv;
  dtvvar0  = dtv * var0;
  dtvvar0H = dtvvar0;

  sprintf (coord_system_name, "cart%dd", gdim);
  for (i = 0; i < gdim; i++)
  {
    /* Radiative boundaries need the underlying Cartesian coordinates */
    coords[i] = GH->data[CCTK_CoordIndex (i + 1, NULL, coord_system_name)][0];

    /* According to the Cactus spec, the true delta_space values for a
       grid are calculated as follows: */
    dxyz[i] = GH->cctk_delta_space[i] / GH->cctk_levfac[i];

    rho[i] = dtv / dxyz[i];

    offset[i] = i == 0 ? 1 : offset[i-1] * GH->cctk_lsh[i-1];
  }
  sprintf (coord_system_name, "spher%dd", gdim);
  coords[MAXDIM] = GH->data[CCTK_CoordIndex (-1, "r", coord_system_name)][0];

  /* see if we have a symmetry array */
  sGHex = (SymmetryGHex *) CCTK_GHExtension (GH, "Symmetry");

  /* now loop over all variables */
  for (var_to = first_var_to, var_from = first_var_from;
       var_to < first_var_to + num_vars;
       var_to++, var_from++)
  {
    /* Apply condition if:
       + boundary is not a symmetry boundary
         (no symmetry or unset(=unsed))
       + boundary is a physical boundary
       + have enough grid points
    */
    memset (doBC, 1, sizeof (doBC));
    if (sGHex)
    {
      for (i = 0; i < 2 * MAXDIM; i++)
      {
        doBC[i] = sGHex->GFSym[var_to][i] == GFSYM_NOSYM ||
                  sGHex->GFSym[var_to][i] == GFSYM_UNSET;
      }
    }
    for (i = 0; i < MAXDIM; i++)
    {
      doBC[i*2]   &= GH->cctk_lsh[i] > 1 && GH->cctk_bbox[i*2];
      doBC[i*2+1] &= GH->cctk_lsh[i] > 1 && GH->cctk_bbox[i*2+1];
      if (dir != 0)
      {
        doBC[i*2]   &= (dir < 0 && (i + 1 == abs (dir)));
        doBC[i*2+1] &= (dir > 0 && (i + 1 == abs (dir)));
      }
    }

    switch (CCTK_VarTypeI (var_to))
    {
      case CCTK_VARIABLE_CHAR:
        RADIATIVE_BOUNDARY (GH->cctk_lsh, doBC, stencil, coords, offset,
                            GH->data[var_to][timelvl_to],
                            GH->data[var_from][timelvl_from], gdim, CCTK_CHAR);
        break;

      case CCTK_VARIABLE_INT:
        RADIATIVE_BOUNDARY (GH->cctk_lsh, doBC, stencil, coords, offset,
                            GH->data[var_to][timelvl_to],
                            GH->data[var_from][timelvl_from], gdim, CCTK_INT);
        break;

      case CCTK_VARIABLE_REAL:
        RADIATIVE_BOUNDARY (GH->cctk_lsh, doBC, stencil, coords, offset,
                            GH->data[var_to][timelvl_to],
                            GH->data[var_from][timelvl_from], gdim, CCTK_REAL);
        break;

#ifdef CCTK_INT2
      case CCTK_VARIABLE_INT2:
        RADIATIVE_BOUNDARY (GH->cctk_lsh, doBC, stencil, coords, offset,
                            GH->data[var_to][timelvl_to],
                            GH->data[var_from][timelvl_from], gdim, CCTK_INT2);
        break;
#endif

#ifdef CCTK_INT4
      case CCTK_VARIABLE_INT4:
        RADIATIVE_BOUNDARY (GH->cctk_lsh, doBC, stencil, coords, offset,
                            GH->data[var_to][timelvl_to],
                            GH->data[var_from][timelvl_from], gdim, CCTK_INT4);
        break;
#endif

#ifdef CCTK_INT8
      case CCTK_VARIABLE_INT8:
        RADIATIVE_BOUNDARY (GH->cctk_lsh, doBC, stencil, coords, offset,
                            GH->data[var_to][timelvl_to],
                            GH->data[var_from][timelvl_from], gdim, CCTK_INT8);
        break;
#endif

#ifdef CCTK_REAL4
      case CCTK_VARIABLE_REAL4:
        RADIATIVE_BOUNDARY (GH->cctk_lsh, doBC, stencil, coords, offset,
                            GH->data[var_to][timelvl_to],
                            GH->data[var_from][timelvl_from], gdim, CCTK_REAL4);
        break;
#endif

#ifdef CCTK_REAL8
      case CCTK_VARIABLE_REAL8:
        RADIATIVE_BOUNDARY (GH->cctk_lsh, doBC, stencil, coords, offset,
                            GH->data[var_to][timelvl_to],
                            GH->data[var_from][timelvl_from], gdim, CCTK_REAL8);
        break;
#endif

#ifdef CCTK_REAL16
      case CCTK_VARIABLE_REAL16:
        RADIATIVE_BOUNDARY (GH->cctk_lsh, doBC, stencil, coords, offset,
                            GH->data[var_to][timelvl_to],
                            GH->data[var_from][timelvl_from], gdim,CCTK_REAL16);
        break;
#endif

      default:
        CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,
                    "Unsupported variable type %d for variable '%s'",
                    CCTK_VarTypeI (var_to), CCTK_VarName (var_to));
        return (-4);
    }
  }

  USE_CCTK_PARAMETERS;   return (0);
}
