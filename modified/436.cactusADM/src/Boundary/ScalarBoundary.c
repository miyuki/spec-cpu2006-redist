#ifdef SPEC_CPU
# define THORN_IS_Boundary
#endif /* SPEC_CPU */
 /*@@
   @file      ScalarBoundary.c
   @date      Mon Mar 15 15:09:00 1999
   @author    Gabrielle Allen, Gerd Lanfermann
   @desc
              Routines for applying scalar boundary conditions
   @enddesc
   @history
   @hdate     Tue 10 Apr 2001
   @hauthor   Thomas Radke
   @hdesc     BC routines generalized for applying to arbitrary CCTK data types
   @endhistory
   @version   $Id: ScalarBoundary.c,v 1.22 2001/12/18 20:44:49 rideout Exp $
 @@*/

#include <stdlib.h>
#include <string.h>

#include "cctk.h"
#include "cctk_Parameters.h"
#include "cctk_FortranString.h"

#include "Symmetry.h"
#include "Boundary.h"

/* the rcs ID and its dummy function to use it */
static const char *rcsid = "$Header: /cactus/CactusBase/Boundary/src/ScalarBoundary.c,v 1.22 2001/12/18 20:44:49 rideout Exp $";
CCTK_FILEVERSION(CactusBase_Boundary_ScalarBoundary_c)


/********************************************************************
 ********************    External Routines   ************************
 ********************************************************************/
/* prototypes for external C routines are declared in header Boundary.h
   here only follow the fortran wrapper prototypes */
void CCTK_FCALL bndscalardirvi_
                           (int *ierr,
                            const cGH *GH,
                            const int *stencil_size,
                            const int *dir,
                            const CCTK_REAL *scalar,
                            const int *vi);
void CCTK_FCALL bndscalarvi_
                           (int *ierr,
                            const cGH *GH,
                            const int *stencil,
                            const CCTK_REAL *scalar,
                            const int *vi);
void CCTK_FCALL bndscalardirgi_
                           (int *ierr,
                            const cGH *GH,
                            const int *stencil_size,
                            const int *dir,
                            const CCTK_REAL *scalar,
                            const int *gi);
void CCTK_FCALL bndscalargi_
                           (int *ierr,
                            const cGH *GH,
                            const int *stencil,
                            const CCTK_REAL *scalar,
                            const int *gi);
void CCTK_FCALL bndscalardirgn_
                           (int *ierr,
                            const cGH *GH,
                            const int *stencil_size,
                            const int *dir,
                            const CCTK_REAL *scalar,
                            ONE_FORTSTRING_ARG);
void CCTK_FCALL bndscalargn_
                           (int *ierr,
                            const cGH *GH,
                            const int *stencil,
                            const CCTK_REAL *scalar,
                            ONE_FORTSTRING_ARG);
void CCTK_FCALL bndscalardirvn_
                           (int *ierr,
                            const cGH *GH,
                            const int *stencil_size,
                            const int *dir,
                            const CCTK_REAL *scalar,
                            ONE_FORTSTRING_ARG);
void CCTK_FCALL bndscalarvn_
                           (int *ierr,
                            const cGH *GH,
                            const int *stencil,
                            const CCTK_REAL *scalar,
                            ONE_FORTSTRING_ARG);

/********************************************************************
 ********************    Internal Routines   ************************
 ********************************************************************/
static int ApplyBndScalar (const cGH *GH,
                           int stencil_dir,
                           const int *stencil_alldirs,
                           int dir,
                           CCTK_REAL scalar,
                           int first_var,
                           int num_vars);


 /*@@
   @routine    BndScalarDirVI
   @date       Tue Jan 16 2001
   @author     Gabrielle Allen
   @desc
               Apply scalar boundary conditions by variable index
               in given direction
   @enddesc
   @calls      ApplyBndScalar

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
   @var        scalar
   @vdesc      scalar to set the boundaries to
   @vtype      CCTK_REAL
   @vio        in
   @endvar
   @var        vi
   @vdesc      index of variable to apply BC to
   @vtype      int
   @vio        in
   @endvar

   @returntype int
   @returndesc
               return code of @seeroutine ApplyBndScalar <BR>
               -1 if invalid variable index was given
   @endreturndesc
@@*/
int BndScalarDirVI (const cGH *GH,
                    int stencil_size,
                    int dir,
                    CCTK_REAL scalar,
                    int vi)
{
  int retval;


  if (vi >= 0 && vi < CCTK_NumVars ())
  {
    retval = ApplyBndScalar (GH, stencil_size, NULL, dir, scalar, vi, 1);
  }
  else
  {
    CCTK_VWarn (2, __LINE__, __FILE__, CCTK_THORNSTRING,
                "Invalid variable index %d in BndScalarDirVI", vi);
    retval = -1;
  }

  return (retval);
}

void CCTK_FCALL bndscalardirvi_
                           (int *ierr,
                            const cGH *GH,
                            const int *stencil_size,
                            const int *dir,
                            const CCTK_REAL *scalar,
                            const int *vi)
{
  *ierr = BndScalarDirVI (GH, *stencil_size, *dir, *scalar, *vi);
}


 /*@@
   @routine    BndScalarVI
   @date       Thu Mar  2 11:07:11 2000
   @author     Gerd Lanfermann
   @desc
               Apply scalar boundary conditions by variable index
   @enddesc
   @calls      ApplyBndScalar

   @var        GH
   @vdesc      Pointer to CCTK grid hierarchy
   @vtype      const cGH *
   @vio        in
   @endvar
   @var        stencil
   @vdesc      stencil width
   @vtype      int
   @vio        in
   @endvar
   @var        scalar
   @vdesc      scalar to set the boundaries to
   @vtype      CCTK_REAL
   @vio        in
   @endvar
   @var        vi
   @vdesc      index of variable to apply BC to
   @vtype      int
   @vio        in
   @endvar

   @returntype int
   @returndesc
               return code of @seeroutine ApplyBndScalar <BR>
               -1 if invalid variable index was given
   @endreturndesc
@@*/
int BndScalarVI (const cGH *GH,
                 const int *stencil,
                 CCTK_REAL scalar,
                 int vi)
{
  int retval;


  if (vi >= 0 && vi < CCTK_NumVars ())
  {
    retval = ApplyBndScalar (GH, -1, stencil, 0, scalar, vi, 1);
  }
  else
  {
    CCTK_VWarn (2, __LINE__, __FILE__, CCTK_THORNSTRING,
                "Invalid variable index %d in ApplyBndScalar", vi);
    retval = -1;
  }

  return (retval);
}

void CCTK_FCALL bndscalarvi_
                           (int *ierr,
                            const cGH *GH,
                            const int *stencil,
                            const CCTK_REAL *scalar,
                            const int *vi)
{
  *ierr = BndScalarVI (GH, stencil, *scalar, *vi);
}


/* ===================================================================== */

 /*@@
   @routine    BndScalarDirGI
   @date       Tue Jan 16 2001
   @author     Gabrielle Allen
   @desc
               Apply scalar boundary conditions by group index
               in given direction
   @enddesc
   @calls      ApplyBndScalar

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
   @var        scalar
   @vdesc      scalar to set the boundaries to
   @vtype      CCTK_REAL
   @vio        in
   @endvar
   @var        gi
   @vdesc      index of group to apply BC to
   @vtype      int
   @vio        in
   @endvar

   @returntype int
   @returndesc
               return code of @seeroutine ApplyBndScalar <BR>
               -1 if invalid group index was given
   @endreturndesc
@@*/
int BndScalarDirGI (const cGH *GH,
                    int stencil_size,
                    int dir,
                    CCTK_REAL scalar,
                    int gi)
{
  int first_vi, retval;


  first_vi = CCTK_FirstVarIndexI (gi);
  if (first_vi >= 0)
  {
    retval = ApplyBndScalar (GH, stencil_size, NULL, dir, scalar, first_vi,
                             CCTK_NumVarsInGroupI (gi));
  }
  else
  {
    CCTK_VWarn (2, __LINE__, __FILE__, CCTK_THORNSTRING,
                "Invalid group index %d in BndScalarDirGI", gi);
    retval = -1;
  }

  return (retval);
}

void CCTK_FCALL bndscalardirgi_
                           (int *ierr,
                            const cGH *GH,
                            const int *stencil_size,
                            const int *dir,
                            const CCTK_REAL *scalar,
                            const int *gi)
{
  *ierr = BndScalarDirGI (GH, *stencil_size, *dir, *scalar, *gi);
}


 /*@@
   @routine    BndScalarGI
   @date       Thu Mar  2 11:07:11 2000
   @author     Gerd Lanfermann
   @desc
               Apply scalar boundary conditions by group index
   @enddesc
   @calls      ApplyBndScalar

   @var        GH
   @vdesc      Pointer to CCTK grid hierarchy
   @vtype      const cGH *
   @vio        in
   @endvar
   @var        stencil
   @vdesc      stencil width
   @vtype      int
   @vio        in
   @endvar
   @var        scalar
   @vdesc      scalar to set the boundaries to
   @vtype      CCTK_REAL
   @vio        in
   @endvar
   @var        gi
   @vdesc      index of group to apply BC to
   @vtype      int
   @vio        in
   @endvar

   @returntype int
   @returndesc
               return code of @seeroutine ApplyBndScalar <BR>
               -1 if invalid group index was given
   @endreturndesc
@@*/
int BndScalarGI (const cGH *GH,
                 const int *stencil,
                 CCTK_REAL scalar,
                 int gi)
{
  int first_vi, retval;


  first_vi = CCTK_FirstVarIndexI (gi);
  if (first_vi >= 0)
  {
    retval = ApplyBndScalar (GH, -1, stencil, 0, scalar, first_vi,
                             CCTK_NumVarsInGroupI (gi));
  }
  else
  {
    CCTK_VWarn (2, __LINE__, __FILE__, CCTK_THORNSTRING,
                "Invalid group index %d in BndScalarGI", gi);
    retval = -1;
  }

  return (retval);
}

void CCTK_FCALL bndscalargi_
                           (int *ierr,
                            const cGH *GH,
                            const int *stencil,
                            const CCTK_REAL *scalar,
                            const int *gi)
{
  *ierr = BndScalarGI (GH, stencil, *scalar, *gi);
}


/* ===================================================================== */

 /*@@
   @routine    BndScalarDirGN
   @date       Tue Jan 16 2001
   @author     Gabrielle Allen
   @desc
               Apply scalar boundary conditions by group name in given direction
   @enddesc
   @calls      BndScalarDirGI

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
   @var        scalar
   @vdesc      scalar to set the boundaries to
   @vtype      CCTK_REAL
   @vio        in
   @endvar
   @var        gname
   @vdesc      name of group to apply BC to
   @vtype      const char *
   @vio        in
   @endvar

   @returntype int
   @returndesc
               return code of @seeroutine BndScalarDirGI <BR>
               -1 if invalid group name was given
   @endreturndesc
@@*/
int BndScalarDirGN (const cGH *GH,
                    int stencil_size,
                    int dir,
                    CCTK_REAL scalar,
                    const char *gname)
{
  int gi, retval;


  gi = CCTK_GroupIndex (gname);
  if (gi >= 0)
  {
    retval = BndScalarDirGI (GH, stencil_size, dir, scalar, gi);
  }
  else
  {
    CCTK_VWarn (2, __LINE__, __FILE__, CCTK_THORNSTRING,
                "Invalid group name '%s' in BndScalarDirGN", gname);
    retval = -1;
  }

  return (retval);
}

void CCTK_FCALL bndscalardirgn_
                           (int *ierr,
                            const cGH *GH,
                            const int *stencil_size,
                            const int *dir,
                            const CCTK_REAL *scalar,
                            ONE_FORTSTRING_ARG)
{
  ONE_FORTSTRING_CREATE (gname)
  *ierr = BndScalarDirGN (GH, *stencil_size, *dir, *scalar, gname);
  free (gname);
}


 /*@@
   @routine    BndScalarGN
   @date       Thu Mar  2 11:07:11 2000
   @author     Gerd Lanfermann
   @desc
               Apply scalar boundary conditions by group name
   @enddesc
   @calls      BndScalarGI

   @var        GH
   @vdesc      Pointer to CCTK grid hierarchy
   @vtype      const cGH *
   @vio        in
   @endvar
   @var        stencil
   @vdesc      stencil width
   @vtype      int
   @vio        in
   @endvar
   @var        scalar
   @vdesc      scalar to set the boundaries to
   @vtype      CCTK_REAL
   @vio        in
   @endvar
   @var        gname
   @vdesc      name of group to apply BC to
   @vtype      const char *
   @vio        in
   @endvar

   @returntype int
   @returndesc
               return code of @seeroutine BndScalarGI <BR>
               -1 if invalid group name was given
   @endreturndesc
@@*/
int BndScalarGN (const cGH *GH,
                 const int *stencil,
                 CCTK_REAL scalar,
                 const char *gname)
{
  int gi, retval;


  gi = CCTK_GroupIndex (gname);
  if (gi >= 0)
  {
    retval = BndScalarGI (GH, stencil, scalar, gi);
  }
  else
  {
    CCTK_VWarn (2, __LINE__, __FILE__, CCTK_THORNSTRING,
                "Invalid group name '%s' in BndScalarGN", gname);
    retval = -1;
  }

  return (retval);
}

void CCTK_FCALL bndscalargn_
                           (int *ierr,
                            const cGH *GH,
                            const int *stencil,
                            const CCTK_REAL *scalar,
                            ONE_FORTSTRING_ARG)
{
  ONE_FORTSTRING_CREATE (gname)
  *ierr = BndScalarGN (GH, stencil, *scalar, gname);
  free (gname);
}


 /*@@
   @routine    BndScalarDirVN
   @date       Tue Jan 16 2001
   @author     Gabrielle Allen
   @desc
               Apply scalar boundaries by variable name in given direction
   @enddesc
   @calls      BndScalarDirVI

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
   @var        scalar
   @vdesc      scalar to set the boundaries to
   @vtype      CCTK_REAL
   @vio        in
   @endvar
   @var        vname
   @vdesc      name of variable to apply BC to
   @vtype      const char *
   @vio        in
   @endvar

   @returntype int
   @returndesc
               return code of @seeroutine BndScalarDirVI <BR>
               -1 if invalid variable name was given
   @endreturndesc
@@*/
int BndScalarDirVN (const cGH *GH,
                    int stencil_size,
                    int dir,
                    CCTK_REAL scalar,
                    const char *vname)
{
  int vi, retval;


  vi = CCTK_VarIndex (vname);
  if (vi >= 0)
  {
    retval = BndScalarDirVI (GH, stencil_size, dir, scalar, vi);
  }
  else
  {
    CCTK_VWarn (2, __LINE__, __FILE__, CCTK_THORNSTRING,
                "Invalid variable name '%s' in BndScalarDirVN", vname);
    retval = -1;
  }

  return (retval);
}

void CCTK_FCALL bndscalardirvn_
                           (int *ierr,
                            const cGH *GH,
                            const int *stencil_size,
                            const int *dir,
                            const CCTK_REAL *scalar,
                            ONE_FORTSTRING_ARG)
{
  ONE_FORTSTRING_CREATE (vname)
  *ierr = BndScalarDirVN (GH, *stencil_size, *dir, *scalar, vname);
  free (vname);
}


 /*@@
   @routine    BndScalarVN
   @date       Thu Mar  2 11:07:11 2000
   @author     Gerd Lanfermann
   @desc
               Apply scalar boundaries by variable name
   @enddesc
   @calls      BndScalarVI

   @var        GH
   @vdesc      Pointer to CCTK grid hierarchy
   @vtype      const cGH *
   @vio        in
   @endvar
   @var        stencil
   @vdesc      stencil width
   @vtype      int
   @vio        in
   @endvar
   @var        scalar
   @vdesc      scalar to set the boundaries to
   @vtype      CCTK_REAL
   @vio        in
   @endvar
   @var        vname
   @vdesc      name of variable to apply BC to
   @vtype      const char *
   @vio        in
   @endvar

   @returntype int
   @returndesc
               return code of @seeroutine BndScalarVI <BR>
               -1 if invalid variable name was given
   @endreturndesc
@@*/
int BndScalarVN (const cGH *GH,
                 const int *stencil,
                 CCTK_REAL scalar,
                 const char *vname)
{
  int vi, retval;

  vi = CCTK_VarIndex (vname);
  if (vi >= 0)
  {
    retval = BndScalarVI (GH, stencil, scalar, vi);
  }
  else
  {
    CCTK_VWarn (2, __LINE__, __FILE__, CCTK_THORNSTRING,
                "Invalid variable name '%s' in BndScalarVN", vname);
    retval = -1;
  }

  return (retval);
}

void CCTK_FCALL bndscalarvn_
                           (int *ierr,
                            const cGH *GH,
                            const int *stencil,
                            const CCTK_REAL *scalar,
                            ONE_FORTSTRING_ARG)
{
  ONE_FORTSTRING_CREATE (vname)
  *ierr = BndScalarVN (GH, stencil, *scalar, vname);
  free (vname);
}



/********************************************************************
 *********************     Local Routines   *************************
 ********************************************************************/

/* maximum dimension we can deal with */
#define MAXDIM  3

/* macro to compute the linear index of a 3D point */
#define INDEX_3D(lsh, i, j, k)      ((i) + (lsh)[0]*((j) + (lsh)[1]*(k)))

/* an empty macro */
#define NOTHING

/*@@
   @routine    SCALAR_BOUNDARY_TYPED
   @date       Sat 20 Jan 2001
   @author     Thomas Radke
   @desc
               Macro to apply scalar boundary conditions to a variable
               of given datatype
               Currently it is limited up to 3D variables only.
   @enddesc

   @var        doBC
   @vdesc      flag telling whether to apply boundary conditions or not
   @vtype      int
   @vio        in
   @endvar
   @var        iend, jend, kend
   @vdesc      upper ranges for the loopers
   @vtype      int
   @vio        in
   @endvar
   @var        ii, jj, kk
   @vdesc      indices of the current grid point
   @vtype      int
   @vio        in
   @endvar
   @var        cctk_type
   @vdesc      CCTK datatype of the variable
   @vtype      <cctk_type>
   @vio        in
   @endvar
@@*/
#define SCALAR_BOUNDARY_TYPED(doBC,                                           \
                              iend, jend, kend,                               \
                              ii, jj, kk,                                     \
                              left_cctk_type, right_cctk_type, part)          \
{                                                                             \
  if (doBC)                                                                   \
  {                                                                           \
    for (k = 0; k < kend; k++)                                                \
    {                                                                         \
      for (j = 0; j < jend; j++)                                              \
      {                                                                       \
        for (i = 0; i < iend; i++)                                            \
        {                                                                     \
          int _index;                                                         \
                                                                              \
                                                                              \
          _index = INDEX_3D (lsh, ii, jj, kk);                                \
          ((left_cctk_type *) GH->data[var][timelvl])[_index] part = (right_cctk_type) scalar; \
        }                                                                     \
      }                                                                       \
    }                                                                         \
  }                                                                           \
}


/*@@
   @routine    SCALAR_BOUNDARY
   @date       Sat 20 Jan 2001
   @author     Thomas Radke
   @desc
               Macro to apply scalar boundary conditions to a variable
               of a given datatype in all directions
               Currently it is limited up to 3D variables only.
   @enddesc

   @var        cctk_type
   @vdesc      CCTK datatype of the variable
   @vtype      <cctk_type>
   @vio        in
   @endvar
@@*/
#define SCALAR_BOUNDARY(left_cctk_type, right_cctk_type, part)                \
{                                                                             \
  /* now set the boundaries face by face */                                   \
  if (gdim > 0)                                                               \
  {                                                                           \
    /* lower x */                                                             \
    SCALAR_BOUNDARY_TYPED (doBC[0], stencil[0], lssh[1], lssh[2],             \
                           i, j, k, left_cctk_type, right_cctk_type, part);   \
    /* upper x */                                                             \
    SCALAR_BOUNDARY_TYPED (doBC[1], stencil[0], lssh[1], lssh[2],             \
                           lssh[0]-i-1, j, k, left_cctk_type, right_cctk_type, part); \
  }                                                                           \
  if (gdim > 1)                                                               \
  {                                                                           \
    /* lower y */                                                             \
    SCALAR_BOUNDARY_TYPED (doBC[2], lssh[0], stencil[1], lssh[2],             \
                           i, j, k, left_cctk_type, right_cctk_type, part);   \
    /* upper y */                                                             \
    SCALAR_BOUNDARY_TYPED (doBC[3], lssh[0], stencil[1], lssh[2],             \
                           i, lssh[1]-j-1, k, left_cctk_type, right_cctk_type, part); \
  }                                                                           \
  if (gdim > 2)                                                               \
  {                                                                           \
    /* lower z */                                                             \
    SCALAR_BOUNDARY_TYPED (doBC[4], lssh[0], lssh[1], stencil[2],             \
                           i, j, k, left_cctk_type, right_cctk_type, part);   \
    /* upper z */                                                             \
    SCALAR_BOUNDARY_TYPED (doBC[5], lssh[0], lssh[1], stencil[2],             \
                           i, j, lssh[2]-k-1, left_cctk_type, right_cctk_type, part); \
  }                                                                           \
}


 /*@@
   @routine    ApplyBndScalar
   @date       Tue Jul 18 18:10:33 2000
   @author     Gerd Lanfermann
   @desc
               Apply scalar boundary conditions to a group of grid functions
               given by their indices
               This routine is called by the various BndScalarXXX wrappers.

               Although it is currently limited to handle 3D variables only
               it can easily be extended for other dimensions
               by adapting the appropriate macros.
   @enddesc
   @calls      CCTK_VarTypeI
               CCTK_GroupDimFromVarI
               SCALAR_BOUNDARY

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
   @vdesc      direction to set boundaries (0 for setting all directions)
   @vtype      int
   @vio        in
   @endvar
   @var        scalar
   @vdesc      scalar value to set the boundaries to
   @vtype      CCTK_REAL
   @vio        in
   @endvar
   @var        first_var
   @vdesc      index of first variable to apply boundary conditions to
   @vtype      int
   @vio        in
   @endvar
   @var        num_vars
   @vdesc      number of variables
   @vtype      int
   @vio        in
   @endvar

   @history
   @hdate      Tue 10 Apr 2001
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
   @endreturndesc
@@*/
static int ApplyBndScalar (const cGH *GH,
                           int stencil_dir,
                           const int *stencil_alldirs,
                           int dir,
                           CCTK_REAL scalar,
                           int first_var,
                           int num_vars)
{
  int i, j, k;
  int gindex, gdim;
  int var, timelvl;
  int doBC[2*MAXDIM], dstag[MAXDIM], lsh[MAXDIM], lssh[MAXDIM], stencil[MAXDIM];
  SymmetryGHex *sGHex;

  /* check the direction parameter */
  if (abs (dir) > MAXDIM)
  {
    CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,
                "ApplyBndScalar: direction %d is greater than maximum "
                "dimension %d", dir, MAXDIM);
    return (-1);
  }

  /* get the group index and dimensionality */
  gindex = CCTK_GroupIndexFromVarI (first_var);
  gdim   = CCTK_GroupDimI (gindex);

  /* check the dimensionality */
  if (gdim > MAXDIM)
  {
    CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,
                "ApplyBndScalar: variable dimension of %d not supported",
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
    CCTK_WARN (1, "ApplyBndScalar: NULL pointer passed "
                  "for stencil width array");
    return (-3);
  }

  /* initialize arrays for variables with less dimensions than MAXDIM
     so that we can use the INDEX_3D macro later on */
  for (i = gdim; i < MAXDIM; i++)
  {
    lsh[i]  = 0;
    lssh[i] = 1;
  }

  /* get the directional staggering of the group */
  CCTK_GroupStaggerDirArrayGI (dstag, gdim, gindex);

  /* get the current timelevel */
  timelvl = 0;

  /* see if we have a symmetry array */
  sGHex = (SymmetryGHex *) CCTK_GHExtension (GH, "Symmetry");

  /* now loop over all variables */
  for (var = first_var; var < first_var + num_vars; var++)
  {
    /* Apply condition if:
       + boundary is not a symmetry boundary (no symmetry or unset(=unsed))
       + boundary is a physical boundary
       + have enough grid points
    */
    memset (doBC, 1, sizeof (doBC));
    if (sGHex)
    {
      for (i = 0; i < 2 * gdim; i++)
      {
        doBC[i] = sGHex->GFSym[var][i] == GFSYM_NOSYM ||
                  sGHex->GFSym[var][i] == GFSYM_UNSET;
      }
    }
    for (i = 0; i < gdim; i++)
    {
      lsh[i]       = GH->cctk_lsh[i];
      lssh[i]      = GH->cctk_lssh[CCTK_LSSH_IDX (dstag[i], i)];
      doBC[i*2]   &= GH->cctk_lsh[i] > 1 && GH->cctk_bbox[i*2];
      doBC[i*2+1] &= GH->cctk_lsh[i] > 1 && GH->cctk_bbox[i*2+1];
      if (dir != 0)
      {
        doBC[i*2]   &= (dir < 0 && (i + 1 == abs (dir)));
        doBC[i*2+1] &= (dir > 0 && (i + 1 == abs (dir)));
      }
    }

    switch (CCTK_VarTypeI (var))
    {
      case CCTK_VARIABLE_CHAR:
        SCALAR_BOUNDARY (CCTK_CHAR, CCTK_CHAR, NOTHING); break;

      case CCTK_VARIABLE_INT:
        SCALAR_BOUNDARY (CCTK_INT, CCTK_INT, NOTHING); break;

      case CCTK_VARIABLE_REAL:
        SCALAR_BOUNDARY (CCTK_REAL, CCTK_REAL, NOTHING); break;

      case CCTK_VARIABLE_COMPLEX:
	SCALAR_BOUNDARY (CCTK_COMPLEX, CCTK_REAL, .Re);
	scalar = 0.;
	SCALAR_BOUNDARY (CCTK_COMPLEX, CCTK_REAL, .Im); break;

#ifdef CCTK_INT2
      case CCTK_VARIABLE_INT2:
        SCALAR_BOUNDARY (CCTK_INT2, CCTK_INT2, NOTHING); break;
#endif

#ifdef CCTK_INT4
      case CCTK_VARIABLE_INT4:
        SCALAR_BOUNDARY (CCTK_INT4, CCTK_INT4, NOTHING); break;
#endif

#ifdef CCTK_INT8
      case CCTK_VARIABLE_INT8:
        SCALAR_BOUNDARY (CCTK_INT8, CCTK_INT8, NOTHING); break;
#endif

#ifdef CCTK_REAL4
      case CCTK_VARIABLE_REAL4:
        SCALAR_BOUNDARY (CCTK_REAL4, CCTK_REAL4, NOTHING); break;
#endif

#ifdef CCTK_REAL8
      case CCTK_VARIABLE_REAL8:
        SCALAR_BOUNDARY (CCTK_REAL8, CCTK_REAL8, NOTHING); break;
#endif

#ifdef CCTK_REAL16
      case CCTK_VARIABLE_REAL16:
        SCALAR_BOUNDARY (CCTK_REAL16, CCTK_REAL16, NOTHING); break;
#endif

      default:
        CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,
                    "Unsupported variable type %d for variable '%s'",
                    CCTK_VarTypeI (var), CCTK_VarName (var));
        return (-4);
    }
  }

  return(0);
}
