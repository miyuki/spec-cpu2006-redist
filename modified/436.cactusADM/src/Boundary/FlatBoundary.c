#ifdef SPEC_CPU
# define THORN_IS_Boundary
#endif /* SPEC_CPU */
 /*@@
   @file      FlatBoundary.c
   @date      Mon Mar 15 15:09:00 1999
   @author    Gerd Lanfermann
   @desc
              Routines for applying flat boundary conditions
   @enddesc
   @history
   @hdate     Tue 10 Apr 2001
   @hauthor   Thomas Radke
   @hdesc     BC routines generalized for applying to arbitrary CCTK data types
   @endhistory
   @version   $Id: FlatBoundary.c,v 1.32 2001/09/12 09:27:13 tradke Exp $
 @@*/

/*#define DEBUG_BOUNDARY*/

#include <stdlib.h>
#include <string.h>

#include "cctk.h"
#include "cctk_Parameters.h"
#include "cctk_FortranString.h"

#include "Symmetry.h"
#include "Boundary.h"

/* the rcs ID and its dummy function to use it */
static const char *rcsid = "$Header: /cactus/CactusBase/Boundary/src/FlatBoundary.c,v 1.32 2001/09/12 09:27:13 tradke Exp $";
CCTK_FILEVERSION(CactusBase_Boundary_FlatBoundary_c)


/********************************************************************
 ********************    External Routines   ************************
 ********************************************************************/
/* prototypes for external C routines are declared in header Boundary.h
   here only follow the fortran wrapper prototypes */
void CCTK_FCALL bndflatdirgi_
                           (int *ierr,
                            const cGH *GH,
                            const int *stencil_size,
                            const int *dir,
                            const int *gi);
void CCTK_FCALL bndflatgi_
                           (int *ierr,
                            const cGH *GH,
                            const int *stencil,
                            const int *gi);
void CCTK_FCALL bndflatdirgn_
                           (int *ierr,
                            const cGH *GH,
                            const int *stencil_size,
                            const int *dir,
                            ONE_FORTSTRING_ARG);
void CCTK_FCALL bndflatgn_
                           (int *ierr,
                            const cGH *GH,
                            const int *stencil,
                            ONE_FORTSTRING_ARG);
void CCTK_FCALL bndflatdirvi_
                           (int *ierr,
                            const cGH *GH,
                            const int *stencil_size,
                            const int *dir,
                            const int *vi);
void CCTK_FCALL bndflatvi_
                           (int *ierr,
                            const cGH *GH,
                            const int *stencil,
                            const int *vi);
void CCTK_FCALL bndflatdirvn_
                           (int *ierr,
                            const cGH *GH,
                            const int *stencil_size,
                            const int *dir,
                            ONE_FORTSTRING_ARG);
void CCTK_FCALL bndflatvn_
                           (int *ierr,
                            const cGH *GH,
                            const int *stencil,
                            ONE_FORTSTRING_ARG);


/********************************************************************
 ********************    Internal Routines   ************************
 ********************************************************************/
static int ApplyBndFlat (const cGH *GH,
                         int stencil_dir,
                         const int *stencil_alldirs,
                         int dir,
                         int first_var,
                         int num_vars);


/*@@
   @routine    BndFlatDirGI
   @date       Sun Jan 21 2001
   @author     Gabrielle Allen
   @desc
               Apply flat boundary conditions by group index in given direction
   @enddesc
   @calls      ApplyBndFlat

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
   @vdesc      direction to apply boundaries
   @vtype      int
   @vio        in
   @endvar
   @var        gi
   @vdesc      index of group to apply boundaries to
   @vtype      int
   @vio        in
   @endvar

   @returntype int
   @returndesc
               return code of @seeroutine ApplyBndFlat <BR>
               -1 if invalid group index was given
   @endreturndesc
@@*/
int BndFlatDirGI (const cGH *GH,
                  int stencil_size,
                  int dir,
                  int gi)
{
  int first_vi, retval;


  first_vi = CCTK_FirstVarIndexI (gi);
  if (first_vi >= 0)
  {
    retval = ApplyBndFlat (GH, stencil_size, NULL, dir, first_vi,
                           CCTK_NumVarsInGroupI (gi));
  }
  else
  {
    CCTK_VWarn (2, __LINE__, __FILE__, CCTK_THORNSTRING,
                "Invalid group index %d in BndFlatDirGI", gi);
    retval = -1;
  }

  return (retval);
}

void CCTK_FCALL bndflatdirgi_
                           (int *ierr,
                            const cGH *GH,
                            const int *stencil_size,
                            const int *dir,
                            const int *gi)
{
  *ierr = BndFlatDirGI (GH, *stencil_size, *dir, *gi);
}


/*@@
   @routine    BndFlatGI
   @date       Thu Mar  2 11:11:40 2000
   @author     Gerd Lanfermann
   @desc
               Apply flat boundary conditions by group index
   @enddesc
   @calls      ApplyBndFlat

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
   @var        gi
   @vdesc      index of group to apply boundaries to
   @vtype      int
   @vio        in
   @endvar

   @returntype int
   @returndesc
               return code of @seeroutine ApplyBndFlat <BR>
               -1 if invalid group index was given
   @endreturndesc
@@*/
int BndFlatGI (const cGH *GH,
               const int *stencil,
               int gi)
{
  int first_vi, retval;


  first_vi = CCTK_FirstVarIndexI (gi);
  if (first_vi >= 0)
  {
    retval = ApplyBndFlat (GH, -1, stencil, 0, first_vi,
                           CCTK_NumVarsInGroupI (gi));
  }
  else
  {
    CCTK_VWarn (2, __LINE__, __FILE__, CCTK_THORNSTRING,
                "Invalid group index %d in BndFlatGI", gi);
    retval = -1;
  }

  return (retval);
}

void CCTK_FCALL bndflatgi_
                           (int *ierr,
                            const cGH *GH,
                            const int *stencil,
                            const int *gi)
{
  *ierr = BndFlatGI (GH, stencil, *gi);
}


/* ===================================================================== */

/*@@
   @routine    BndFlatDirGN
   @date       Sun Jan 21 2001
   @author     Gabrielle Allen
   @desc
               Apply flat boundary conditions by group name in given direction
   @enddesc
   @calls      BndFlatDirGI

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
   @vdesc      direction to apply boundaries
   @vtype      int
   @vio        in
   @endvar
   @var        gn
   @vdesc      name of group to apply boundaries to
   @vtype      const char *
   @vio        in
   @endvar

   @returntype int
   @returndesc
               return code of @seeroutine BndFlatDirGI <BR>
               -1 if invalid group name was given
   @endreturndesc
@@*/
int BndFlatDirGN (const cGH *GH,
                  int stencil_size,
                  int dir,
                  const char *gn)
{
  int gi, retval;


  gi = CCTK_GroupIndex (gn);
  if (gi >= 0)
  {
    retval = BndFlatDirGI (GH, stencil_size, dir, gi);
  }
  else
  {
    CCTK_VWarn (2, __LINE__, __FILE__, CCTK_THORNSTRING,
                "Invalid group name '%s' in BndFlatDirGN", gn);
    retval = -1;
  }

  return (retval);
}

void CCTK_FCALL bndflatdirgn_
                           (int *ierr,
                            const cGH *GH,
                            const int *stencil_size,
                            const int *dir,
                            ONE_FORTSTRING_ARG)
{
  ONE_FORTSTRING_CREATE (gn)
  *ierr = BndFlatDirGN (GH, *stencil_size, *dir, gn);
  free (gn);
}


/*@@
   @routine    BndFlatGN
   @date       Thu Mar  2 11:11:40 2000
   @author     Gerd Lanfermann
   @desc
               Apply flat boundary conditions by group name
   @enddesc
   @calls      BndFlatGI

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
   @var        gn
   @vdesc      name of group to apply boundaries to
   @vtype      const char *
   @vio        in
   @endvar

   @returntype int
   @returndesc
               return code of @seeroutine BndFlatGI <BR>
               -1 if invalid group name was given
   @endreturndesc
@@*/
int BndFlatGN (const cGH *GH,
               const int *stencil,
               const char *gn)
{
  int gi, retval;


  gi = CCTK_GroupIndex (gn);
  if (gi >= 0)
  {
    retval = BndFlatGI (GH, stencil, gi);
  }
  else
  {
    CCTK_VWarn (2, __LINE__, __FILE__, CCTK_THORNSTRING,
                "Invalid group name '%s' in BndFlatGN", gn);
    retval = -1;
  }

  return (retval);
}

void CCTK_FCALL bndflatgn_
                           (int *ierr,
                            const cGH *GH,
                            const int *stencil,
                            ONE_FORTSTRING_ARG)
{
  ONE_FORTSTRING_CREATE (gn)
  *ierr = BndFlatGN (GH, stencil, gn);
  free (gn);
}


/* ===================================================================== */

/*@@
   @routine    BndFlatDirVI
   @date       Sun Jan 21 2001
   @author     Gabrielle Allen
   @desc
               Apply flat boundary conditions by variable index
               in given direction
   @enddesc
   @calls      ApplyBndFlat

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
   @vdesc      direction to apply boundaries
   @vtype      int
   @vio        in
   @endvar
   @var        vi
   @vdesc      index of variable to apply boundaries to
   @vtype      int
   @vio        in
   @endvar

   @returntype int
   @returndesc
               return code of @seeroutine ApplyBndFlat <BR>
               -1 if invalid variable index was given
   @endreturndesc
@@*/
int BndFlatDirVI (const cGH *GH,
                  int stencil_size,
                  int dir,
                  int vi)
{
  int retval;


  if (vi >= 0 && vi < CCTK_NumVars ())
  {
    retval = ApplyBndFlat (GH, stencil_size, NULL, dir, vi, 1);
  }
  else
  {
    CCTK_VWarn (2, __LINE__, __FILE__, CCTK_THORNSTRING,
                "Invalid variable index %d in BndFlatDirVI", vi);
    retval = -1;
  }

  return (retval);
}

void CCTK_FCALL bndflatdirvi_
                           (int *ierr,
                            const cGH *GH,
                            const int *stencil_size,
                            const int *dir,
                            const int *vi)
{
  *ierr = BndFlatDirVI (GH, *stencil_size, *dir, *vi);
}


/*@@
   @routine    BndFlatVI
   @date       Thu Mar  2 11:11:40 2000
   @author     Gerd Lanfermann
   @desc
               Apply flat boundary conditions by variable index
   @enddesc
   @calls      ApplyBndFlat

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
   @var        vi
   @vdesc      index of variable to apply boundaries to
   @vtype      int
   @vio        in
   @endvar

   @returntype int
   @returndesc
               return code of @seeroutine ApplyBndFlat <BR>
               -1 if invalid variable index was given
   @endreturndesc
@@*/
int BndFlatVI (const cGH *GH,
               const int *stencil,
               int vi)
{
  int retval;


  if (vi >= 0 && vi < CCTK_NumVars ())
  {
    retval = ApplyBndFlat (GH, -1, stencil, 0, vi, 1);
  }
  else
  {
    CCTK_VWarn (2, __LINE__, __FILE__, CCTK_THORNSTRING,
                "Invalid variable index %d in BndFlatVI", vi);
    retval = -1;
  }

  return (retval);
}

void CCTK_FCALL bndflatvi_
                           (int *ierr,
                            const cGH *GH,
                            const int *stencil,
                            const int *vi)
{
  *ierr = BndFlatVI (GH, stencil, *vi);
}


/* ======================================================================= */

/*@@
   @routine    BndFlatDirVN
   @date       Sun Jan 21 2001
   @author     Gabrielle Allen
   @desc
               Apply flat boundary conditions by variable name
               in given direction
   @enddesc
   @calls      BndFlatDirVI

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
   @vdesc      direction to apply boundaries
   @vtype      int
   @vio        in
   @endvar
   @var        vn
   @vdesc      name of variable to apply boundaries to
   @vtype      const char *
   @vio        in
   @endvar

   @returntype int
   @returndesc
               return code of @seeroutine BndFlatDirVI <BR>
               -1 if invalid variable name was given
   @endreturndesc
@@*/
int BndFlatDirVN (const cGH *GH,
                  int stencil_size,
                  int dir,
                  const char *vn)
{
  int vi, retval;


  vi = CCTK_VarIndex (vn);
  if (vi >= 0)
  {
    retval = BndFlatDirVI (GH, stencil_size, dir, vi);
  }
  else
  {
    CCTK_VWarn (2, __LINE__, __FILE__, CCTK_THORNSTRING,
                "Invalid variable name '%s' in BndFlatDirVN", vn);
    retval = -1;
  }

  return (retval);
}

void CCTK_FCALL bndflatdirvn_
                           (int *ierr,
                            const cGH *GH,
                            const int *stencil_size,
                            const int *dir,
                            ONE_FORTSTRING_ARG)
{
  ONE_FORTSTRING_CREATE (vn)
  *ierr = BndFlatDirVN (GH, *stencil_size, *dir, vn);
  free (vn);
}


/*@@
   @routine    BndFlatVN
   @date       Thu Mar  2 11:11:40 2000
   @author     Gerd Lanfermann
   @desc
               Apply flat boundary conditions by variable name
   @enddesc
   @calls      BndFlatVI

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
   @var        vn
   @vdesc      name of variable to apply boundaries to
   @vtype      const char *
   @vio        in
   @endvar

   @returntype int
   @returndesc
               return code of @seeroutine BndFlatVI <BR>
               -1 if invalid variable name was given
   @endreturndesc
@@*/
int BndFlatVN (const cGH *GH,
               const int *stencil,
               const char *vn)
{
  int vi, retval;


  vi = CCTK_VarIndex (vn);
  if (vi >= 0)
  {
    retval = BndFlatVI (GH, stencil, vi);
  }
  else
  {
    CCTK_VWarn (2, __LINE__, __FILE__, CCTK_THORNSTRING,
                "Invalid variable name '%s' in BndFlatVN", vn);
    retval = -1;
  }

  return (retval);
}

void CCTK_FCALL bndflatvn_
                           (int *ierr,
                            const cGH *GH,
                            const int *stencil,
                            ONE_FORTSTRING_ARG)
{
  ONE_FORTSTRING_CREATE (vn)
  *ierr = BndFlatVN (GH, stencil, vn);
  free (vn);
}


/********************************************************************
 *********************     Local Routines   *************************
 ********************************************************************/

/* maximum dimension we can deal with */
#define MAXDIM  3

/* macro to compute the linear index of a 3D point */
#define INDEX_3D(lsh, i, j, k)      ((i) + (lsh)[0]*((j) + (lsh)[1]*(k)))

/*@@
   @routine    FLAT_BOUNDARY
   @date       Tue 10 Apr 2001
   @author     Thomas Radke
   @desc
               Macro to apply flat boundary conditions to a variable
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
   @var        ii_to, jj_to, kk_to
   @vdesc      indices of the current grid point to copy to
   @vtype      int
   @vio        in
   @endvar
   @var        ii_from, jj_from, kk_from
   @vdesc      indices of the current grid point to copy from
   @vtype      int
   @vio        in
   @endvar
@@*/
#define FLAT_BOUNDARY(doBC,                                                   \
                      iend, jend, kend,                                       \
                      ii_to, jj_to, kk_to,                                    \
                      ii_from, jj_from, kk_from)                              \
{                                                                             \
  if (doBC)                                                                   \
  {                                                                           \
    for (k = 0; k < kend; k++)                                                \
    {                                                                         \
      for (j = 0; j < jend; j++)                                              \
      {                                                                       \
        for (i = 0; i < iend; i++)                                            \
        {                                                                     \
          int _index_to, _index_from;                                         \
                                                                              \
          _index_to   = INDEX_3D (lsh, ii_to, jj_to, kk_to) * vtypesize;      \
          _index_from = INDEX_3D (lsh, ii_from, jj_from, kk_from) * vtypesize;\
          memcpy ((char *) GH->data[var][timelvl] + _index_to,                \
                  (char *) GH->data[var][timelvl] + _index_from,              \
                  vtypesize);                                                 \
        }                                                                     \
      }                                                                       \
    }                                                                         \
  }                                                                           \
}


/*@@
   @routine    ApplyBndFlat
   @date       Jul 5 2000
   @author     Gabrielle Allen, Gerd Lanfermann
   @desc
               Apply flat boundary conditions to a group of grid functions
               given by their indices
               This routine is called by the various BndFlatXXX wrappers.

               Although it is currently limited to handle 1D, 2D, or 3D
               variables only it can easily be extended for higher dimensions
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
   @vdesc      direction to set boundaries (0 for setting all directions)
   @vtype      int
   @vio        in
   @endvar
   @var        first_var
   @vdesc      index of first variable to apply boundaries to
   @vtype      int
   @vio        in
   @endvar
   @var        num_vars
   @vdesc      number of variables
   @vtype      int
   @vio        in
   @endvar

   @calls      CCTK_GroupIndexFromVarI
               CCTK_GroupDimI
               CCTK_VarTypeI
               CCTK_GroupStaggerDirArrayGI
               FLAT_BOUNDARY
   @history
   @hdate      Tue 10 Apr 2001
   @hauthor    Thomas Radke
   @hdesc      Merged separate routines for 1D, 2D, and 3D
               into a single generic routine
   @endhistory

   @returntype int
   @returndesc
                0 for success
               -1 if dimension is not supported
               -2 if direction parameter is invalid
               -3 if stencil width array parameter is NULL
   @endreturndesc
@@*/
static int ApplyBndFlat (const cGH *GH,
                         int stencil_dir,
                         const int *stencil_alldirs,
                         int dir,
                         int first_var,
                         int num_vars)
{
  int i, j, k;
  int var, vtypesize, gindex, gdim, timelvl;
  int doBC[2*MAXDIM], dstag[MAXDIM], lsh[MAXDIM], lssh[MAXDIM], stencil[MAXDIM];
  SymmetryGHex *sGHex;


  /* get the group index of the variables */
  gindex = CCTK_GroupIndexFromVarI (first_var);

  /* get the number of dimensions and the size of the variables' type */
  gdim      = CCTK_GroupDimI (gindex);
  vtypesize = CCTK_VarTypeSize (CCTK_VarTypeI (first_var));

  /* make sure we can deal with this number of dimensions */
  if (gdim > MAXDIM)
  {
    CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,
                "ApplyBndFlat: Variable dimension of %d not supported", gdim);
    return (-1);
  }

  /* check the direction parameter */
  if (abs (dir) > gdim)
  {
    CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,
                "ApplyBndFlat: direction %d greater than dimension %d",
                dir, gdim);
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
    CCTK_WARN (1, "ApplyBndFlat: NULL pointer passed for stencil width array");
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

    /* now apply the boundaries face by face */
    if (gdim > 0)
    {
#ifdef DEBUG_BOUNDARY
      if (doBC[0])
      {
	printf("Boundary: Applying lower x flat boundary condition\n");
      }
      if (doBC[1])
      {
	printf("Boundary: Applying upper x flat boundary condition\n");
      }
#endif /* DEBUG_BOUNDARY */
      /* lower x */
      FLAT_BOUNDARY (doBC[0], stencil[0], lssh[1], lssh[2],
                     i, j, k, stencil[0], j, k);
      /* upper x */
      FLAT_BOUNDARY (doBC[1], stencil[0], lssh[1], lssh[2],
                     lssh[0]-i-1, j, k, lssh[0]-stencil[0]-1, j, k);

    }
    if (gdim > 1)
    {
#ifdef DEBUG_BOUNDARY
      if (doBC[2])
      {
	printf("Boundary: Applying lower y flat boundary condition\n");
      }
      if (doBC[3])
      {
	printf("Boundary: Applying upper y flat boundary condition\n");
      }
#endif /* DEBUG_BOUNDARY */
      /* lower y */
      FLAT_BOUNDARY (doBC[2], lssh[0], stencil[1], lssh[2],
                     i, j, k, i, stencil[1], k);
      /* upper y */
      FLAT_BOUNDARY (doBC[3], lssh[0], stencil[1], lssh[2],
                     i, lssh[1]-j-1, k, i, lssh[1]-stencil[1]-1, k);
    }
    if (gdim > 2)
    {
#ifdef DEBUG_BOUNDARY
      if (doBC[4])
      {
	printf("Boundary: Applying lower z flat boundary condition\n");
      }
      if (doBC[5])
      {
	printf("Boundary: Applying upper z flat boundary condition\n");
      }
#endif /* DEBUG_BOUNDARY */
      /* lower z */
      FLAT_BOUNDARY (doBC[4], lssh[0], lssh[1], stencil[2],
                     i, j, k, i, j, stencil[2]);
      /* upper z */
      FLAT_BOUNDARY (doBC[5], lssh[0], lssh[1], stencil[2],
                     i, j, lssh[2]-k-1, i, j, lssh[2]-stencil[2]-1);
    }
  }

  return(0);
}
