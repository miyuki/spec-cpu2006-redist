#ifdef SPEC_CPU
# define THORN_IS_Boundary
#endif /* SPEC_CPU */
 /*@@
   @file      CopyBoundary.c
   @date      Mon Mar 15 15:09:00 1999
   @author    Gerd Lanfermann, Gabrielle Allen
   @desc
              Routines for applying copying-boundary conditions
   @enddesc
   @history
   @hdate     Sun 25 Feb 2001
   @hauthor   Thomas Radke
   @hdesc     BC routines generalized for applying to arbitrary CCTK data types
   @endhistory
   @version   $Id: CopyBoundary.c,v 1.21 2001/09/14 11:46:22 allen Exp $
 @@*/

#include <stdlib.h>
#include <string.h>

#include "cctk.h"
#include "cctk_FortranString.h"

#include "Symmetry.h"
#include "Boundary.h"

/* the rcs ID and its dummy function to use it */
static const char *rcsid = "$Header: /cactus/CactusBase/Boundary/src/CopyBoundary.c,v 1.21 2001/09/14 11:46:22 allen Exp $";
CCTK_FILEVERSION(CactusBase_Boundary_CopyBoundary_c)


/********************************************************************
 ********************    External Routines   ************************
 ********************************************************************/
/* prototypes for external C routines are declared in header Boundary.h
   here only follow the fortran wrapper prototypes */
void CCTK_FCALL bndcopydirvi_
                           (int *ierr,
                            const cGH *GH,
                            const int *stencil_size,
                            const int *dir,
                            const int *vi_to,
                            const int *vi_from);
void CCTK_FCALL bndcopyvi_
                           (int *ierr,
                            const cGH *GH,
                            const int *stencil,
                            const int *vi_to,
                            const int *vi_from);
void CCTK_FCALL bndcopydirgi_
                           (int *ierr,
                            const cGH *GH,
                            const int *stencil_size,
                            const int *dir,
                            const int *gi_to,
                            const int *gi_from);
void CCTK_FCALL bndcopygi_
                           (int *ierr,
                            const cGH *GH,
                            const int *stencil,
                            const int *gi_to,
                            const int *gi_from);
void CCTK_FCALL bndcopydirvn_
                           (int *ierr,
                            const cGH *GH,
                            const int *stencil_size,
                            const int *dir,
                            TWO_FORTSTRING_ARG);
void CCTK_FCALL bndcopyvn_
                           (int *ierr,
                            const cGH *GH,
                            const int *stencil,
                            TWO_FORTSTRING_ARG);
void CCTK_FCALL bndcopydirgn_
                           (int *ierr,
                            const cGH *GH,
                            const int *stencil_size,
                            const int *dir,
                            TWO_FORTSTRING_ARG);
void CCTK_FCALL bndcopygn_
                           (int *ierr,
                            const cGH *GH,
                            const int *stencil,
                            TWO_FORTSTRING_ARG);


/********************************************************************
 ********************    Internal Routines   ************************
 ********************************************************************/
static int ApplyBndCopy (const cGH *GH,
                         int stencil_dir,
                         const int *stencil_alldirs,
                         int dir,
                         int first_var_to,
                         int first_var_from,
                         int num_vars);


/*@@
   @routine    BndCopyDirVI
   @date       Sat Jan 20 2001
   @author     Gabrielle Allen
   @desc
               Apply copy boundary routines by var index in given direction
   @enddesc
   @calls      ApplyBndCopy

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
   @vdesc      direction to copy
   @vtype      int
   @vio        in
   @endvar
   @var        vi_to
   @vdesc      index of variable to copy boundaries to
   @vtype      int
   @vio        in
   @endvar
   @var        vi_from
   @vdesc      index of variable to copy boundaries from
   @vtype      int
   @vio        in
   @endvar

   @returntype int
   @returndesc
               return code of @seeroutine ApplyBndCopy <BR>
               -1 if invalid variable indices are given
   @endreturndesc
@@*/
int BndCopyDirVI (const cGH *GH,
                  int stencil_size,
                  int dir,
                  int vi_to,
                  int vi_from)
{
  int retval, num_vars;


  num_vars = CCTK_NumVars ();
  if (vi_to >= 0 && vi_to < num_vars && vi_from >= 0 && vi_from < num_vars)
  {
    retval = ApplyBndCopy (GH, stencil_size, NULL, dir, vi_to, vi_from, 1);
  }
  else
  {
    CCTK_VWarn (2, __LINE__, __FILE__, CCTK_THORNSTRING,
                "Invalid variable indices %d and/or %d in BndCopyDirVI",
                vi_to, vi_from);
    retval = -1;
  }

  return (retval);
}

void CCTK_FCALL bndcopydirvi_
                           (int *ierr,
                            const cGH *GH,
                            const int *stencil_size,
                            const int *dir,
                            const int *vi_to,
                            const int *vi_from)
{
  *ierr = BndCopyDirVI (GH, *stencil_size, *dir, *vi_to, *vi_from);
}


/*@@
   @routine    BndCopyVI
   @date       Thu Mar  2 11:02:10 2000
   @author     Gerd Lanfermann
   @desc
               Apply copy boundary routines by var index
   @enddesc
   @calls      ApplyBndCopy

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
   @var        vi_to
   @vdesc      index of variable to copy boundaries to
   @vtype      int
   @vio        in
   @endvar
   @var        vi_from
   @vdesc      index of variable to copy boundaries from
   @vtype      int
   @vio        in
   @endvar

   @returntype int
   @returndesc
               return code of @seeroutine ApplyBndCopy <BR>
               -1 if invalid variable indices are given
   @endreturndesc
@@*/
int BndCopyVI (const cGH *GH,
               const int *stencil,
               int vi_to,
               int vi_from)
{
  int retval, num_vars;


  num_vars = CCTK_NumVars ();
  if (vi_to >= 0 && vi_to < num_vars && vi_from >= 0 && vi_from < num_vars)
  {
    retval = ApplyBndCopy (GH, -1, stencil, 0, vi_to, vi_from, 1);
  }
  else
  {
    CCTK_VWarn (2, __LINE__, __FILE__, CCTK_THORNSTRING,
                "Invalid variable indices %d and/or %d in BndCopyVI",
                vi_to, vi_from);
    retval = -1;
  }

  return (retval);
}

void CCTK_FCALL bndcopyvi_
                           (int *ierr,
                            const cGH *GH,
                            const int *stencil,
                            const int *vi_to,
                            const int *vi_from)
{
  *ierr = BndCopyVI (GH, stencil, *vi_to, *vi_from);
}


/* ====================================================== */

 /*@@
   @routine    BndCopyDirGI
   @date       Sat Jan 20 2001
   @author     Gabrielle Allen
   @desc
               Apply copy boundaries by group index in given direction
   @enddesc
   @calls      ApplyBndCopy

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
   @vdesc      direction to copy boundaries
   @vtype      int
   @vio        in
   @endvar
   @var        gi_to
   @vdesc      index of group to copy boundaries to
   @vtype      int
   @vio        in
   @endvar
   @var        gi_from
   @vdesc      index of group to copy boundaries from
   @vtype      int
   @vio        in
   @endvar

   @returntype int
   @returndesc
               return code of @seeroutine ApplyBndCopy <BR>
               -1 if invalid group indices are given
   @endreturndesc
@@*/
int BndCopyDirGI (const cGH *GH,
                  int stencil_size,
                  int dir,
                  int gi_to,
                  int gi_from)
{
  int first_vi_to, first_vi_from, retval;


  first_vi_to   = CCTK_FirstVarIndexI (gi_to);
  first_vi_from = CCTK_FirstVarIndexI (gi_from);
  if (first_vi_to >= 0 && first_vi_from >= 0)
  {
    retval = ApplyBndCopy (GH, stencil_size, NULL, dir, first_vi_to,
                           first_vi_from, CCTK_NumVarsInGroupI (gi_to));
  }
  else
  {
    CCTK_VWarn (2, __LINE__, __FILE__, CCTK_THORNSTRING,
                "Invalid group indices %d and/or %d in BndCopyDirGI",
                gi_to, gi_from);
    retval = -1;
  }

  return (retval);
}

void CCTK_FCALL bndcopydirgi_
                           (int *ierr,
                            const cGH *GH,
                            const int *stencil_size,
                            const int *dir,
                            const int *gi_to,
                            const int *gi_from)
{
  *ierr = BndCopyDirGI (GH, *stencil_size, *dir, *gi_to, *gi_from);
}


 /*@@
   @routine    BndCopyGI
   @date       Thu Mar  2 11:07:11 2000
   @author     Gerd Lanfermann
   @desc
               Apply copy boundaries by group index
   @enddesc
   @calls      ApplyBndCopy

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
   @var        gi_to
   @vdesc      index of group to copy boundaries to
   @vtype      int
   @vio        in
   @endvar
   @var        gi_from
   @vdesc      index of group to copy boundaries from
   @vtype      int
   @vio        in
   @endvar

   @returntype int
   @returndesc
               return code of @seeroutine ApplyBndCopy <BR>
               -1 if invalid group indices are given
   @endreturndesc
@@*/
int BndCopyGI (const cGH *GH,
               const int *stencil,
               int gi_to,
               int gi_from)
{
  int first_vi_to, first_vi_from, retval;


  first_vi_to   = CCTK_FirstVarIndexI (gi_to);
  first_vi_from = CCTK_FirstVarIndexI (gi_from);
  if (first_vi_to >= 0 && first_vi_from >= 0)
  {
    retval = ApplyBndCopy (GH, -1, stencil, 0, first_vi_to, first_vi_from,
                           CCTK_NumVarsInGroupI (gi_to));
  }
  else
  {
    CCTK_VWarn (2, __LINE__, __FILE__, CCTK_THORNSTRING,
                "Invalid group indices %d and/or %d in BndCopyGI",
                gi_to, gi_from);
    retval = -1;
  }

  return (retval);
}

void CCTK_FCALL bndcopygi_
                           (int *ierr,
                            const cGH *GH,
                            const int *stencil,
                            const int *gi_to,
                            const int *gi_from)
{
  *ierr = BndCopyGI (GH, stencil, *gi_to, *gi_from);
}


/* ======================================================= */

/*@@
   @routine    BndCopyDirGN
   @date       Sat Jan 20 2001
   @author     Gabrielle Allen
   @desc
               Apply copy boundary routines by group name in given direction
   @enddesc
   @calls      BndCopyDirGI

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
   @vdesc      direction to copy boundaries
   @vtype      int
   @vio        in
   @endvar
   @var        gname_to
   @vdesc      name of group to copy boundaries to
   @vtype      const char *
   @vio        in
   @endvar
   @var        gname_from
   @vdesc      name of group to copy boundaries from
   @vtype      const char *
   @vio        in
   @endvar

   @returntype int
   @returndesc
               return code of @seeroutine BndCopyDirGI <BR>
               -1 if invalid group names are given
   @endreturndesc
@@*/
int BndCopyDirGN (const cGH *GH,
                  int stencil_size,
                  int dir,
                  const char *gname_to,
                  const char *gname_from)
{
  int gi_to, gi_from, num_groups, retval;


  gi_to      = CCTK_GroupIndex (gname_to);
  gi_from    = CCTK_GroupIndex (gname_from);
  num_groups = CCTK_NumGroups ();

  if (gi_to >= 0 && gi_to < num_groups && gi_from >= 0 && gi_from < num_groups)
  {
    retval = BndCopyDirGI (GH, stencil_size, dir, gi_to, gi_from);
  }
  else
  {
    CCTK_VWarn (2, __LINE__, __FILE__, CCTK_THORNSTRING,
                "Invalid group names '%s' and/or '%s' in BndCopyDirGN",
                gname_to, gname_from);
    retval = -1;
  }

  return (retval);
}

void CCTK_FCALL bndcopydirgn_
                           (int *ierr,
                            const cGH *GH,
                            const int *stencil_size,
                            const int *dir,
                            TWO_FORTSTRING_ARG)
{
  TWO_FORTSTRINGS_CREATE (gname_to, gname_from)
  *ierr = BndCopyDirGN (GH, *stencil_size, *dir, gname_to, gname_from);
  free (gname_to);
  free (gname_from);
}


/*@@
   @routine    BndCopyGN
   @date       Thu Mar  2 11:02:10 2000
   @author     Gerd Lanfermann
   @desc
               Apply copy boundary routines by group name
   @enddesc
   @calls      BndCopyGI

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
   @var        gname_to
   @vdesc      name of group to copy boundaries to
   @vtype      const char *
   @vio        in
   @endvar
   @var        gname_from
   @vdesc      name of group to copy boundaries from
   @vtype      const char *
   @vio        in
   @endvar

   @returntype int
   @returndesc
               return code of @seeroutine BndCopyGI <BR>
               -1 if invalid group names are given
   @endreturndesc
@@*/
int BndCopyGN (const cGH *GH,
               const int *stencil,
               const char *gname_to,
               const char *gname_from)
{
  int gi_to, gi_from, num_groups, retval;


  gi_to      = CCTK_GroupIndex (gname_to);
  gi_from    = CCTK_GroupIndex (gname_from);
  num_groups = CCTK_NumGroups ();

  if (gi_to >= 0 && gi_to < num_groups && gi_from >= 0 && gi_from < num_groups)
  {
    retval = BndCopyGI (GH, stencil, gi_to, gi_from);
  }
  else
  {
    CCTK_VWarn (2, __LINE__, __FILE__, CCTK_THORNSTRING,
                "Invalid group names '%s' and/or '%s' in BndCopyGN",
                gname_to, gname_from);
    retval = -1;
  }

  return (retval);
}

void CCTK_FCALL bndcopygn_
                           (int *ierr,
                            const cGH *GH,
                            const int *stencil,
                            TWO_FORTSTRING_ARG)
{
  TWO_FORTSTRING_CREATE (gname_to, gname_from)
  *ierr = BndCopyGN (GH, stencil, gname_to, gname_from);
  free (gname_to);
  free (gname_from);
}


/* ======================================================= */

/*@@
   @routine    BndCopyDirVN
   @date       Sat Jan 20 2001
   @author     Gabrielle Allen
   @desc
               Apply copy boundary routines by group name in given direction
   @enddesc
   @calls      BndCopyDirVI

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
   @vdesc      direction to copy boundaries
   @vtype      int
   @vio        in
   @endvar
   @var        vname_to
   @vdesc      name of variable to copy boundaries to
   @vtype      const char *
   @vio        in
   @endvar
   @var        vname_from
   @vdesc      name of variable to copy boundaries from
   @vtype      const char *
   @vio        in
   @endvar

   @returntype int
   @returndesc
               return code of @seeroutine BndCopyDirVI <BR>
               -1 if invalid variable names are given
   @endreturndesc
@@*/
int BndCopyDirVN (const cGH *GH,
                  int stencil_size,
                  int dir,
                  const char *vname_to,
                  const char *vname_from)
{
  int vi_to, vi_from, num_vars, retval;


  vi_to    = CCTK_VarIndex (vname_to);
  vi_from  = CCTK_VarIndex (vname_from);
  num_vars = CCTK_NumVars ();

  if (vi_to >= 0 && vi_to < num_vars && vi_from >= 0 && vi_from < num_vars)
  {
    retval = BndCopyDirVI (GH, stencil_size, dir, vi_to, vi_from);
  }
  else
  {
    CCTK_VWarn (2, __LINE__, __FILE__, CCTK_THORNSTRING,
                "Invalid variable names '%s' and/or '%s' in BndCopyDirVN",
                vname_to, vname_from);
    retval = -1;
  }

  return (retval);
}

void CCTK_FCALL bndcopydirvn_
                           (int *ierr,
                            const cGH *GH,
                            const int *stencil_size,
                            const int *dir,
                            TWO_FORTSTRING_ARG)
{
  TWO_FORTSTRINGS_CREATE (vname_to, vname_from)
  *ierr = BndCopyDirVN (GH, *stencil_size, *dir, vname_to, vname_from);
  free (vname_to);
  free (vname_from);
}


/*@@
   @routine    BndCopyVN
   @date       Thu Mar  2 11:02:10 2000
   @author     Gerd Lanfermann
   @desc
               Apply copy boundary routines by variable name
   @enddesc
   @calls      BndCopyVI

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
   @var        vname_to
   @vdesc      name of variable to copy boundaries to
   @vtype      const char *
   @vio        in
   @endvar
   @var        vname_from
   @vdesc      name of variable to copy boundaries from
   @vtype      const char *
   @vio        in
   @endvar

   @returntype int
   @returndesc
               return code of @seeroutine BndCopyVI <BR>
               -1 if invalid variable names are given
   @endreturndesc
@@*/
int BndCopyVN (const cGH *GH,
               const int *stencil,
               const char *vname_to,
               const char *vname_from)
{
  int vi_to, vi_from, num_vars, retval;


  vi_to    = CCTK_VarIndex (vname_to);
  vi_from  = CCTK_VarIndex (vname_from);
  num_vars = CCTK_NumVars ();

  if (vi_to >= 0 && vi_to < num_vars && vi_from >= 0 && vi_from < num_vars)
  {
    retval = BndCopyVI (GH, stencil, vi_to, vi_from);
  }
  else
  {
    CCTK_VWarn (2, __LINE__, __FILE__, CCTK_THORNSTRING,
                "Invalid variable names '%s' and/or '%s' in BndCopyVN",
                vname_to, vname_from);
    retval = -1;
  }

  return (retval);
}

void CCTK_FCALL bndcopyvn_
                           (int *ierr,
                            const cGH *GH,
                            const int *stencil,
                            TWO_FORTSTRINGS_ARGS)
{
  TWO_FORTSTRINGS_CREATE (vname_to, vname_from)
  *ierr = BndCopyVN (GH, stencil, vname_to, vname_from);
  free (vname_to);
  free (vname_from);
}


/********************************************************************
 *********************     Local Routines   *************************
 ********************************************************************/

/* maximum dimension we can deal with */
#define MAXDIM  3

/* macro to compute the linear index of a 3D point */
#define INDEX_3D(lsh, i, j, k)      ((i) + (lsh)[0]*((j) + (lsh)[1]*(k)))

/*@@
   @routine    COPY_BOUNDARY
   @date       Sat 20 Jan 2001
   @author     Thomas Radke
   @desc
               Macro to apply copy boundary conditions to a variable
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
@@*/
#define COPY_BOUNDARY(doBC,                                                   \
                      iend, jend, kend,                                       \
                      ii, jj, kk)                                             \
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
          _index = INDEX_3D (lsh, ii, jj, kk) * vtypesize;                    \
          memcpy ((char *) GH->data[var_to][timelvl_to] + _index,             \
                  (char *) GH->data[var_from][timelvl_from] + _index,         \
                  vtypesize);                                                 \
        }                                                                     \
      }                                                                       \
    }                                                                         \
  }                                                                           \
}


/*@@
   @routine    ApplyBndCopy
   @date       Thu Mar  2 11:02:10 2000
   @author     Gerd Lanfermann
   @desc
               Apply copy boundary conditions to a group of grid functions
               given by their indices
               This routine is called by the various BndCopyXXX wrappers.

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
   @vdesc      direction to copy boundaries (0 for copying all directions)
   @vtype      int
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

   @calls      CCTK_GroupIndexFromVarI
               CCTK_GroupDimI
               CCTK_VarTypeI
               CCTK_GroupStaggerDirArrayGI
               COPY_BOUNDARY
   @history
   @hdate      Sat 20 Jan 2001
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
static int ApplyBndCopy (const cGH *GH,
                         int stencil_dir,
                         const int *stencil_alldirs,
                         int dir,
                         int first_var_to,
                         int first_var_from,
                         int num_vars)
{
  int i, j, k;
  int timelvl_to, timelvl_from;
  int gindex, gdim;
  int var_to, var_from, vtypesize;
  int doBC[2*MAXDIM], dstag[MAXDIM], lsh[MAXDIM], lssh[MAXDIM], stencil[MAXDIM];
  SymmetryGHex *sGHex;


  /* get the group index of the target variable */
  gindex = CCTK_GroupIndexFromVarI (first_var_to);

  /* get the number of dimensions and the size of the variable's type */
  gdim      = CCTK_GroupDimI (gindex);
  vtypesize = CCTK_VarTypeSize (CCTK_VarTypeI (first_var_to));

  /* make sure we can deal with this number of dimensions */
  if (gdim > MAXDIM)
  {
    CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,
                "Variable dimension of %d not supported", gdim);
    return (-1);
  }

  /* check the direction parameter */
  if (abs (dir) > gdim)
  {
    CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,
                "ApplyBndCopy: direction %d greater than dimension %d",
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
    CCTK_WARN (1, "ApplyBndCopy: NULL pointer passed for stencil width array");
    return (-3);
  }

  /* initialize arrays for variables with less dimensions than MAXDIM
     so that we can use the INDEX_3D macro later on */
  for (i = gdim; i < MAXDIM; i++)
  {
    lssh[i] = 1;
    lsh[i]  = 0;
  }

  /* get the directional staggering of the group */
  CCTK_GroupStaggerDirArrayGI (dstag, gdim, gindex);

  /* get the current timelevel */
  timelvl_to   = 0;
  timelvl_from = 0;

  /* see if we have a symmetry array */
  sGHex = (SymmetryGHex *) CCTK_GHExtension (GH, "Symmetry");

  /* now loop over all variables */
  for (var_to = first_var_to, var_from = first_var_from;
       var_to < first_var_to + num_vars;
       var_to++, var_from++)
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
        doBC[i] = sGHex->GFSym[var_to][i] == GFSYM_NOSYM ||
                  sGHex->GFSym[var_to][i] == GFSYM_UNSET;
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

    /* now copy the boundaries face by face */
    if (gdim > 0)
    {
      /* lower x */
      COPY_BOUNDARY (doBC[0], stencil[0], lssh[1], lssh[2], i, j, k);
      /* upper x */
      COPY_BOUNDARY (doBC[1], stencil[0], lssh[1], lssh[2], lssh[0]-i-1, j, k);
    }
    if (gdim > 1)
    {
      /* lower y */
      COPY_BOUNDARY (doBC[2], lssh[0], stencil[1], lssh[2], i, j, k);
      /* upper y */
      COPY_BOUNDARY (doBC[3], lssh[0], stencil[1], lssh[2], i, lssh[1]-j-1, k);
    }
    if (gdim > 2)
    {
      /* lower z */
      COPY_BOUNDARY (doBC[4], lssh[0], lssh[1], stencil[2], i, j, k);
      /* upper z */
      COPY_BOUNDARY (doBC[5], lssh[0], lssh[1], stencil[2], i, j, lssh[2]-k-1);
    }
  }

  return(0);
}
