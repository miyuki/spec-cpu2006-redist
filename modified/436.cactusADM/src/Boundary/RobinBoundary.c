#ifdef SPEC_CPU
# define THORN_IS_Boundary
#endif /* SPEC_CPU */
 /*@@
   @file      RobinBoundary.c
   @date      July 6th 2000
   @author    Miguel Alcubierre, Gabrielle Allen, Gerd Lanfermann
   @desc
              Routines for Robin boundary conditions
   @enddesc
   @history
   @hdate     Tue 10 Apr 2001
   @hauthor   Thomas Radke
   @hdesc     BC routines generalized for applying to arbitrary CCTK data types
   @endhistory
   @version   $Id: RobinBoundary.c,v 1.23 2001/12/13 16:18:05 allen Exp $
 @@*/

#include <math.h>
#include <stdlib.h>
#include <string.h>

#include "cctk.h"
#include "cctk_Parameters.h"
#include "cctk_FortranString.h"

#include "Symmetry.h"
#include "Boundary.h"

/* the rcs ID and its dummy function to use it */
static const char *rcsid = "$Header: /cactus/CactusBase/Boundary/src/RobinBoundary.c,v 1.23 2001/12/13 16:18:05 allen Exp $";
CCTK_FILEVERSION(CactusBase_Boundary_RobinBoundary_c)


/********************************************************************
 ********************    External Routines   ************************
 ********************************************************************/
/* prototypes for external C routines are declared in header Boundary.h
   here only follow the fortran wrapper prototypes */
void CCTK_FCALL bndrobingi_
                           (int *ierr,
                            const cGH *GH,
                            const int *stencil,
                            const CCTK_REAL *finf,
                            const int *npow,
                            const int *gi);
void CCTK_FCALL bndrobingn_
                           (int *ierr,
                            const cGH *GH,
                            const int *stencil,
                            const CCTK_REAL *finf,
                            const int *npow,
                            ONE_FORTSTRING_ARG);
void CCTK_FCALL bndrobinvi_
                           (int *ierr,
                            const cGH *GH,
                            const int *stencil,
                            const CCTK_REAL  *finf,
                            const int *npow,
                            const int *vi);
void CCTK_FCALL bndrobinvn_
                           (int *ierr,
                            const cGH *GH,
                            const int *stencil,
                            const CCTK_REAL *finf,
                            const int *npow,
                            ONE_FORTSTRING_ARG);


/********************************************************************
 ********************    Internal Routines   ************************
 ********************************************************************/
static int ApplyBndRobin (const cGH *GH,
                          const int *stencil,
                          CCTK_REAL finf,
                          int npow,
                          int first_var,
                          int num_vars);


/*@@
   @routine    BndRobinGI
   @date       Tue Jul 18 18:08:28 2000
   @author     Gerd Lanfermann
   @desc
               Apply Robin boundary conditions by group index
   @enddesc
   @calls      ApplyBndRobin

   @var        GH
   @vdesc      Pointer to CCTK grid hierarchy
   @vtype      const cGH *
   @vio        in
   @endvar
   @var        stencil
   @vdesc      stencil width array
   @vtype      int [ dimension of group ]
   @vio        in
   @endvar
   @var        finf
   @vdesc      value of f at infimum
   @vtype      CCTK_REAL
   @vio        in
   @endvar
   @var        npow
   @vdesc      power of decay rate
   @vtype      int
   @vio        in
   @endvar
   @var        gi
   @vdesc      index of group to apply boundary conditions to
   @vtype      int
   @vio        in
   @endvar

   @returntype int
   @returndesc
               return code of @seeroutine ApplyBndRobin <BR>
               -1 if invalid group index was given
   @endreturndesc
@@*/
int BndRobinGI (const cGH *GH,
                const int *stencil,
                CCTK_REAL finf,
                int npow,
                int gi)
{
  int first_vi, retval;


  first_vi = CCTK_FirstVarIndexI (gi);
  if (first_vi >= 0)
  {
    retval = ApplyBndRobin (GH, stencil, finf, npow, first_vi,
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

void CCTK_FCALL bndrobingi_
                           (int *ierr,
                            const cGH *GH,
                            const int *stencil,
                            const CCTK_REAL *finf,
                            const int *npow,
                            const int *gi)
{
  *ierr = BndRobinGI (GH, stencil, *finf, *npow, *gi);
}


/*@@
   @routine    BndRobinGN
   @date       Tue Jul 18 18:08:28 2000
   @author     Gerd Lanfermann
   @desc
               Apply Robin boundary conditions by group name
   @enddesc
   @calls      BndRobinGI

   @var        GH
   @vdesc      Pointer to CCTK grid hierarchy
   @vtype      const cGH *
   @vio        in
   @endvar
   @var        stencil
   @vdesc      stencil width array
   @vtype      int [ dimension of group ]
   @vio        in
   @endvar
   @var        finf
   @vdesc      value of f at infimum
   @vtype      CCTK_REAL
   @vio        in
   @endvar
   @var        npow
   @vdesc      power of decay rate
   @vtype      int
   @vio        in
   @endvar
   @var        gname
   @vdesc      name of group to apply boundary conditions to
   @vtype      const char *
   @vio        in
   @endvar

   @returntype int
   @returndesc
               return code of @seeroutine BndRobinGI <BR>
               -1 if invalid group index was given
   @endreturndesc
@@*/
int BndRobinGN (const cGH *GH,
                const int *stencil,
                CCTK_REAL finf,
                int npow,
                const char *gname)
{
  int gi, retval;


  gi = CCTK_GroupIndex (gname);
  if (gi >= 0)
  {
    retval = BndRobinGI (GH, stencil, finf, npow, gi);
  }
  else
  {
    CCTK_VWarn (2, __LINE__, __FILE__, CCTK_THORNSTRING,
                "Invalid group name '%s' in BndRobinGN", gname);
    retval = -1;
  }

  return (retval);
}

void CCTK_FCALL bndrobingn_
                           (int *ierr,
                            const cGH *GH,
                            const int *stencil,
                            const CCTK_REAL *finf,
                            const int *npow,
                            ONE_FORTSTRING_ARG)
{
  ONE_FORTSTRING_CREATE (gname)
  *ierr = BndRobinGN (GH, stencil, *finf, *npow, gname);
  free (gname);
}


/*@@
   @routine    BndRobinVI
   @date       Tue Jul 18 18:08:28 2000
   @author     Gerd Lanfermann
   @desc
               Apply Robin boundary conditions by variable index
   @enddesc
   @calls      ApplyBndRobin

   @var        GH
   @vdesc      Pointer to CCTK grid hierarchy
   @vtype      const cGH *
   @vio        in
   @endvar
   @var        stencil
   @vdesc      stencil width array
   @vtype      int [ dimension of variable ]
   @vio        in
   @endvar
   @var        finf
   @vdesc      value of f at infimum
   @vtype      CCTK_REAL
   @vio        in
   @endvar
   @var        npow
   @vdesc      power of decay rate
   @vtype      int
   @vio        in
   @endvar
   @var        vi
   @vdesc      index of variable to apply boundary conditions to
   @vtype      const char *
   @vio        in
   @endvar

   @returntype int
   @returndesc
               return code of @seeroutine ApplyBndRobin <BR>
               -1 if invalid group index was given
   @endreturndesc
@@*/
int BndRobinVI (const cGH *GH,
                const int *stencil,
                CCTK_REAL finf,
                int npow,
                int vi)
{
  int retval;


  if (vi >= 0 && vi < CCTK_NumVars ())
  {
    retval = ApplyBndRobin (GH, stencil, finf, npow, vi, 1);
  }
  else
  {
    CCTK_VWarn (2, __LINE__, __FILE__, CCTK_THORNSTRING,
                "BndRobinVI: Invalid variable index %d", vi);
    retval = -1;
  }

  return (retval);
}

void CCTK_FCALL bndrobinvi_
                           (int *ierr,
                            const cGH *GH,
                            const int *stencil,
                            const CCTK_REAL  *finf,
                            const int *npow,
                            const int *vi)
{
  *ierr = BndRobinVI (GH, stencil, *finf, *npow, *vi);
}


/*@@
   @routine    BndRobinVN
   @date       Tue Jul 18 18:08:28 2000
   @author     Gerd Lanfermann
   @desc
               Apply Robin boundary conditions by variable name
   @enddesc
   @calls      BndRobinVI

   @var        GH
   @vdesc      Pointer to CCTK grid hierarchy
   @vtype      const cGH *
   @vio        in
   @endvar
   @var        stencil
   @vdesc      stencil width array
   @vtype      int [ dimension of variable ]
   @vio        in
   @endvar
   @var        finf
   @vdesc      value of f at infimum
   @vtype      CCTK_REAL
   @vio        in
   @endvar
   @var        npow
   @vdesc      power of decay rate
   @vtype      int
   @vio        in
   @endvar
   @var        vname
   @vdesc      name of variable to apply boundary conditions to
   @vtype      const char *
   @vio        in
   @endvar

   @returntype int
   @returndesc
               return code of @seeroutine BndRobinVI <BR>
               -1 if invalid group index was given
   @endreturndesc
@@*/
int BndRobinVN (const cGH *GH,
                const int *stencil,
                CCTK_REAL finf,
                int npow,
                const char *vname)
{
  int vi, retval;


  vi = CCTK_VarIndex (vname);
  if (vi >= 0)
  {
    retval = BndRobinVI (GH, stencil, finf, npow, vi);
  }
  else
  {
    CCTK_VWarn (2, __LINE__, __FILE__, CCTK_THORNSTRING,
                "Invalid variable name '%s' in BndRobinVN", vname);
    retval = -1;
  }

  return (retval);
}

void CCTK_FCALL bndrobinvn_
                           (int *ierr,
                            const cGH *GH,
                            const int *stencil,
                            const CCTK_REAL *finf,
                            const int *npow,
                            ONE_FORTSTRING_ARG)
{
  ONE_FORTSTRING_CREATE (vname)
  *ierr = BndRobinVN (GH, stencil, *finf, *npow, vname);
  free (vname);
}


/********************************************************************
 *********************     Local Routines   *************************
 ********************************************************************/

/* maximum dimension we can deal with */
#define MAXDIM  3

/* macro to compute x*x */
#define SQR(x)  ((x) * (x))


/*@@
   @routine    SET_LINEAR_INDICES
   @date       Thu 7 June 2001
   @author     Thomas Radke
   @desc
               Macro to set the linear indices for the source and destination
               element of the current grid variable
   @enddesc

   @var        i
   @vdesc      x index to use
   @vtype      int
   @vio        in
   @endvar
@@*/
#define SET_LINEAR_INDICES(i)                                                 \
{                                                                             \
  dst = CCTK_GFINDEX3D (GH, i, j, k);                                         \
  src = CCTK_GFINDEX3D (GH, i+dx, j+dy, k+dz);                                \
  distance = dist[abs(dx) + 2*abs (dy) + 4*abs (dz)];                         \
}


/*@@
   @routine    ROBIN_BOUNDARY_TYPED_3D
   @date       Thu 7 June 2001
   @author     Thomas Radke
   @desc
               Macro to apply Robin boundary conditions to a 3D variable
               of given datatype
   @enddesc

   @var        cctk_type
   @vdesc      CCTK datatype of the variable
   @vtype      <cctk_type>
   @vio        in
   @endvar
@@*/
#define ROBIN_BOUNDARY_TYPED_3D(cctk_type)                                    \
{                                                                             \
  cctk_type *data;                                                            \
  double u_src, u_dst, aux;                                                   \
                                                                              \
                                                                              \
  /* avoid the else branch with the expensive sqrt() operation if possible */ \
  if (abs (dx) + abs (dy) + abs (dz) == 1)                                    \
  {                                                                           \
    u_dst = fabs ((double) (dx ? x[dst] : (dy ? y[dst] : z[dst])));           \
    u_src = fabs ((double) (dx ? x[src] : (dy ? y[src] : z[src])));           \
  }                                                                           \
  else                                                                        \
  {                                                                           \
    u_dst = sqrt (SQR (dx * x[dst]) + SQR (dy * y[dst]) + SQR (dz * z[dst])); \
    u_src = sqrt (SQR (dx * x[src]) + SQR (dy * y[src]) + SQR (dz * z[src])); \
  }                                                                           \
                                                                              \
  aux = decay * distance * (u_src + u_dst) / SQR (r[src] + r[dst]);           \
                                                                              \
  data = (cctk_type *) GH->data[var][0];                                      \
  data[dst] = (cctk_type) ((2*aux*finf + data[src]*(1 - aux)) / (1 + aux));   \
}


/*@@
   @routine    ROBIN_BOUNDARY
   @date       Thu 7 June 2001
   @author     Thomas Radke
   @desc
               Macro to apply Robin boundary conditions to a variable
               of a given datatype in all directions
               Currently it is limited up to 3D variables only.
   @enddesc
   @calls      SET_LINEAR_INDICES
               ROBIN_BOUNDARY_TYPED_3D

   @var        cctk_type
   @vdesc      CCTK datatype of the variable
   @vtype      <cctk_type>
   @vio        in
   @endvar
@@*/
#define ROBIN_BOUNDARY(cctk_type)                                             \
{                                                                             \
  int i, j, k;                                                                \
  int dx, dy, dz;                                                             \
  int src, dst;                                                               \
  double distance;                                                            \
                                                                              \
                                                                              \
  /* check the dimensionality */                                              \
  if (gdim != 3)                                                              \
  {                                                                           \
    CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,                      \
                "ApplyBndRobin: variable dimension of %d not supported",      \
                gdim);                                                        \
    return (-5);                                                              \
  }                                                                           \
                                                                              \
  /* outermost loop over all z points */                                      \
  for (k = 0; k < GH->cctk_lsh[2]; k++)                                       \
  {                                                                           \
    if ((k == 0 && ! doBC[4]) || (k == GH->cctk_lsh[2]-1 && ! doBC[5]))       \
    {                                                                         \
      continue;                                                               \
    }                                                                         \
                                                                              \
    dz = 0;                                                                   \
    if (k == 0 && doBC[4])                                                    \
    {                                                                         \
      dz = +1;                                                                \
    }                                                                         \
    else if (k == GH->cctk_lsh[2]-1 && doBC[5])                               \
    {                                                                         \
      dz = -1;                                                                \
    }                                                                         \
                                                                              \
    /* middle loop over all y points */                                       \
    for (j = 0; j < GH->cctk_lsh[1]; j++)                                     \
    {                                                                         \
      dy = 0;                                                                 \
      if (j == 0 && doBC[2])                                                  \
      {                                                                       \
        dy = +1;                                                              \
      }                                                                       \
      else if (j == GH->cctk_lsh[1]-1 && doBC[3])                             \
      {                                                                       \
        dy = -1;                                                              \
      }                                                                       \
                                                                              \
      /* lower x */                                                           \
      if (doBC[0])                                                            \
      {                                                                       \
        dx = +1;                                                              \
        SET_LINEAR_INDICES (0);                                               \
        ROBIN_BOUNDARY_TYPED_3D (cctk_type);                                  \
      }                                                                       \
                                                                              \
      /* lower/upper y and/or z */                                            \
      if (dy || dz)                                                           \
      {                                                                       \
        dx = 0;                                                               \
        SET_LINEAR_INDICES (1);                                               \
        for (i = 1; i < GH->cctk_lsh[0]-1; i++, src++, dst++)                 \
        {                                                                     \
          ROBIN_BOUNDARY_TYPED_3D (cctk_type);                                \
        }                                                                     \
      }                                                                       \
                                                                              \
      /* upper x */                                                           \
      if (doBC[1])                                                            \
      {                                                                       \
        dx = -1;                                                              \
        SET_LINEAR_INDICES (GH->cctk_lsh[0]-1);                               \
        ROBIN_BOUNDARY_TYPED_3D (cctk_type);                                  \
      }                                                                       \
    }                                                                         \
  }                                                                           \
}


/*@@
   @routine    ApplyBndRobin
   @date       Tue Jul 18 18:08:28 2000
   @author     Gerd Lanfermann
   @desc
               Apply Robin boundary conditions to a group of grid functions
               given by their indices
               This routine is called by the various BndRobinXXX wrappers.

               Although it is currently limited to handle 3D variables only
               it can easily be extended for higher dimensions
               by adapting the appropriate macros.
   @enddesc

   @var        GH
   @vdesc      Pointer to CCTK grid hierarchy
   @vtype      const cGH *
   @vio        in
   @endvar
   @var        stencil
   @vdesc      stencil width array
   @vtype      int [ dimension of variable ]
   @vio        in
   @endvar
   @var        finf
   @vdesc      value of f at infinity
   @vtype      CCTK_REAL
   @vio        in
   @endvar
   @var        npow
   @vdesc      power of decay rate
   @vtype      int
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

   @calls      CCTK_VarTypeI
               CCTK_GroupDimFromVarI
               ROBIN_BOUNDARY
   @history
   @hdate      Tue 10 Apr 2001
   @hauthor    Thomas Radke
   @hdesc      Merged separate routines for 1D, 2D, and 3D
               into a single generic routine
   @endhistory

   @returntype int
   @returndesc
                0 for success
               -1 if variable dimension is not supported
               -2 if NULL pointer passed as stencil width array
               -3 if stencil width is other than 1
               -4 if variable type is not supported
               -5 if variable dimension is other than 3D
               -6 if no coordinate information is available
   @endreturndesc
@@*/
static int ApplyBndRobin (const cGH *GH,
                          const int *stencil,
                          CCTK_REAL finf,
                          int npow,
                          int first_var,
                          int num_vars)
{
  int var, vtype, dim, gdim;
  int doBC[2*MAXDIM];
  SymmetryGHex *sGHex;
  char coord_system_name[20];
  double decay;
  const CCTK_REAL *x, *y, *z, *r;
  double dist[8];


  /* get the number of dimensions and the variables' type */
  gdim  = CCTK_GroupDimI (CCTK_GroupIndexFromVarI (first_var));
  vtype = CCTK_VarTypeI (first_var);

  /* make sure we can deal with this number of dimensions */
  if (gdim > MAXDIM)
  {
    CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,
                "ApplyBndRobin: Variable dimension of %d not supported", gdim);
    return (-1);
  }

  /* check the stencil width */
  if (! stencil)
  {
    CCTK_WARN (1, "ApplyBndRobin: NULL pointer passed for stencil width array");
    return (-2);
  }

  for (dim = 0; dim < gdim; dim++)
  {
    if (stencil[dim] != 1)
    {
      CCTK_WARN (1, "ApplyBndRobin: Stencil width must be 1 "
                    "for Robin boundary conditions");
      return (-3);
    }
  }

  /* Robin boundaries need the underlying grid coordinates */
  sprintf (coord_system_name, "cart%dd", gdim);
  if (CCTK_CoordSystemHandle (coord_system_name) < 0)
  {
    CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,
                "ApplyBndRobin: Couldn't get coordinates from '%s'",
                coord_system_name);
    return (-6);
  }
  x = GH->data[CCTK_CoordIndex (-1, "x", coord_system_name)][0];
  y = GH->data[CCTK_CoordIndex (-1, "y", coord_system_name)][0];
  z = GH->data[CCTK_CoordIndex (-1, "z", coord_system_name)][0];

  sprintf (coord_system_name, "spher%dd", gdim);
  if (CCTK_CoordSystemHandle (coord_system_name) < 0)
  {
    CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,
                "ApplyBndRobin: Couldn't get coordinates from '%s'",
                coord_system_name);
    return (-6);
  }
  r = GH->data[CCTK_CoordIndex (-1, "r", coord_system_name)][0];

  /* see if we have a symmetry array */
  sGHex = (SymmetryGHex *) CCTK_GHExtension (GH, "Symmetry");

  /* get the decay rate as a double */
  decay = (double) npow;

  /* precompute the distance to all 8 neighbors in a 3D grid */
  dist[0] = 0;                                    /* not used */
  dist[1] = GH->cctk_delta_space[0];
  dist[2] = GH->cctk_delta_space[1];
  dist[3] = sqrt (SQR (dist[1]) + SQR (dist[2]));
  dist[4] = GH->cctk_delta_space[2];
  dist[5] = sqrt (SQR (dist[1]) + SQR (dist[4]));
  dist[6] = sqrt (SQR (dist[2]) + SQR (dist[4]));
  dist[7] = sqrt (SQR (dist[1]) + SQR (dist[2]) + SQR (dist[4]));

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
      for (dim = 0; dim < 2 * gdim; dim++)
      {
        doBC[dim] = sGHex->GFSym[var][dim] == GFSYM_NOSYM ||
                    sGHex->GFSym[var][dim] == GFSYM_UNSET;
      }
    }
    for (dim = 0; dim < gdim; dim++)
    {
      doBC[dim*2]   &= GH->cctk_lsh[dim] > 1 && GH->cctk_bbox[dim*2];
      doBC[dim*2+1] &= GH->cctk_lsh[dim] > 1 && GH->cctk_bbox[dim*2+1];
    }

    switch (vtype)
    {
      case CCTK_VARIABLE_CHAR:
        ROBIN_BOUNDARY (CCTK_CHAR); break;

      case CCTK_VARIABLE_INT:
        ROBIN_BOUNDARY (CCTK_INT); break;

      case CCTK_VARIABLE_REAL:
        ROBIN_BOUNDARY (CCTK_REAL); break;

#ifdef CCTK_INT2
      case CCTK_VARIABLE_INT2:
        ROBIN_BOUNDARY (CCTK_INT2); break;
#endif

#ifdef CCTK_INT4
      case CCTK_VARIABLE_INT4:
        ROBIN_BOUNDARY (CCTK_INT4); break;
#endif

#ifdef CCTK_INT8
      case CCTK_VARIABLE_INT8:
        ROBIN_BOUNDARY (CCTK_INT8); break;
#endif

#ifdef CCTK_REAL4
      case CCTK_VARIABLE_REAL4:
        ROBIN_BOUNDARY (CCTK_REAL4); break;
#endif

#ifdef CCTK_REAL8
      case CCTK_VARIABLE_REAL8:
        ROBIN_BOUNDARY (CCTK_REAL8); break;
#endif

#ifdef CCTK_REAL16
      case CCTK_VARIABLE_REAL16:
        ROBIN_BOUNDARY (CCTK_REAL16); break;
#endif

      default:
        CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,
                    "ApplyBndRobin: Unsupported variable type %d for "
                    "variable '%s'", CCTK_VarTypeI (var), CCTK_VarName (var));
        return (-4);
    }
  }

  return(0);
}
