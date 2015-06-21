#ifdef SPEC_CPU
# define THORN_IS_Cactus
#endif /* SPEC_CPU */
 /*@@
   @file      Coord.c
   @date      11-12th April 1999
   @author    Gabrielle Allen
   @desc
              Routines to deal with cooordinates and coordinate registration
   @enddesc
   @version   $Id: Coord.c,v 1.44 2001/12/29 11:18:10 allen Exp $
 @@*/

/*#define DEBUG_COORD*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "cctk_Flesh.h"
#include "cctk_ActiveThorns.h"
#include "cctk_Coord.h"
#include "cctk_FortranString.h"
#include "cctk_Groups.h"
#include "cctk_WarnLevel.h"

#include "StoreHandledData.h"

static const char *rcsid = "$Header: /cactus/Cactus/src/main/Coord.c,v 1.44 2001/12/29 11:18:10 allen Exp $";

CCTK_FILEVERSION(main_Coord_c)


/********************************************************************
 *********************     Local Data Types   ***********************
\ ********************************************************************/

struct Coordsystem
{
  const char *implementation;
  int dimension;
  const char *systemname;
  struct Coordprops *coords;
};

struct Coordprops
{
  char *    name;
  int       index;
  struct Coordpropslistcomp *listcomp;
  struct Coordpropslistphysi *listphysi;
};

struct Coordpropslistcomp
{

  cGH *GH;

  CCTK_REAL lower;                /* Coord of lower range (computational) */
  CCTK_REAL upper;                /* Coord of upper range (computational) */

  struct Coordpropslistcomp *next;     /* List */
};

struct Coordpropslistphysi
{

  cGH *GH;

  int lower;            /* Index of lower range (physical) */
  int upper;            /* Index of upper range (physical) */

  struct Coordpropslistphysi *next;     /* List */
};


/********************************************************************
 *********************     Local Data   *****************************
 ********************************************************************/

static cHandledData *CoordSystems = NULL;
static int num_systems = 0;

/********************************************************************
 *********************     Fortran Wrappers    **********************
 ********************************************************************/
void CCTK_FCALL cctki_coordregistersystem_
                           (int *ierr, const int *dim, TWO_FORTSTRING_ARG);
void CCTK_FCALL cctk_coordregisterdata_
                           (int *handle,const int *dir,THREE_FORTSTRING_ARG);
void CCTK_FCALL cctk_coordregisterrange_
                           (int *ierr,
                            cGH *GH,
                            const CCTK_REAL *lower,
                            const CCTK_REAL *upper,
                            const int *dir,
                            TWO_FORTSTRING_ARG);
void CCTK_FCALL cctk_coordregisterrangephysindex_
                           (int *ierr,
                            cGH *GH,
                            const int *lower,
                            const int *upper,
                            const int *dir,
                            TWO_FORTSTRING_ARG);
void CCTK_FCALL cctk_coordsystemhandle_
                           (int *ierr, ONE_FORTSTRING_ARG);
void CCTK_FCALL cctk_coordindex_
                           (int *vindex, const int *dir, TWO_FORTSTRING_ARG);
void CCTK_FCALL cctk_coordsystemdim_
                           (int *dim, ONE_FORTSTRING_ARG);
void CCTK_FCALL cctk_coorddir_
                           (int *dir, TWO_FORTSTRING_ARG);
void CCTK_FCALL cctk_coordrange_
                           (int *ierr,
                            const cGH *GH,
                            CCTK_REAL *lower,
                            CCTK_REAL *upper,
                            const int *dir,
                            TWO_FORTSTRING_ARG);
void CCTK_FCALL cctk_coordrangephysindex_
                           (int *ierr,
                            const cGH *GH,
                            int *lower,
                            int *upper,
                            const int *dir,
                            TWO_FORTSTRING_ARG);
void CCTK_FCALL cctk_coordlocalrange_
                           (int *ierr,
                            const cGH *GH,
                            CCTK_REAL *lower,
                            CCTK_REAL *upper,
                            const int *dir,
                            TWO_FORTSTRING_ARG);


/********************************************************************
 *********************     External Routines   **********************
 ********************************************************************/

 /*@@
   @routine    CoordRegisterSystem
   @date       18th June 2000
   @author     Gabrielle Allen
   @desc
               Register a coordinate system name and its dimension
   @enddesc
   @calls

   @returntype int
   @returndesc
   Returns 0 for success and negative integer for failure
   0  = success
   -1 = this system name already registered with different dimension
   -2 = dimension not valid
   @endreturndesc

   @@*/
int CCTKi_CoordRegisterSystem (int dim,
                              const char *thorn,
			      const char *systemname)
{
  int retval=-1;
  struct Coordsystem *new_system;


  /* Check if system already exists */
  Util_GetHandle (CoordSystems, systemname, (void **) &new_system);
  if (! new_system)
  {
    /* Allocate the memory */
    new_system = (struct Coordsystem *) malloc (sizeof (struct Coordsystem));

    /* Set the data and store */
    if (new_system)
    {
      if (dim > 0)
      {
        new_system->dimension  = dim;
	new_system->implementation = CCTK_ThornImplementation (thorn);
        new_system->systemname = strdup (systemname);
        new_system->coords = (struct Coordprops *) calloc (dim,
                                                     sizeof(struct Coordprops));
        retval = Util_NewHandle (&CoordSystems, systemname, new_system);

	/* Remember how many systems there are */
	num_systems++;

      }
      else
      {
        retval = -2;
        CCTK_VWarn(1,__LINE__,__FILE__,"Cactus",
                   "CCTK_CoordRegisterSystem: Dimension %d not valid",
                   dim);
      }
    }
  }
  else
  {
    if (new_system->dimension == dim)
    {
      retval = 0;
      CCTK_VWarn(4,__LINE__,__FILE__,"Cactus",
                 "CCTK_CoordRegisterSystem: System '%s' already registered",
                 systemname);
    }
    else
    {
      retval = -1;
      CCTK_VWarn(1,__LINE__,__FILE__,"Cactus",
                 "CCTK_CoordRegisterSystem: System '%s' already registered "
                 "with different dimension",
                 systemname);
    }
  }

  return (retval);
}

void CCTK_FCALL cctki_coordregistersystem_
                           (int *ierr, const int *dim, TWO_FORTSTRING_ARG)
{
  TWO_FORTSTRING_CREATE (thorn,systemname)
  *ierr = CCTKi_CoordRegisterSystem (*dim, thorn, systemname);
  free (systemname);
  free (thorn);
}


 /*@@
   @routine    CoordRegisterData
   @date       11-12th April 1999
   @author     Gabrielle Allen
   @desc
               Register a GF as a coordinate with a name and
               a direction.
   @enddesc
   @calls

   @returntype int
   @returndesc
   Returns 0 for success and negative integer for failure
   0  = success
   -1 = coordinate system not registered
   -2 = direction outside system dimension
   -3 = coordinate name already registered
   -4 = coordinate direction already registered
   @endreturndesc
@@*/
int CCTK_CoordRegisterData(int dir,
                           const char *gfname,
                           const char *coordname,
                           const char *systemname)
{
  int i;
  int retval=0;
  int dup=0;
  struct Coordsystem *coord_system;


  /* Check if system exists */
  Util_GetHandle (CoordSystems, systemname, (void **) &coord_system);
  if (! coord_system)
  {
    CCTK_VWarn (1, __LINE__, __FILE__, "Cactus",
               "CCTK_CoordRegisterData: System '%s' not registered",systemname);
    retval = -1;
  }
  else
  {
    /* Check direction correct */
    if (dir < 1 || dir > coord_system->dimension)
    {
      CCTK_VWarn (1, __LINE__, __FILE__, "Cactus",
                  "CCTK_CoordRegisterData: Direction %d outside system "
                  "dimension %d",
                  dir, coord_system->dimension);
      retval = -2;
    }
    else
    {

      /* Check name not already registered */
      for (i = 0; i < coord_system->dimension; i++)
      {
        if (coord_system->coords[i].name &&
            CCTK_Equals (coord_system->coords[i].name, coordname))
        {
          CCTK_VWarn (1, __LINE__, __FILE__, "Cactus",
                      "CCTK_CoordRegisterData: Coordinate name '%s' already "
                      "registered", coordname);
          dup = 1;
          retval = -3;
        }
      }
      /* Check direction not already registered */
      if (coord_system->coords[dir-1].name)
      {
        CCTK_VWarn (1, __LINE__, __FILE__, "Cactus",
                    "CCTK_CoordRegisterData: Coordinate direction %d already "
                    "registered", dir);
        dup = 1;
        retval = -4;
      }

      if (dup == 0)
      {
        /* Register name */
        coord_system->coords[dir-1].name = strdup (coordname);
        /* Register index if grid variable */
        coord_system->coords[dir-1].index = CCTK_VarIndex (gfname);
        if (coord_system->coords[dir-1].index < 0)
        {
          CCTK_VWarn (1, __LINE__, __FILE__, "Cactus",
                      "CCTK_CoordRegisterData: No grid variable registered");
        }
      }
    }
  }

  return (retval);
}

void CCTK_FCALL cctk_coordregisterdata_
                           (int *handle, const int *dir, THREE_FORTSTRING_ARG)
{
  THREE_FORTSTRING_CREATE (gf, name, systemname)
  *handle = CCTK_CoordRegisterData (*dir, gf, name, systemname);
  free (gf);
  free (name);
  free (systemname);
}




 /*@@
   @routine    CoordRegisterRange
   @date       
   @author     Gabrielle Allen
   @desc
               Register the computational range for a coordinate
   @enddesc
   @calls

   @var     GH
   @vdesc   GH data
   @vtype   cGH *
   @vio     in
   @endvar
   @var     min
   @vdesc   Minimum of computational range
   @vtype   CCTK_REAL
   @vio     in
   @endvar
   @var     max
   @vdesc   Maximum of computational range
   @vtype   CCTK_REAL
   @vio     in
   @endvar
   @var     dir
   @vdesc   Direction of coordinate with this range
   @vtype   int
   @vio     in
   @endvar
   @var     coordname
   @vdesc   Name of coordinate with this range
   @vtype   const char *
   @vio     in
   @endvar
   @var     systemname
   @vdesc   Name of coordinate system
   @vtype   const char *
   @vio     in
   @endvar

   @returntype int
   @returndesc
   Returns 0 for success and negative integer for failure
   0  = success
   -1 = coordinate system not registered
   -2 = direction outside system dimension
   -3 = coordinate name not registered
   -4 = coordinate direction not registered
   -5 = memory allocation failed
   @endreturndesc
@@*/


int CCTK_CoordRegisterRange (cGH *GH,
                             CCTK_REAL min,
                             CCTK_REAL max,
                             int dir,
                             const char *coordname,
                             const char *systemname)
{
  int i;
  int retval = 0;
  int vindex = -1;
  struct Coordpropslistcomp *newguy;
  struct Coordsystem *coord_system;


  /* Check if system exists */
  Util_GetHandle (CoordSystems, systemname, (void **) &coord_system);
  if (! coord_system)
  {
    CCTK_VWarn (1, __LINE__, __FILE__, "Cactus",
               "CCTK_CoordRegisterRange: System '%s' not registered",
               systemname);
    retval = -1;
  }
  else
  {
    if (dir > -1)
    {
      if (dir == 0 || dir > coord_system->dimension)
      {
        CCTK_VWarn (1, __LINE__, __FILE__, "Cactus",
                    "CCTK_CoordRegisterRange: Direction %d outside system "
                    "dimension %d", dir, coord_system->dimension);
        retval = -2;
      }
      if (coord_system->coords[dir-1].name)
      {
        vindex = dir-1;
      }
      else
      {
        CCTK_VWarn (1, __LINE__, __FILE__, "Cactus",
                    "CCTK_CoordRegisterRange: Coordinate direction %d "
                    "not registered", dir);
        retval = -4;
      }
    }
    else
    {
      for (i = 0; i < coord_system->dimension; i++)
      {
        if (coord_system->coords[i].name &&
            CCTK_Equals(coord_system->coords[i].name,coordname))
        {
          vindex = i;
        }
      }
      if (vindex == -1)
      {
        CCTK_VWarn (1, __LINE__, __FILE__, "Cactus",
                    "CCTK_CoordRegisterRange: Coordinate name %s not "
                    "registered", coordname);
        retval = -3;
      }
    }

    if (vindex != -1)
    {

      /* Is range already registered */
      if (coord_system->coords[vindex].listcomp)
      {
        CCTK_VWarn (3, __LINE__, __FILE__, "Cactus",
                    "CCTK_CoordRange: Range already registered", systemname);
        coord_system->coords[vindex].listcomp->lower = min;
        coord_system->coords[vindex].listcomp->upper = max;
      }
      else
      {
        /* New coord_range */
        newguy = (struct Coordpropslistcomp *) 
          malloc (sizeof(struct Coordpropslistcomp));

        if (! newguy)
        {
          CCTK_Warn (1, __LINE__, __FILE__, "Cactus",
                     "CCTK_CoordRegisterRange: Cannot allocate data "
                     "for coordinate range");
          retval = -5;
        }
        else
        {
          newguy->GH    = GH;
          newguy->lower = min;
          newguy->upper = max;
          newguy->next  = coord_system->coords[vindex].listcomp;
          coord_system->coords[vindex].listcomp = newguy;
        }
      }
    }
  }

  return (retval);
}

void CCTK_FCALL cctk_coordregisterrange_
                           (int *ierr,
                            cGH *GH,
                            const CCTK_REAL *lower,
                            const CCTK_REAL *upper,
                            const int *dir,
                            TWO_FORTSTRING_ARG)
{
  TWO_FORTSTRING_CREATE (name, systemname)
  *ierr = CCTK_CoordRegisterRange (GH, *lower, *upper, *dir, name, systemname);
  free (name);
  free (systemname);
}


 /*@@
   @routine    CoordRegisterRangePhysIndex
   @date       
   @author     Gabrielle Allen
   @desc
               Register the physical range for a coordinate by 
               global grid index
   @enddesc
   @calls

   @var     GH
   @vdesc   GH data
   @vtype   cGH *
   @vio     in
   @endvar
   @var     min
   @vdesc   index for minimum of physical range
   @vtype   int
   @vio     in
   @endvar
   @var     max
   @vdesc   index for maximum of computational range
   @vtype   CCTK_REAL
   @vio     in
   @endvar
   @var     dir
   @vdesc   Direction of coordinate with this range
   @vtype   int
   @vio     in
   @endvar
   @var     coordname
   @vdesc   Name of coordinate with this range
   @vtype   const char *
   @vio     in
   @endvar
   @var     systemname
   @vdesc   Name of coordinate system
   @vtype   const char *
   @vio     in
   @endvar

   @returntype int
   @returndesc
   Returns 0 for success and negative integer for failure
   0  = success
   -1 = coordinate system not registered
   -2 = direction outside system dimension
   -3 = coordinate name not registered
   -4 = coordinate direction not registered
   -5 = memory allocation failed
   @endreturndesc
@@*/
int CCTK_CoordRegisterRangePhysIndex (cGH *GH,
                                      int min,
                                      int max,
                                      int dir,
                                      const char *coordname,
                                      const char *systemname)
{
  int i;
  int retval = 0;
  int vindex = -1;
  struct Coordpropslistphysi *newguy;
  struct Coordsystem *coord_system;


  /* Check if system exists */
  Util_GetHandle (CoordSystems, systemname, (void **) &coord_system);
  if (! coord_system)
  {
    CCTK_VWarn (1, __LINE__, __FILE__, "Cactus",
                "CCTK_CoordRegisterRangePhysIndex: System '%s' not registered",
               systemname);
    retval = -1;
  }
  else
  {
    if (dir > -1)
    {
      if (dir == 0 || dir > coord_system->dimension)
      {
        CCTK_VWarn (1, __LINE__, __FILE__, "Cactus",
                    "CCTK_CoordRegisterRangePhysIndex: Direction %d outside system "
                    "dimension %d", dir, coord_system->dimension);
        retval = -2;
      }
      if (coord_system->coords[dir-1].name)
      {
        vindex = dir-1;
      }
      else
      {
        CCTK_VWarn (1, __LINE__, __FILE__, "Cactus",
                    "CCTK_CoordRegisterRangePhysIndex: "
                    "Coordinate direction %d "
                    "not registered", dir);
        retval = -4;
      }
    }
    else
    {
      for (i = 0; i < coord_system->dimension; i++)
      {
        if (coord_system->coords[i].name &&
            CCTK_Equals(coord_system->coords[i].name,coordname))
        {
          vindex = i;
        }
      }
      if (vindex == -1)
      {
        CCTK_VWarn (1, __LINE__, __FILE__, "Cactus",
                    "CCTK_CoordRegisterRangePhysIndex: Coordinate name %s not "
                    "registered", coordname);
        retval = -3;
      }
    }

    if (vindex != -1)
    {

      /* Is range already registered */
      if (coord_system->coords[vindex].listphysi)
      {
        CCTK_VWarn (3, __LINE__, __FILE__, "Cactus",
                    "CCTK_CoordRegisterRangePhysIndex: "
                    "Range already registered", systemname);
        coord_system->coords[vindex].listphysi->lower = min;
        coord_system->coords[vindex].listphysi->upper = max;
      }
      else
      {
        /* New coord_range */
        newguy = (struct Coordpropslistphysi *) 
          malloc (sizeof(struct Coordpropslistphysi));

        if (! newguy)
        {
          CCTK_Warn (1, __LINE__, __FILE__, "Cactus",
                     "CCTK_CoordRegisterRangePhysIndex: Cannot allocate data "
                     "for coordinate range");
          retval = -5;
        }
        else
        {
          newguy->GH    = GH;
          newguy->lower = min;
          newguy->upper = max;
          newguy->next  = coord_system->coords[vindex].listphysi;
          coord_system->coords[vindex].listphysi = newguy;
        }
      }
    }
  }

  return (retval);
}

void CCTK_FCALL cctk_coordregisterrangephysindex_
                           (int *ierr,
                            cGH *GH,
                            const int *lower,
                            const int *upper,
                            const int *dir,
                            TWO_FORTSTRING_ARG)
{
  TWO_FORTSTRING_CREATE (name, systemname)
  *ierr = CCTK_CoordRegisterRangePhysIndex (GH, *lower, *upper, *dir, name, systemname);
  free (name);
  free (systemname);
}


 /*@@
   @routine    CCTK_CoordSystemHandle
   @date       Fri 02 Feb 2001
   @author     Thomas Radke
   @desc
               Returns the handle for a coordinate system
   @enddesc
   @calls      Util_GetHandle

   @var        systemname
   @vdesc      name of the coordinate system
   @vtype      const char *
   @vio        in
   @endvar

   @returntype int
   @returndesc
               >= 0 handle for coordinate system
               <  0 no such coordinate system found
   @endreturndesc
@@*/
int CCTK_CoordSystemHandle (const char *systemname)
{
  int handle;


  handle = Util_GetHandle (CoordSystems, systemname, NULL);

  if (handle < 0)
  {
    CCTK_VWarn (3, __LINE__, __FILE__, "Cactus",
                "No coordinate system '%s' registered",
                systemname);
  }

  return (handle);
}


 /*@@
   @routine    CCTK_CoordSystemName
   @date       Fri 02 Feb 2001
   @author     Thomas Radke
   @desc
               Returns the name of a coordinate system
   @enddesc
   @calls      Util_GetHandle

   @var        handle
   @vdesc      handle for the coordinate system
   @vtype      int
   @vio        in
   @endvar

   @returntype const char *
   @returndesc
               the coordinate system name or NULL if handle is invalid
   @endreturndesc
@@*/
const char *CCTK_CoordSystemName (int handle)
{
  const char *systemname;
  struct Coordsystem *coord_system;


  /* Check if system exists */
  coord_system = (struct Coordsystem *)
                 Util_GetHandledData (CoordSystems, handle);
  if (coord_system)
  {
    systemname = (const char *) coord_system->systemname;
  }
  else
  {
    systemname = NULL;
  }

  return (systemname);
}

void CCTK_FCALL cctk_coordsystemhandle_
                           (int *ierr, ONE_FORTSTRING_ARG)
{
  ONE_FORTSTRING_CREATE (systemname)
  *ierr = CCTK_CoordSystemHandle (systemname);
  free (systemname);
}


 /*@@
   @routine    CoordIndex
   @date
   @author     Gabrielle Allen
   @desc
   Return the index for a grid variable registered as a coordinate, taking
   as input the coordinate system name and either the coordinate name or the
   coordinate direction
   @enddesc
   @calls

   @var     dir
   @vdesc   the direction of the coordinate
   @vtype   int
   @vio     in
   @vcomment
   The range of the coordinate direction is (1,coord system dimension)
   If the coord direction is > 0 it is used to calculate the coordinate grid
   variable index, if it is <=0 then the coordinate name is used
   @endvar
   @var     name
   @vdesc   the name of the coordinate (used only if dir <= 0)
   @vtype   const char *
   @vio     in
   @vcomment
   If the coord direction is > 0 it is used to calculate the coordinate grid
   variable index, if it is <=0 then the coordinate name is used
   @endvar
   @var     systemname
   @vdesc   the name of the coordinate system
   @vtype   const char *
   @vio     in
   @vcomment

   @endvar

   @returntype int
   @returndesc
   >=0      = grid variable index for coordinate
   -1       = coordinate system not registered
   -2       = coordinate name not found
   -3       = coordinate direction greater than system dimension
   @endreturndesc

   @@*/

int CCTK_CoordIndex (int dir, const char *name, const char *systemname)
{
  int i;
  int vindex=-1;
  int foundit = 0;
  struct Coordsystem *coord_system;


  /* Check if system exists */
  Util_GetHandle (CoordSystems, systemname, (void **) &coord_system);
  if (! coord_system)
  {
    CCTK_VWarn (1, __LINE__, __FILE__, "Cactus",
               "CCTK_CoordIndex: System '%s' not registered",systemname);
    vindex = -1;
  }
  else
  {
    if (dir > 0)
    {
      if (dir > coord_system->dimension)
      {
        CCTK_VWarn (2, __LINE__, __FILE__, "Cactus",
                    "CCTK_CoordIndex: Direction %d outside dimension %d",
                    dir, coord_system->dimension);
        vindex = -3;
      }
      else
      {
        vindex = coord_system->coords[dir-1].index;
      }
    }
    else
    {
      for (i = 0; i < coord_system->dimension; i++)
      {
        if (coord_system->coords[i].name &&
            CCTK_Equals (coord_system->coords[i].name, name))
        {
          foundit = 1;
          vindex = coord_system->coords[i].index;
        }
      }
      if (foundit == 0)
      {
        CCTK_VWarn (4, __LINE__, __FILE__, "Cactus",
                    "CCTK_CoordIndex: Coordinate name '%s' not found",
                    name);
        vindex = -2;
      }
    }
  }

  return (vindex);
}

void CCTK_FCALL cctk_coordindex_
                           (int *vindex, const int *dir, TWO_FORTSTRING_ARG)
{
  TWO_FORTSTRING_CREATE (name, systemname)
  *vindex = CCTK_CoordIndex (*dir, name, systemname);
  free (name);
  free (systemname);
}


int CCTK_CoordSystemDim (const char *systemname)
{
  int dim;
  struct Coordsystem *coord_system;


  /* Check if system exists */
  Util_GetHandle (CoordSystems, systemname, (void **) &coord_system);
  if (! coord_system)
  {
    CCTK_VWarn (1, __LINE__, __FILE__, "Cactus",
               "CCTK_CoordSystemDim: System '%s' not registered",systemname);
    dim = -1;
  }
  else
  {
    dim = coord_system->dimension;
  }

  return (dim);
}

void CCTK_FCALL cctk_coordsystemdim_
                           (int *dim, ONE_FORTSTRING_ARG)
{
  ONE_FORTSTRING_CREATE (systemname)
  *dim = CCTK_CoordSystemDim (systemname);
  free (systemname);
}


 /*@@
   @routine    CCTK_CoordDir
   @date       18th June 2000
   @author     Gabrielle Allen
   @desc
               Supplies the direction of a coordinate
   @enddesc
   @calls

   @returntype int
   @returndesc
   Returns the direction, or a negative integer for an error
   -1 = coordinate system not registered
   -2 = coordinate not found in this system
   @endreturndesc

   @@*/

int CCTK_CoordDir (const char *name, const char *systemname)
{
  int i;
  int dir;
  struct Coordsystem *coord_system;


  /* Check if system exists */
  dir = -1;
  Util_GetHandle (CoordSystems, systemname, (void **) &coord_system);
  if (! coord_system)
  {
    CCTK_VWarn (1, __LINE__, __FILE__, "Cactus",
               "CCTK_CoordDir: System '%s' not registered", systemname);
  }
  else
  {
    for (i = 0; i < coord_system->dimension; i++)
    {
      if (CCTK_Equals (coord_system->coords[i].name, name))
      {
        dir = i+1;
      }
    }
    if (dir < 1)
    {
      CCTK_VWarn (2, __LINE__, __FILE__, "Cactus",
                  "CCTK_CoordDir: Could not find coordinate '%s' in '%s'",
                  name, systemname);
      dir = -2;
    }
  }

  return (dir);
}

void CCTK_FCALL cctk_coorddir_
                           (int *dir, TWO_FORTSTRING_ARG)
{
  TWO_FORTSTRING_CREATE (name, systemname)
  *dir = CCTK_CoordDir (name, systemname);
  free (name);
  free (systemname);
}


 /*@@
   @routine    CCTK_CoordRange
   @date       10th January 2000
   @author     Gabrielle Allen
   @desc
               Supplies the global range of the named coordinate.
        
               You specify the direction (coorddir=-1;1,2,...)
               or the name (coordname). The name will be used
               if coordir==-1
        
   @enddesc
   @calls

   @var     GH
   @vdesc   GH data
   @vtype   const cGH *
   @vio     in
   @endvar
   @var     lower
   @vdesc   Minimum of computational range
   @vtype   CCTK_REAL *
   @vio     out
   @endvar
   @var     upper
   @vdesc   Maximum of computational range
   @vtype   CCTK_REAL *
   @vio     out
   @endvar
   @var     dir
   @vdesc   Direction of coordinate with this range
   @vtype   int
   @vio     in
   @endvar
   @var     coordname
   @vdesc   Name of coordinate with this range
   @vtype   const char *
   @vio     in
   @endvar
   @var     systemname
   @vdesc   Name of coordinate system
   @vtype   const char *
   @vio     in
   @endvar

   @returntype int
   @returndesc Returns zero for success and negative for error
      -1 = coordinate system not registered 
      -2 = no coordinate name provided
      -3 = no coordinate system name provided
      -4 = coordinate name not registered
      -5 = NULL pointer(s) passed for lower/upper
      -6 = no range registered
   @endreturndesc
@@*/

int CCTK_CoordRange (const cGH *GH,
                     CCTK_REAL *lower,
                     CCTK_REAL *upper,
                     int coorddir,
                     const char *coordname,
                     const char *systemname)
{
  int i;
  int retval=0;
  int gotrange;
  struct Coordpropslistcomp *curr;
  struct Coordsystem    *coord_system;
  struct Coordprops     *coord;

  if (lower == NULL || upper == NULL)
  {
    CCTK_Warn (2, __LINE__, __FILE__, "Cactus",
               "CCTK_CoordRange: NULL pointer(s) passed for lower/upper");
    retval = -1;
  }
  else if (coorddir <= 0 && coordname == NULL)
  {
    CCTK_Warn (2, __LINE__, __FILE__, "Cactus",
               "CCTK_CoordRange: No coordinate name given");
    retval = -2;
  }
  else if (systemname == NULL)
  {
    CCTK_Warn (2, __LINE__, __FILE__, "Cactus",
               "CCTK_CoordRange: No coordinate system name given");
    retval = -3;
  }
  else
  {
    /* Check if system exists */
    Util_GetHandle (CoordSystems, systemname, (void **) &coord_system);
    if (! coord_system)
    {
      CCTK_VWarn (2, __LINE__, __FILE__, "Cactus",
                 "CCTK_CoordRange: System '%s' not registered", systemname);
      retval = -1;
    }
    else
    {
      if (coorddir > 0)
      {
        coord = &coord_system->coords[coorddir-1];
      }
      else
      {
        coord = NULL;
        for (i = 0; i < coord_system->dimension; i++)
        {
          if (CCTK_Equals (coord_system->coords[i].name, coordname))
          {
            coord = &coord_system->coords[i];
            break;
          }
        }
        if (coord == NULL)
        {
          CCTK_VWarn (2, __LINE__, __FILE__, "Cactus",
                      "CCTK_CoordRange: Coordinate name '%s' not registered",
                      coordname);
          retval = -4;
        }
      }
      if (coord)
      {
	gotrange = 0;
        for (curr = coord->listcomp; curr; curr = curr->next)
        {

#ifdef DEBUG_COORD
          printf("curr  = %p\n",curr);
          printf("lower = %f\n",curr->lower);
          printf("upper = %f\n",curr->upper);
          printf("next  = %p\n",curr->next);
#endif

          if (curr->GH == GH)
          {
	    gotrange = 1;
            *lower = curr->lower;
            *upper = curr->upper;

#ifdef DEBUG_COORD
            printf("Returning range (%f,%f) (from %p)\n", *lower,*upper,curr);
#endif
          }
        }
	if (!gotrange)
	{
	  retval = -6;
	}
      }
    }
  }

  return (retval);
}

void CCTK_FCALL cctk_coordrange_
                           (int *ierr,
                            const cGH *GH,
                            CCTK_REAL *lower,
                            CCTK_REAL *upper,
                            const int *dir,
                            TWO_FORTSTRING_ARG)
{
  TWO_FORTSTRING_CREATE (name, systemname)
  *ierr = CCTK_CoordRange (GH, lower, upper, *dir, name, systemname);
  free (name);
  free (systemname);
}


 /*@@
   @routine    CCTK_CoordRangePhysIndex
   @date       3rd July 2001
   @author     Gabrielle Allen
   @desc
               Supplies the global physical range of the named coordinate
               by index
        
               You specify the direction (coorddir=-1;1,2,...)
               or the name (coordname). The name will be used
               if coordir==-1
        
   @enddesc
   @calls

   @var     GH
   @vdesc   GH data
   @vtype   const cGH *
   @vio     in
   @endvar
   @var     lower
   @vdesc   Index for minimum of physical range
   @vtype   int *
   @vio     out
   @endvar
   @var     upper
   @vdesc   Index for maximum of physical range
   @vtype   int *
   @vio     out
   @endvar
   @var     dir
   @vdesc   Direction of coordinate with this range
   @vtype   int
   @vio     in
   @endvar
   @var     coordname
   @vdesc   Name of coordinate with this range
   @vtype   const char *
   @vio     in
   @endvar
   @var     systemname
   @vdesc   Name of coordinate system
   @vtype   const char *
   @vio     in
   @endvar

   @returntype int
   @returndesc Returns zero for success and negative for error
      -1 = NULL pointer(s) passed for lower and/or upper
      -2 = no coordinate name given
      -3 = no coordinate system name given
      -4 = coordinate system not registered 
      -5 = coordinate name not registered
      -6 = physical coordinate range not registered
   @endreturndesc
@@*/
int CCTK_CoordRangePhysIndex (const cGH *GH,
                              int *lower,
                              int *upper,
                              int coorddir,
                              const char *coordname,
                              const char *systemname)
{
  int i;
  int retval=0;
  struct Coordpropslistphysi *curr;
  struct Coordsystem    *coord_system;
  struct Coordprops     *coord;


  if (lower == NULL || upper == NULL)
  {
    CCTK_Warn (2, __LINE__, __FILE__, "Cactus",
               "CCTK_CoordRangePhysIndex: NULL pointer(s) passed "
               "for lower/upper");
    retval = -1;
  }
  else if (coorddir <= 0 && coordname == NULL)
  {
    CCTK_Warn (2, __LINE__, __FILE__, "Cactus",
               "CCTK_CoordRangePhysIndex: No coordinate name given");
    retval = -2;
  }
  else if (systemname == NULL)
  {
    CCTK_Warn (2, __LINE__, __FILE__, "Cactus",
               "CCTK_CoordRangePhysIndex: No coordinate system name given");
    retval = -3;
  }
  else
  {
    /* Check if system exists */
    Util_GetHandle (CoordSystems, systemname, (void **) &coord_system);
    if (! coord_system)
    {
      CCTK_VWarn (2, __LINE__, __FILE__, "Cactus",
                 "CCTK_CoordRangePhysIndex: System '%s' not registered", systemname);
      retval = -4;
    }
    else
    {
      if (coorddir > 0)
      {
        coord = &coord_system->coords[coorddir-1];
      }
      else
      {
        coord = NULL;
        for (i = 0; i < coord_system->dimension; i++)
        {
          if (CCTK_Equals (coord_system->coords[i].name, coordname))
          {
            coord = &coord_system->coords[i];
            break;
          }
        }
        if (coord == NULL)
        {
          CCTK_VWarn (2, __LINE__, __FILE__, "Cactus",
                      "CCTK_CoordRangePhysIndex: Coordinate name '%s' not registered",
                      coordname);
          retval = -5;
        }
      }
      if (coord)
      {
        for (curr = coord->listphysi; curr; curr = curr->next)
        {

#ifdef DEBUG_COORD
          printf("curr  = %p\n",curr);
          printf("lower = %d\n",curr->lower);
          printf("upper = %d\n",curr->upper);
          printf("next  = %p\n",curr->next);
#endif

          if (curr->GH == GH)
          {
#ifdef DEBUG_COORD
            printf("Returning range (%d,%d) (from %p)\n", *lower,*upper,curr);
#endif

            *lower = curr->lower;
            *upper = curr->upper;
            break;
          }
        }
        if (curr == NULL)
        {
          CCTK_VWarn (2, __LINE__, __FILE__, "Cactus",
                      "CCTK_CoordRangePhysIndex: no physical range registered "
                      "for coordinate '%s' in system '%s'",
                      coord->name, systemname);
          retval = -6;
        }
      }
    }
  }

  return (retval);
}

void CCTK_FCALL cctk_coordrangephysindex_
                           (int *ierr,
                            const cGH *GH,
                            int *lower,
                            int *upper,
                            const int *dir,
                            TWO_FORTSTRING_ARG)
{
  TWO_FORTSTRING_CREATE (name, systemname)
  *ierr = CCTK_CoordRangePhysIndex (GH, lower, upper, *dir, name, systemname);
  free (name);
  free (systemname);
}


 /*@@
   @routine    CCTK_CoordLocalRange
   @date       10th January 2000
   @author     Gabrielle Allen
   @desc
               Returns the range of the coordinate on this processor
               For now this is done in a straightforward manner, assuming
               that a regular grid is used and that the coordinate is a
               Grid function.
   @enddesc
   @calls

   @var
   @vdesc
   @vtype
   @vio
   @vcomment
   @endvar

   @returntype int
   @returndesc
   @endreturndesc
@@*/
int CCTK_CoordLocalRange(const cGH *GH,
                         CCTK_REAL *lower,
                         CCTK_REAL *upper,
                         int dir,
                         const char *name,
                         const char *systemname)
{
  int retval;
  int realdir;
  CCTK_REAL global_lower;
  CCTK_REAL global_upper;


  retval = CCTK_CoordRange (GH, &global_lower, &global_upper, dir, name,
                            systemname);
  if (retval >= 0)
  {
    if (dir > 0)
    {
      realdir = dir;
    }
    else
    {
      realdir = CCTK_CoordDir (name, systemname);
    }
    *lower = global_lower +
             GH->cctk_lbnd[realdir-1] * GH->cctk_delta_space[realdir-1];
    *upper = global_lower +
            (GH->cctk_ubnd[realdir-1] + 1) * GH->cctk_delta_space[realdir-1];
  }
  else
  {
    CCTK_Warn (4, __LINE__, __FILE__, "Cactus",
               "Error finding coordinate range");
  }

#ifdef DEBUG_COORD
  printf("Upper/Lower are %f,%f\n",*lower,*upper);
#endif

  return (retval);
}

void CCTK_FCALL cctk_coordlocalrange_
                           (int *ierr,
                            const cGH *GH,
                            CCTK_REAL *lower,
                            CCTK_REAL *upper,
                            const int *dir,
                            TWO_FORTSTRING_ARG)
{
  TWO_FORTSTRING_CREATE (name, systemname)
  *ierr = CCTK_CoordLocalRange (GH, lower, upper, *dir, name, systemname);
  free (name);
  free (systemname);
}


 /*@@
   @routine    CCTK_NumCoordSystems
   @date       Sat Dec 29 2001
   @author     Gabrielle Allen
   @desc
               The number of coord systems registered
   @enddesc
   @returntype int
   @returndesc
               number of coordinate systems
   @endreturndesc
@@*/

int CCTK_NumCoordSystems (void)
{
  return (num_systems);
}


 /*@@
   @routine    CCTK_CoordSystemImplementation
   @date       Sat Dec 29 2001
   @author     Gabrielle Allen
   @desc
               Provide the implementation which registered a coordinate
	       system
   @enddesc
   @var        handle
   @vdesc      handle of coordinate system
   @vtype      int
   @vio        in
   @endvar

   @returntype const char *
   @returndesc
               Implementation which registered this system
   @endreturndesc
@@*/
const char *CCTK_CoordSystemImplementation (int handle)
{
  struct Coordsystem *coord_system;

  coord_system = (struct Coordsystem *) 
    Util_GetHandledData (CoordSystems, handle);

  return (coord_system ? coord_system->implementation : NULL);
}


 /*@@
   @routine    CCTK_CoordName
   @date       Sat Dec 29 2001
   @author     Gabrielle
   @desc
               Returns the name of a coordinate from a given system
   @enddesc
   @calls      Util_GetHandle

   @var        dir
   @vdesc      coordinate direction in system
   @vtype      int
   @vio        in
   @endvar
   @var        system
   @vdesc      name of coordinate system
   @vtype      const char *
   @vio        in
   @endvar

   @returntype const char *
   @returndesc
               the coordinate name or NULL if handle is coordinate name
	       cannot be found
   @endreturndesc
@@*/
const char *CCTK_CoordName (int dir, const char *systemname)
{
  struct Coordsystem *coord_system;
  const char *retval;

  Util_GetHandle (CoordSystems, systemname, (void **) &coord_system);
  if (! coord_system)
  {
    CCTK_VWarn (4, __LINE__, __FILE__, "Cactus",
		"CCTK_CoordName: System '%s' not registered", systemname);
      retval = NULL;
  }
  else
  {
    retval = (const char *) coord_system->coords[dir-1].name;
  }

  return (retval);
}
