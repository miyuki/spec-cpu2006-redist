#ifdef SPEC_CPU
# define THORN_IS_PUGH
#endif /* SPEC_CPU */
 /*@@
   @file      SetupGroup.c
   @date      Mon Feb  8 19:31:45 1999
   @author    Tom Goodale
   @desc
              Subroutines for setting up a group on a pGH.
   @enddesc
   @version   $Id: SetupGroup.c,v 1.45 2001/10/07 10:06:31 goodale Exp $
 @@*/

#include <stdlib.h>
#include <string.h>

#include "pugh.h"
#include "pughi.h"

#include "cctk_Parameters.h"

static const char *rcsid = "$Header: /cactus/CactusPUGH/PUGH/src/SetupGroup.c,v 1.45 2001/10/07 10:06:31 goodale Exp $";
CCTK_FILEVERSION (CactusPUGH_PUGH_SetupGroup_c)


/********************************************************************
 ********************    Internal Routines   ************************
 ********************************************************************/
static int PUGH_SetupScalarGroup (pGH *newGH,
                                  int vtype,
                                  int n_variables,
                                  int n_timelevels,
                                  int vectorgroup);

static int PUGH_SetupGAGroup (pGH *newGH,
                              int *nsize,
                              int *ghostsize,
                              int  gtype,
                              int  vtype,
                              int  dim,
                              int  n_variables,
                              int  staggercode,
                              int  n_timelevels,
                              int vectorgroup);


 /*@@
   @routine    PUGH_SetupScalarGroup
   @date       Wed Feb 17 04:45:49 1999
   @author     Tom Goodale
   @desc
               Set up a group of scalar variables on a pGH.
               For efficiency reasons, PUGH allocates storage for scalars
               only once when the group is created.
               The current state of storage allocation (which is toggled by
               Enable/DisableGroupStorage) is stored in a byte-sized flag
               immediately after the scalar data.
   @enddesc

   @var        newGH
   @vdesc      Pointer to PUGH grid hierarchy
   @vtype      pGH *
   @vio        in
   @endvar
   @var        vtype
   @vdesc      CCTK data type for variables in this group
   @vtype      int
   @vio        in
   @endvar
   @var        n_variables
   @vdesc      number of variables in this group
   @vtype      int
   @vio        in
   @endvar
   @var        n_timelevels
   @vdesc      number of timelevels in this group
   @vtype      int
   @vio        in
   @endvar
   @var        vectorgroup
   @vdesc      is this a vector group ?
   @vtype      int
   @vio        in
   @endvar

   @returntype int
   @returndesc
               PUGH_SUCCESS (0) if successful, or
               PUGH_ERRORMEMORY (negative) if memory allocation failed
   @endreturndesc
@@*/
static int PUGH_SetupScalarGroup (pGH *newGH,
                                  int vtype,
                                  int n_variables,
                                  int n_timelevels,
                                  int vectorgroup)
{
  DECLARE_CCTK_PARAMETERS
  int variable, level, vtypesize, retval;
  void *temp;


  retval = 0;  /* PUGH_SUCCESS */

  temp = realloc (newGH->variables,
                  (newGH->nvariables + n_variables) * sizeof (void **));
  if (temp)
  {
    newGH->variables = (void ***) temp;
    vtypesize = CCTK_VarTypeSize (vtype);

    for (variable = 0; variable < n_variables && retval == 0; variable++)
    {
      temp = malloc (n_timelevels * sizeof (void *));
      if (temp)
      {
        newGH->variables[newGH->nvariables + variable] = (void **) temp;
        for (level = 0; level < n_timelevels; level++)
        {
          /* allocate one more byte for the query_storage flag */
          temp = malloc (vtypesize + 1);
          if (temp)
          {
            newGH->variables[newGH->nvariables + variable][level] = temp;

            /* reset the query_storage flag */
            ((char *) temp)[vtypesize] = PUGH_NOSTORAGE;
          }
          else
          {
            retval = PUGH_ERRORMEMORY;
            break;
          }
        }
      }
      else
      {
        retval = PUGH_ERRORMEMORY;
      }
    }
  }
  else
  {
    retval = PUGH_ERRORMEMORY;
  }

  if (! retval)
  {
    newGH->nvariables += n_variables;
  }
  else
  {
    CCTK_WARN (1, "Memory allocation error in PUGH_SetupScalarGroup");
  }

  USE_CCTK_PARAMETERS;   return (retval);
}


 /*@@
   @routine    PUGH_SetupGAGroup
   @date       January 19 2000
   @author     Gabrielle Allen
   @desc
               Set up a group of grid array variables (CCTK_GF or CCTK_ARRAY
               types) on a pGH.
   @enddesc
   @calls      CCTK_VarTypeSize
               PUGH_SetupConnectivity
               PUGH_SetupPGExtras
               PUGH_SetupGArrayGroupComm
               PUGH_SetupGArray

   @var        newGH
   @vdesc      Pointer to PUGH grid hierarchy
   @vtype      pGH *
   @vio        in
   @endvar
   @var        nsize
   @vdesc      size dimensions for variables in this group
   @vtype      int *
   @vio        in
   @endvar
   @var        ghostsize
   @vdesc      ghostsize dimensions for variables in this group
   @vtype      int *
   @vio        in
   @endvar
   @var        gtype
   @vdesc      group type
   @vtype      int
   @vio        in
   @endvar
   @var        vtype
   @vdesc      CCTK data type for variables in this group
   @vtype      int
   @vio        in
   @endvar
   @var        dim
   @vdesc      number of dimensions for variables in this group
   @vtype      int
   @vio        in
   @endvar
   @var        n_variables
   @vdesc      number of variables in this group
   @vtype      int
   @vio        in
   @endvar
   @var        staggercode
   @vdesc      stagger code for variables in this group
   @vtype      int
   @vio        in
   @endvar
   @var        n_timelevels
   @vdesc      number of timelevels in this group
   @vtype      int
   @vio        in
   @endvar
   @var        vectorgroup
   @vdesc      is this a vector group ?
   @vtype      int
   @vio        in
   @endvar

   @returntype int
   @returndesc
               PUGH_SUCCESS (0) if successful, or
               PUGH_ERRORMEMORY (negative) if memory allocation failed
   @endreturndesc
@@*/
static int PUGH_SetupGAGroup (pGH *newGH,
                              int *nsize,
                              int *ghostsize,
                              int  gtype,
                              int  vtype,
                              int  dim,
                              int  n_variables,
                              int  staggercode,
                              int  n_timelevels,
                              int  vectorgroup)
{
  int i;
  int           variable;
  int           var_size;
  int           retval;
  int           level;
  void       ***temp;
  pConnectivity *connectivity;
  pGExtras      *extras;
  pComm         *groupcomm;
  int *perme;
  int *nprocs;


  retval = PUGH_SUCCESS;
  var_size = CCTK_VarTypeSize (vtype);

  if (gtype == CCTK_ARRAY)
  {
    /* Arrays can't (yet) have periodicity and manual setup,
       so initialize them to zero */
    perme = (int *) calloc (dim, sizeof (int));
    nprocs = (int *) calloc (dim, sizeof (int));
    if (! (perme && nprocs))
    {
      CCTK_WARN (0, "Memory allocation error in PUGH_SetupGAGroup");
    }

    /* Check that there are enough grid points in this dimension
     * to make parallelising it worthwhile
     */
    for (i = 0 ; i < dim; i++)
    {
      if (! nprocs[i] && abs (nsize[i]) <= 2*ghostsize[i] + 1)
      {
        nprocs[i] = 1;
      }
    }

    connectivity = PUGH_SetupConnectivity (dim, newGH->nprocs, nprocs, perme);

    extras = PUGH_SetupPGExtras (dim,
                                 perme,
                                 staggercode,
                                 nsize,
                                 ghostsize,
                                 newGH->nprocs,
                                 connectivity->nprocs,
                                 newGH->myproc);

    free (nprocs);
    free (perme);
  }
  else
  {
    /* this is for CCTK_GF variables */
    connectivity = newGH->Connectivity[dim-1];
    extras = newGH->GFExtras[dim-1];
  }

  /* Set up the communication buffer used for all variables within this group.
     Note: only with allocated buffers we can have group communication. */
  if(newGH->commmodel == PUGH_ALLOCATEDBUFFERS)
  {
    groupcomm = PUGH_SetupGArrayGroupComm (newGH,
                                           dim,
                                           newGH->nvariables,
                                           n_variables,
                                           0,
                                           vtype,
                                           extras);
  }
  else
  {
    groupcomm = NULL;
  }

  temp = (void ***) realloc (newGH->variables, (newGH->nvariables+n_variables) *
                             sizeof (void **));

  if(temp)
  {
    newGH->variables = temp;

    for (variable = 0; variable < n_variables; variable++)
    {
      newGH->variables[newGH->nvariables] = (void **) malloc (n_timelevels *
                                                              sizeof (void *));

      if (newGH->variables[newGH->nvariables])
      {
        for (level = 0; level < n_timelevels; level++)
        {
          newGH->variables[newGH->nvariables][level] =
            PUGH_SetupGArray (newGH,
                              extras,
                              connectivity,
                              groupcomm,
                              CCTK_VarName (newGH->nvariables),
                              newGH->nvariables,
                              newGH->narrays,
                              var_size,
                              vtype,
                              staggercode,
                              vectorgroup ? n_variables : 1,
                              variable,
                              variable > 0 ? newGH->variables[newGH->nvariables-variable][level] : NULL);
          newGH->narrays++;
        }
        newGH->nvariables++;
      }
      else
      {
        retval = PUGH_ERRORMEMORY;
        break;
      }
    }
  }
  else
  {
    retval = PUGH_ERRORMEMORY;
  }

  if (retval)
  {
    CCTK_WARN (1, "Memory allocation error in PUGH_SetupGAGroup");
  }

  return (retval);
}


 /*@@
   @routine    PUGH_SetupGroup
   @date       Mon Feb  8 19:37:55 1999
   @author     Tom Goodale
   @desc
               Sets up a group on a pGH
   @enddesc
   @calls      PUGH_SetupScalarGroup
               PUGH_SetupGAGroup

   @var        newGH
   @vdesc      Pointer to PUGH grid hierarchy
   @vtype      pGH *
   @vio        in
   @endvar
   @var        nsize
   @vdesc      size dimensions for variables in this group
   @vtype      int *
   @vio        in
   @endvar
   @var        ghostsize
   @vdesc      ghostsize dimensions for variables in this group
   @vtype      int *
   @vio        in
   @endvar
   @var        gtype
   @vdesc      group type
   @vtype      int
   @vio        in
   @endvar
   @var        vtype
   @vdesc      CCTK data type for variables in this group
   @vtype      int
   @vio        in
   @endvar
   @var        dim
   @vdesc      number of dimensions for variables in this group
   @vtype      int
   @vio        in
   @endvar
   @var        n_variables
   @vdesc      number of variables in this group
   @vtype      int
   @vio        in
   @endvar
   @var        staggercode
   @vdesc      stagger code for variables in this group
   @vtype      int
   @vio        in
   @endvar
   @var        n_timelevels
   @vdesc      number of timelevels in this group
   @vtype      int
   @vio        in
   @endvar
   @var        vectorgroup
   @vdesc      is this a vector group ?
   @vtype      int
   @vio        in
   @endvar

   @returntype int
   @returndesc
               return code of @seeroutine PUGH_SetupScalarGroup or
               @seeroutine PUGH_SetupGAGroup, or
               PUGH_ERROR if invalid group type was passed in
   @endreturndesc
@@*/
int PUGH_SetupGroup (pGH *newGH,
                     int *nsize,
                     int *nghostsize,
                     int  gtype,
                     int  vtype,
                     int  dim,
                     int  n_variables,
                     int  staggercode,
                     int  n_timelevels,
                     int  vectorgroup)
{
  int retval;

  if (gtype == CCTK_SCALAR)
  {
    retval = PUGH_SetupScalarGroup (newGH, vtype, n_variables, n_timelevels, vectorgroup);
  }
  else if (gtype == CCTK_ARRAY || gtype == CCTK_GF)
  {
    retval = PUGH_SetupGAGroup (newGH, nsize, nghostsize, gtype, vtype, dim,
                                n_variables, staggercode, n_timelevels,vectorgroup);
  }
  else
  {
    CCTK_WARN (0, "Unknown group type in PUGH_SetupGroup");
    retval = PUGH_ERROR;
  }

  return (retval);
}
