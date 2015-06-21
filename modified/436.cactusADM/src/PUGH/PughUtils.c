#ifdef SPEC_CPU
# define THORN_IS_PUGH
#endif /* SPEC_CPU */
 /*@@
   @file      PUGH_utils.c
   @date      Sunday 12th September 1999
   @author    Gabrielle Allen
   @desc
   @version   $Id: PughUtils.c,v 1.32 2001/11/11 17:52:25 goodale Exp $
   @enddesc
 @@*/

#include <stdlib.h>
#include <string.h>

#include "cctk.h"
#include "cctk_Arguments.h"
#include "cctk_Parameters.h"

#include "pugh.h"

static const char *rcsid = "$Header: /cactus/CactusPUGH/PUGH/src/PughUtils.c,v 1.32 2001/11/11 17:52:25 goodale Exp $";

CCTK_FILEVERSION(CactusPUGH_PUGH_PughUtils_c)


/********************************************************************
 ********************    Static Variables   *************************
 ********************************************************************/

/********************************************************************
 ********************    Internal Routines   ************************
 ********************************************************************/


/********************************************************************
 ********************    External Routines   ************************
 ********************************************************************/
int PUGH_QueryGroupStorage (const cGH *GH, int group, const char *groupname);
void PUGH_Report(const cGH *GH);
void PUGH_PrintStorageReport (const cGH *GH);
void PUGHi_PrintStorageReport (void);
void PUGH_PrintFinalStorageReport (const cGH *GH);
void PUGH_PrintStorage(const cGH *GH);
void CCTK_FCALL pugh_printstorage_
                           (const cGH *GH);

 /*@@
   @routine    PUGH_Topology
   @date       Sunday 28 October 2001
   @author     Gabrielle Allen
   @desc
               Return the processor topology for grid functions
               for a given dimension
   @enddesc
@@*/
const int *PUGH_Topology (const cGH *GH, int dim)
{
  pGH *pughGH;

  pughGH = PUGH_pGH (GH);

  return pughGH->Connectivity[dim-1]->nprocs;
}

 /*@@
   @routine    PUGH_Report
   @date       Sunday 12 September 1999
   @author     Gabrielle Allen
   @desc
               Report on PUGH set up
   @enddesc
@@*/
void PUGH_Report (const cGH *GH)
{
  pGH *pughGH;
  int i,gi,dim;
  int *havedims;
  char *mess;
#ifdef CCTK_MPI
  int in;
#endif
  DECLARE_CCTK_PARAMETERS

  pughGH = PUGH_pGH(GH);

  havedims = (int *)calloc(GH->cctk_dim,sizeof(int));

  mess = (char *)malloc(200*sizeof(char));

#ifdef CCTK_MPI
  CCTK_VInfo(CCTK_THORNSTRING, "MPI Evolution on %d processors", pughGH->nprocs);
#else
  CCTK_VInfo(CCTK_THORNSTRING, "Single processor evolution");
#endif

  /* Report on grid decomposition for any grid functions */
  for (gi=0;gi<CCTK_NumGroups();gi++)
  {
    if (CCTK_GroupTypeI(gi) == CCTK_GF)
    {
      dim = CCTK_GroupDimI(gi);
      havedims[dim-1]=1;
    }
  }

  for (dim=0;dim<GH->cctk_dim;dim++)
  {
    if (havedims[dim])
    {
      CCTK_VInfo(CCTK_THORNSTRING, "%d-dimensional grid functions",dim+1);
      sprintf(mess,"  Size:");
      for (i=0;i<dim+1;i++)
      {
        sprintf(mess,"%s %d",mess,pughGH->GFExtras[dim]->nsize[i]);
      }
      CCTK_INFO(mess);
#ifdef CCTK_MPI
      sprintf(mess,"  Processor topology:");
      for (i=0;i<dim;i++)
      {
        sprintf(mess,"%s %d x",mess,pughGH->Connectivity[dim]->nprocs[i]);
      }
      sprintf(mess,"%s %d",mess,pughGH->Connectivity[dim]->nprocs[dim]);
      CCTK_INFO(mess);

      if (CCTK_Equals(partition, "automatic"))
      {
        sprintf(mess,"  Local load: %d   [",pughGH->GFExtras[dim]->npoints);
        for (i=0;i<dim;i++)
        {
          sprintf(mess,"%s%d x ",mess,pughGH->GFExtras[dim]->lnsize[i]);
        }
        sprintf(mess,"%s%d]",mess,pughGH->GFExtras[dim]->lnsize[i]);
        CCTK_INFO(mess);

        if (CCTK_Equals(info,"load"))
        {
          for (in=0; in<pughGH->nprocs; in++)
          {
            sprintf(mess,"  Local load on processor %d:  %d  [",
                    in,pughGH->GFExtras[dim]->rnpoints[in]);
            for (i=0;i<dim;i++)
            {
              sprintf(mess,"%s%d x ",mess,
                      pughGH->GFExtras[dim]->rnsize[in][i]);
            }
            sprintf(mess,"%s%d]",mess,pughGH->GFExtras[dim]->rnsize[in][i]);
            CCTK_INFO(mess);
          }
        }

      }
      else
      { /* manual partition */
        for (in=0; in<pughGH->nprocs; in++)
        {
          sprintf(mess,"  Local load on proc %d: %d   [",
                  in,pughGH->GFExtras[dim]->rnpoints[in]);
          for (i=0;i<dim;i++)
          {
            sprintf(mess,"%s%d x ",mess,pughGH->GFExtras[dim]->rnsize[in][i]);
          }
          sprintf(mess,"%s%d]",mess,pughGH->GFExtras[dim]->rnsize[in][i]);
          CCTK_INFO(mess);
        }
      }
      CCTK_VInfo(CCTK_THORNSTRING, "  Maximum load skew: %f",
               pughGH->GFExtras[dim]->maxskew);

#endif

    }
  }

  free(havedims);
  free(mess);

  USE_CCTK_PARAMETERS; }

 /*@@
   @routine    PUGH_pGH
   @date       Wed Feb  2 13:27:41 2000
   @author     Tom Goodale
   @desc
               This takes a cGH and returns the active pGH.
               Needed once the pugh GH Extension changes from being
               a pGH to a structure containing pGHs for multi-patch stuff.
   @enddesc
   @calls      CCTK_GHExtension

   @var        GH
   @vdesc      pointer to grid hierarchy
   @vtype      const cGH *
   @vio        in
   @endvar

   @returntype pGH *
   @returndesc
               the PUGH GH extension pointer,<BR>
               or NULL if no PUGH GH extension was registered yet
   @endreturndesc
@@*/
pGH *PUGH_pGH (const cGH *GH)
{
  return ((pGH *) CCTK_GHExtension (GH, "PUGH"));
}


 /*@@
   @routine    PUGH_MPIDataType
   @date       Fri 26 Jan 2001
   @author     Thomas Radke
   @desc
               This routine returns the MPI data type to use
               for communicating a given CCTK datatype.
   @enddesc

   @var        pughGH
   @vdesc      Pointer to PUGH extensions
   @vtype      const pGH *
   @vio        in
   @endvar
   @var        cctk_type
   @vdesc      CCTK data type identifier
   @vtype      int
   @vio        in
   @endvar

   @returntype MPI_Datatype
   @returndesc
               the appropriate datatype for success
               otherwise MPI_DATATYPE_NULL
   @endreturndesc
@@*/
#ifdef CCTK_MPI
MPI_Datatype PUGH_MPIDataType (const pGH *pughGH, int cctk_type)
{
  MPI_Datatype retval;


  switch (cctk_type)
  {
    case CCTK_VARIABLE_CHAR:      retval = PUGH_MPI_CHAR; break;
    case CCTK_VARIABLE_INT:       retval = PUGH_MPI_INT; break;
    case CCTK_VARIABLE_REAL:      retval = PUGH_MPI_REAL; break;
    case CCTK_VARIABLE_COMPLEX:   retval = pughGH->PUGH_mpi_complex; break;
#ifdef CCTK_INT2
    case CCTK_VARIABLE_INT2:      retval = PUGH_MPI_INT2; break;
#endif
#ifdef CCTK_INT4
    case CCTK_VARIABLE_INT4:      retval = PUGH_MPI_INT4; break;
#endif
#ifdef CCTK_INT8
    case CCTK_VARIABLE_INT8:      retval = PUGH_MPI_INT8; break;
#endif
#ifdef CCTK_REAL4
    case CCTK_VARIABLE_REAL4:     retval = PUGH_MPI_REAL4; break;
    case CCTK_VARIABLE_COMPLEX8:  retval = pughGH->PUGH_mpi_complex8; break;
#endif
#ifdef CCTK_REAL8
    case CCTK_VARIABLE_REAL8:     retval = PUGH_MPI_REAL8; break;
    case CCTK_VARIABLE_COMPLEX16: retval = pughGH->PUGH_mpi_complex16; break;
#endif
#ifdef CCTK_REAL16
    case CCTK_VARIABLE_REAL16:    retval = PUGH_MPI_REAL16; break;
    case CCTK_VARIABLE_COMPLEX32: retval = pughGH->PUGH_mpi_complex32; break;
#endif

    default: CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,
                         "Unsupported CCTK variable type %d", cctk_type);
             retval = MPI_DATATYPE_NULL; break;
  }

  return (retval);
}
#endif /* CCTK_MPI */


 /*@@
   @routine    PUGH_PrintStorageReport
   @author     Gabrielle Allen
   @date       16th Sept 2001
   @desc
               Print a report about the use of storage
   @enddesc
@@*/
void PUGH_PrintStorageReport (const cGH *GH)
{
  DECLARE_CCTK_PARAMETERS


  if (storage_report_every > 0 &&
      GH->cctk_iteration % storage_report_every == 0)
  {
    PUGHi_PrintStorageReport();
  }
  USE_CCTK_PARAMETERS; }


 /*@@
   @routine    PUGH_PrintFinalStorageReport
   @author     Gabrielle Allen
   @date       16th Sept 2001
   @desc
               Print a report about the use of storage
   @enddesc
@@*/
void PUGH_PrintFinalStorageReport (const cGH *GH)
{
  GH = GH;
  PUGHi_PrintStorageReport ();
}


 /*@@
   @routine    PUGH_PrintStorage
   @author     Gabrielle Allen
   @date       13th Oct 2001
   @desc
               Print grid variables with storage assigned
   @enddesc
@@*/
void PUGH_PrintStorage (const cGH *GH)
{
  int i;
  int countgf;
  int countarray;
  int countscalar;
  char *messgf=NULL;
  char *messarray=NULL;
  char *messscalar=NULL;

  countgf = 0;
  countarray = 0;
  countscalar = 0;

  /* Work out how long the strings are */

  for (i=0;i<CCTK_NumGroups();i++)
  {
    if (PUGH_QueryGroupStorage(GH,i,NULL))
    {
      if (CCTK_GroupTypeI(i) == CCTK_GF)
      {
        countgf += strlen(CCTK_GroupName(i))+1;
      }
      else if (CCTK_GroupTypeI(i) == CCTK_ARRAY)
      {
        countarray += strlen(CCTK_GroupName(i))+1;
      }
      else if (CCTK_GroupTypeI(i) == CCTK_SCALAR)
      {
        countscalar += strlen(CCTK_GroupName(i))+1;
      }
    }
  }

  /* Now assemble the strings */
  if (countgf)
  {
    messgf = (char *)malloc((countgf+1)*sizeof(char));
    strcpy(messgf,"");
  }
  if (countarray)
  {
    messarray = (char *)malloc((countarray+100)*sizeof(char));
    strcpy(messarray,"");
  }
  if (countscalar)
  {
    messscalar = (char *)malloc((countscalar+100)*sizeof(char));
    strcpy(messscalar,"");
  }

  for (i=0;i<CCTK_NumGroups();i++)
  {
    if (PUGH_QueryGroupStorage(GH,i,NULL))
    {
      if (CCTK_GroupTypeI(i) == CCTK_GF)
      {
        sprintf(messgf,"%s%s ",messgf,CCTK_GroupName(i));
      }
      else if (CCTK_GroupTypeI(i) == CCTK_ARRAY)
      {
        sprintf(messarray,"%s%s ",messarray,CCTK_GroupName(i));
      }
      else if (CCTK_GroupTypeI(i) == CCTK_SCALAR)
      {
        sprintf(messscalar,"%s%s ",messscalar,CCTK_GroupName(i));
      }
    }
  }

  if (messgf||messarray||messscalar)
  {
    CCTK_VInfo(CCTK_THORNSTRING,"Grid Variables with storage enabled:");
    if (messgf)
    {
      CCTK_VInfo(CCTK_THORNSTRING," Grid Functions: %s",messgf);
    }
    if (messarray)
    {
      CCTK_VInfo(CCTK_THORNSTRING," Grid Arrays: %s",messarray);
    }
    if (messscalar)
    {
      CCTK_VInfo(CCTK_THORNSTRING," Grid Scalars: %s",messscalar);
    }
  }

  if (messgf) free(messgf);
  if (messarray) free(messarray);
  if (messscalar) free(messscalar);

}

void CCTK_FCALL pugh_printstorage_
                           (const cGH *GH)
{
  PUGH_PrintStorage (GH);
}
