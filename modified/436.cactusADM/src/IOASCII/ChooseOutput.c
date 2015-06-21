#ifdef SPEC_CPU
# define THORN_IS_IOASCII
#endif /* SPEC_CPU */
/*@@
  @file      ChooseOutput.c
  @author    Gabrielle Allen
  @date      July 6 2000
  @desc
             Choose what data to write for different IO methods in IOASCII
  @enddesc

  @version   $Id: ChooseOutput.c,v 1.15 2001/12/13 12:08:36 tradke Exp $
 @@*/

#include <stdlib.h>
#include <string.h>

#include "cctk.h"
#include "cctk_Arguments.h"
#include "cctk_Parameters.h"
#include "ioutil_Utils.h"
#include "ioASCIIGH.h"

/* the rcs ID and its dummy function to use it */
static const char *rcsid = "$Header: /cactus/CactusBase/IOASCII/src/ChooseOutput.c,v 1.15 2001/12/13 12:08:36 tradke Exp $";
CCTK_FILEVERSION(CactusBase_IOASCII_ChooseOutput_c)


/* macro to choose origin according actual parameter settings:
     1. Indices from IOASCII
     2. Indices from IOUtil
     3. Coords from IOASCII
     4. Coords from IOUtil
 */
#define GET_SLICE(IOASCII_param, IOUtil_param, origin_index, origin_phys)     \
          origin_index = -1;                                                  \
          if (CCTK_ParameterQueryTimesSet (#IOASCII_param "i", "IOASCII") > 0)\
          {                                                                   \
            origin_index = IOASCII_param##i;                                  \
          }                                                                   \
          else if (CCTK_ParameterQueryTimesSet (#IOUtil_param "i", "IOUtil")>0)\
          {                                                                   \
            origin_index = IOUtil_param##i;                                   \
          }                                                                   \
          else if (CCTK_ParameterQueryTimesSet (#IOASCII_param, "IOASCII") > 0)\
          {                                                                   \
            origin_phys = IOASCII_param;                                      \
          }                                                                   \
          else                                                                \
          {                                                                   \
            origin_phys = IOUtil_param;                                       \
          }

/* prototypes of routines defined in this source file */
void IOASCII_Choose1D (cGH *GH);
void IOASCII_Choose2D (cGH *GH);


/*@@
   @routine   IOASCII_Choose1D
   @author    Gabrielle Allen
   @date      July 6 2000
   @desc
              Use parameters to choose the 1D slices through the output data. 
   @enddesc

   @calls     CCTK_ParameterQueryTimesSet
              IOUtil_1DLines

   @var       GH
   @vdesc     Pointer to CCTK grid hierarchy
   @vtype     cGH *
   @vio       in
   @endvar
@@*/
void IOASCII_Choose1D (cGH *GH)
{
  DECLARE_CCTK_PARAMETERS
  int i, j;
  asciiioGH *myGH;
  int *origin_index[3];
  CCTK_REAL *origin_phys[3];


  /* allocate arrays for origins */
  origin_phys[0] = (CCTK_REAL *) malloc (3 * 3 * sizeof (CCTK_REAL));
  origin_phys[1] = origin_phys[0] + 3;
  origin_phys[2] = origin_phys[1] + 3;
  origin_index[0] = (int *) malloc (3 * 3 * sizeof (int));
  origin_index[1] = origin_index[0] + 3;
  origin_index[2] = origin_index[1] + 3;

  /* get slice points */
  GET_SLICE (out1D_xline_y, out_xline_y, origin_index[0][1], origin_phys[0][1]);
  GET_SLICE (out1D_xline_z, out_xline_z, origin_index[0][2], origin_phys[0][2]);
  GET_SLICE (out1D_yline_x, out_yline_x, origin_index[1][0], origin_phys[1][0]);
  GET_SLICE (out1D_yline_z, out_yline_z, origin_index[1][2], origin_phys[1][2]);
  GET_SLICE (out1D_zline_x, out_zline_x, origin_index[2][0], origin_phys[2][0]);
  GET_SLICE (out1D_zline_y, out_zline_y, origin_index[2][1], origin_phys[2][1]);

  myGH = (asciiioGH *) CCTK_GHExtension (GH, "IOASCII");

  for (i = 1; i <= CCTK_MaxDim (); i++)
  {
    if (i <= 3)
    {
      IOUtil_1DLines (GH, i, origin_index, origin_phys, myGH->spxyz[i-1]);
    }
    else
    {
      for (j = 0; j < i; j++)
      {
        memset (myGH->spxyz[i-1][j], 0, i);
      }
    }
  }

  /* free allocated resources */
  free (origin_phys[0]);
  free (origin_index[0]);
  USE_CCTK_PARAMETERS; }


/*@@
   @routine   IOASCII_Choose2D
   @author    Gabrielle Allen
   @date      July 6 2000
   @desc
              Use parameters to choose the 2D slices through the output data. 
   @enddesc

   @calls     CCTK_ParameterQueryTimesSet
              IOUtil_2DPlanes

   @var       GH
   @vdesc     Pointer to CCTK grid hierarchy
   @vtype     cGH *
   @vio       in
   @endvar
 @@*/
void IOASCII_Choose2D (cGH *GH)
{
  DECLARE_CCTK_PARAMETERS
  int i;
  asciiioGH *myGH;                  /* IOASCII extension handle */
  int origin_index[3];              /* Specify output planes by indices */
  CCTK_REAL origin_phys[3];         /* Specify output planes by coordinates */


  GET_SLICE (out2D_xyplane_z, out_xyplane_z, origin_index[0], origin_phys[0]);
  GET_SLICE (out2D_xzplane_y, out_xzplane_y, origin_index[1], origin_phys[1]);
  GET_SLICE (out2D_yzplane_x, out_yzplane_x, origin_index[2], origin_phys[2]);

  myGH = (asciiioGH *) CCTK_GHExtension (GH, "IOASCII");

  for (i = 1; i <= CCTK_MaxDim (); i++)
  {
    if (i > 1 && i <= 3)
    {
      IOUtil_2DPlanes (GH, i, origin_index, origin_phys, myGH->sp2xyz[i-1]);
    }
    else
    {
      memset (myGH->sp2xyz[i-1], 0, i * sizeof (myGH->sp2xyz[i-1][0]));
    }
  }
  USE_CCTK_PARAMETERS; }
