#ifdef SPEC_CPU
# define THORN_IS_PUGH
#endif /* SPEC_CPU */
/*@@
   @file      Overloadables.c
   @author    Gabrielle Allen
   @date      2000/06/22
   @desc
              PUGH routines which overload CCTK routines in the flesh
   @enddesc
   @version   $Id: Overloadables.c,v 1.5 2001/11/05 15:01:46 tradke Exp $
 @@*/

#include "cctk.h"
#include "pugh.h"
#include "pugh_Comm.h"

static const char *rcsid = "$Header: /cactus/CactusPUGH/PUGH/src/Overloadables.c,v 1.5 2001/11/05 15:01:46 tradke Exp $";
CCTK_FILEVERSION(CactusPUGH_PUGH_Overloadables_c)


/*@@
   @routine    PUGH_GroupDynamicData
   @author     Gabrielle Allen
   @date       2000/06/22
   @desc
               Returns the driver's internal data for a given group
               of CCTK_GF or CCTK_ARRAY variables
   @enddesc
   @calls      CCTK_GroupTypeI
               CCTK_FirstVarIndexI

   @var        GH
   @vdesc      Pointer to CCTK GH
   @vtype      const cGH *
   @vio        in
   @endvar
   @var        group
   @vdesc      index of group
   @vtype      int
   @vio        in
   @endvar
   @var        data
   @vdesc      Pointer to caller-supplied data structure to store group data
   @vtype      cGroupDynamicData *
   @vio        out
   @endvar

   @returntype int
   @returndesc
                0 for success <BR>
               -1 if given pointer to data structure is NULL <BR>
               -2 if given group is not of type CCTK_GF or CCTK_ARRAY <BR>
               -3 if given GH pointer is invalid
   @endreturndesc
@@*/
int PUGH_GroupDynamicData (const cGH *GH, int group, cGroupDynamicData *data)
{
  int gtype, var;
  pGH *pughGH;
  pGExtras *extras;
  int retval;


  if (data)
  {
    gtype = CCTK_GroupTypeI (group);
    if (gtype == CCTK_GF || gtype == CCTK_ARRAY)
    {
      /* Get the first variable in the group */
      var = CCTK_FirstVarIndexI (group);

      pughGH = PUGH_pGH (GH);
      if (pughGH)
      {
        extras = ((pGA ***) pughGH->variables)[var][0]->extras;

        data->dim         = extras->dim;
        data->lsh         = extras->lnsize;
        data->gsh         = extras->nsize;
        data->lbnd        = extras->lb[pughGH->myproc];
        data->ubnd        = extras->ub[pughGH->myproc];
        data->nghostzones = extras->nghostzones;
        data->bbox        = extras->bbox;

        retval = 0;
      }
      else
      {
        retval = -3;
      }
    }
    else
    {
      retval = -2;
    }
  }
  else
  {
    retval = -1;
  }

  return (retval);
}
