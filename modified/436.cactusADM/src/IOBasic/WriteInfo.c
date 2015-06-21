#ifdef SPEC_CPU
# define THORN_IS_IOBasic
#endif /* SPEC_CPU */
/*@@
   @file      WriteInfo.c
   @date      June 31 1999
   @author    Gabrielle Allen, Paul Walker, Thomas Radke
   @desc
              Gets the data for IOBasic's info output.
   @enddesc
   @version   $Id: /cactusdevcvs/CactusBase/IOBasic/src/OutputInfo.c,v 1.24 2001
@@*/

#include <stdlib.h>

#include "cctk.h"
#include "iobasicGH.h"

/* the rcs ID and its dummy function to use it */
static const char *rcsid = "$Header: /cactus/CactusBase/IOBasic/src/WriteInfo.c,v 1.16 2001/12/28 21:22:52 tradke Exp $";
CCTK_FILEVERSION(CactusBase_IOBasic_WriteInfo_c)


 /*@@
   @routine    IOBasic_WriteInfo
   @date       Tue 31 July 2001
   @author     Thomas Radke
   @desc
               Gets the data values to output for a given CCTK variable.
               For CCTK_SCALAR variables, their value is taken,
               for CCTK_GF and CCTK_ARRAY variables the appropriate
               reduction operations are performed and their results taken.
   @enddesc

   @calls      CCTK_VarDataPtrI
               CCTK_Reduce

   @var       GH
   @vdesc     Pointer to CCTK grid hierarchy
   @vtype     const cGH *
   @vio       in
   @endvar
   @var       vindex
   @vdesc     CCTK index of the variable to output
   @vtype     int
   @vio       in
   @endvar
   @var       alias
   @vdesc     alias name to use for building the output filename
   @vtype     const char *
   @vio       in
   @endvar

   @returntype int
   @returndesc
                0 for success, or<BR>
               -1 if variable has no storage assigned
   @endreturndesc
@@*/
int IOBasic_WriteInfo (const cGH *GH, int vindex)
{
  int vtype;
  char *fullname;
  iobasicGH *myGH;
  void *ptr;
  iobasic_reduction_t *reduction;
  /*** FIXME: can CCTK_Reduce() have a 'const cGH *' parameter ?? ***/
  union
  {
    const cGH *const_ptr;
    cGH *non_const_ptr;
  } GH_fake_const;


  GH_fake_const.const_ptr = GH;


  myGH = (iobasicGH *) CCTK_GHExtension (GH, "IOBasic");
  reduction = myGH->info_reductions[vindex].reductions;

  /* first, check if variable has storage assigned */
  if (! CCTK_QueryGroupStorageI (GH, CCTK_GroupIndexFromVarI (vindex)))
  {
    fullname = CCTK_FullName (vindex);
    CCTK_VWarn (2, __LINE__, __FILE__, CCTK_THORNSTRING,
                "IOBasic_WriteInfo: No info output for '%s' (no storage)",
                fullname);
    free (fullname);
    /* invalidate data buffer for this variable */
    while (reduction)
    {
      reduction->is_valid = 0;
      reduction = reduction->next;
    }
    return (-1);
  }

  /* CCTK scalars are printed by their value, cast into a C double */
  if (CCTK_GroupTypeFromVarI (vindex) == CCTK_SCALAR)
  {
    reduction->is_valid = 1;

    /* get the variable's data type and data pointer */
    ptr = CCTK_VarDataPtrI (GH, 0, vindex);
    vtype = CCTK_VarTypeI (vindex);
    if (vtype == CCTK_VARIABLE_CHAR)
    {
      reduction->value = (double) *(CCTK_CHAR *) ptr;
    }
    else if (vtype == CCTK_VARIABLE_INT)
    {
      reduction->value = (double) *(CCTK_INT *) ptr;
    }
    else if (vtype == CCTK_VARIABLE_REAL)
    {
      reduction->value = (double) *(CCTK_REAL *) ptr;
    }
    else if (vtype == CCTK_VARIABLE_COMPLEX)
    {
      reduction->value = (double) ((CCTK_REAL *) ptr)[0];
      reduction->next->value = (double) ((CCTK_REAL *) ptr)[1];
      reduction->next->is_valid = 1;
    }
#ifdef CCTK_INT2
    else if (vtype == CCTK_VARIABLE_INT2)
    {
      reduction->value = (double) *(CCTK_INT2 *) ptr;
    }
#endif
#ifdef CCTK_INT4
    else if (vtype == CCTK_VARIABLE_INT4)
    {
      reduction->value = (double) *(CCTK_INT4 *) ptr;
    }
#endif
#ifdef CCTK_INT8
    else if (vtype == CCTK_VARIABLE_INT8)
    {
      reduction->value = (double) *(CCTK_INT8 *) ptr;
    }
#endif
#ifdef CCTK_REAL4
    else if (vtype == CCTK_VARIABLE_REAL4)
    {
      reduction->value = (double) *(CCTK_REAL4 *) ptr;
    }
    else if (vtype == CCTK_VARIABLE_COMPLEX8)
    {
      reduction->value = (double) ((CCTK_REAL4 *) ptr)[0];
      reduction->next->value = (double) ((CCTK_REAL4 *) ptr)[1];
      reduction->next->is_valid = 1;
    }
#endif
#ifdef CCTK_REAL8
    else if (vtype == CCTK_VARIABLE_REAL8)
    {
      reduction->value = (double) *(CCTK_REAL8 *) ptr;
    }
    else if (vtype == CCTK_VARIABLE_COMPLEX16)
    {
      reduction->value = (double) ((CCTK_REAL8 *) ptr)[0];
      reduction->next->value = (double) ((CCTK_REAL8 *) ptr)[1];
      reduction->next->is_valid = 1;
    }
#endif
#ifdef CCTK_REAL16
    else if (vtype == CCTK_VARIABLE_REAL16)
    {
      reduction->value = (double) *(CCTK_REAL16 *) ptr;
    }
    else if (vtype == CCTK_VARIABLE_COMPLEX32)
    {
      reduction->value = (double) ((CCTK_REAL16 *) ptr)[0];
      reduction->next->value = (double) ((CCTK_REAL16 *) ptr)[1];
      reduction->next->is_valid = 1;
    }
#endif
    else
    {
      CCTK_WARN (3, "IOBasic_WriteInfo: Unsupported data type");
      reduction->is_valid = 0;
    }
  }
  else
  {
    /* for CCTK_GF and CCTK_ARRAY variables: loop over all reductions */
    while (reduction)
    {
      reduction->is_valid = CCTK_Reduce (GH_fake_const.non_const_ptr, 0,
                                         reduction->handle, 1,
                                         CCTK_VARIABLE_REAL,
                                         &reduction->value, 1, vindex) == 0;
      if (! reduction->is_valid)
      {
        CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,
                    "IOBasic_WriteInfo: Internal error in reduction '%s'",
                    reduction->name);
      }

      reduction = reduction->next;
    }
  }

  return (0);
}
