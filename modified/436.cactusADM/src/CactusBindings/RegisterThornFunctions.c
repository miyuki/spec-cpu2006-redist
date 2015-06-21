#ifdef SPEC_CPU
# define THORN_IS_CactusBindings
#endif /* SPEC_CPU */
/*@@
  @header    RegisterAllFunctions.c
  @desc
  Register aliased functions from active thorns
  @enddesc 
  @returntype int
  @returndesc 
  Minus number of failed overloads
  @endreturndesc
  @@*/

#include "cctk_Flesh.h"
#include "cctk_ActiveThorns.h"

int CCTKBindings_IOASCIIAliases(void);
int CCTKBindings_BenchADMAliases(void);
int CCTKBindings_CactusAliases(void);
int CCTKBindings_BoundaryAliases(void);
int CCTKBindings_PUGHAliases(void);
int CCTKBindings_IOUtilAliases(void);
int CCTKBindings_PUGHSlabAliases(void);
int CCTKBindings_CartGrid3DAliases(void);
int CCTKBindings_IOBasicAliases(void);
int CCTKBindings_PUGHReduceAliases(void);
int CCTKBindings_IDLinearWavesAliases(void);
int CCTKBindings_TimeAliases(void);
int CCTKBindings_EinsteinAliases(void);
int CCTKBindings_RegisterThornFunctions(void);

int CCTKBindings_RegisterThornFunctions(void)
{
  int retval = 0;
  if (CCTK_IsThornActive("IOASCII"))
  {
    retval =+ CCTKBindings_IOASCIIAliases();
  }
  if (CCTK_IsThornActive("BenchADM"))
  {
    retval =+ CCTKBindings_BenchADMAliases();
  }
  if (CCTK_IsThornActive("Cactus"))
  {
    retval =+ CCTKBindings_CactusAliases();
  }
  if (CCTK_IsThornActive("Boundary"))
  {
    retval =+ CCTKBindings_BoundaryAliases();
  }
  if (CCTK_IsThornActive("PUGH"))
  {
    retval =+ CCTKBindings_PUGHAliases();
  }
  if (CCTK_IsThornActive("IOUtil"))
  {
    retval =+ CCTKBindings_IOUtilAliases();
  }
  if (CCTK_IsThornActive("PUGHSlab"))
  {
    retval =+ CCTKBindings_PUGHSlabAliases();
  }
  if (CCTK_IsThornActive("CartGrid3D"))
  {
    retval =+ CCTKBindings_CartGrid3DAliases();
  }
  if (CCTK_IsThornActive("IOBasic"))
  {
    retval =+ CCTKBindings_IOBasicAliases();
  }
  if (CCTK_IsThornActive("PUGHReduce"))
  {
    retval =+ CCTKBindings_PUGHReduceAliases();
  }
  if (CCTK_IsThornActive("IDLinearWaves"))
  {
    retval =+ CCTKBindings_IDLinearWavesAliases();
  }
  if (CCTK_IsThornActive("Time"))
  {
    retval =+ CCTKBindings_TimeAliases();
  }
  if (CCTK_IsThornActive("Einstein"))
  {
    retval =+ CCTKBindings_EinsteinAliases();
  }
  return retval;
}
