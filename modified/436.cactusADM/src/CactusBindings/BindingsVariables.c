#ifdef SPEC_CPU
# define THORN_IS_CactusBindings
#endif /* SPEC_CPU */
#include "cctk_ActiveThorns.h"
int CactusBindingsVariables_IOASCII_Initialise(void);
int CactusBindingsVariables_BenchADM_Initialise(void);
int CactusBindingsVariables_Cactus_Initialise(void);
int CactusBindingsVariables_Boundary_Initialise(void);
int CactusBindingsVariables_PUGH_Initialise(void);
int CactusBindingsVariables_IOUtil_Initialise(void);
int CactusBindingsVariables_PUGHSlab_Initialise(void);
int CactusBindingsVariables_CartGrid3D_Initialise(void);
int CactusBindingsVariables_IOBasic_Initialise(void);
int CactusBindingsVariables_PUGHReduce_Initialise(void);
int CactusBindingsVariables_IDLinearWaves_Initialise(void);
int CactusBindingsVariables_Time_Initialise(void);
int CactusBindingsVariables_Einstein_Initialise(void);

int CCTKi_BindingsVariablesInitialise(void);

int CCTKi_BindingsVariablesInitialise(void)
{
  if(CCTK_IsThornActive("IOASCII"))
  {
    CactusBindingsVariables_IOASCII_Initialise();
  }
  if(CCTK_IsThornActive("BenchADM"))
  {
    CactusBindingsVariables_BenchADM_Initialise();
  }
  if(CCTK_IsThornActive("Cactus"))
  {
    CactusBindingsVariables_Cactus_Initialise();
  }
  if(CCTK_IsThornActive("Boundary"))
  {
    CactusBindingsVariables_Boundary_Initialise();
  }
  if(CCTK_IsThornActive("PUGH"))
  {
    CactusBindingsVariables_PUGH_Initialise();
  }
  if(CCTK_IsThornActive("IOUtil"))
  {
    CactusBindingsVariables_IOUtil_Initialise();
  }
  if(CCTK_IsThornActive("PUGHSlab"))
  {
    CactusBindingsVariables_PUGHSlab_Initialise();
  }
  if(CCTK_IsThornActive("CartGrid3D"))
  {
    CactusBindingsVariables_CartGrid3D_Initialise();
  }
  if(CCTK_IsThornActive("IOBasic"))
  {
    CactusBindingsVariables_IOBasic_Initialise();
  }
  if(CCTK_IsThornActive("PUGHReduce"))
  {
    CactusBindingsVariables_PUGHReduce_Initialise();
  }
  if(CCTK_IsThornActive("IDLinearWaves"))
  {
    CactusBindingsVariables_IDLinearWaves_Initialise();
  }
  if(CCTK_IsThornActive("Time"))
  {
    CactusBindingsVariables_Time_Initialise();
  }
  if(CCTK_IsThornActive("Einstein"))
  {
    CactusBindingsVariables_Einstein_Initialise();
  }
  return 0;
}
