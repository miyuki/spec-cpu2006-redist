#ifdef SPEC_CPU
# define THORN_IS_CactusBindings
#endif /* SPEC_CPU */
#include "SKBinTree.h"
#include "cctk_ActiveThorns.h"

/* Prototypes for functions to be registered. */
int CCTKi_BindingsParameterRecoveryInitialise(void);
int CCTKi_BindingsParameterRecovery_BenchADM(void);
int CCTKi_BindingsParameterRecovery_Boundary(void);
int CCTKi_BindingsParameterRecovery_Cactus(void);
int CCTKi_BindingsParameterRecovery_CartGrid3D(void);
int CCTKi_BindingsParameterRecovery_Einstein(void);
int CCTKi_BindingsParameterRecovery_IDLinearWaves(void);
int CCTKi_BindingsParameterRecovery_IOASCII(void);
int CCTKi_BindingsParameterRecovery_IOBasic(void);
int CCTKi_BindingsParameterRecovery_IOUtil(void);
int CCTKi_BindingsParameterRecovery_PUGH(void);
int CCTKi_BindingsParameterRecovery_PUGHReduce(void);
int CCTKi_BindingsParameterRecovery_PUGHSlab(void);
int CCTKi_BindingsParameterRecovery_Time(void);
/*@@
  @routine    CCTKi_BindingsParameterRecoveryInitialise
  @date       
  @author     
  @desc 
  Calls all the thorn parameter recovery bindings file if the thorns are active.
  @enddesc 
  @calls     
  @calledby   
  @history 

  @endhistory

@@*/
int CCTKi_BindingsParameterRecoveryInitialise(void)
{
  int result;
  int retval = 0;

  do
  {
    if(CCTK_IsThornActive("BenchADM"))
    {
      result = CCTKi_BindingsParameterRecovery_BenchADM();
      if (result != 0)
        retval = result;
      if (retval > 0)
        break;
    }
    if(CCTK_IsThornActive("Boundary"))
    {
      result = CCTKi_BindingsParameterRecovery_Boundary();
      if (result != 0)
        retval = result;
      if (retval > 0)
        break;
    }
    if(CCTK_IsThornActive("Cactus"))
    {
      result = CCTKi_BindingsParameterRecovery_Cactus();
      if (result != 0)
        retval = result;
      if (retval > 0)
        break;
    }
    if(CCTK_IsThornActive("CartGrid3D"))
    {
      result = CCTKi_BindingsParameterRecovery_CartGrid3D();
      if (result != 0)
        retval = result;
      if (retval > 0)
        break;
    }
    if(CCTK_IsThornActive("Einstein"))
    {
      result = CCTKi_BindingsParameterRecovery_Einstein();
      if (result != 0)
        retval = result;
      if (retval > 0)
        break;
    }
    if(CCTK_IsThornActive("IDLinearWaves"))
    {
      result = CCTKi_BindingsParameterRecovery_IDLinearWaves();
      if (result != 0)
        retval = result;
      if (retval > 0)
        break;
    }
    if(CCTK_IsThornActive("IOASCII"))
    {
      result = CCTKi_BindingsParameterRecovery_IOASCII();
      if (result != 0)
        retval = result;
      if (retval > 0)
        break;
    }
    if(CCTK_IsThornActive("IOBasic"))
    {
      result = CCTKi_BindingsParameterRecovery_IOBasic();
      if (result != 0)
        retval = result;
      if (retval > 0)
        break;
    }
    if(CCTK_IsThornActive("IOUtil"))
    {
      result = CCTKi_BindingsParameterRecovery_IOUtil();
      if (result != 0)
        retval = result;
      if (retval > 0)
        break;
    }
    if(CCTK_IsThornActive("PUGH"))
    {
      result = CCTKi_BindingsParameterRecovery_PUGH();
      if (result != 0)
        retval = result;
      if (retval > 0)
        break;
    }
    if(CCTK_IsThornActive("PUGHReduce"))
    {
      result = CCTKi_BindingsParameterRecovery_PUGHReduce();
      if (result != 0)
        retval = result;
      if (retval > 0)
        break;
    }
    if(CCTK_IsThornActive("PUGHSlab"))
    {
      result = CCTKi_BindingsParameterRecovery_PUGHSlab();
      if (result != 0)
        retval = result;
      if (retval > 0)
        break;
    }
    if(CCTK_IsThornActive("Time"))
    {
      result = CCTKi_BindingsParameterRecovery_Time();
      if (result != 0)
        retval = result;
      if (retval > 0)
        break;
    }
  } while (0);
  return retval;
}
