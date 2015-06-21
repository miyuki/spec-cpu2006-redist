#ifdef SPEC_CPU
# define THORN_IS_CactusBindings
#endif /* SPEC_CPU */
#include "SKBinTree.h"
#include "cctk_ActiveThorns.h"

/* Prototypes for functions to be registered. */
void CCTKi_BindingsSchedule_BenchADM(void);
void CCTKi_BindingsSchedule_Boundary(void);
void CCTKi_BindingsSchedule_Cactus(void);
void CCTKi_BindingsSchedule_CartGrid3D(void);
void CCTKi_BindingsSchedule_Einstein(void);
void CCTKi_BindingsSchedule_IDLinearWaves(void);
void CCTKi_BindingsSchedule_IOASCII(void);
void CCTKi_BindingsSchedule_IOBasic(void);
void CCTKi_BindingsSchedule_IOUtil(void);
void CCTKi_BindingsSchedule_PUGH(void);
void CCTKi_BindingsSchedule_PUGHReduce(void);
void CCTKi_BindingsSchedule_PUGHSlab(void);
void CCTKi_BindingsSchedule_Time(void);
/*@@
  @routine    CCTKi_BindingsScheduleInitialise
  @date       
  @author     
  @desc 
  Calls all the thorn schedule bindings file if the thorns are active.
  @enddesc 
  @calls     
  @calledby   
  @history 

  @endhistory

@@*/
int CCTKi_BindingsScheduleInitialise(void);
int CCTKi_BindingsScheduleInitialise(void)
{
  if(CCTK_IsThornActive("BenchADM"))
  {
    CCTKi_BindingsSchedule_BenchADM();
  }
  if(CCTK_IsThornActive("Boundary"))
  {
    CCTKi_BindingsSchedule_Boundary();
  }
  if(CCTK_IsThornActive("Cactus"))
  {
    CCTKi_BindingsSchedule_Cactus();
  }
  if(CCTK_IsThornActive("CartGrid3D"))
  {
    CCTKi_BindingsSchedule_CartGrid3D();
  }
  if(CCTK_IsThornActive("Einstein"))
  {
    CCTKi_BindingsSchedule_Einstein();
  }
  if(CCTK_IsThornActive("IDLinearWaves"))
  {
    CCTKi_BindingsSchedule_IDLinearWaves();
  }
  if(CCTK_IsThornActive("IOASCII"))
  {
    CCTKi_BindingsSchedule_IOASCII();
  }
  if(CCTK_IsThornActive("IOBasic"))
  {
    CCTKi_BindingsSchedule_IOBasic();
  }
  if(CCTK_IsThornActive("IOUtil"))
  {
    CCTKi_BindingsSchedule_IOUtil();
  }
  if(CCTK_IsThornActive("PUGH"))
  {
    CCTKi_BindingsSchedule_PUGH();
  }
  if(CCTK_IsThornActive("PUGHReduce"))
  {
    CCTKi_BindingsSchedule_PUGHReduce();
  }
  if(CCTK_IsThornActive("PUGHSlab"))
  {
    CCTKi_BindingsSchedule_PUGHSlab();
  }
  if(CCTK_IsThornActive("Time"))
  {
    CCTKi_BindingsSchedule_Time();
  }
  return 0;
}
