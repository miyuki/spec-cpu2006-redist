#ifdef SPEC_CPU
# define THORN_IS_Cactus
#endif /* SPEC_CPU */
 /*@@
   @file      Dummies.c
   @date      Tue Feb  2 18:56:38 1999
   @author    Tom Goodale
   @desc 
   A file to contain some dummy functions until the real ones are written. 
   @enddesc 
 @@*/

#include "Dummies.h"
#include "cctk_Flesh.h"
#include "cctk_WarnLevel.h"

static const char *rcsid = "$Header: /cactus/Cactus/src/main/Dummies.c,v 1.8 2001/07/04 13:18:00 tradke Exp $";

CCTK_FILEVERSION(main_Dummies_c)

int CCTKi_DummyStorageOn(void *GH, int group)
{
  GH = GH;
  group = group;
  CCTK_VWarn (0, __LINE__, __FILE__, "Cactus",
              "No driver thorn activated to provide storage for variables");
  return 0;
}

int CCTKi_DummyStorageOff(void *GH, int group)
{
  GH = GH;
  group = group;
  CCTK_VWarn (0, __LINE__, __FILE__, "Cactus",
              "No driver thorn activated to provide storage for variables");
  return 0;
}

int CCTKi_DummyCommunicationOn(void *GH, int group)
{
  GH = GH;
  group = group;
  CCTK_VWarn (0, __LINE__, __FILE__, "Cactus",
              "No driver thorn activated to provide communication for variables");
  return 0;
}

int CCTKi_DummyCommunicationOff(void *GH, int group)
{
  GH = GH;
  group = group;
  CCTK_VWarn (0, __LINE__, __FILE__, "Cactus",
              "No driver thorn activated to provide communication for variables");
  return 0;
}

int CCTKi_DummyTriggerable(int variable)
{
  variable = variable;
  return 0;
}

int CCTKi_DummyTriggerSaysGo(void *GH, int variable)
{
  GH = GH;
  variable = variable;
  return 1;
}

int CCTKi_DummyTriggerAction(void *GH, int group)
{
  GH = GH;
  group = group;
  return 0;
}
