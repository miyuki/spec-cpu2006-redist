 /*@@
   @header    cctk_Flesh.h
   @date      Thu Sep 24 10:18:52 1998
   @author    Tom Goodale
   @desc 
   Header file for flesh functions.
   @enddesc 
   @version $Header: /cactus/Cactus/src/include/cctk_Flesh.h,v 1.20 2001/11/05 14:58:51 tradke Exp $
 @@*/

#include "cGH.h"


#ifndef _CCTK_FLESH_H_
#define _CCTK_FLESH_H_

/*  Typedefs */

typedef struct
{
  char *parameter_file_name;

  /* Array of pointers to cactus grid hierarchies. */
  cGH **GH;
  unsigned int nGHs;

  /*  cTimer *timer[3];*/
} tFleshConfig;

#ifdef __cplusplus

#define CCTK_FILEVERSION(file) extern "C" const char *CCTKi_version_##file(void);  const char *CCTKi_version_##file(void){ return rcsid; }

#else

#define CCTK_FILEVERSION(file) const char *CCTKi_version_##file(void); const char *CCTKi_version_##file(void) { return rcsid; }

#endif

/* Function prototypes */

#ifdef __cplusplus
extern "C" 
{
#endif

int CCTK_Traverse(cGH *GH, const char *where);

  /*int CCTKi_SetParameter(const char *parameter, const char *value);*/

int CCTKi_ProcessCommandLine(int *argc, char ***argv, tFleshConfig *ConfigData);

int CCTKi_InitialiseDataStructures(tFleshConfig *ConfigData);

int CCTKi_ProcessParameterDatabase(tFleshConfig *ConfigData);

int CCTKi_CallStartupFunctions(tFleshConfig *ConfigData);

int CCTKi_AddGH(tFleshConfig *config, unsigned int convergence_level, cGH *GH);

int CCTKi_InitialiseCactus(int *argc, char ***argv, tFleshConfig *ConfigData);

int CCTKi_ShutdownCactus(tFleshConfig *ConfigData);

int CCTKi_DummyExit(cGH *GH, int retval);

#ifdef __cplusplus
}
#endif

#endif
