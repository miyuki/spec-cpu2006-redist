/*@@
  @header    cctk_FunctionAliases.h
  @desc
  Prototypes for overloaded functions used by all thorn
  @enddesc 
  @@*/

#ifndef _CCTK_FUNCTIONALIASES_H_
#define _CCTK_FUNCTIONALIASES_H_

#ifdef CCODE
int CCTK_IsOverloaded(const char *function);

#endif

#ifdef THORN_IS_IOASCII
#include "IOASCII_Prototypes.h"
#endif

#ifdef THORN_IS_BenchADM
#include "BenchADM_Prototypes.h"
#endif

#ifdef THORN_IS_Cactus
#include "Cactus_Prototypes.h"
#endif

#ifdef THORN_IS_Boundary
#include "Boundary_Prototypes.h"
#endif

#ifdef THORN_IS_PUGH
#include "PUGH_Prototypes.h"
#endif

#ifdef THORN_IS_IOUtil
#include "IOUtil_Prototypes.h"
#endif

#ifdef THORN_IS_PUGHSlab
#include "PUGHSlab_Prototypes.h"
#endif

#ifdef THORN_IS_CartGrid3D
#include "CartGrid3D_Prototypes.h"
#endif

#ifdef THORN_IS_IOBasic
#include "IOBasic_Prototypes.h"
#endif

#ifdef THORN_IS_PUGHReduce
#include "PUGHReduce_Prototypes.h"
#endif

#ifdef THORN_IS_IDLinearWaves
#include "IDLinearWaves_Prototypes.h"
#endif

#ifdef THORN_IS_Time
#include "Time_Prototypes.h"
#endif

#ifdef THORN_IS_Einstein
#include "Einstein_Prototypes.h"
#endif

#endif

