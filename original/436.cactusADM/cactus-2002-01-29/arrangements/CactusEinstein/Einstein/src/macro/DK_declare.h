/*@@
  @header   DG_declare.h
  @date     Jun 98
  @author   Gabrielle Allen
  @desc

  Declarations for macro to calculate all the first derivatives of the 
  extrinsic curvature with respect to x, y, z using the subsidary macros
  @seefile DXDK_guts.h @seefile DYDK_guts.h and @seefile DZDK_guts.h.

  Calls the macros @seefile DXDK_declare.h @seefile DYDK_declare.h
  and @seefile DZDK_declare.h.

  @enddesc
@@*/

#ifndef DK_DECLARE
#define DK_DECLARE

#include "CactusEinstein/Einstein/src/macro/DXDK_declare.h"
#include "CactusEinstein/Einstein/src/macro/DYDK_declare.h"
#include "CactusEinstein/Einstein/src/macro/DZDK_declare.h"

#endif
