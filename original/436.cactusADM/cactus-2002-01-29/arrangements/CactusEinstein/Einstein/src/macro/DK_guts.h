/*@@
  @header   DK_guts.h
  @date     Jul 98
  @author   Gabrielle Allen
  @desc
  Macro to calculate all the first derivatives of the 
  extrinsic curvature with respect to x, y, z

  Calls the macros @seefile DXDK_guts.h @seefile DYDK_guts.h
  and @seefile DZDK_guts.h.

  The macro is defined in terms of standard variables in
  @seefile DXDK_declare.h ,@seefile DYDK_declare.h and
  @seefile DZDK_declare.h
  @enddesc
@@*/

#ifndef DK_GUTS
#define DK_GUTS

#include "CactusEinstein/Einstein/src/macro/DXDK_guts.h"
#include "CactusEinstein/Einstein/src/macro/DYDK_guts.h"
#include "CactusEinstein/Einstein/src/macro/DZDK_guts.h"

#endif
