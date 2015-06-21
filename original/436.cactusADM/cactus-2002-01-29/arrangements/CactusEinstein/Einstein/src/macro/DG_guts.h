/*@@
  @header   DG_guts.h
  @date     Jun 98
  @author   Gabrielle Allen
  @desc
  Macro to calculate all the first derivatives of the 
  physical metric with respect to x, y, z

  Calls the macros @seefile DXDG_guts.h @seefile DYDG_guts.h
  and @seefile DZDG_guts.h.

  The macro is defined in terms of standard variables in
  @seefile DXDG_declare.h ,@seefile DYDG_declare.h and
  @seefile DZDG_declare.h
  @enddesc
@@*/

#ifndef DG_GUTS
#define DG_GUTS

#include "CactusEinstein/Einstein/src/macro/DXDG_guts.h"
#include "CactusEinstein/Einstein/src/macro/DYDG_guts.h"
#include "CactusEinstein/Einstein/src/macro/DZDG_guts.h"

#endif
