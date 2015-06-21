/*@@
  @header   DG_declare.h
  @date     Jun 98
  @author   Gabrielle Allen
  @desc

  Declarations for macro to calculate all the first derivatives of the 
  physical metric with respect to x, y, z using the subsidary macros
  @seefile DXDG_guts.h @seefile DYDG_guts.h and @seefile DZDG_guts.h.

  Calls the macros @seefile DXDG_declare.h @seefile DYDG_declare.h
  and @seefile DZDG_declare.h.

  @enddesc
@@*/

#ifndef DG_DECLARE
#define DG_DECLARE

#include "CactusEinstein/Einstein/src/macro/DXDG_declare.h"
#include "CactusEinstein/Einstein/src/macro/DYDG_declare.h"
#include "CactusEinstein/Einstein/src/macro/DZDG_declare.h"

#endif
