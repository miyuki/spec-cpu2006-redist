/*@@
  @header   DDG_guts.h
  @date     Jun 98
  @author   Gabrielle Allen
  @desc
  Macro to calculate all the second (and first) derivatives of the 
  physical metric with respect to x, y, z

  Calls the macros @seefile D??DG_guts.h and @seefile D?DG_guts.h

  The macro is defined in terms of standard variables in
  @seefile D??DG_declare.h ,@seefile D?DG_declare.h
  @enddesc
@@*/ 

#ifndef DDG_GUTS 
#define DDG_GUTS

#include "CactusEinstein/Einstein/src/macro/DXXDG_guts.h"
#include "CactusEinstein/Einstein/src/macro/DXYDG_guts.h"
#include "CactusEinstein/Einstein/src/macro/DXZDG_guts.h"
#include "CactusEinstein/Einstein/src/macro/DYYDG_guts.h"
#include "CactusEinstein/Einstein/src/macro/DYZDG_guts.h"
#include "CactusEinstein/Einstein/src/macro/DZZDG_guts.h"

#endif
