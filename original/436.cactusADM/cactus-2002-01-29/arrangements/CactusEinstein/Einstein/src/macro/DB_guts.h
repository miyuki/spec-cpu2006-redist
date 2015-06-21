/*@@
  @header   DB_guts.h
  @date     Jul 98
  @author   Gabrielle Allen
  @desc
  Macro to calculate all the first derivatives of the 
  shift vector with respect to x, y, z

  Calls the macros @seefile DXDB_guts.h @seefile DYDB_guts.h
  and @seefile DZDB_guts.h.

  The macro is defined in terms of standard variables in
  @seefile DXDB_declare.h ,@seefile DYDB_declare.h and
  @seefile DZDB_declare.h
  @enddesc
@@*/

#ifndef DB_GUTS
#define DB_GUTS

#include "CactusEinstein/Einstein/src/macro/DXDB_guts.h"
#include "CactusEinstein/Einstein/src/macro/DYDB_guts.h"
#include "CactusEinstein/Einstein/src/macro/DZDB_guts.h"

#endif
