/*@@
  @header   DB_declare.h
  @date     Jul 98
  @author   Gabrielle Allen
  @desc

  Declarations for macro to calculate all the first derivatives of the 
  shift vector with respect to x, y, z using the subsidary macros
  @seefile DXDB_guts.h @seefile DYDB_guts.h and @seefile DZDB_guts.h.

  Calls the macros @seefile DXDB_declare.h @seefile DYDB_declare.h
  and @seefile DZDB_declare.h.

  @enddesc
@@*/

#ifndef DB_DECLARE
#define DB_DECLARE

#include "CactusEinstein/Einstein/src/macro/DXDB_declare.h"
#include "CactusEinstein/Einstein/src/macro/DYDB_declare.h"
#include "CactusEinstein/Einstein/src/macro/DZDB_declare.h"

#endif
