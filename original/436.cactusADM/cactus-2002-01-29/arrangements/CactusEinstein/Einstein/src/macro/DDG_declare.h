/*@@
  @header   DDG_declare.h
  @date     Jun 98
  @author   Gabrielle Allen
  @desc

  Declarations for macro to calculate all the second (and first)
  derivatives of the physical metric with respect to x, y, z using the 
  subsidary macros @seefile D??DG_guts.h and D?DG_guts.h

  Calls the macros @seefile D??DG_declare.h which in turn call
  @seefile D?DG_declare.h

  @enddesc
@@*/

#ifndef DDG_DECLARE
#define DDG_DECLARE

#include "CactusEinstein/Einstein/src/macro/DXXDG_declare.h"
#include "CactusEinstein/Einstein/src/macro/DXYDG_declare.h"
#include "CactusEinstein/Einstein/src/macro/DXZDG_declare.h"
#include "CactusEinstein/Einstein/src/macro/DYYDG_declare.h"
#include "CactusEinstein/Einstein/src/macro/DYZDG_declare.h"
#include "CactusEinstein/Einstein/src/macro/DZZDG_declare.h"

#undef  DDG_DXXDGXX
#define DDG_DXXDGXX DXXDG_DXXDGXX
#undef  DDG_DXYDGXX
#define DDG_DXYDGXX DXYDG_DXYDGXX
#undef  DDG_DXZDGXX
#define DDG_DXZDGXX DXZDG_DXZDGXX
#undef  DDG_DYYDGXX
#define DDG_DYYDGXX DYYDG_DYYDGXX
#undef  DDG_DYZDGXX
#define DDG_DYZDGXX DYZDG_DYZDGXX
#undef  DDG_DZZDGXX
#define DDG_DZZDGXX DZZDG_DZZDGXX

#undef  DDG_DXXDGXY
#define DDG_DXXDGXY DXXDG_DXXDGXY
#undef  DDG_DXYDGXY
#define DDG_DXYDGXY DXYDG_DXYDGXY
#undef  DDG_DXZDGXY
#define DDG_DXZDGXY DXZDG_DXZDGXY
#undef  DDG_DYYDGXY
#define DDG_DYYDGXY DYYDG_DYYDGXY
#undef  DDG_DYZDGXY
#define DDG_DYZDGXY DYZDG_DYZDGXY
#undef  DDG_DZZDGXY
#define DDG_DZZDGXY DZZDG_DZZDGXY

#undef  DDG_DXXDGXZ
#define DDG_DXXDGXZ DXXDG_DXXDGXZ
#undef  DDG_DXYDGXZ
#define DDG_DXYDGXZ DXYDG_DXYDGXZ
#undef  DDG_DXZDGXZ
#define DDG_DXZDGXZ DXZDG_DXZDGXZ
#undef  DDG_DYYDGXZ
#define DDG_DYYDGXZ DYYDG_DYYDGXZ
#undef  DDG_DYZDGXZ
#define DDG_DYZDGXZ DYZDG_DYZDGXZ
#undef  DDG_DZZDGXZ
#define DDG_DZZDGXZ DZZDG_DZZDGXZ

#undef  DDG_DXXDGYY
#define DDG_DXXDGYY DXXDG_DXXDGYY
#undef  DDG_DXYDGYY
#define DDG_DXYDGYY DXYDG_DXYDGYY
#undef  DDG_DXZDGYY
#define DDG_DXZDGYY DXZDG_DXZDGYY
#undef  DDG_DYYDGYY
#define DDG_DYYDGYY DYYDG_DYYDGYY
#undef  DDG_DYZDGYY
#define DDG_DYZDGYY DYZDG_DYZDGYY
#undef  DDG_DZZDGYY
#define DDG_DZZDGYY DZZDG_DZZDGYY

#undef  DDG_DXXDGYZ
#define DDG_DXXDGYZ DXXDG_DXXDGYZ
#undef  DDG_DXYDGYZ
#define DDG_DXYDGYZ DXYDG_DXYDGYZ
#undef  DDG_DXZDGYZ
#define DDG_DXZDGYZ DXZDG_DXZDGYZ
#undef  DDG_DYYDGYZ
#define DDG_DYYDGYZ DYYDG_DYYDGYZ
#undef  DDG_DYZDGYZ
#define DDG_DYZDGYZ DYZDG_DYZDGYZ
#undef  DDG_DZZDGYZ
#define DDG_DZZDGYZ DZZDG_DZZDGYZ

#undef  DDG_DXXDGZZ
#define DDG_DXXDGZZ DXXDG_DXXDGZZ
#undef  DDG_DXYDGZZ
#define DDG_DXYDGZZ DXYDG_DXYDGZZ
#undef  DDG_DXZDGZZ
#define DDG_DXZDGZZ DXZDG_DXZDGZZ
#undef  DDG_DYYDGZZ
#define DDG_DYYDGZZ DYYDG_DYYDGZZ
#undef  DDG_DYZDGZZ
#define DDG_DYZDGZZ DYZDG_DYZDGZZ
#undef  DDG_DZZDGZZ
#define DDG_DZZDGZZ DZZDG_DZZDGZZ

#endif
