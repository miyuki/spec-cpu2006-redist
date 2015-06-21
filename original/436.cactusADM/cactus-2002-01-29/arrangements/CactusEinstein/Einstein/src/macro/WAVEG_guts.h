/*@@
  @header  WAVEG_guts.h
  @date     Jul 98
  @author   Gabrielle Allen
  @desc
  Macro to calculate wave operator acting on the metric 

  That is g^lm g_ij,lm

  @enddesc
@@*/

#ifndef WAVEG_GUTS
#define WAVEG_GUTS

#include "CactusEinstein/Einstein/src/macro/UPPERMET_guts.h"
#include "CactusEinstein/Einstein/src/macro/DDG_guts.h"

#ifdef FCODE 

      WAVEG_DDGXX =   UPPERMET_UXX*DDG_DXXDGXX +
     &            2D0*UPPERMET_UXY*DDG_DXYDGXX +
     &            2D0*UPPERMET_UXZ*DDG_DXZDGXX +
     &                UPPERMET_UYY*DDG_DYYDGXX +
     &            2D0*UPPERMET_UYZ*DDG_DYZDGXX +
     &                UPPERMET_UZZ*DDG_DZZDGXX

      WAVEG_DDGXY =   UPPERMET_UXX*DDG_DXXDGXY +
     &            2D0*UPPERMET_UXY*DDG_DXYDGXY +
     &            2D0*UPPERMET_UXZ*DDG_DXZDGXY +
     &                UPPERMET_UYY*DDG_DYYDGXY +
     &            2D0*UPPERMET_UYZ*DDG_DYZDGXY +
     &                UPPERMET_UZZ*DDG_DZZDGXY

      WAVEG_DDGXZ =   UPPERMET_UXX*DDG_DXXDGXZ +
     &            2D0*UPPERMET_UXY*DDG_DXYDGXZ +
     &            2D0*UPPERMET_UXZ*DDG_DXZDGXZ +
     &                UPPERMET_UYY*DDG_DYYDGXZ +
     &            2D0*UPPERMET_UYZ*DDG_DYZDGXZ +
     &                UPPERMET_UZZ*DDG_DZZDGXZ

      WAVEG_DDGYY =   UPPERMET_UXX*DDG_DXXDGYY +
     &            2D0*UPPERMET_UXY*DDG_DXYDGYY +
     &            2D0*UPPERMET_UXZ*DDG_DXZDGYY +
     &                UPPERMET_UYY*DDG_DYYDGYY +
     &            2D0*UPPERMET_UYZ*DDG_DYZDGYY +
     &                UPPERMET_UZZ*DDG_DZZDGYY

      WAVEG_DDGYZ =   UPPERMET_UXX*DDG_DXXDGYZ +
     &            2D0*UPPERMET_UXY*DDG_DXYDGYZ +
     &            2D0*UPPERMET_UXZ*DDG_DXZDGYZ +
     &                UPPERMET_UYY*DDG_DYYDGYZ +
     &            2D0*UPPERMET_UYZ*DDG_DYZDGYZ +
     &                UPPERMET_UZZ*DDG_DZZDGYZ

      WAVEG_DDGZZ =   UPPERMET_UXX*DDG_DXXDGZZ +
     &            2D0*UPPERMET_UXY*DDG_DXYDGZZ +
     &            2D0*UPPERMET_UXZ*DDG_DXZDGZZ +
     &                UPPERMET_UYY*DDG_DYYDGZZ +
     &            2D0*UPPERMET_UYZ*DDG_DYZDGZZ +
     &                UPPERMET_UZZ*DDG_DZZDGZZ

#endif

#ifdef CCODE

      WAVEG_DDGXX =   UPPERMET_UXX*DDG_DXXDGXX +
                    2*UPPERMET_UXY*DDG_DXYDGXX +
                    2*UPPERMET_UXZ*DDG_DXZDGXX +
                      UPPERMET_UYY*DDG_DYYDGXX +
                    2*UPPERMET_UYZ*DDG_DYZDGXX +
                      UPPERMET_UZZ*DDG_DYYDGXX;

      WAVEG_DDGXY =   UPPERMET_UXX*DDG_DXXDGXY +
                    2*UPPERMET_UXY*DDG_DXYDGXY +
                    2*UPPERMET_UXZ*DDG_DXZDGXY +
                      UPPERMET_UYY*DDG_DYYDGXY +
                    2*UPPERMET_UYZ*DDG_DYZDGXY +
                      UPPERMET_UZZ*DDG_DYYDGXY;

      WAVEG_DDGXZ =   UPPERMET_UXX*DDG_DXXDGXZ +
                    2*UPPERMET_UXY*DDG_DXYDGXZ +
                    2*UPPERMET_UXZ*DDG_DXZDGXZ +
                      UPPERMET_UYY*DDG_DYYDGXZ +
                    2*UPPERMET_UYZ*DDG_DYZDGXZ +
                      UPPERMET_UZZ*DDG_DYYDGXZ;

      WAVEG_DDGYY =   UPPERMET_UXX*DDG_DXXDGYY +
                    2*UPPERMET_UXY*DDG_DXYDGYY +
                    2*UPPERMET_UXZ*DDG_DXZDGYY +
                      UPPERMET_UYY*DDG_DYYDGYY +
                    2*UPPERMET_UYZ*DDG_DYZDGYY +
                      UPPERMET_UZZ*DDG_DYYDGYY;

      WAVEG_DDGYZ =   UPPERMET_UXX*DDG_DXXDGYZ +
                    2*UPPERMET_UXY*DDG_DXYDGYZ +
                    2*UPPERMET_UXZ*DDG_DXZDGYZ +
                      UPPERMET_UYY*DDG_DYYDGYZ +
                    2*UPPERMET_UYZ*DDG_DYZDGYZ +
                      UPPERMET_UZZ*DDG_DYYDGYZ;

      WAVEG_DDGZZ =   UPPERMET_UXX*DDG_DXXDGZZ +
                    2*UPPERMET_UXY*DDG_DXYDGZZ +
                    2*UPPERMET_UXZ*DDG_DXZDGZZ +
                      UPPERMET_UYY*DDG_DYYDGZZ +
                    2*UPPERMET_UYZ*DDG_DYZDGZZ +
                      UPPERMET_UZZ*DDG_DYYDGZZ;


#endif

#endif

