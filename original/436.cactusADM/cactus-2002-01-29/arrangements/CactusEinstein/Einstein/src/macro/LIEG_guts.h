/*@@
  @header   LIEG_guts.h
  @date     July 2000
  @author   Gabrielle Allen, Miguel Alcubierre
  @desc

  Macro to calculate the Lie derivative of the lower 
  conformal metric.  Notice that the advection term
  can be calculated in several different ways.

  IMPORTANT:  THE C VERSION ONLY HAS CENTERED DIFFERENCES!

  @enddesc
@@*/

#ifndef LIEG_GUTS
#define LIEG_GUTS

#include "CactusEinstein/Einstein/src/macro/DB_guts.h"
#include "CactusEinstein/Einstein/src/macro/DG_guts.h"

#ifdef FCODE

c     Advection.

      LIEG_LGXX = 0.0D0
      LIEG_LGYY = 0.0D0
      LIEG_LGZZ = 0.0D0
      LIEG_LGXY = 0.0D0
      LIEG_LGXZ = 0.0D0
      LIEG_LGYZ = 0.0D0

      if (CCTK_Equals(advection,'center').eq.1) then

         LIEG_LGXX = DXDCG_DXDCGXX*LIEG_BX + DYDCG_DYDCGXX*LIEG_BY
     &      + DZDCG_DZDCGXX*LIEG_BZ

         LIEG_LGYY = DXDCG_DXDCGYY*LIEG_BX + DYDCG_DYDCGYY*LIEG_BY
     &      + DZDCG_DZDCGYY*LIEG_BZ

         LIEG_LGZZ = DXDCG_DXDCGZZ*LIEG_BX + DYDCG_DYDCGZZ*LIEG_BY
     &      + DZDCG_DZDCGZZ*LIEG_BZ

         LIEG_LGXY = DXDCG_DXDCGXY*LIEG_BX + DYDCG_DYDCGXY*LIEG_BY
     &      + DZDCG_DZDCGXY*LIEG_BZ

         LIEG_LGXZ = DXDCG_DXDCGXZ*LIEG_BX + DYDCG_DYDCGXZ*LIEG_BY
     &      + DZDCG_DZDCGXZ*LIEG_BZ

         LIEG_LGYZ = DXDCG_DXDCGYZ*LIEG_BX + DYDCG_DYDCGYZ*LIEG_BY
     &      + DZDCG_DZDCGYZ*LIEG_BZ

      else if ((CCTK_Equals(advection,'upwind1').eq.1).or.
     &   (i.eq.2).or.(i.eq.cctk_lsh(1)-1).or.
     &   (j.eq.2).or.(j.eq.cctk_lsh(2)-1).or.
     &   (k.eq.2).or.(k.eq.cctk_lsh(3)-1)) then

         if (LIEG_BX.gt.0.0D0) then

            LIEG_LGXX = LIEG_LGXX + LIEG_BX
     &         *(gxx(i+1,j,k) - gxx(i,j,k))/dx

            LIEG_LGYY = LIEG_LGYY + LIEG_BX
     &         *(gyy(i+1,j,k) - gyy(i,j,k))/dx

            LIEG_LGZZ = LIEG_LGZZ + LIEG_BX
     &         *(gzz(i+1,j,k) - gzz(i,j,k))/dx

            LIEG_LGXY = LIEG_LGXY + LIEG_BX
     &         *(gxy(i+1,j,k) - gxy(i,j,k))/dx

            LIEG_LGXZ = LIEG_LGXZ + LIEG_BX
     &         *(gxz(i+1,j,k) - gxz(i,j,k))/dx

            LIEG_LGYZ = LIEG_LGYZ + LIEG_BX
     &         *(gyz(i+1,j,k) - gyz(i,j,k))/dx

         else

            LIEG_LGXX = LIEG_LGXX + LIEG_BX
     &         *(gxx(i,j,k) - gxx(i-1,j,k))/dx

            LIEG_LGYY = LIEG_LGYY + LIEG_BX
     &         *(gyy(i,j,k) - gyy(i-1,j,k))/dx

            LIEG_LGZZ = LIEG_LGZZ + LIEG_BX
     &         *(gzz(i,j,k) - gzz(i-1,j,k))/dx

            LIEG_LGXY = LIEG_LGXY + LIEG_BX
     &         *(gxy(i,j,k) - gxy(i-1,j,k))/dx

            LIEG_LGXZ = LIEG_LGXZ + LIEG_BX
     &         *(gxz(i,j,k) - gxz(i-1,j,k))/dx

            LIEG_LGYZ = LIEG_LGYZ + LIEG_BX
     &         *(gyz(i,j,k) - gyz(i-1,j,k))/dx

         end if

         if (LIEG_BY.gt.0.0D0) then

            LIEG_LGXX = LIEG_LGXX + LIEG_BY
     &         *(gxx(i,j+1,k) - gxx(i,j,k))/dy

            LIEG_LGYY = LIEG_LGYY + LIEG_BY
     &         *(gyy(i,j+1,k) - gyy(i,j,k))/dy

            LIEG_LGZZ = LIEG_LGZZ + LIEG_BY
     &         *(gzz(i,j+1,k) - gzz(i,j,k))/dy

            LIEG_LGXY = LIEG_LGXY + LIEG_BY
     &         *(gxy(i,j+1,k) - gxy(i,j,k))/dy

            LIEG_LGXZ = LIEG_LGXZ + LIEG_BY
     &         *(gxz(i,j+1,k) - gxz(i,j,k))/dy

            LIEG_LGYZ = LIEG_LGYZ + LIEG_BY
     &         *(gyz(i,j+1,k) - gyz(i,j,k))/dy

         else

            LIEG_LGXX = LIEG_LGXX + LIEG_BY
     &         *(gxx(i,j,k) - gxx(i,j-1,k))/dy

            LIEG_LGYY = LIEG_LGYY + LIEG_BY
     &         *(gyy(i,j,k) - gyy(i,j-1,k))/dy

            LIEG_LGZZ = LIEG_LGZZ + LIEG_BY
     &         *(gzz(i,j,k) - gzz(i,j-1,k))/dy

            LIEG_LGXY = LIEG_LGXY + LIEG_BY
     &         *(gxy(i,j,k) - gxy(i,j-1,k))/dy

            LIEG_LGXZ = LIEG_LGXZ + LIEG_BY
     &         *(gxz(i,j,k) - gxz(i,j-1,k))/dy

            LIEG_LGYZ = LIEG_LGYZ + LIEG_BY
     &         *(gyz(i,j,k) - gyz(i,j-1,k))/dy

         end if

         if (LIEG_BZ.gt.0.0D0) then

            LIEG_LGXX = LIEG_LGXX + LIEG_BZ
     &         *(gxx(i,j,k+1) - gxx(i,j,k))/dz

            LIEG_LGYY = LIEG_LGYY + LIEG_BZ
     &         *(gyy(i,j,k+1) - gyy(i,j,k))/dz

            LIEG_LGZZ = LIEG_LGZZ + LIEG_BZ
     &         *(gzz(i,j,k+1) - gzz(i,j,k))/dz

            LIEG_LGXY = LIEG_LGXY + LIEG_BZ
     &         *(gxy(i,j,k+1) - gxy(i,j,k))/dz

            LIEG_LGXZ = LIEG_LGXZ + LIEG_BZ
     &         *(gxz(i,j,k+1) - gxz(i,j,k))/dz

            LIEG_LGYZ = LIEG_LGYZ + LIEG_BZ
     &         *(gyz(i,j,k+1) - gyz(i,j,k))/dz

         else

            LIEG_LGXX = LIEG_LGXX + LIEG_BZ
     &         *(gxx(i,j,k) - gxx(i,j,k-1))/dz

            LIEG_LGYY = LIEG_LGYY + LIEG_BZ
     &         *(gyy(i,j,k) - gyy(i,j,k-1))/dz

            LIEG_LGZZ = LIEG_LGZZ + LIEG_BZ
     &         *(gzz(i,j,k) - gzz(i,j,k-1))/dz

            LIEG_LGXY = LIEG_LGXY + LIEG_BZ
     &         *(gxy(i,j,k) - gxy(i,j,k-1))/dz

            LIEG_LGXZ = LIEG_LGXZ + LIEG_BZ
     &         *(gxz(i,j,k) - gxz(i,j,k-1))/dz

            LIEG_LGYZ = LIEG_LGYZ + LIEG_BZ
     &         *(gyz(i,j,k) - gyz(i,j,k-1))/dz

         end if

      else if (CCTK_Equals(advection,'upwind2').eq.1) then

         if (LIEG_BX.gt.0.0D0) then

            LIEG_LGXX = LIEG_LGXX - 0.5D0*LIEG_BX/dx
     &         *(3.0D0*gxx(i,j,k) - 4.0D0*gxx(i+1,j,k) + gxx(i+2,j,k))

            LIEG_LGYY = LIEG_LGYY - 0.5D0*LIEG_BX/dx
     &         *(3.0D0*gyy(i,j,k) - 4.0D0*gyy(i+1,j,k) + gyy(i+2,j,k))

            LIEG_LGZZ = LIEG_LGZZ - 0.5D0*LIEG_BX/dx
     &         *(3.0D0*gzz(i,j,k) - 4.0D0*gzz(i+1,j,k) + gzz(i+2,j,k))

            LIEG_LGXY = LIEG_LGXY - 0.5D0*LIEG_BX/dx
     &         *(3.0D0*gxy(i,j,k) - 4.0D0*gxy(i+1,j,k) + gxy(i+2,j,k))

            LIEG_LGXZ = LIEG_LGXZ - 0.5D0*LIEG_BX/dx
     &         *(3.0D0*gxz(i,j,k) - 4.0D0*gxz(i+1,j,k) + gxz(i+2,j,k))

            LIEG_LGYZ = LIEG_LGYZ - 0.5D0*LIEG_BX/dx
     &         *(3.0D0*gyz(i,j,k) - 4.0D0*gyz(i+1,j,k) + gyz(i+2,j,k))

         else

            LIEG_LGXX = LIEG_LGXX + 0.5D0*LIEG_BX/dx
     &         *(3.0D0*gxx(i,j,k) - 4.0D0*gxx(i-1,j,k) + gxx(i-2,j,k))

            LIEG_LGYY = LIEG_LGYY + 0.5D0*LIEG_BX/dx
     &         *(3.0D0*gyy(i,j,k) - 4.0D0*gyy(i-1,j,k) + gyy(i-2,j,k))

            LIEG_LGZZ = LIEG_LGZZ + 0.5D0*LIEG_BX/dx
     &         *(3.0D0*gzz(i,j,k) - 4.0D0*gzz(i-1,j,k) + gzz(i-2,j,k))

            LIEG_LGXY = LIEG_LGXY + 0.5D0*LIEG_BX/dx
     &         *(3.0D0*gxy(i,j,k) - 4.0D0*gxy(i-1,j,k) + gxy(i-2,j,k))

            LIEG_LGXZ = LIEG_LGXZ + 0.5D0*LIEG_BX/dx
     &         *(3.0D0*gxz(i,j,k) - 4.0D0*gxz(i-1,j,k) + gxz(i-2,j,k))

            LIEG_LGYZ = LIEG_LGYZ + 0.5D0*LIEG_BX/dx
     &         *(3.0D0*gyz(i,j,k) - 4.0D0*gyz(i-1,j,k) + gyz(i-2,j,k))

         end if

         if (LIEG_BY.gt.0.0D0) then

            LIEG_LGXX = LIEG_LGXX - 0.5D0*LIEG_BY/dy
     &         *(3.0D0*gxx(i,j,k) - 4.0D0*gxx(i,j+1,k) + gxx(i,j+2,k))

            LIEG_LGYY = LIEG_LGYY - 0.5D0*LIEG_BY/dy
     &         *(3.0D0*gyy(i,j,k) - 4.0D0*gyy(i,j+1,k) + gyy(i,j+2,k))

            LIEG_LGZZ = LIEG_LGZZ - 0.5D0*LIEG_BY/dy
     &         *(3.0D0*gzz(i,j,k) - 4.0D0*gzz(i,j+1,k) + gzz(i,j+2,k))

            LIEG_LGXY = LIEG_LGXY - 0.5D0*LIEG_BY/dy
     &         *(3.0D0*gxy(i,j,k) - 4.0D0*gxy(i,j+1,k) + gxy(i,j+2,k))

            LIEG_LGXZ = LIEG_LGXZ - 0.5D0*LIEG_BY/dy
     &         *(3.0D0*gxz(i,j,k) - 4.0D0*gxz(i,j+1,k) + gxz(i,j+2,k))

            LIEG_LGYZ = LIEG_LGYZ - 0.5D0*LIEG_BY/dy
     &         *(3.0D0*gyz(i,j,k) - 4.0D0*gyz(i,j+1,k) + gyz(i,j+2,k))

         else

            LIEG_LGXX = LIEG_LGXX + 0.5D0*LIEG_BY/dy
     &         *(3.0D0*gxx(i,j,k) - 4.0D0*gxx(i,j-1,k) + gxx(i,j-2,k))

            LIEG_LGYY = LIEG_LGYY + 0.5D0*LIEG_BY/dy
     &         *(3.0D0*gyy(i,j,k) - 4.0D0*gyy(i,j-1,k) + gyy(i,j-2,k))

            LIEG_LGZZ = LIEG_LGZZ + 0.5D0*LIEG_BY/dy
     &         *(3.0D0*gzz(i,j,k) - 4.0D0*gzz(i,j-1,k) + gzz(i,j-2,k))

            LIEG_LGXY = LIEG_LGXY + 0.5D0*LIEG_BY/dy
     &         *(3.0D0*gxy(i,j,k) - 4.0D0*gxy(i,j-1,k) + gxy(i,j-2,k))

            LIEG_LGXZ = LIEG_LGXZ + 0.5D0*LIEG_BY/dy
     &         *(3.0D0*gxz(i,j,k) - 4.0D0*gxz(i,j-1,k) + gxz(i,j-2,k))

            LIEG_LGYZ = LIEG_LGYZ + 0.5D0*LIEG_BY/dy
     &         *(3.0D0*gyz(i,j,k) - 4.0D0*gyz(i,j-1,k) + gyz(i,j-2,k))

         end if

         if (LIEG_BZ.gt.0.0D0) then

            LIEG_LGXX = LIEG_LGXX - 0.5D0*LIEG_BZ/dz
     &         *(3.0D0*gxx(i,j,k) - 4.0D0*gxx(i,j,k+1) + gxx(i,j,k+2))

            LIEG_LGYY = LIEG_LGYY - 0.5D0*LIEG_BZ/dz
     &         *(3.0D0*gyy(i,j,k) - 4.0D0*gyy(i,j,k+1) + gyy(i,j,k+2))

            LIEG_LGZZ = LIEG_LGZZ - 0.5D0*LIEG_BZ/dz
     &         *(3.0D0*gzz(i,j,k) - 4.0D0*gzz(i,j,k+1) + gzz(i,j,k+2))

            LIEG_LGXY = LIEG_LGXY - 0.5D0*LIEG_BZ/dz
     &         *(3.0D0*gxy(i,j,k) - 4.0D0*gxy(i,j,k+1) + gxy(i,j,k+2))

            LIEG_LGXZ = LIEG_LGXZ - 0.5D0*LIEG_BZ/dz
     &         *(3.0D0*gxz(i,j,k) - 4.0D0*gxz(i,j,k+1) + gxz(i,j,k+2))

            LIEG_LGYZ = LIEG_LGYZ - 0.5D0*LIEG_BZ/dz
     &         *(3.0D0*gyz(i,j,k) - 4.0D0*gyz(i,j,k+1) + gyz(i,j,k+2))

         else

            LIEG_LGXX = LIEG_LGXX + 0.5D0*LIEG_BZ/dz
     &         *(3.0D0*gxx(i,j,k) - 4.0D0*gxx(i,j,k-1) + gxx(i,j,k-2))

            LIEG_LGYY = LIEG_LGYY + 0.5D0*LIEG_BZ/dz
     &         *(3.0D0*gyy(i,j,k) - 4.0D0*gyy(i,j,k-1) + gyy(i,j,k-2))

            LIEG_LGZZ = LIEG_LGZZ + 0.5D0*LIEG_BZ/dz
     &         *(3.0D0*gzz(i,j,k) - 4.0D0*gzz(i,j,k-1) + gzz(i,j,k-2))

            LIEG_LGXY = LIEG_LGXY + 0.5D0*LIEG_BZ/dz
     &         *(3.0D0*gxy(i,j,k) - 4.0D0*gxy(i,j,k-1) + gxy(i,j,k-2))

            LIEG_LGXZ = LIEG_LGXZ + 0.5D0*LIEG_BZ/dz
     &         *(3.0D0*gxz(i,j,k) - 4.0D0*gxz(i,j,k-1) + gxz(i,j,k-2))

            LIEG_LGYZ = LIEG_LGYZ + 0.5D0*LIEG_BZ/dz
     &         *(3.0D0*gyz(i,j,k) - 4.0D0*gyz(i,j,k-1) + gyz(i,j,k-2))

         end if

      end if

c     Extra terms in the Lie derivative.

      LIEG_LGXX = LIEG_LGXX + 2.0D0*(DXDB_DXDBX*LIEG_GXX
     &   + DXDB_DXDBY*LIEG_GXY + DXDB_DXDBZ*LIEG_GXZ)

      LIEG_LGYY = LIEG_LGYY + 2.0D0*(DYDB_DYDBX*LIEG_GXY
     &   + DYDB_DYDBY*LIEG_GYY + DYDB_DYDBZ*LIEG_GYZ)

      LIEG_LGZZ = LIEG_LGZZ + 2.0D0*(DZDB_DZDBX*LIEG_GXZ
     &   + DZDB_DZDBY*LIEG_GYZ + DZDB_DZDBZ*LIEG_GZZ)

      LIEG_LGXY = LIEG_LGXY + DYDB_DYDBX*LIEG_GXX + DXDB_DXDBY*LIEG_GYY
     &   + (DXDB_DXDBX + DYDB_DYDBY)*LIEG_GXY
     &   + DYDB_DYDBZ*LIEG_GXZ + DXDB_DXDBZ*LIEG_GYZ

      LIEG_LGXZ = LIEG_LGXZ + DZDB_DZDBX*LIEG_GXX + DXDB_DXDBZ*LIEG_GZZ
     &   + (DXDB_DXDBX + DZDB_DZDBZ)*LIEG_GXZ
     &   + DZDB_DZDBY*LIEG_GXY + DXDB_DXDBY*LIEG_GYZ

      LIEG_LGYZ = LIEG_LGYZ + DZDB_DZDBY*LIEG_GYY + DYDB_DYDBZ*LIEG_GZZ
     &   + (DYDB_DYDBY + DZDB_DZDBZ)*LIEG_GYZ
     &   + DZDB_DZDBX*LIEG_GXY + DYDB_DYDBX*LIEG_GXZ

#endif

#ifdef CCODE

/* Advection */

      LIEG_LGXX = DXDCG_DXDCGXX*LIEG_BX + DYDCG_DYDCGXX*LIEG_BY
         + DZDCG_DZDCGXX*LIEG_BZ;

      LIEG_LGYY = DXDCG_DXDCGYY*LIEG_BX + DYDCG_DYDCGYY*LIEG_BY
         + DZDCG_DZDCGYY*LIEG_BZ;

      LIEG_LGZZ = DXDCG_DXDCGZZ*LIEG_BX + DYDCG_DYDCGZZ*LIEG_BY
         + DZDCG_DZDCGZZ*LIEG_BZ;

      LIEG_LGXY = DXDCG_DXDCGXY*LIEG_BX + DYDCG_DYDCGXY*LIEG_BY
         + DZDCG_DZDCGXY*LIEG_BZ;

      LIEG_LGXZ = DXDCG_DXDCGXZ*LIEG_BX + DYDCG_DYDCGXZ*LIEG_BY
         + DZDCG_DZDCGXZ*LIEG_BZ;

      LIEG_LGYZ = DXDCG_DXDCGYZ*LIEG_BX + DYDCG_DYDCGYZ*LIEG_BY
         + DZDCG_DZDCGYZ*LIEG_BZ;

/* Extra terms in the Lie derivative */

      LIEG_LGXX = LIEG_LGXX + 2*(DXDB_DXDBX*LIEG_GXX
         + DXDB_DXDBY*LIEG_GXY + DXDB_DXDBZ*LIEG_GXZ);

      LIEG_LGYY = LIEG_LGYY + 2*(DYDB_DYDBX*LIEG_GXY
         + DYDB_DYDBY*LIEG_GYY + DYDB_DYDBZ*LIEG_GYZ);

      LIEG_LGZZ = LIEG_LGZZ + 2*(DZDB_DZDBX*LIEG_GXZ
         + DZDB_DZDBY*LIEG_GYZ + DZDB_DZDBZ*LIEG_GZZ);

      LIEG_LGXY = LIEG_LGXY + DYDB_DYDBX*LIEG_GXX + DXDB_DXDBY*LIEG_GYY
         + (DXDB_DXDBX + DYDB_DYDBY)*LIEG_GXY
         + DYDB_DYDBZ*LIEG_GXZ + DXDB_DXDBZ*LIEG_GYZ;

      LIEG_LGXZ = LIEG_LGXZ + DZDB_DZDBX*LIEG_GXX + DXDB_DXDBZ*LIEG_GZZ
         + (DXDB_DXDBX + DZDB_DZDBZ)*LIEG_GXZ
         + DZDB_DZDBY*LIEG_GXY + DXDB_DXDBY*LIEG_GYZ;

      LIEG_LGYZ = LIEG_LGYZ + DZDB_DZDBY*LIEG_GYY + DYDB_DYDBZ*LIEG_GZZ
         + (DYDB_DYDBY + DZDB_DZDBZ)*LIEG_GYZ
         + DZDB_DZDBX*LIEG_GXY + DYDB_DYDBX*LIEG_GXZ;

#endif

#endif
