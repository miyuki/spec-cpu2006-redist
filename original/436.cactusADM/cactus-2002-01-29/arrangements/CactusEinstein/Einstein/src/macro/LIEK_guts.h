/*@@
  @header   LIEK_guts.h
  @date     Jul 2000
  @author   Gabrielle Allen + Miguel Alcubierre
  @desc

  Macro to calculate the Lie derivative of the lower 
  extrinsic curvature along the shift vector.   Notice
  that the advection term can be calculated in several
  different ways.

  IMPORTANT:  THE C VERSION ONLY HAS CENTERED DIFFERENCES!

  @enddesc
@@*/

#ifndef LIEK_GUTS
#define LIEK_GUTS

#include "CactusEinstein/Einstein/src/macro/DB_guts.h"
#include "CactusEinstein/Einstein/src/macro/DK_guts.h"

#ifdef FCODE

c     Advection.

      LIEK_LKXX = 0.0D0
      LIEK_LKYY = 0.0D0
      LIEK_LKZZ = 0.0D0
      LIEK_LKXY = 0.0D0
      LIEK_LKXZ = 0.0D0
      LIEK_LKYZ = 0.0D0

      if (CCTK_Equals(advection,'center').eq.1) then

         LIEK_LKXX = DXDK_DXDKXX*LIEK_BX + DYDK_DYDKXX*LIEK_BY
     &      + DZDK_DZDKXX*LIEK_BZ

         LIEK_LKYY = DXDK_DXDKYY*LIEK_BX + DYDK_DYDKYY*LIEK_BY
     &      + DZDK_DZDKYY*LIEK_BZ

         LIEK_LKZZ = DXDK_DXDKZZ*LIEK_BX + DYDK_DYDKZZ*LIEK_BY
     &      + DZDK_DZDKZZ*LIEK_BZ

         LIEK_LKXY = DXDK_DXDKXY*LIEK_BX + DYDK_DYDKXY*LIEK_BY
     &      + DZDK_DZDKXY*LIEK_BZ

         LIEK_LKXZ = DXDK_DXDKXZ*LIEK_BX + DYDK_DYDKXZ*LIEK_BY
     &      + DZDK_DZDKXZ*LIEK_BZ

         LIEK_LKYZ = DXDK_DXDKYZ*LIEK_BX + DYDK_DYDKYZ*LIEK_BY
     &      + DZDK_DZDKYZ*LIEK_BZ

      else if ((CCTK_Equals(advection,'upwind1').eq.1).or.
     &   (i.eq.2).or.(i.eq.cctk_lsh(1)-1).or.
     &   (j.eq.2).or.(j.eq.cctk_lsh(2)-1).or.
     &   (k.eq.2).or.(k.eq.cctk_lsh(3)-1)) then

         if (LIEK_BX.gt.0.0D0) then

            LIEK_LKXX = LIEK_LKXX + LIEK_BX
     &         *(kxx(i+1,j,k) - kxx(i,j,k))/dx

            LIEK_LKYY = LIEK_LKYY + LIEK_BX
     &         *(kyy(i+1,j,k) - kyy(i,j,k))/dx

            LIEK_LKZZ = LIEK_LKZZ + LIEK_BX
     &         *(kzz(i+1,j,k) - kzz(i,j,k))/dx

            LIEK_LKXY = LIEK_LKXY + LIEK_BX
     &         *(kxy(i+1,j,k) - kxy(i,j,k))/dx

            LIEK_LKXZ = LIEK_LKXZ + LIEK_BX
     &         *(kxz(i+1,j,k) - kxz(i,j,k))/dx

            LIEK_LKYZ = LIEK_LKYZ + LIEK_BX
     &         *(kyz(i+1,j,k) - kyz(i,j,k))/dx

         else

            LIEK_LKXX = LIEK_LKXX + LIEK_BX
     &         *(kxx(i,j,k) - kxx(i-1,j,k))/dx

            LIEK_LKYY = LIEK_LKYY + LIEK_BX
     &         *(kyy(i,j,k) - kyy(i-1,j,k))/dx

            LIEK_LKZZ = LIEK_LKZZ + LIEK_BX
     &         *(kzz(i,j,k) - kzz(i-1,j,k))/dx

            LIEK_LKXY = LIEK_LKXY + LIEK_BX
     &         *(kxy(i,j,k) - kxy(i-1,j,k))/dx

            LIEK_LKXZ = LIEK_LKXZ + LIEK_BX
     &         *(kxz(i,j,k) - kxz(i-1,j,k))/dx

            LIEK_LKYZ = LIEK_LKYZ + LIEK_BX
     &         *(kyz(i,j,k) - kyz(i-1,j,k))/dx

         end if

         if (LIEK_BY.gt.0.0D0) then

            LIEK_LKXX = LIEK_LKXX + LIEK_BY
     &         *(kxx(i,j+1,k) - kxx(i,j,k))/dy

            LIEK_LKYY = LIEK_LKYY + LIEK_BY
     &         *(kyy(i,j+1,k) - kyy(i,j,k))/dy

            LIEK_LKZZ = LIEK_LKZZ + LIEK_BY
     &         *(kzz(i,j+1,k) - kzz(i,j,k))/dy

            LIEK_LKXY = LIEK_LKXY + LIEK_BY
     &         *(kxy(i,j+1,k) - kxy(i,j,k))/dy

            LIEK_LKXZ = LIEK_LKXZ + LIEK_BY
     &         *(kxz(i,j+1,k) - kxz(i,j,k))/dy

            LIEK_LKYZ = LIEK_LKYZ + LIEK_BY
     &         *(kyz(i,j+1,k) - kyz(i,j,k))/dy

         else

            LIEK_LKXX = LIEK_LKXX + LIEK_BY
     &         *(kxx(i,j,k) - kxx(i,j-1,k))/dy

            LIEK_LKYY = LIEK_LKYY + LIEK_BY
     &         *(kyy(i,j,k) - kyy(i,j-1,k))/dy

            LIEK_LKZZ = LIEK_LKZZ + LIEK_BY
     &         *(kzz(i,j,k) - kzz(i,j-1,k))/dy

            LIEK_LKXY = LIEK_LKXY + LIEK_BY
     &         *(kxy(i,j,k) - kxy(i,j-1,k))/dy

            LIEK_LKXZ = LIEK_LKXZ + LIEK_BY
     &         *(kxz(i,j,k) - kxz(i,j-1,k))/dy

            LIEK_LKYZ = LIEK_LKYZ + LIEK_BY
     &         *(kyz(i,j,k) - kyz(i,j-1,k))/dy

         end if

         if (LIEK_BZ.gt.0.0D0) then

            LIEK_LKXX = LIEK_LKXX + LIEK_BZ
     &         *(kxx(i,j,k+1) - kxx(i,j,k))/dz

            LIEK_LKYY = LIEK_LKYY + LIEK_BZ
     &         *(kyy(i,j,k+1) - kyy(i,j,k))/dz

            LIEK_LKZZ = LIEK_LKZZ + LIEK_BZ
     &         *(kzz(i,j,k+1) - kzz(i,j,k))/dz

            LIEK_LKXY = LIEK_LKXY + LIEK_BZ
     &         *(kxy(i,j,k+1) - kxy(i,j,k))/dz

            LIEK_LKXZ = LIEK_LKXZ + LIEK_BZ
     &         *(kxz(i,j,k+1) - kxz(i,j,k))/dz

            LIEK_LKYZ = LIEK_LKYZ + LIEK_BZ
     &         *(kyz(i,j,k+1) - kyz(i,j,k))/dz

         else

            LIEK_LKXX = LIEK_LKXX + LIEK_BZ
     &         *(kxx(i,j,k) - kxx(i,j,k-1))/dz

            LIEK_LKYY = LIEK_LKYY + LIEK_BZ
     &         *(kyy(i,j,k) - kyy(i,j,k-1))/dz

            LIEK_LKZZ = LIEK_LKZZ + LIEK_BZ
     &         *(kzz(i,j,k) - kzz(i,j,k-1))/dz

            LIEK_LKXY = LIEK_LKXY + LIEK_BZ
     &         *(kxy(i,j,k) - kxy(i,j,k-1))/dz

            LIEK_LKXZ = LIEK_LKXZ + LIEK_BZ
     &         *(kxz(i,j,k) - kxz(i,j,k-1))/dz

            LIEK_LKYZ = LIEK_LKYZ + LIEK_BZ
     &         *(kyz(i,j,k) - kyz(i,j,k-1))/dz

         end if

      else if (CCTK_Equals(advection,'upwind2').eq.1) then

         if (LIEK_BX.gt.0.0D0) then

            LIEK_LKXX = LIEK_LKXX - 0.5D0*LIEK_BX/dx
     &         *(3.0D0*kxx(i,j,k) - 4.0D0*kxx(i+1,j,k) + kxx(i+2,j,k))

            LIEK_LKYY = LIEK_LKYY - 0.5D0*LIEK_BX/dx
     &         *(3.0D0*kyy(i,j,k) - 4.0D0*kyy(i+1,j,k) + kyy(i+2,j,k))

            LIEK_LKZZ = LIEK_LKZZ - 0.5D0*LIEK_BX/dx
     &         *(3.0D0*kzz(i,j,k) - 4.0D0*kzz(i+1,j,k) + kzz(i+2,j,k))

            LIEK_LKXY = LIEK_LKXY - 0.5D0*LIEK_BX/dx
     &         *(3.0D0*kxy(i,j,k) - 4.0D0*kxy(i+1,j,k) + kxy(i+2,j,k))

            LIEK_LKXZ = LIEK_LKXZ - 0.5D0*LIEK_BX/dx
     &         *(3.0D0*kxz(i,j,k) - 4.0D0*kxz(i+1,j,k) + kxz(i+2,j,k))

            LIEK_LKYZ = LIEK_LKYZ - 0.5D0*LIEK_BX/dx
     &         *(3.0D0*kyz(i,j,k) - 4.0D0*kyz(i+1,j,k) + kyz(i+2,j,k))

         else

            LIEK_LKXX = LIEK_LKXX + 0.5D0*LIEK_BX/dx
     &         *(3.0D0*kxx(i,j,k) - 4.0D0*kxx(i-1,j,k) + kxx(i-2,j,k))

            LIEK_LKYY = LIEK_LKYY + 0.5D0*LIEK_BX/dx
     &         *(3.0D0*kyy(i,j,k) - 4.0D0*kyy(i-1,j,k) + kyy(i-2,j,k))

            LIEK_LKZZ = LIEK_LKZZ + 0.5D0*LIEK_BX/dx
     &         *(3.0D0*kzz(i,j,k) - 4.0D0*kzz(i-1,j,k) + kzz(i-2,j,k))

            LIEK_LKXY = LIEK_LKXY + 0.5D0*LIEK_BX/dx
     &         *(3.0D0*kxy(i,j,k) - 4.0D0*kxy(i-1,j,k) + kxy(i-2,j,k))

            LIEK_LKXZ = LIEK_LKXZ + 0.5D0*LIEK_BX/dx
     &         *(3.0D0*kxz(i,j,k) - 4.0D0*kxz(i-1,j,k) + kxz(i-2,j,k))

            LIEK_LKYZ = LIEK_LKYZ + 0.5D0*LIEK_BX/dx
     &         *(3.0D0*kyz(i,j,k) - 4.0D0*kyz(i-1,j,k) + kyz(i-2,j,k))

         end if

         if (LIEK_BY.gt.0.0D0) then

            LIEK_LKXX = LIEK_LKXX - 0.5D0*LIEK_BY/dy
     &         *(3.0D0*kxx(i,j,k) - 4.0D0*kxx(i,j+1,k) + kxx(i,j+2,k))

            LIEK_LKYY = LIEK_LKYY - 0.5D0*LIEK_BY/dy
     &         *(3.0D0*kyy(i,j,k) - 4.0D0*kyy(i,j+1,k) + kyy(i,j+2,k))

            LIEK_LKZZ = LIEK_LKZZ - 0.5D0*LIEK_BY/dy
     &         *(3.0D0*kzz(i,j,k) - 4.0D0*kzz(i,j+1,k) + kzz(i,j+2,k))

            LIEK_LKXY = LIEK_LKXY - 0.5D0*LIEK_BY/dy
     &         *(3.0D0*kxy(i,j,k) - 4.0D0*kxy(i,j+1,k) + kxy(i,j+2,k))

            LIEK_LKXZ = LIEK_LKXZ - 0.5D0*LIEK_BY/dy
     &         *(3.0D0*kxz(i,j,k) - 4.0D0*kxz(i,j+1,k) + kxz(i,j+2,k))

            LIEK_LKYZ = LIEK_LKYZ - 0.5D0*LIEK_BY/dy
     &         *(3.0D0*kyz(i,j,k) - 4.0D0*kyz(i,j+1,k) + kyz(i,j+2,k))

         else

            LIEK_LKXX = LIEK_LKXX + 0.5D0*LIEK_BY/dy
     &         *(3.0D0*kxx(i,j,k) - 4.0D0*kxx(i,j-1,k) + kxx(i,j-2,k))

            LIEK_LKYY = LIEK_LKYY + 0.5D0*LIEK_BY/dy
     &         *(3.0D0*kyy(i,j,k) - 4.0D0*kyy(i,j-1,k) + kyy(i,j-2,k))

            LIEK_LKZZ = LIEK_LKZZ + 0.5D0*LIEK_BY/dy
     &         *(3.0D0*kzz(i,j,k) - 4.0D0*kzz(i,j-1,k) + kzz(i,j-2,k))

            LIEK_LKXY = LIEK_LKXY + 0.5D0*LIEK_BY/dy
     &         *(3.0D0*kxy(i,j,k) - 4.0D0*kxy(i,j-1,k) + kxy(i,j-2,k))

            LIEK_LKXZ = LIEK_LKXZ + 0.5D0*LIEK_BY/dy
     &         *(3.0D0*kxz(i,j,k) - 4.0D0*kxz(i,j-1,k) + kxz(i,j-2,k))

            LIEK_LKYZ = LIEK_LKYZ + 0.5D0*LIEK_BY/dy
     &         *(3.0D0*kyz(i,j,k) - 4.0D0*kyz(i,j-1,k) + kyz(i,j-2,k))

         end if

         if (LIEK_BZ.gt.0.0D0) then

            LIEK_LKXX = LIEK_LKXX - 0.5D0*LIEK_BZ/dz
     &         *(3.0D0*kxx(i,j,k) - 4.0D0*kxx(i,j,k+1) + kxx(i,j,k+2))

            LIEK_LKYY = LIEK_LKYY - 0.5D0*LIEK_BZ/dz
     &         *(3.0D0*kyy(i,j,k) - 4.0D0*kyy(i,j,k+1) + kyy(i,j,k+2))

            LIEK_LKZZ = LIEK_LKZZ - 0.5D0*LIEK_BZ/dz
     &         *(3.0D0*kzz(i,j,k) - 4.0D0*kzz(i,j,k+1) + kzz(i,j,k+2))

            LIEK_LKXY = LIEK_LKXY - 0.5D0*LIEK_BZ/dz
     &         *(3.0D0*kxy(i,j,k) - 4.0D0*kxy(i,j,k+1) + kxy(i,j,k+2))

            LIEK_LKXZ = LIEK_LKXZ - 0.5D0*LIEK_BZ/dz
     &         *(3.0D0*kxz(i,j,k) - 4.0D0*kxz(i,j,k+1) + kxz(i,j,k+2))

            LIEK_LKYZ = LIEK_LKYZ - 0.5D0*LIEK_BZ/dz
     &         *(3.0D0*kyz(i,j,k) - 4.0D0*kyz(i,j,k+1) + kyz(i,j,k+2))

         else

            LIEK_LKXX = LIEK_LKXX + 0.5D0*LIEK_BZ/dz
     &         *(3.0D0*kxx(i,j,k) - 4.0D0*kxx(i,j,k-1) + kxx(i,j,k-2))

            LIEK_LKYY = LIEK_LKYY + 0.5D0*LIEK_BZ/dz
     &         *(3.0D0*kyy(i,j,k) - 4.0D0*kyy(i,j,k-1) + kyy(i,j,k-2))

            LIEK_LKZZ = LIEK_LKZZ + 0.5D0*LIEK_BZ/dz
     &         *(3.0D0*kzz(i,j,k) - 4.0D0*kzz(i,j,k-1) + kzz(i,j,k-2))

            LIEK_LKXY = LIEK_LKXY + 0.5D0*LIEK_BZ/dz
     &         *(3.0D0*kxy(i,j,k) - 4.0D0*kxy(i,j,k-1) + kxy(i,j,k-2))

            LIEK_LKXZ = LIEK_LKXZ + 0.5D0*LIEK_BZ/dz
     &         *(3.0D0*kxz(i,j,k) - 4.0D0*kxz(i,j,k-1) + kxz(i,j,k-2))

            LIEK_LKYZ = LIEK_LKYZ + 0.5D0*LIEK_BZ/dz
     &         *(3.0D0*kyz(i,j,k) - 4.0D0*kyz(i,j,k-1) + kyz(i,j,k-2))

         end if

      end if

c     Extra terms in the Lie derivative.

      LIEK_LKXX = LIEK_LKXX + 2.0D0*(DXDB_DXDBX*LIEK_KXX
     &   + DXDB_DXDBY*LIEK_KXY + DXDB_DXDBZ*LIEK_KXZ)

      LIEK_LKYY = LIEK_LKYY + 2.0D0*(DYDB_DYDBX*LIEK_KXY
     &   + DYDB_DYDBY*LIEK_KYY + DYDB_DYDBZ*LIEK_KYZ)

      LIEK_LKZZ = LIEK_LKZZ + 2.0D0*(DZDB_DZDBX*LIEK_KXZ
     &   + DZDB_DZDBY*LIEK_KYZ + DZDB_DZDBZ*LIEK_KZZ)

      LIEK_LKXY = LIEK_LKXY + DYDB_DYDBX*LIEK_KXX + DXDB_DXDBY*LIEK_KYY
     &   + (DXDB_DXDBX + DYDB_DYDBY)*LIEK_KXY
     &   + DYDB_DYDBZ*LIEK_KXZ + DXDB_DXDBZ*LIEK_KYZ

      LIEK_LKXZ = LIEK_LKXZ + DZDB_DZDBX*LIEK_KXX + DXDB_DXDBZ*LIEK_KZZ
     &   + (DXDB_DXDBX + DZDB_DZDBZ)*LIEK_KXZ
     &   + DZDB_DZDBY*LIEK_KXY + DXDB_DXDBY*LIEK_KYZ

      LIEK_LKYZ = LIEK_LKYZ + DZDB_DZDBY*LIEK_KYY + DYDB_DYDBZ*LIEK_KZZ
     &   + (DYDB_DYDBY + DZDB_DZDBZ)*LIEK_KYZ
     &   + DZDB_DZDBX*LIEK_KXY + DYDB_DYDBX*LIEK_KXZ


#endif

#ifdef CCODE

/* Advection */

      LIEK_LKXX = DXDK_DXDKXX*LIEK_BX + DYDK_DYDKXX*LIEK_BY
         + DZDK_DZDKXX*LIEK_BZ;

      LIEK_LKYY = DXDK_DXDKYY*LIEK_BX + DYDK_DYDKYY*LIEK_BY
         + DZDK_DZDKYY*LIEK_BZ;

      LIEK_LKZZ = DXDK_DXDKZZ*LIEK_BX + DYDK_DYDKZZ*LIEK_BY
         + DZDK_DZDKZZ*LIEK_BZ;

      LIEK_LKXY = DXDK_DXDKXY*LIEK_BX + DYDK_DYDKXY*LIEK_BY
         + DZDK_DZDKXY*LIEK_BZ;

      LIEK_LKXZ = DXDK_DXDKXZ*LIEK_BX + DYDK_DYDKXZ*LIEK_BY
         + DZDK_DZDKXZ*LIEK_BZ;

      LIEK_LKYZ = DXDK_DXDKYZ*LIEK_BX + DYDK_DYDKYZ*LIEK_BY
         + DZDK_DZDKYZ*LIEK_BZ;

/* Extra terms in the Lie derivative */

      LIEK_LKXX = LIEK_LKXX + 2*(DXDB_DXDBX*LIEK_KXX
         + DXDB_DXDBY*LIEK_KXY + DXDB_DXDBZ*LIEK_KXZ);

      LIEK_LKYY = LIEK_LKYY + 2*(DYDB_DYDBX*LIEK_KXY
         + DYDB_DYDBY*LIEK_KYY + DYDB_DYDBZ*LIEK_KYZ);

      LIEK_LKZZ = LIEK_LKZZ + 2*(DZDB_DZDBX*LIEK_KXZ
         + DZDB_DZDBY*LIEK_KYZ + DZDB_DZDBZ*LIEK_KZZ);

      LIEK_LKXY = LIEK_LKXY + DYDB_DYDBX*LIEK_KXX + DXDB_DXDBY*LIEK_KYY
         + (DXDB_DXDBX + DYDB_DYDBY)*LIEK_KXY
         + DYDB_DYDBZ*LIEK_KXZ + DXDB_DXDBZ*LIEK_KYZ;

      LIEK_LKXZ = LIEK_LKXZ + DZDB_DZDBX*LIEK_KXX + DXDB_DXDBZ*LIEK_KZZ
         + (DXDB_DXDBX + DZDB_DZDBZ)*LIEK_KXZ
         + DZDB_DZDBY*LIEK_KXY + DXDB_DXDBY*LIEK_KYZ;

      LIEK_LKYZ = LIEK_LKYZ + DZDB_DZDBY*LIEK_KYY + DYDB_DYDBZ*LIEK_KZZ
         + (DYDB_DYDBY + DZDB_DZDBZ)*LIEK_KYZ
         + DZDB_DZDBX*LIEK_KXY + DYDB_DYDBX*LIEK_KXZ;

#endif

#endif
