 /*@@
   @file      Boundary.h
   @date      Tue Sep 26 11:50:46 2000
   @author    Gerd Lanfermann
   @desc
              Prototypes for boundary routines
   @enddesc
 @@*/


#ifndef _BOUNDARY_H_
#define _BOUNDARY_H_


#ifdef __cplusplus
extern "C"
{
#endif


/* Scalar boundaries */
int BndScalarDirGI (const cGH *GH,
                    int stencil_size,
                    int dir,
                    CCTK_REAL var0,
                    int gi);
int BndScalarDirGN (const cGH *GH,
                    int stencil_size,
                    int dir,
                    CCTK_REAL var0,
                    const char *gname);
int BndScalarDirVI (const cGH *GH,
                    int stencil_size,
                    int dir,
                    CCTK_REAL var0,
                    int vi);
int BndScalarDirVN (const cGH *GH,
                    int stencil_size,
                    int dir,
                    CCTK_REAL var0,
                    const char *vname);

int BndScalarGI (const cGH *GH,
                 const int *stencil,
                 CCTK_REAL var0,
                 int gi);
int BndScalarGN (const cGH *GH,
                 const int *stencil,
                 CCTK_REAL var0,
                 const char *gname);
int BndScalarVI (const cGH *GH,
                 const int *stencil,
                 CCTK_REAL var0,
                 int vi);
int BndScalarVN (const cGH *GH,
                 const int *stencil,
                 CCTK_REAL var0,
                 const char *vname);


/* Copying boundaries  */
int BndCopyDirGI (const cGH *GH,
                  int stencil_size,
                  int dir,
                  int gi_to,
                  int gi_from);
int BndCopyDirGN (const cGH *GH,
                  int stencil_size,
                  int dir,
                  const char *gname_to,
                  const char *gname_from);
int BndCopyDirVI (const cGH *GH,
                  int stencil_size,
                  int dir,
                  int vi_to,
                  int vi_from);
int BndCopyDirVN (const cGH *GH,
                  int stencil_size,
                  int dir,
                  const char *vname_to,
                  const char *vname_from);

int BndCopyGI (const cGH *GH,
               const int *stencil,
               int gi_to,
               int gi_from);
int BndCopyGN (const cGH *GH,
               const int *stencil,
               const char *gname_to,
               const char *gname_from);
int BndCopyVI (const cGH *GH,
               const int *stencil,
               int vi_to,
               int vi_from);
int BndCopyVN (const cGH *GH,
               const int *stencil,
               const char *vname_to,
               const char *vname_from);


/* Radiative boundaries */
int BndRadiativeDirGI (const cGH *GH,
                       int stencil_size,
                       int dir,
                       CCTK_REAL var0,
                       CCTK_REAL v0,
                       int gi,
                       int gi_p);
int BndRadiativeDirGN (const cGH *GH,
                       int stencil_size,
                       int dir,
                       CCTK_REAL var0,
                       CCTK_REAL v0,
                       const char *gname_to,
                       const char *gname_from);
int BndRadiativeDirVI (const cGH *GH,
                       int stencil_size,
                       int dir,
                       CCTK_REAL var0,
                       CCTK_REAL v0,
                       int vi,
                       int vi_p);
int BndRadiativeDirVN (const cGH *GH,
                       int stencil_size,
                       int dir,
                       CCTK_REAL var0,
                       CCTK_REAL v0,
                       const char *vname_to,
                       const char *vname_from);

int BndRadiativeGI (const cGH *GH,
                    const int *stencil,
                    CCTK_REAL var0,
                    CCTK_REAL v0,
                    int gi,
                    int gi_p);
int BndRadiativeGN (const cGH *GH,
                    const int *stencil,
                    CCTK_REAL var0,
                    CCTK_REAL v0,
                    const char *gname_to,
                    const char *gname_from);
int BndRadiativeVI (const cGH *GH,
                    const int *stencil,
                    CCTK_REAL var0,
                    CCTK_REAL v0,
                    int vi,
                    int vi_p);
int BndRadiativeVN (const cGH *GH,
                    const int *stencil,
                    CCTK_REAL var0,
                    CCTK_REAL v0,
                    const char *vname_to,
                    const char *vname_from);


/* Robin boundaries     */
int BndRobinGI (const cGH *GH,
                const int *stencil,
                CCTK_REAL finf,
                int npow,
                int gi);
int BndRobinGN (const cGH *GH,
                const int *stencil,
                CCTK_REAL finf,
                int npow,
                const char *gname);
int BndRobinVI (const cGH *GH,
                const int *stencil,
                CCTK_REAL finf,
                int npow,
                int vi);
int BndRobinVN (const cGH *GH,
                const int *stencil,
                CCTK_REAL finf,
                int npow,
                const char *vname);


/* Flat boundaries      */
int BndFlatDirGI (const cGH *GH,
                  int stencil_size,
                  int dir,
                  int gi);
int BndFlatDirGN (const cGH *GH,
                  int stencil_size,
                  int dir,
                  const char *gname);
int BndFlatDirVI (const cGH *GH,
                  int stencil_size,
                  int dir,
                  int vi);
int BndFlatDirVN (const cGH *GH,
                  int stencil_size,
                  int dir,
                  const char *vname);

int BndFlatGI (const cGH *GH,
               const int *stencil,
               int gi);
int BndFlatGN (const cGH *GH,
               const int *stencil,
               const char *gname);
int BndFlatVI (const cGH *GH,
               const int *stencil,
               int vi);
int BndFlatVN (const cGH *GH,
               const int *stencil,
               const char *vname);


#ifdef __cplusplus
}
#endif


#endif  /* _BOUNDARY_H_ */
