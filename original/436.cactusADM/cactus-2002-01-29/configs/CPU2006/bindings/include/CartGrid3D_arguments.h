#ifdef FCODE
#define DECLARE_CARTGRID3D_PRIVATE_FARGUMENTS \


#define CARTGRID3D_PRIVATE_FARGUMENTS \



#endif /*FCODE*/


#ifdef CCODE
#define DECLARE_CARTGRID3D_PRIVATE_CARGUMENTS \


#define USE_CARTGRID3D_PRIVATE_CARGUMENTS \


#define DECLARE_CARTGRID3D_PRIVATE_C2F \


#define INITIALISE_CARTGRID3D_PRIVATE_C2F \


#define CARTGRID3D_PRIVATE_C2F_PROTO \



#define PASS_CARTGRID3D_PRIVATE_C2F(xGH) \



#endif /*CCODE*/


#ifdef FCODE
#define DECLARE_CARTGRID3D_PROTECTED_FARGUMENTS \


#define CARTGRID3D_PROTECTED_FARGUMENTS \



#endif /*FCODE*/


#ifdef CCODE
#define DECLARE_CARTGRID3D_PROTECTED_CARGUMENTS \


#define USE_CARTGRID3D_PROTECTED_CARGUMENTS \


#define DECLARE_CARTGRID3D_PROTECTED_C2F \


#define INITIALISE_CARTGRID3D_PROTECTED_C2F \


#define CARTGRID3D_PROTECTED_C2F_PROTO \



#define PASS_CARTGRID3D_PROTECTED_C2F(xGH) \



#endif /*CCODE*/


#ifdef FCODE
#define DECLARE_CARTGRID3D_PUBLIC_FARGUMENTS \
INTEGER Xcoordinates0&&\
INTEGER Xcoordinates1&&\
INTEGER Xcoordinates2&&\
CCTK_REAL coarse_dx&&\
CCTK_REAL coarse_dy&&\
CCTK_REAL coarse_dz&&\
CCTK_REAL r(Xcoordinates0,Xcoordinates1,Xcoordinates2)&&\
CCTK_REAL x(Xcoordinates0,Xcoordinates1,Xcoordinates2)&&\
CCTK_REAL y(Xcoordinates0,Xcoordinates1,Xcoordinates2)&&\
CCTK_REAL z(Xcoordinates0,Xcoordinates1,Xcoordinates2)&&\


#define CARTGRID3D_PUBLIC_FARGUMENTS \
Xcoordinates0,Xcoordinates1,Xcoordinates2,coarse_dx,coarse_dy,coarse_dz,r,x,y,z


#endif /*FCODE*/


#ifdef CCODE
#define DECLARE_CARTGRID3D_PUBLIC_CARGUMENTS \
CCTK_REAL *coarse_dx=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("grid::coarse_dx")][0]); \
CCTK_REAL *coarse_dy=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("grid::coarse_dy")][0]); \
CCTK_REAL *coarse_dz=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("grid::coarse_dz")][0]); \
CCTK_REAL *r=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("grid::r")][0]); \
CCTK_REAL *x=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("grid::x")][0]); \
CCTK_REAL *y=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("grid::y")][0]); \
CCTK_REAL *z=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("grid::z")][0]); \


#define USE_CARTGRID3D_PUBLIC_CARGUMENTS \
cctk_dummy_pointer = &coarse_dx; \
cctk_dummy_pointer = &coarse_dy; \
cctk_dummy_pointer = &coarse_dz; \
cctk_dummy_pointer = &r; \
cctk_dummy_pointer = &x; \
cctk_dummy_pointer = &y; \
cctk_dummy_pointer = &z; \


#define DECLARE_CARTGRID3D_PUBLIC_C2F \
static int CCTKARGNUM_coarse_dx = -1; \
static int CCTKGROUPNUM_gridspacings = -1; \
static int CCTKARGNUM_coarse_dy = -1; \
static int CCTKARGNUM_coarse_dz = -1; \
static int CCTKARGNUM_r = -1; \
static int CCTKGROUPNUM_coordinates = -1; \
static int CCTKARGNUM_x = -1; \
static int CCTKARGNUM_y = -1; \
static int CCTKARGNUM_z = -1; \


#define INITIALISE_CARTGRID3D_PUBLIC_C2F \
if(CCTKARGNUM_coarse_dx == -1) CCTKARGNUM_coarse_dx = CCTK_VarIndex("grid::coarse_dx"); \
if(CCTKGROUPNUM_gridspacings == -1) CCTKGROUPNUM_gridspacings = CCTK_GroupIndex("grid::gridspacings"); \
if(CCTKARGNUM_coarse_dy == -1) CCTKARGNUM_coarse_dy = CCTK_VarIndex("grid::coarse_dy"); \
if(CCTKARGNUM_coarse_dz == -1) CCTKARGNUM_coarse_dz = CCTK_VarIndex("grid::coarse_dz"); \
if(CCTKARGNUM_r == -1) CCTKARGNUM_r = CCTK_VarIndex("grid::r"); \
if(CCTKGROUPNUM_coordinates == -1) CCTKGROUPNUM_coordinates = CCTK_GroupIndex("grid::coordinates"); \
if(CCTKARGNUM_x == -1) CCTKARGNUM_x = CCTK_VarIndex("grid::x"); \
if(CCTKARGNUM_y == -1) CCTKARGNUM_y = CCTK_VarIndex("grid::y"); \
if(CCTKARGNUM_z == -1) CCTKARGNUM_z = CCTK_VarIndex("grid::z"); \


#define CARTGRID3D_PUBLIC_C2F_PROTO \
const int *,const int *,const int *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *


#define PASS_CARTGRID3D_PUBLIC_C2F(xGH) \
(const int *)(CCTKGROUPNUM_coordinates<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 0, "grid::coordinates"))),(const int *)(CCTKGROUPNUM_coordinates<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 1, "grid::coordinates"))),(const int *)(CCTKGROUPNUM_coordinates<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 2, "grid::coordinates"))),(CCTK_REAL *)(CCTKARGNUM_coarse_dx<0 ? NULL : (xGH)->data[CCTKARGNUM_coarse_dx][1-1]),(CCTK_REAL *)(CCTKARGNUM_coarse_dy<0 ? NULL : (xGH)->data[CCTKARGNUM_coarse_dy][1-1]),(CCTK_REAL *)(CCTKARGNUM_coarse_dz<0 ? NULL : (xGH)->data[CCTKARGNUM_coarse_dz][1-1]),(CCTK_REAL *)(CCTKARGNUM_r<0 ? NULL : (xGH)->data[CCTKARGNUM_r][1-1]),(CCTK_REAL *)(CCTKARGNUM_x<0 ? NULL : (xGH)->data[CCTKARGNUM_x][1-1]),(CCTK_REAL *)(CCTKARGNUM_y<0 ? NULL : (xGH)->data[CCTKARGNUM_y][1-1]),(CCTK_REAL *)(CCTKARGNUM_z<0 ? NULL : (xGH)->data[CCTKARGNUM_z][1-1])


#endif /*CCODE*/


#ifdef FCODE
#define CARTGRID3D_FARGUMENTS _CCTK_FARGUMENTS\
,CARTGRID3D_PUBLIC_FARGUMENTS\


#define DECLARE_CARTGRID3D_FARGUMENTS _DECLARE_CCTK_FARGUMENTS \
DECLARE_CARTGRID3D_PUBLIC_FARGUMENTS \


#endif /*FCODE*/


#ifdef CCODE
#define DECLARE_CARTGRID3D_CARGUMENTS _DECLARE_CCTK_CARGUMENTS \
DECLARE_CARTGRID3D_PUBLIC_CARGUMENTS \


#define USE_CARTGRID3D_CARGUMENTS _USE_CCTK_CARGUMENTS \
USE_CARTGRID3D_PUBLIC_CARGUMENTS \


#endif /*CCODE*/


#ifdef CCODE
#define CARTGRID3D_C2F_PROTO _CCTK_C2F_PROTO\
,CARTGRID3D_PUBLIC_C2F_PROTO\


#define PASS_CARTGRID3D_C2F(xGH) _PASS_CCTK_C2F(xGH)\
,PASS_CARTGRID3D_PUBLIC_C2F(xGH)\


#define DECLARE_CARTGRID3D_C2F _DECLARE_CCTK_C2F \
DECLARE_CARTGRID3D_PUBLIC_C2F \


#define INITIALISE_CARTGRID3D_C2F _INITIALISE_CCTK_C2F \
INITIALISE_CARTGRID3D_PUBLIC_C2F \


#define CARTGRID3D_CARGUMENTS cGH *cctkGH 


#endif /*CCODE*/


