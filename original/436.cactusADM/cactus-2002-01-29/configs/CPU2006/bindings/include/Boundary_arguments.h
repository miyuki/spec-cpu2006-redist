#ifdef FCODE
#define DECLARE_BOUNDARY_PRIVATE_FARGUMENTS \


#define BOUNDARY_PRIVATE_FARGUMENTS \



#endif /*FCODE*/


#ifdef CCODE
#define DECLARE_BOUNDARY_PRIVATE_CARGUMENTS \


#define USE_BOUNDARY_PRIVATE_CARGUMENTS \


#define DECLARE_BOUNDARY_PRIVATE_C2F \


#define INITIALISE_BOUNDARY_PRIVATE_C2F \


#define BOUNDARY_PRIVATE_C2F_PROTO \



#define PASS_BOUNDARY_PRIVATE_C2F(xGH) \



#endif /*CCODE*/


#ifdef FCODE
#define DECLARE_BOUNDARY_PROTECTED_FARGUMENTS \


#define BOUNDARY_PROTECTED_FARGUMENTS \



#endif /*FCODE*/


#ifdef CCODE
#define DECLARE_BOUNDARY_PROTECTED_CARGUMENTS \


#define USE_BOUNDARY_PROTECTED_CARGUMENTS \


#define DECLARE_BOUNDARY_PROTECTED_C2F \


#define INITIALISE_BOUNDARY_PROTECTED_C2F \


#define BOUNDARY_PROTECTED_C2F_PROTO \



#define PASS_BOUNDARY_PROTECTED_C2F(xGH) \



#endif /*CCODE*/


#ifdef FCODE
#define DECLARE_BOUNDARY_PUBLIC_FARGUMENTS \
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


#define BOUNDARY_PUBLIC_FARGUMENTS \
Xcoordinates0,Xcoordinates1,Xcoordinates2,coarse_dx,coarse_dy,coarse_dz,r,x,y,z


#endif /*FCODE*/


#ifdef CCODE
#define DECLARE_BOUNDARY_PUBLIC_CARGUMENTS \
CCTK_REAL *coarse_dx=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("GRID::coarse_dx")][0]); \
CCTK_REAL *coarse_dy=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("GRID::coarse_dy")][0]); \
CCTK_REAL *coarse_dz=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("GRID::coarse_dz")][0]); \
CCTK_REAL *r=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("GRID::r")][0]); \
CCTK_REAL *x=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("GRID::x")][0]); \
CCTK_REAL *y=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("GRID::y")][0]); \
CCTK_REAL *z=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("GRID::z")][0]); \


#define USE_BOUNDARY_PUBLIC_CARGUMENTS \
cctk_dummy_pointer = &coarse_dx; \
cctk_dummy_pointer = &coarse_dy; \
cctk_dummy_pointer = &coarse_dz; \
cctk_dummy_pointer = &r; \
cctk_dummy_pointer = &x; \
cctk_dummy_pointer = &y; \
cctk_dummy_pointer = &z; \


#define DECLARE_BOUNDARY_PUBLIC_C2F \
static int CCTKARGNUM_coarse_dx = -1; \
static int CCTKGROUPNUM_gridspacings = -1; \
static int CCTKARGNUM_coarse_dy = -1; \
static int CCTKARGNUM_coarse_dz = -1; \
static int CCTKARGNUM_r = -1; \
static int CCTKGROUPNUM_coordinates = -1; \
static int CCTKARGNUM_x = -1; \
static int CCTKARGNUM_y = -1; \
static int CCTKARGNUM_z = -1; \


#define INITIALISE_BOUNDARY_PUBLIC_C2F \
if(CCTKARGNUM_coarse_dx == -1) CCTKARGNUM_coarse_dx = CCTK_VarIndex("GRID::coarse_dx"); \
if(CCTKGROUPNUM_gridspacings == -1) CCTKGROUPNUM_gridspacings = CCTK_GroupIndex("GRID::gridspacings"); \
if(CCTKARGNUM_coarse_dy == -1) CCTKARGNUM_coarse_dy = CCTK_VarIndex("GRID::coarse_dy"); \
if(CCTKARGNUM_coarse_dz == -1) CCTKARGNUM_coarse_dz = CCTK_VarIndex("GRID::coarse_dz"); \
if(CCTKARGNUM_r == -1) CCTKARGNUM_r = CCTK_VarIndex("GRID::r"); \
if(CCTKGROUPNUM_coordinates == -1) CCTKGROUPNUM_coordinates = CCTK_GroupIndex("GRID::coordinates"); \
if(CCTKARGNUM_x == -1) CCTKARGNUM_x = CCTK_VarIndex("GRID::x"); \
if(CCTKARGNUM_y == -1) CCTKARGNUM_y = CCTK_VarIndex("GRID::y"); \
if(CCTKARGNUM_z == -1) CCTKARGNUM_z = CCTK_VarIndex("GRID::z"); \


#define BOUNDARY_PUBLIC_C2F_PROTO \
const int *,const int *,const int *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *


#define PASS_BOUNDARY_PUBLIC_C2F(xGH) \
(const int *)(CCTKGROUPNUM_coordinates<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 0, "GRID::coordinates"))),(const int *)(CCTKGROUPNUM_coordinates<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 1, "GRID::coordinates"))),(const int *)(CCTKGROUPNUM_coordinates<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 2, "GRID::coordinates"))),(CCTK_REAL *)(CCTKARGNUM_coarse_dx<0 ? NULL : (xGH)->data[CCTKARGNUM_coarse_dx][1-1]),(CCTK_REAL *)(CCTKARGNUM_coarse_dy<0 ? NULL : (xGH)->data[CCTKARGNUM_coarse_dy][1-1]),(CCTK_REAL *)(CCTKARGNUM_coarse_dz<0 ? NULL : (xGH)->data[CCTKARGNUM_coarse_dz][1-1]),(CCTK_REAL *)(CCTKARGNUM_r<0 ? NULL : (xGH)->data[CCTKARGNUM_r][1-1]),(CCTK_REAL *)(CCTKARGNUM_x<0 ? NULL : (xGH)->data[CCTKARGNUM_x][1-1]),(CCTK_REAL *)(CCTKARGNUM_y<0 ? NULL : (xGH)->data[CCTKARGNUM_y][1-1]),(CCTK_REAL *)(CCTKARGNUM_z<0 ? NULL : (xGH)->data[CCTKARGNUM_z][1-1])


#endif /*CCODE*/


#ifdef FCODE
#define BOUNDARY_FARGUMENTS _CCTK_FARGUMENTS\
,BOUNDARY_PUBLIC_FARGUMENTS\


#define DECLARE_BOUNDARY_FARGUMENTS _DECLARE_CCTK_FARGUMENTS \
DECLARE_BOUNDARY_PUBLIC_FARGUMENTS \


#endif /*FCODE*/


#ifdef CCODE
#define DECLARE_BOUNDARY_CARGUMENTS _DECLARE_CCTK_CARGUMENTS \
DECLARE_BOUNDARY_PUBLIC_CARGUMENTS \


#define USE_BOUNDARY_CARGUMENTS _USE_CCTK_CARGUMENTS \
USE_BOUNDARY_PUBLIC_CARGUMENTS \


#endif /*CCODE*/


#ifdef CCODE
#define BOUNDARY_C2F_PROTO _CCTK_C2F_PROTO\
,BOUNDARY_PUBLIC_C2F_PROTO\


#define PASS_BOUNDARY_C2F(xGH) _PASS_CCTK_C2F(xGH)\
,PASS_BOUNDARY_PUBLIC_C2F(xGH)\


#define DECLARE_BOUNDARY_C2F _DECLARE_CCTK_C2F \
DECLARE_BOUNDARY_PUBLIC_C2F \


#define INITIALISE_BOUNDARY_C2F _INITIALISE_CCTK_C2F \
INITIALISE_BOUNDARY_PUBLIC_C2F \


#define BOUNDARY_CARGUMENTS cGH *cctkGH 


#endif /*CCODE*/


