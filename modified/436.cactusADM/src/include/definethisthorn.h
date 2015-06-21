/* Defines for thorn this file is part of */

#ifdef THORN_IS_IOASCII
#define CCTK_THORN   IOASCII
#define CCTK_THORNSTRING "IOASCII"
#define CCTK_ARRANGEMENT CactusBase
#define CCTK_ARRANGEMENTSTRING "CactusBase"
#endif

#ifdef THORN_IS_BenchADM
#define CCTK_THORN   BenchADM
#define CCTK_THORNSTRING "BenchADM"
#define CCTK_ARRANGEMENT CactusBench
#define CCTK_ARRANGEMENTSTRING "CactusBench"
#endif

#ifdef THORN_IS_Cactus
#define CCTK_THORN   src
#define CCTK_THORNSTRING "src"
#define CCTK_ARRANGEMENT Cactus
#define CCTK_ARRANGEMENTSTRING "Cactus"
#endif

#ifdef THORN_IS_Boundary
#define CCTK_THORN   Boundary
#define CCTK_THORNSTRING "Boundary"
#define CCTK_ARRANGEMENT CactusBase
#define CCTK_ARRANGEMENTSTRING "CactusBase"
#endif

#ifdef THORN_IS_PUGH
#define CCTK_THORN   PUGH
#define CCTK_THORNSTRING "PUGH"
#define CCTK_ARRANGEMENT CactusPUGH
#define CCTK_ARRANGEMENTSTRING "CactusPUGH"
#endif

#ifdef THORN_IS_IOUtil
#define CCTK_THORN   IOUtil
#define CCTK_THORNSTRING "IOUtil"
#define CCTK_ARRANGEMENT CactusBase
#define CCTK_ARRANGEMENTSTRING "CactusBase"
#endif

#ifdef THORN_IS_PUGHSlab
#define CCTK_THORN   PUGHSlab
#define CCTK_THORNSTRING "PUGHSlab"
#define CCTK_ARRANGEMENT CactusPUGH
#define CCTK_ARRANGEMENTSTRING "CactusPUGH"
#endif

#ifdef THORN_IS_CartGrid3D
#define CCTK_THORN   CartGrid3D
#define CCTK_THORNSTRING "CartGrid3D"
#define CCTK_ARRANGEMENT CactusBase
#define CCTK_ARRANGEMENTSTRING "CactusBase"
#endif

#ifdef THORN_IS_IOBasic
#define CCTK_THORN   IOBasic
#define CCTK_THORNSTRING "IOBasic"
#define CCTK_ARRANGEMENT CactusBase
#define CCTK_ARRANGEMENTSTRING "CactusBase"
#endif

#ifdef THORN_IS_PUGHReduce
#define CCTK_THORN   PUGHReduce
#define CCTK_THORNSTRING "PUGHReduce"
#define CCTK_ARRANGEMENT CactusPUGH
#define CCTK_ARRANGEMENTSTRING "CactusPUGH"
#endif

#ifdef THORN_IS_IDLinearWaves
#define CCTK_THORN   IDLinearWaves
#define CCTK_THORNSTRING "IDLinearWaves"
#define CCTK_ARRANGEMENT CactusEinstein
#define CCTK_ARRANGEMENTSTRING "CactusEinstein"
#endif

#ifdef THORN_IS_Time
#define CCTK_THORN   Time
#define CCTK_THORNSTRING "Time"
#define CCTK_ARRANGEMENT CactusBase
#define CCTK_ARRANGEMENTSTRING "CactusBase"
#endif

#ifdef THORN_IS_Einstein
#define CCTK_THORN   Einstein
#define CCTK_THORNSTRING "Einstein"
#define CCTK_ARRANGEMENT CactusEinstein
#define CCTK_ARRANGEMENTSTRING "CactusEinstein"
#endif



