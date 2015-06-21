@BOTTOM@

#if SIZEOF_INT==4
#define Int int
#elif SIZEOF_SHORT_INT==4
#define Int short int
#else
#error "Unable to find a 4 byte integer type. "
#endif

#define POINTER integer*SIZEOF_CHAR_P

#define CCTK_STRING POINTER

#define CCTK_REAL double
