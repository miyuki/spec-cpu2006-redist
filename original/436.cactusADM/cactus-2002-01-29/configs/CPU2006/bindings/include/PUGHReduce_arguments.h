#ifdef FCODE
#define DECLARE_PUGHREDUCE_PRIVATE_FARGUMENTS \


#define PUGHREDUCE_PRIVATE_FARGUMENTS \



#endif /*FCODE*/


#ifdef CCODE
#define DECLARE_PUGHREDUCE_PRIVATE_CARGUMENTS \


#define USE_PUGHREDUCE_PRIVATE_CARGUMENTS \


#define DECLARE_PUGHREDUCE_PRIVATE_C2F \


#define INITIALISE_PUGHREDUCE_PRIVATE_C2F \


#define PUGHREDUCE_PRIVATE_C2F_PROTO \



#define PASS_PUGHREDUCE_PRIVATE_C2F(xGH) \



#endif /*CCODE*/


#ifdef FCODE
#define DECLARE_PUGHREDUCE_PROTECTED_FARGUMENTS \


#define PUGHREDUCE_PROTECTED_FARGUMENTS \



#endif /*FCODE*/


#ifdef CCODE
#define DECLARE_PUGHREDUCE_PROTECTED_CARGUMENTS \


#define USE_PUGHREDUCE_PROTECTED_CARGUMENTS \


#define DECLARE_PUGHREDUCE_PROTECTED_C2F \


#define INITIALISE_PUGHREDUCE_PROTECTED_C2F \


#define PUGHREDUCE_PROTECTED_C2F_PROTO \



#define PASS_PUGHREDUCE_PROTECTED_C2F(xGH) \



#endif /*CCODE*/


#ifdef FCODE
#define DECLARE_PUGHREDUCE_PUBLIC_FARGUMENTS \


#define PUGHREDUCE_PUBLIC_FARGUMENTS \



#endif /*FCODE*/


#ifdef CCODE
#define DECLARE_PUGHREDUCE_PUBLIC_CARGUMENTS \


#define USE_PUGHREDUCE_PUBLIC_CARGUMENTS \


#define DECLARE_PUGHREDUCE_PUBLIC_C2F \


#define INITIALISE_PUGHREDUCE_PUBLIC_C2F \


#define PUGHREDUCE_PUBLIC_C2F_PROTO \



#define PASS_PUGHREDUCE_PUBLIC_C2F(xGH) \



#endif /*CCODE*/


#ifdef FCODE
#define PUGHREDUCE_FARGUMENTS _CCTK_FARGUMENTS\


#define DECLARE_PUGHREDUCE_FARGUMENTS _DECLARE_CCTK_FARGUMENTS \


#endif /*FCODE*/


#ifdef CCODE
#define DECLARE_PUGHREDUCE_CARGUMENTS _DECLARE_CCTK_CARGUMENTS \


#define USE_PUGHREDUCE_CARGUMENTS _USE_CCTK_CARGUMENTS \


#endif /*CCODE*/


#ifdef CCODE
#define PUGHREDUCE_C2F_PROTO _CCTK_C2F_PROTO\


#define PASS_PUGHREDUCE_C2F(xGH) _PASS_CCTK_C2F(xGH)\


#define DECLARE_PUGHREDUCE_C2F _DECLARE_CCTK_C2F \


#define INITIALISE_PUGHREDUCE_C2F _INITIALISE_CCTK_C2F \


#define PUGHREDUCE_CARGUMENTS cGH *cctkGH 


#endif /*CCODE*/

