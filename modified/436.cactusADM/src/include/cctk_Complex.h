 /*@@
   @header    cctk_Complex.h
   @date      Tue Dec 14 12:28:05 1999
   @author    Tom Goodale
   @desc 
   Prototypes for complex numbers.
   @enddesc 
   @version $Header: /cactus/Cactus/src/include/cctk_Complex.h,v 1.4 2002/01/02 17:04:27 tradke Exp $
 @@*/

#ifndef _CCTK_COMPLEX_H_
#define _CCTK_COMPLEX_H_

#ifdef __cplusplus
extern "C" 
{
#endif


/* a macro which expands to nothing */
#ifdef NOTHING
#undef NOTHING
#endif
#define NOTHING

/* macro to declare a set of complex functions for a given precision */
#define DECLARE_CMPLX_FUNCTIONS(CCTK_Cmplx, cctk_real, cctk_complex)          \
cctk_complex CCTK_Cmplx        (cctk_real Re, cctk_real Im);                  \
cctk_real    CCTK_Cmplx##Real  (cctk_complex complex_number);                 \
cctk_real    CCTK_Cmplx##Imag  (cctk_complex complex_number);                 \
cctk_complex CCTK_Cmplx##Conjg (cctk_complex complex_number);                 \
cctk_real    CCTK_Cmplx##Abs   (cctk_complex complex_number);                 \
cctk_complex CCTK_Cmplx##Add   (cctk_complex a, cctk_complex b);              \
cctk_complex CCTK_Cmplx##Sub   (cctk_complex a, cctk_complex b);              \
cctk_complex CCTK_Cmplx##Mul   (cctk_complex a, cctk_complex b);              \
cctk_complex CCTK_Cmplx##Div   (cctk_complex a, cctk_complex b);              \
cctk_complex CCTK_Cmplx##Sin   (cctk_complex complex_number);                 \
cctk_complex CCTK_Cmplx##Cos   (cctk_complex complex_number);                 \
cctk_complex CCTK_Cmplx##Exp   (cctk_complex complex_number);                 \
cctk_complex CCTK_Cmplx##Sqrt  (cctk_complex complex_number);


/* declare complex functions for all available precisions */
#ifdef CCTK_REAL4
DECLARE_CMPLX_FUNCTIONS (CCTK_Cmplx8, CCTK_REAL4, CCTK_COMPLEX8)
#endif

#ifdef CCTK_REAL8
DECLARE_CMPLX_FUNCTIONS (CCTK_Cmplx16, CCTK_REAL8, CCTK_COMPLEX16)
#endif

#ifdef CCTK_REAL16
DECLARE_CMPLX_FUNCTIONS (CCTK_Cmplx32, CCTK_REAL16, CCTK_COMPLEX32)
#endif


/* get the default precision for complex numbers */
#ifdef CCTK_REAL_PRECISION_4
#define CCTK_COMPLEX_PRECISION 8
#elif CCTK_REAL_PRECISION_8
#define CCTK_COMPLEX_PRECISION 16
#elif CCTK_REAL_PRECISION_16
#define CCTK_COMPLEX_PRECISION 32
#endif

/* finally declare the default precision complex functions */
#define CCTK_Cmplx      CCTK_Cmplx_(CCTK_COMPLEX_PRECISION, NOTHING)
#define CCTK_CmplxReal  CCTK_Cmplx_(CCTK_COMPLEX_PRECISION, Real)
#define CCTK_CmplxImag  CCTK_Cmplx_(CCTK_COMPLEX_PRECISION, Imag)
#define CCTK_CmplxConjg CCTK_Cmplx_(CCTK_COMPLEX_PRECISION, Conjg)
#define CCTK_CmplxAbs   CCTK_Cmplx_(CCTK_COMPLEX_PRECISION, Abs)
#define CCTK_CmplxAdd   CCTK_Cmplx_(CCTK_COMPLEX_PRECISION, Add)
#define CCTK_CmplxSub   CCTK_Cmplx_(CCTK_COMPLEX_PRECISION, Sub)
#define CCTK_CmplxMul   CCTK_Cmplx_(CCTK_COMPLEX_PRECISION, Mul)
#define CCTK_CmplxDiv   CCTK_Cmplx_(CCTK_COMPLEX_PRECISION, Div)

#define CCTK_Cmplx_(precision, function)   CCTK_Cmplx__(precision, function)
#define CCTK_Cmplx__(precision, function)  CCTK_Cmplx##precision##function

#undef NOTHING

#ifdef __cplusplus
}
#endif

#endif /* __CCTK_COMPLEX_H_ */
