#ifdef SPEC_CPU
# define THORN_IS_PUGHSlab
#endif /* SPEC_CPU */
 /*@@
   @file      DatatypeConversion.c
   @date      Thursday Nov 23 2000
   @author    Thomas Radke
   @desc 
              Predefined datatype conversion routines for hyperslab extraction.
   @enddesc 
   @version   $Id: DatatypeConversion.c,v 1.7 2001/12/03 22:10:04 tradke Exp $
 @@*/

#include "cctk.h"
#include "PUGHSlab.h"
#include "PUGHSlabi.h"


/* the rcs ID and its dummy function to use it */
static const char *rcsid="$Id: DatatypeConversion.c,v 1.7 2001/12/03 22:10:04 tradke Exp $";
CCTK_FILEVERSION(CactusPUGH_PUGHSlab_DatatypeConversion_c)


/* macro to generate a predefined conversion function
   along with its prototype */
#define CONVERSION_FUNCTION(src_type, dst_type, dst_scalartype, conversion)   \
        static void Convert_##src_type##_to_##dst_type (const void *src,      \
                                                        void *dst,            \
                                                        CCTK_INT nelems,      \
                                                        CCTK_INT src_stride,  \
                                                        CCTK_INT dst_stride); \
                                                                              \
        static void Convert_##src_type##_to_##dst_type (const void *src,      \
                                                        void *dst,            \
                                                        CCTK_INT nelems,      \
                                                        CCTK_INT src_stride,  \
                                                        CCTK_INT dst_stride)  \
        {                                                                     \
          dst_type *_dst = (dst_type *) dst;                                  \
          const src_type *_src = (const src_type *) src;                      \
                                                                              \
                                                                              \
          while (nelems--)                                                    \
          {                                                                   \
            conversion (*_src, *_dst, dst_scalartype);                        \
            _src += src_stride; _dst += dst_stride;                           \
          }                                                                   \
        }

/* the conversion of scalar datatypes (integers and reals) */
#ifdef  CONVERT
#undef  CONVERT
#endif
#define CONVERT(src, dst, dst_type)         (dst) = (dst_type) (src)

/* predefined conversion functions for integers */
#if defined(CCTK_INT2) && defined(CCTK_INT4)
CONVERSION_FUNCTION (CCTK_INT4, CCTK_INT2, CCTK_INT2, CONVERT)
#endif
#if defined(CCTK_INT2) && defined(CCTK_INT8)
CONVERSION_FUNCTION (CCTK_INT8, CCTK_INT2, CCTK_INT2, CONVERT)
#endif
#if defined(CCTK_INT4) && defined(CCTK_INT8)
CONVERSION_FUNCTION (CCTK_INT8, CCTK_INT4, CCTK_INT4, CONVERT)
#endif

/* predefined conversion functions for reals */
#if defined(CCTK_REAL4) && defined(CCTK_REAL8)
CONVERSION_FUNCTION (CCTK_REAL8, CCTK_REAL4, CCTK_REAL4, CONVERT)
#endif
#if defined(CCTK_REAL4) && defined(CCTK_REAL16)
CONVERSION_FUNCTION (CCTK_REAL16, CCTK_REAL4, CCTK_REAL4, CONVERT)
#endif
#if defined(CCTK_REAL8) && defined(CCTK_REAL16)
CONVERSION_FUNCTION (CCTK_REAL16, CCTK_REAL8, CCTK_REAL8, CONVERT)
#endif

/* the conversion of complex datatypes */
#undef  CONVERT
#define CONVERT(src, dst, dst_type)         (dst).Re = (dst_type) (src).Re;   \
                                            (dst).Im = (dst_type) (src).Im

/* predefined conversion functions for complex */
#if defined(CCTK_REAL4) && defined(CCTK_REAL8)
CONVERSION_FUNCTION (CCTK_COMPLEX16, CCTK_COMPLEX8, CCTK_REAL4, CONVERT)
#endif
#if defined(CCTK_REAL4) && defined(CCTK_REAL16)
CONVERSION_FUNCTION (CCTK_COMPLEX32, CCTK_COMPLEX16, CCTK_REAL8, CONVERT)
#endif
#if defined(CCTK_REAL8) && defined(CCTK_REAL16)
CONVERSION_FUNCTION (CCTK_COMPLEX32, CCTK_COMPLEX8, CCTK_REAL4, CONVERT)
#endif


/* prototypes of routines defined in this source file */
static int PUGHSlabi_PrecisionVarType (int type);


 /*@@
   @routine    PUGHSlabi_GetDatatypeConversionFn
   @date       Fri 23 Nov 2000
   @author     Thomas Radke
   @desc 
               Finds a predefined datatype conversion function.
   @enddesc 

   @calls      PUGHSlab_REAL8_to_REAL4
 
   @var        src_type
   @vdesc      CCTK datatype of the source
   @vtype      int
   @vio        in
   @endvar
   @var        dst_type
   @vdesc      CCTK datatype of the destination (hyperslab datatype)
   @vtype      int
   @vio        in
   @endvar

   @returntype PUGHSlab_conversion_fn
   @returndesc
               the appropriate function pointer
               or NULL if no predefined conversion function was found
   @endreturndesc
@@*/
t_hslabConversionFn PUGHSlabi_GetDatatypeConversionFn (int src_type,
                                                       int dst_type)
{
  t_hslabConversionFn retval;


  /* get the precision datatype for generic CCTK_TYPE variable types */
  src_type = PUGHSlabi_PrecisionVarType (src_type);
  dst_type = PUGHSlabi_PrecisionVarType (dst_type);

  /* get the appropriate conversion routine */
  if (0)
  {
    /* this is just because of the following #ifdef'ed else branches */
  }
#if defined(CCTK_INT2) && defined(CCTK_INT4)
  else if (src_type == CCTK_VARIABLE_INT4 && dst_type == CCTK_VARIABLE_INT2)
  {
    retval = Convert_CCTK_INT4_to_CCTK_INT2;
  }
#endif
#if defined(CCTK_INT2) && defined(CCTK_INT8)
  else if (src_type == CCTK_VARIABLE_INT8 && dst_type == CCTK_VARIABLE_INT2)
  {
    retval = Convert_CCTK_INT8_to_CCTK_INT2;
  }
#endif
#if defined(CCTK_INT4) && defined(CCTK_INT8)
  else if (src_type == CCTK_VARIABLE_INT8 && dst_type == CCTK_VARIABLE_INT4)
  {
    retval = Convert_CCTK_INT8_to_CCTK_INT4;
  }
#endif
#if defined(CCTK_REAL4) && defined(CCTK_REAL8)
  else if (src_type == CCTK_VARIABLE_REAL8 && dst_type == CCTK_VARIABLE_REAL4)
  {
    retval = Convert_CCTK_REAL8_to_CCTK_REAL4;
  }
#endif
#if defined(CCTK_REAL4) && defined(CCTK_REAL16)
  else if (src_type == CCTK_VARIABLE_REAL16 && dst_type == CCTK_VARIABLE_REAL4)
  {
    retval = Convert_CCTK_REAL16_to_CCTK_REAL4;
  }
#endif
#if defined(CCTK_REAL8) && defined(CCTK_REAL16)
  else if (src_type == CCTK_VARIABLE_REAL16 && dst_type == CCTK_VARIABLE_REAL8)
  {
    retval = Convert_CCTK_REAL16_to_CCTK_REAL8;
  }
#endif
#if defined(CCTK_REAL4) && defined(CCTK_REAL8)
  else if (src_type == CCTK_VARIABLE_COMPLEX16 && dst_type == CCTK_VARIABLE_COMPLEX8)
  {
    retval = Convert_CCTK_COMPLEX16_to_CCTK_COMPLEX8;
  }
#endif
#if defined(CCTK_REAL8) && defined(CCTK_REAL16)
  else if (src_type == CCTK_VARIABLE_COMPLEX32 && dst_type == CCTK_VARIABLE_COMPLEX8)
  {
    retval = Convert_CCTK_COMPLEX32_to_CCTK_COMPLEX8;
  }
#endif
#if defined(CCTK_REAL8) && defined(CCTK_REAL16)
  else if (src_type == CCTK_VARIABLE_COMPLEX32 && dst_type == CCTK_VARIABLE_COMPLEX16)
  {
    retval = Convert_CCTK_COMPLEX32_to_CCTK_COMPLEX16;
  }
#endif
  else
  {
    retval = NULL;
  }

  return (retval);
}


/**************************************************************************/
/*                             local functions                            */
/**************************************************************************/
/* get the precision datatype for generic CCTK_TYPE variable types */
static int PUGHSlabi_PrecisionVarType (int type)
{
  if (type == CCTK_VARIABLE_INT)
  {
#ifdef CCTK_INTEGER_PRECISION_8
    type = CCTK_VARIABLE_INT8;
#elif  CCTK_INTEGER_PRECISION_4
    type = CCTK_VARIABLE_INT4;
#elif  CCTK_INTEGER_PRECISION_2
    type = CCTK_VARIABLE_INT2;
#endif
  }
  else if (type == CCTK_VARIABLE_REAL)
  {
#ifdef CCTK_REAL_PRECISION_16
    type = CCTK_VARIABLE_REAL16;
#elif  CCTK_REAL_PRECISION_8
    type = CCTK_VARIABLE_REAL8;
#elif  CCTK_REAL_PRECISION_4
    type = CCTK_VARIABLE_REAL4;
#endif
  }
  else if (type == CCTK_VARIABLE_COMPLEX)
  {
#ifdef CCTK_REAL_PRECISION_16
    type = CCTK_VARIABLE_COMPLEX32;
#elif  CCTK_REAL_PRECISION_8
    type = CCTK_VARIABLE_COMPLEX16;
#elif  CCTK_REAL_PRECISION_4
    type = CCTK_VARIABLE_COMPLEX8;
#endif
  }

  return (type);
}
