 /*@@
   @header    util_Expression.h
   @date      Tue Sep 19 22:02:45 2000
   @author    Tom Goodale
   @desc 
   Header for expression stuff.
   @enddesc
   @version $Header: /cactus/Cactus/src/include/util_Expression.h,v 1.2 2001/11/07 00:30:40 goodale Exp $
 @@*/

#ifndef __UTIL_EXPRESSION_H__
#define __UTIL_EXPRESSION_H__ 1

#ifdef __cplusplus
extern "C" 
{
#endif

  /* Structure to hold values. */
typedef struct 
{
  enum {rval,ival} type;
  
  union 
  {
    double rval;
    int    ival;
  } value;
} uExpressionValue;

#ifndef __UTILI_EXPRESSION_H__
  /* Externally visible representation of the expression. */
typedef void *uExpression;
#endif /*__UTIL_EXPRESSION_H__ */

uExpression Util_ExpressionParse(const char *expression);

int Util_ExpressionEvaluate(const uExpression buffer,
                            uExpressionValue *retval,
                            int (*eval)(int, const char * const *, uExpressionValue *, void *),
                            void *data);

void Util_ExpressionFree(uExpression buffer);

#ifdef __cplusplus
}
#endif

#endif /* __UTIL_EXPRESSION_H__ */
