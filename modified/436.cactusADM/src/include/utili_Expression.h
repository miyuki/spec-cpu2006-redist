 /*@@
   @header    utili_Expression.h
   @date      Wed Nov  7 00:39:24 2001
   @author    Tom Goodale
   @desc 
   Internal definitions for the expression parser.
   @enddesc
   @version $Header: /cactus/Cactus/src/include/utili_Expression.h,v 1.1 2001/11/07 00:30:42 goodale Exp $
 @@*/

#ifndef __UTILI_EXPRESSION_H__
#define __UTILI_EXPRESSION_H__ 1

#ifdef __cplusplus
extern "C" 
{
#endif

  /* Defined operators */
typedef enum {OP_NONE,
              OP_EQUALS,
              OP_LESS_THAN,
              OP_GREATER_THAN,
              OP_LEQUALS,
              OP_GEQUALS,
              OP_AND,
              OP_OR,
              OP_PLUS,
              OP_MINUS,
              OP_DIV,
              OP_TIMES,
              OP_POWER,
              OP_ACOS,
              OP_ASIN,
              OP_ATAN,
              OP_CEIL,
              OP_COS, 
              OP_COSH,
              OP_EXP,
              OP_FABS,
              OP_FLOOR,
              OP_LOG,
              OP_LOG10,
              OP_SIN,
              OP_SINH,
              OP_SQRT,
              OP_TAN,
              OP_TANH}
  uExpressionOpcode;

  /* What sort of expression types we have. */
typedef enum {val,unary,binary} uExpressionType;

  /* RPN object. */
typedef struct 
{
  uExpressionType type;

  union
  {
    uExpressionOpcode opcode;
    int varnum;
  } token;
} uExpressionToken;

  /* Parsed expression object. */
typedef struct
{
  int ntokens;
  uExpressionToken *tokens;
  int nvars;
  char **vars;
} uExpressionInternals;

  /* Internal representation of the expression. */
typedef uExpressionInternals *uExpression;

#ifdef __cplusplus
}
#endif

#endif /* __UTIL_EXPRESSION_H__ */
