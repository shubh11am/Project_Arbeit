/*

   This header file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Compiler Framework

   and is property of its respective copyright holder. It must neither be used
   nor published even in parts without explicit written permission.

   Copyright 2010 - 2022

   Hamburg University of Technology (TUHH)
   Institute of Embedded Systems
   21071 Hamburg
   Germany

   http://www.tuhh.de/es/esd/research/wcc

*/


#ifndef _MINMAX_INCL_H_
#define _MINMAX_INCL_H_


//
// Class forward declarations
//

class IR_Exp;

namespace WIR {
class TC_DRegV;
}

class LLIR_Register;


//
// Header section
//

enum class MinMaxType : char
{
  MINMAX_NONE,
  MINMAX_MIN_SIGNED,
  MINMAX_MIN_UNSIGNED,
  MINMAX_MAX_SIGNED,
  MINMAX_MAX_UNSIGNED
};


struct MinMaxRegisterComparison
{
  LLIR_Register *op1;
  LLIR_Register *op2;
  reference_wrapper<WIR::TC_DRegV> r1;
  reference_wrapper<WIR::TC_DRegV> r2;

  MinMaxRegisterComparison( void );
  MinMaxRegisterComparison( LLIR_Register *, LLIR_Register *,
                            const reference_wrapper<WIR::TC_DRegV> &,
                            const reference_wrapper<WIR::TC_DRegV> & );
};


struct MinMaxConstantComparison
{
  LLIR_Register *op1;
  reference_wrapper<WIR::TC_DRegV> r;
  IR_Integer v;

  MinMaxConstantComparison( void );
  MinMaxConstantComparison( LLIR_Register *,
                            const reference_wrapper<WIR::TC_DRegV> &,
                            const IR_Integer & );
};


/*!
  A simple ICD-C iterator to detect expressions with defs or volatile exps.
*/
void checkDefOrVolatile( IR_Exp &, bool * );


/*!
  @brief isMinMaxCondExp determines whether the given expression is a
         conditional expression of a certain form feasible for the TriCore's
         MIN/MAX operations.

  @param[in] exp A const reference to an IR expression to be examined.
  @return An enum encoding whether the given IR expression is feasible for
          MIN/MAX operations.

  The actually supported forms of an IR expression are:
    ( A < B )  ? A : B
    ( B > A )  ? A : B
    ( A <= B ) ? A : B
    ( B >= A ) ? A : B

  or the analogous forms where the then and else branch are exchanged. The
  former corresponds to a minimum, and the latter to a maximum operation. The IR
  expression can be covered by a TriCore MIN / MAX operation if:

    1) A and B do not define anything
    2) A and B do not contain volatile symbols
    3) the result type is integral and can be stored in a data register
    4) A and B are either both signed or both unsigned
*/
MinMaxType isMinMaxCondExp( const IR_Exp &exp );

#endif
