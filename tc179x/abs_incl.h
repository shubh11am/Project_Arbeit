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


#ifndef _ABS_H_
#define _ABS_H_


//
// Class forward declarations
//

class IR_Exp;


//
// Header section
//

enum class AbsType : char
{
  ABS_NONE,
  ABS_ABS
};


/*!
  isAbsCondExp returns whether the given expression is a conditional expression
  of the form:

    ( A < 0 )  ? -A : A
    ( 0 > A )  ? -A : A
    ( A <= 0 ) ? -A : A
    ( 0 >= A ) ? -A : A

    ( A > 0 )  ? A : -A
    ( 0 < A )  ? A : -A
    ( A >= 0 ) ? A : -A
    ( 0 <= A ) ? A : -A

  This function returns whether the operation can be covered by a TriCore ABS
  operation. This is the case if:

  1) A does not define anything,
  2) A does not contain volatile symbols,
  3) the result type is integral and can be stored in a data register,
  4) A is signed.
*/
AbsType isAbsCondExp( const IR_Exp & );


#endif
