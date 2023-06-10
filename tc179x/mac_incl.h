/*

   This header file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Compiler Framework

   and is property of its respective copyright holder. It must neither be used
   nor published even in parts without explicit written permission.

   Copyright 2009 - 2022

   Hamburg University of Technology (TUHH)
   Institute of Embedded Systems
   21071 Hamburg
   Germany

   http://www.tuhh.de/es/esd/research/wcc

*/


#ifndef _MAC_RULES_H_
#define _MAC_RULES_H_


//
// Class forward declarations
//

namespace WIR {
class TC_DRegV;
}

class LLIR_Register;


//
// Header section
//

struct RegPair
{
  LLIR_Register *reg0;
  LLIR_Register *reg1;
  reference_wrapper<WIR::TC_DRegV> r1;
  reference_wrapper<WIR::TC_DRegV> r2;

  RegPair( void );
  RegPair( LLIR_Register *, LLIR_Register *,
           const reference_wrapper<WIR::TC_DRegV> &,
           const reference_wrapper<WIR::TC_DRegV> & );
};


struct RegConst
{
  LLIR_Register *reg0;
  reference_wrapper<WIR::TC_DRegV> r;
  unsigned long constant;

  RegConst( void );
  RegConst( LLIR_Register *, const reference_wrapper<WIR::TC_DRegV> &,
            unsigned long );
};


/*!
  @brief checkSignednessForMADD checks whether the given IR expression has a
         form that allows to generate MADD instructions for it.

  @param[in] exp A const reference to an IR expression to be checked.
  @return true if exp has a suitable form for MADD, false otherwise.

  In more detail, checkSignednessForMADD checks whether:
   - 'exp' is a binary PLUS or MINUS expression (a +/- b)
   - its second operand is a multiplication (a +/- (b * c)), and
   - 'a', 'b' and 'c' have the same sign.
*/
bool checkSignednessForMADD( const IR_Exp &exp );

#endif
