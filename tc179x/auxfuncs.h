/*

   This header file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Compiler Framework

   and is property of its respective copyright holder. It must neither be used
   nor published even in parts without explicit written permission.

   Copyright 2005 - 2022

   Hamburg University of Technology (TUHH)
   Institute of Embedded Systems
   21071 Hamburg
   Germany

   http://www.tuhh.de/es/esd/research/wcc

*/


#ifndef _TC179x_AUXFUNCS_H
#define _TC179x_AUXFUNCS_H


//
// Include section
//

// Include standard headers
#include <string>

// Include ICD headers
#include <icdint/icdint.h>


//
// Class forward declarations
//

class IR_Exp;
class IR_Integer;
class IR_TreeElem;
class IR_Type;


//
// Global data type declarations
//

/*!
  @brief Integer is the internal host-representation of integer numbers for the
         considered target architecture.
*/
using Integer = ICDInt64;

/*!
  @brief Float is the internal host-representation of single-precision floating
         point numbers for the considered target architecture.
*/
using Float = ICD_Float::Float<ICD_Float::Single>;

/*!
  @brief Double is the internal host-representation of double-precision floating
         point numbers for the considered target architecture.
*/
using Double = ICD_Float::Float<ICD_Float::Double>;


//
// Header section
//

/*!
  DEBUG_RULE_ACTION should be invoked right at the beginning of the action part
  of each rule. If debugging is enabled, it prints out which action part is
  entered, for which C expression. If debugging is disabled, DEBUG_RULE_ACTION
  does nothing.

  This function is named in uppercase letters against the usual convention,
  which would force us to use lowercase letters, because it mimics a debugmacro
  functionality and all the debugmacro stuff is written in uppercase letters.
*/
void DEBUG_RULE_ACTION( const std::string &, IR_TreeElem * );


/*!
  getConstIntValue returns the constant integer value associated with the
  IR_TreeElem passed as argument. It returns VALUE_INVALID on error.

  The function should only be used from within the cost computations. In the
  action parts, the appropriate action should be called to obtain the integer
  constant.
*/
long long getConstIntValue( IR_TreeElem * );


/*!
  For a given pointer/array type, getBaseType returns the base type, else
  returns 0.
*/
IR_Type *getBaseType( const IR_Type & );


/*!
  For a given pointer/array exp, getBaseType returns the base type, else returns
  0.
*/
IR_Type *getBaseType( const IR_Exp & );


/*!
  getLowerLongLongWord extracts the lower word from a long long.
*/
Integer getLowerLongLongWord( const IR_Integer & );


/*!
  getUpperLongLongWord extracts the upper word from a long long.
*/
Integer getUpperLongLongWord( const IR_Integer & );

#endif  // _TC179x_AUXFUNCS_H
