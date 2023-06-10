/*

   This header file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Compiler Framework

   and is property of its respective copyright holder. It must neither be used
   nor published even in parts without explicit written permission.

   Copyright 2020 - 2022

   Hamburg University of Technology (TUHH)
   Institute of Embedded Systems
   21071 Hamburg
   Germany

   http://www.tuhh.de/es/esd/research/wcc

*/


#ifndef _ARM7_AUXFUNCS_H
#define _ARM7_AUXFUNCS_H


//
// Include section
//

// Standard headers
#include <deque>

// Include ICD headers
#include <icdint/icdint.h>


//
// Class forward declarations
//

class IR_Exp;
class IR_Integer;
class IR_TreeElem;
class IR_Type;

class LLIR_Register;


//
// Global data type declarations
//

//! A queue of registers.
using RegisterQueue = std::deque<LLIR_Register *>;

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

#endif  // _ARM7_AUXFUNCS_H
