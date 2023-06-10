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


#ifndef _LONGLONG_INCL_H_
#define _LONGLONG_INCL_H_


//
// Include section
//

// Include standard headers
#include <memory>


//
// Class forward declarations
//

class IR_Exp;
class IR_SourceSymbol;

class LLIR_Register;


//
// Header section
//

/*!
  Workaround for getting an integer type object.
*/
std::shared_ptr<IR_SourceSymbol> getIntSymbol( void );


/*!
  Extracts the low-valued register from a long long ereg.
*/
inline LLIR_Register *getLVLLChild( LLIR_Register * );


/*!
  Extracts the high-valued register from a long long ereg.
*/
inline LLIR_Register *getHVLLChild( LLIR_Register * );


/*!
  Returns whether 'exp' is a unary increment.
*/
inline bool isUnaryIncrement( const IR_Exp & );


/*!
  Returns whether 'exp' is a unary decrement.
*/
inline bool isUnaryDecrement( const IR_Exp & );


/*!
  Returns whether the given expression is added to or subtracted from a pointer.
*/
inline bool isUsedForPointerArithmetic( const IR_Exp & );


/*!
  Returns whether the given expression is an index inside an index expression,
  as, e.g., "x" in "a[x]" is.
*/
inline bool isUsedAsIndex( const IR_Exp & );

#endif
