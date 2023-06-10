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


#ifndef _COMPOSED_INITLIST_INCL_H_
#define _COMPOSED_INITLIST_INCL_H_


//
// Include section
//

// Include local headers
#include <tc179x/auxfuncs.h>


//
// Class forward declarations
//

class IR_Exp;
class IR_InitListExp;
class IR_Symbol;
class IR_Type;

class TC_AddressWithOffset;


//
// Header section
//

/*!
  Returns the type of the given expression inside an enclosing init list. If the
  expression is not part of an init list, then the result is nullptr.
*/
const IR_Type *getTypeInInitList( const IR_Exp & );


/*!
  @brief initWithZeroCost computes the approximated costs of instructions that
          fill a memory area of the given size with zeroes.

  @param[in] initSize The size of the memory area to be initialized.
  @param[in] exp A const reference to an IR expression used to generate debug
                 information.
  @return The costs for initializing a memory area with 0.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
COST initWithZeroCost( long initSize, const IR_Exp &exp );


/*!
  @brief initWithZero fills the memory area of the given object with zeroes.

  @param[in] addr A const reference to an address/offset struct describing the
                  memory area to be initialized.
  @param[in] initSize The size of the memory area to be initialized.
  @param[in] exp A const reference to an IR expression used to generate debug
                 information.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
void initWithZero( const TC_AddressWithOffset &addr, long initSize,
                   const IR_Exp &exp );


/*!
  Returns the number of initializable elements in the given type.
*/
unsigned int countAllElements( const IR_Type & );


/*!
  Returns the number of init expressions in the init list and its sublists.
*/
unsigned int countAllElements( const IR_InitListExp & );


/*!
  Determines whether the given init list expression has initializer expressions
  for all elements of the initialized type that have to be initialized.
*/
bool coversAllElements( const IR_InitListExp & );


/*!
  Returns the symbol on the LHS of the assignment expression, if the given
  expression is an assignment expression.
*/
IR_Symbol *getLHSSymbol( const IR_Exp & );

#endif
