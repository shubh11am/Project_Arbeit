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


#ifndef _COMMA_INCL_H_
#define _COMMA_INCL_H_


//
// Class forward declarations
//

class IR_Exp;


//
// Header section
//

/*!
  @brief isFirstCommaOperand determines whether the given IR expression is the
         first operand of a comma expression.

  @param[in] exp A const reference to an IR expression.
  @return true if exp is the first operand of a comma expresison, false
          otherwise.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
inline bool isFirstCommaOperand( const IR_Exp &exp );

#endif
