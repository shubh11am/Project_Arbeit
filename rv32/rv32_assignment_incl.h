/*

   This header file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Compiler Framework

   and is property of its respective copyright holder. It must neither be used
   nor published even in parts without explicit written permission.

   Copyright 2022

   Hamburg University of Technology (TUHH)
   Institute of Embedded Systems
   21071 Hamburg
   Germany

   http://www.tuhh.de/es/esd/research/wcc

*/

/*!
  @file rv32_assignment_incl.h
  @brief This file provides the interface of various helper functions used by
         the RISC-V RV32 code selector's assignment rules of the tree grammar.
*/


#ifndef _RV32_ASSIGNMENT_INCL_H_
#define _RV32_ASSIGNMENT_INCL_H_


//
// Include section
//

// Include standard headers
#include <functional>

// Include local headers
#include <rv32/rv32lvalue.h>


//
// Class forward declarations
//

class IR_Exp;
class IR_Type;

namespace WIR {
class WIR_VirtualRegister;
}


//
// Header section
//

namespace RV32 {


//
// Type declarations.
//

/*!
  Struct RV32_WriteBackInfo specifies a deref info and a Boolean flag whether
  the result should really be rewritten to memory and the deref info itself. If
  the object does not need to be rewritten to memory, then still the result
  register to which it must be written is stored in
  'derefLocation.resultRegister' (getResultReg()).
*/
struct RV32_WriteBackInfo
{

  public:

    /*!
      @brief writeBackToMemory denotes whether a write-back actually has to be
             done or not.
    */
    bool writeBackToMemory;

    /*!
      @brief derefLocation specifies all information relevant for the memory
             write-back.
    */
    RV32_LValue derefLocation;

    /*!
      @brief mTmpReg refers to the %WIR register to which intermediate results
             shall be written.
    */
    std::reference_wrapper<WIR::WIR_VirtualRegister> mTmpReg;

    /*!
      @brief Constructor initializing an empty write-back info.

      @note Do not use, this constructor is only provided for internal technical
            reasons.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    RV32_WriteBackInfo( void );

    /*!
      @brief Constructor initializing a register-based write-back info.

      @param[in] rReg A const reference to the %WIR register that shall hold the
                      result of the memory access.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    RV32_WriteBackInfo( const WIR::WIR_VirtualRegister &rReg );

    /*!
      @brief Constructor initializing a memory-based write-back info.

      @param[in] writeTo A const reference to a deref info describing the memory
                         access.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    RV32_WriteBackInfo( const RV32_LValue &writeTo );


    WIR::WIR_VirtualRegister &getTempReg( void ) const;

};


//
// Helper functions.
//


/*!
  @brief Determines whether 'exp' is the LHS of an assignment expression.

  @param[in] exp A const reference to an IR expression.
  @return A reference to the IR_AssignExp expression being the assignment's
          left-hand side, otherwise nullptr.
*/
IR_AssignExp *isAssignmentLHS( const IR_Exp & );


/*!
  @brief getComputationLevelType determines the type to which both operands of
         an assignment must be converted before executing the assignment
         operation.

  @param[in] exp A const reference to an IR expression that must be an
                 assignment expression or a child of an assignment expression.
  @return A reference to the IR type to be used for conversion.

  If the given IR expression is neither an assignment nor a child of an
  assignment expression, getComputationLevelType fails with an assertion.
*/
const IR_Type &getComputationLevelType( const IR_Exp &exp );


/*!
  @brief For a given assignment expression, getLHS returns the expression of the
         assignment's left-hand side.

  @param[in] exp A const reference to an IR expression.
  @return A reference to the IR expression being the assignment's left-hand
          side.

  If the given expression is not an assignment, getLHS fails with an assertion.
*/
IR_Exp &getLHS( const IR_Exp &exp );


/*!
  @brief lhsConversionCost computes the costs for converting a register value to
         its target type if the register value represents the LHS of an
         assignment.

  @param[in] exp A const reference to an IR expression that is an LHS side of
                 an assignment.
  @return The costs for the required conversion instructions.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
COST lhsConversionCost( const IR_Exp &exp );


/*!
  @brief doLhsConversion converts a register value to the type that it must have
         if it is the left-hand side of an assignment expression.

  @param[in] exp A const reference to an IR expression that is an LHS of an
                 assignment.
  @param[in,out] wb A reference to a write-back info describing the assignment
                    operation.

  doLhsConversion also sets the temporary variable to use during the assignment
  operations.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
void doLhsConversion( const IR_Exp &exp, RV32_WriteBackInfo &wb );


/*!
  @brief castBackAndStoreCost computes the costs of applying the function
         'castBackAndStore'.

  @param[in] exp A const reference to an IR assignment expression.
  @return The costs for casting and storing back an assignment expression.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
COST castBackAndStoreCost( const IR_Exp &exp );


/*!
  @brief castBackAndStore casts the result of an assignment back into its proper
         format (if needed) and moves the result from its temporary register to
         the result register from where the value originally came. Finally, it
         stores the result in memory.

  @param[in] exp A const reference to an IR assignment expression.
  @param[in,out] wb A reference to a write-back info describing the required
                    memory access.
  @return A reference to a virtual %WIR register where the final result is
          stored.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
WIR::WIR_VirtualRegister *castBackAndStore( const IR_Exp &exp,
                                            RV32_WriteBackInfo &wb );

}


#endif  // _RV32_ASSIGNMENT_INCL_H_
