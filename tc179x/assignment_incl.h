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


#ifndef _ASSIGNMENT_INCL_H_
#define _ASSIGNMENT_INCL_H_


//
// Include section
//

// Include standard headers
#include <string>
#include <utility>

// Include WIR headers
#include <wir/wirtypes.h>

// Include local headers
#include <tc179x/cs_tc179x.h>
#include <tc179x/tclvalue.h>


//
// Class forward declarations
//

class IR_AssignExp;
class IR_Exp;
class IR_Type;

namespace WIR {
class WIR_VirtualRegister;
}

class LLIR_Register;


//
// Header section
//

/*!
  WriteBackInfo specifies a deref info and a Boolean flag whether the result
  should really be rewritten to memory and the deref info itself. If the object
  does not need to be rewritten to memory, then still the result register to
  which it must be written is stored in 'derefLocation.resultRegister'
  (getResultReg()).
*/
struct WriteBackInfo
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
    TC_LValue derefLocation;

    /*!
      @brief tempReg points to an LLIR register to which intermediate results
             shall be written.
    */
    LLIR_Register *tempReg;

    /*!
      @brief mTmpReg refers to the %WIR register to which intermediate results
             shall be written.
    */
    reference_wrapper<WIR::WIR_VirtualRegister> mTmpReg;

    /*!
      @brief Constructor initializing an empty write-back info.

      @note Do not use, this constructor is only provided for internal technical
            reasons.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WriteBackInfo( void );

    /*!
      @brief Constructor initializing only a register-based write-back info.

      @param[in] resultRegister A pointer to the LLIR register that shall hold
                                the result of the memory access.
      @param[in] rReg A const reference to the %WIR register that shall hold the
                      result of the memory access.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WriteBackInfo( LLIR_Register *resultRegister,
                   const WIR::WIR_VirtualRegister &rReg );

    /*!
      @brief Constructor initializing a memory-based write-back info.

      @param[in] writeTo A const reference to a deref info describing the memory
                         access.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WriteBackInfo( const TC_LValue &writeTo );

    /*!
      @brief getTempReg returns the LLIR register to which intermediate results
             shall be written.

      @return A pointer to the %LLIR register holding intermediate results.

      For the LHS of a compound assignment like, e.g.,
      "((long long)i) += ((float)d)", the tempReg for "i" will be a data
      register whereas the ResultReg will be an extended register.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    LLIR_Register *getTempReg( void ) const;

    /*!
      @brief getResultReg returns the register to which the result shall be
             written.

      @return A pointer to the result LLIR register.

      For the LHS of a compound assignment like, e.g.,
      "((long long)i) += ((float)d)", the tempReg for "i" will be a data
      register whereas the ResultReg will be an extended register.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    LLIR_Register *getResultReg( void ) const;

};


/*!
  Determines whether 'exp' is the LHS of an assignment expression.
*/
IR_AssignExp *isAssignmentLHS( const IR_Exp & );


/*!
  If the operand is an assignment expression or a child of an assignment
  expression, then this function returns the type to which both of the operands
  must be converted before executing the operation, else this function throws
  an assertion.
*/
const IR_Type &getComputationLevelType( const IR_Exp & );


/*!
  If the expression at the node is an assignment expression, return the
  compilation tree node of the LHS of the assignment, else an assertion is
  thrown.
*/
IR_Exp &getLHS( const IR_Exp & );


/*!
  If the expression at the node is an assignment expression, return the
  compilation tree node of the RHS of the assignment, else an assertion is
  thrown.
*/
IR_Exp &getRHS( const IR_Exp & );


/*!
  Returns a new LLIR_Register object for register 'target' matching the type
  'type' of the LHS of an assignment expression.
 */
LLIR_Register *getNewLHSRegister( const std::string &, const IR_Type & );


/*!
  @brief lhsConversionCost computes the cost for converting a register value to
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
         if it is the LHS 'exp' of an assignment expression.

  @param[in] exp A const reference to an IR expression that is an LHS side of
                 an assignment.
  @param[in,out] wb A reference to a write-back info describing the assignment
                    operation.

  doLhsConversion also sets the temporary variable to use during the assignment
  operations.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
void doLhsConversion( const IR_Exp &exp, WriteBackInfo &wb );


/*!
  @brief castBackAndStoreCost returns the cost of applying the function
         'castBackAndStore'.

  @param[in] exp A const reference to an IR assignment expression.

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
  @return A pair containing a pointer to an LLIR register and a reference to a
          virtual %WIR register where the final result is stored.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
std::pair<LLIR_Register *,
          WIR::WIR_VirtualRegister *> castBackAndStore( const IR_Exp &exp,
                                                        WriteBackInfo &wb );

#endif  // _ASSIGNMENT_INCL_H_
