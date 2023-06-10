/*

   This source file belongs to the

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
  @file rv32_assignment_prolog.m4
  @brief This file implements various helper functions used by the RISC-V RV32
         code selector's assignment rules of the tree grammar.
*/


using namespace std;
using namespace WIR;


namespace RV32 {

//
// Code section
//

/*
  Constructor initializing an empty write-back info.
*/
RV32_WriteBackInfo::RV32_WriteBackInfo( void ) :
  writeBackToMemory { false },
  derefLocation {},
  mTmpReg { ref( dummyRegV ) }
{
  DSTART( "RV32_WriteBackInfo::RV32_WriteBackInfo()" );
};


/*
  Constructor initializing a register-based write-back info.
*/
RV32_WriteBackInfo::RV32_WriteBackInfo( const WIR::WIR_VirtualRegister &rReg ) :
  writeBackToMemory { false },
  derefLocation {},
  mTmpReg( ref( dummyRegV ) )
{
  DSTART(
    "RV32_WriteBackInfo::RV32_WriteBackInfo(const WIR_VirtualRegister&)" );

  derefLocation.setResultReg( const_cast<WIR_VirtualRegister *>( &rReg ) );
};


/*
  Constructor initializing a memory-based write-back info.
*/
RV32_WriteBackInfo::RV32_WriteBackInfo( const RV32_LValue &writeTo ) :
  writeBackToMemory { true },
  derefLocation { writeTo },
  mTmpReg { ref( dummyRegV ) }
{
  DSTART( "RV32_WriteBackInfo::RV32_WriteBackInfo(const RV32_LValue&)" );

  // If the derefLocation does not contain a loaded result, we have to create a
  // new register here that will serve as the result container.
  if ( derefLocation.getResultReg() == nullptr )
    derefLocation.setResultReg(
      &(getNewRegister( *derefLocation.getBaseType() )) );
};


/*
  Determines whether 'exp' is the LHS of an assignment expression.
*/
IR_AssignExp *isAssignmentLHS( const IR_Exp &exp )
{
  auto *aexp = dynamic_cast<IR_AssignExp *>( exp.getParent() );
  if ( aexp && ( &aexp->getLHS() == &exp ) )
    return( aexp );
  else
    return( nullptr );
};


/*
  getComputationLevelType determines the type to which both operands of an
  assignment must be converted before executing the assignment operation.
*/
const IR_Type &getComputationLevelType( const IR_Exp &exp )
{
  DSTART( "const IR_Type& getComputationLevelType(const IR_Exp&)" );

  auto *aexp = dynamic_cast<const IR_AssignExp *>( &exp );
  if ( !aexp )
    aexp = dynamic_cast<const IR_AssignExp *>( exp.getParent() );

  if ( aexp ) {
    if ( aexp->getOperator() != IR_AssignExp::ASSIGN ) {
      const IR_Type &lhsType = effectiveType( aexp->getLHS() );
      const IR_Type &rhsType = effectiveType( aexp->getRHS() );

      return( *lhsType.usualBinaryConversion( rhsType ) );
    } else
      return( exp.getType() );
  }

  ufAssertT( false, "Internal error: Invalid argument!" );
  return( exp.getType() );
};


/*
  For a given assignment expression, getLHS returns the expression of the
  assignment's left-hand side.

  If the given expression is not an assignment, getLHS fails with an assertion.
*/
IR_Exp &getLHS( const IR_Exp &exp )
{
  DSTART( "IR_Exp& getLHS(const IR_Exp&)" );

  auto *aexp = dynamic_cast<const IR_AssignExp *>( &exp );
  if ( aexp )
    return( aexp->getLHS() );

  ufAssertT( false, "Internal error: Illegal argument!" );
  return( const_cast<IR_Exp &>( exp ) );
};


/*
  lhsConversionCost computes the costs for converting a register value to its
  target type if the register value represents the LHS of an assignment.
*/
COST lhsConversionCost( const IR_Exp &exp )
{
  DSTART( "COST lhsConversionCost(const IR_Exp&)" );

  (void) exp;

  // Check if conversion is needed (only in compound assignments).
  return( RV32IMC::OperationFormat::RR_1.getSize() );
};


/*
  doLhsConversion converts a register value to the type that it must have if it
  is the left-hand side of an assignment expression.

  doLhsConversion also sets the temporary variable to use during the assignment
  operations.
*/
void doLhsConversion( const IR_Exp &exp, RV32_WriteBackInfo &wb )
{
  DSTART( "void doLhsConversion(const IR_Exp&, RV32_WriteBackInfo&)" );

  auto *aexp = dynamic_cast<const IR_AssignExp *>( exp.getParent() );
  ufAssertT( aexp, "Internal error: Illegal argument!" );

  // Fill the temporary register field if that was not done by the conversion.
  if ( wb.mTmpReg.get() == dummyRegV ) {
    if ( wb.derefLocation.getResultReg() != nullptr )
      wb.mTmpReg = ref( *(wb.derefLocation.getResultReg()) );
    else
      wb.mTmpReg = ref( getNewRegister( exp.getType() ) );
  }
};


/*
  castBackAndStoreCost computes the costs of applying the function
  'castBackAndStore'.
*/
COST castBackAndStoreCost( const IR_Exp &exp )
{
  DSTART( "COST castBackAndStoreCost(const IR_Exp&)" );

  // Check if a conversion is needed (only in compound assignments).
  auto *aexp = dynamic_cast<const IR_AssignExp *>( &exp );
  if ( aexp->getOperator() != IR_AssignExp::ASSIGN ) {
    // TODO: What is wrong with the following lines, why commented out?
    // const IR_Type &currentType = getComputationLevelType( exp );
    // const IR_Type &targetType  = exp.getType();

    // Cast::castingCost( targetType, currentType ) +
    return( RV32_AddressModification::createStoreCost( &getLHS( exp ) ) );
  } else
    return( RV32_AddressModification::createStoreCost( &getLHS( exp ) ) );
};


/*
  castBackAndStore casts the result of an assignment back into its proper format
  (if needed) and moves the result from its temporary register to the result
  register from where the value originally came. Finally, it stores the result
  in memory.
*/
WIR_VirtualRegister *castBackAndStore( const IR_Exp &exp,
                                       RV32_WriteBackInfo &wb )
{
  DSTART(
    "WIR_VirtualRegister* castBackAndStore(const IR_Exp&, "
    "RV32_WriteBackInfo&)" );
  WIR_VirtualRegister &castedResult = wb.getTempReg();

  // After casting, we must move the result back to the variable's register (if
  // it is not already there).
  ufAssertT( wb.derefLocation.getResultReg() != nullptr, "Missing wb.derefLocation.getResultReg()!" );
  WIR_VirtualRegister &movedResult = *(wb.derefLocation.getResultReg());

  if ( movedResult != movedResult )
    RVINSTRUCTIONS.insertMOV(
      dynamic_cast<RV_RegV &>( movedResult ),
      dynamic_cast<RV_RegV &>( castedResult ), const_cast<IR_Exp *>( &exp ) );

  // Store the result if needed.

  if ( wb.writeBackToMemory )
    wb.derefLocation.storeBack( &getLHS( exp ), &movedResult );

  return( &movedResult );
};

WIR_VirtualRegister &RV32_WriteBackInfo::getTempReg( void ) const
{
  DSTART( "WIR_VirtualRegister* WriteBackInfo::getTempReg() const" );

  return( mTmpReg );
};


}       // namespace RV32
