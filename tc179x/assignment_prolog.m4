/*

   This source file belongs to the

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


/*
  Constructor initializing an empty write-back info.
*/
WriteBackInfo::WriteBackInfo( void ) :
  writeBackToMemory { false },
  derefLocation {},
  tempReg { nullptr },
  mTmpReg { ref( dummyDRegV ) }
{
  DSTART( "WriteBackInfo::WriteBackInfo()" );
};


/*
  Constructor initializing only a register-based write-back info.
*/
WriteBackInfo::WriteBackInfo( LLIR_Register *resultRegister,
                              const WIR::WIR_VirtualRegister &rReg ) :
  writeBackToMemory { false },
  derefLocation {},
  tempReg { nullptr },
  mTmpReg { ref( dummyDRegV ) }
{
  DSTART(
    "WriteBackInfo::WriteBackInfo(LLIR_Register*, const WIR_VirtualRegister&)" );

  derefLocation.setResultRegister( resultRegister );
  derefLocation.setResultReg( const_cast<WIR_VirtualRegister *>( &rReg ) );
};


/*
  Constructor initializing a memory-based write-back info.
*/
WriteBackInfo::WriteBackInfo( const TC_LValue &writeTo ) :
  writeBackToMemory { true },
  derefLocation { writeTo },
  tempReg { nullptr },
  mTmpReg { ref( dummyDRegV ) }
{
  DSTART( "WriteBackInfo::WriteBackInfo(const TC_LValue&)" );

  // If the derefLocation does not contain a loaded result, we have to create a
  // new register here that will serve as the result container.
  if ( !derefLocation.getResultRegister() )
    derefLocation.setResultRegister(
      getNewLHSRegister( "", *derefLocation.getBaseType() ) );

  // If the derefLocation does not contain a loaded result, we have to create a
  // new register here that will serve as the result container.
  if ( derefLocation.getResultReg() == nullptr )
    derefLocation.setResultReg(
      &(getNewRegister( *derefLocation.getBaseType() )) );
};


/*
  getTempReg returns the LLIR register to which intermediate results shall be
  written.

  For the LHS of a compound assignment like, e.g.,
  "((long long)i) += ((float)d)", the tempReg for "i" will be a data register
  whereas the ResultReg will be an extended register.
*/
LLIR_Register *WriteBackInfo::getTempReg( void ) const
{
  DSTART( "LLIR_Register* WriteBackInfo::getTempReg() const" );

  return( tempReg );
};


/*
  getResultReg returns the register to which the result shall be written.

  For the LHS of a compound assignment like, e.g.,
  "((long long)i) += ((float)d)", the tempReg for "i" will be a data register
  whereas the ResultReg will be an extended register.
*/
LLIR_Register *WriteBackInfo::getResultReg( void ) const
{
  DSTART( "LLIR_Register* WriteBackInfo::getResultReg() const" );

  return( derefLocation.getResultRegister() );
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
  If the operand is an assignment expression or a child of an assignment
  expression, then this function returns the type to which both of the operands
  must be converted before executing the operation, else this function throws
  an assertion.
*/
const IR_Type &getComputationLevelType( const IR_Exp &exp )
{
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
  } else {
    ufAssertT( 0, "Internal error: Invalid argument!" );
    return( exp.getType() ); // To get rid of compiler warning.
  }
};


/*
  If the expression at the node is an assignment expression, return the
  compilation tree node of the LHS of the assignment, else an assertion is
  thrown.
*/
IR_Exp &getLHS( const IR_Exp &exp )
{
  auto *aexp = dynamic_cast<const IR_AssignExp *>( &exp );
  if ( aexp )
    return( aexp->getLHS() );
  else {
    ufAssertT( 0, "Internal error: Illegal argument!" );
    return( const_cast<IR_Exp &>( exp ) ); // To get rid of compiler warning.
  }
};


/*
  If the expression at the node is an assignment expression, return the
  compilation tree node of the RHS of the assignment, else an assertion is
  thrown.
*/
IR_Exp &getRHS( const IR_Exp &exp )
{
  auto *aexp = dynamic_cast<const IR_AssignExp *>( &exp );
  if ( aexp )
    return( aexp->getRHS() );
  else {
    ufAssertT( 0, "Internal error: Illegal argument!" );
    return( const_cast<IR_Exp &>( exp ) ) ; // To get rid of compiler warning.
  }
};


/*!
  Returns a new LLIR_Register object for register 'target' matching the type
  'type' of the LHS of an assignment expression.
 */
LLIR_Register *getNewLHSRegister( const string &target, const IR_Type &t )
{
  if ( isARegType( t ) )
    return( TCINSTRUCTIONS.CreateRegister( target, true ) );
  else
    return(
      ( t.bitSize() <= 32 ) ?
        TCINSTRUCTIONS.CreateRegister( target ) :
        TCINSTRUCTIONS.CreateERegister( target ) );
};


/*
  lhsConversionCost computes the cost for converting a register value to its
  target type if the register value represents the LHS of an assignment.
*/
COST lhsConversionCost( const IR_Exp &exp )
{
  DSTART( "COST lhsConversionCost(const IR_Exp&)" );

  // Check if conversion is needed (only in compound assignments)
  auto *aexp = dynamic_cast<const IR_AssignExp *>( exp.getParent() );
  ufAssertT( aexp, "Internal error: Illegal argument!" );

  if ( aexp->getOperator() != IR_AssignExp::ASSIGN ) {
    auto &t = getComputationLevelType( exp );
    return(
      ( ( t.bitSize() <= 32 || isARegType( exp ) ) ? 1 : 2 ) *
        TC13::OperationFormat::SDD_1.getSize() +
      Cast::castingCost( t, exp.getType() ) );
  } else
    return( 0 );
};


/*
  doLhsConversion converts a register value to the type that it must have if it
  is the LHS 'exp' of an assignment expression.

  doLhsConversion also sets the temporary variable to use during the assignment
  operations.
*/
void doLhsConversion( const IR_Exp &exp, WriteBackInfo &wb )
{
  DSTART( "void doLhsConversion(const IR_Exp&, WriteBackInfo&)" );

  auto *aexp = dynamic_cast<const IR_AssignExp *>( exp.getParent() );
  ufAssertT( aexp, "Internal error: Illegal argument!" );

  // LLIR
  LLIR_Register *resultReg = wb.getResultReg();

  // Check if conversion is needed (only in compound assignments).
  if ( aexp->getOperator() != IR_AssignExp::ASSIGN ) {
    ufAssertT( resultReg, "Cast is needed, but there is no result to cast!" );

    // Do the conversion.
    const IR_Type &targetType = getComputationLevelType( exp );
    auto *castedReg = Cast::doCasting( targetType, exp.getType(), resultReg );

    // We must move the result into the register for the casted value.
    if ( castedReg )
      wb.tempReg = castedReg;
  }

  // Fill the temporary register field if that was not done by the conversion.
  if ( !wb.tempReg ) {
    if ( resultReg )
      wb.tempReg = resultReg;
    else
      wb.tempReg = getNewLHSRegister( "", exp.getType() );
  }

  // WIR

  // Check if conversion is needed (only in compound assignments).
  if ( aexp->getOperator() != IR_AssignExp::ASSIGN ) {
    ufAssertT(
      wb.derefLocation.getResultReg() != nullptr,
      "Cast is needed, but there is no result to cast!" );

    // Do the conversion.
    const IR_Type &targetType = getComputationLevelType( exp );
    auto &castedReg =
      Cast::doCasting(
        targetType, exp.getType(), *(wb.derefLocation.getResultReg()),
        const_cast<IR_Exp *>( &exp ) );

    // We must move the result into the register for the casted value.
    if ( castedReg.getID() != wb.derefLocation.getResultReg()->getID() )
      wb.mTmpReg = ref( castedReg );
  }

  // Fill the temporary register field if that was not done by the conversion.
  if ( wb.mTmpReg.get() == dummyDRegV ) {
    if ( wb.derefLocation.getResultReg() != nullptr )
      wb.mTmpReg = ref( *(wb.derefLocation.getResultReg()) );
    else
      wb.mTmpReg = ref( getNewRegister( exp.getType() ) );
  }
};


/*
  castBackAndStoreCost returns the cost of applying the function
  'castBackAndStore'.
*/
COST castBackAndStoreCost( const IR_Exp &exp )
{
  DSTART( "COST castBackAndStoreCost(const IR_Exp&)" );

  // Check if conversion is needed (only in compound assignments).
  auto *aexp = dynamic_cast<const IR_AssignExp *>( &exp );
  if ( aexp->getOperator() != IR_AssignExp::ASSIGN ) {
    const IR_Type &currentType = getComputationLevelType( exp );
    const IR_Type &targetType  = exp.getType();

    return(
      Cast::castingCost( targetType, currentType ) +
      TC_AddressModification::createStoreCost( &getLHS( exp ) ) );
  } else
    return( TC_AddressModification::createStoreCost( &getLHS( exp ) ) );
};


/*
  castBackAndStore casts the result of an assignment back into its proper format
  (if needed) and moves the result from its temporary register to the result
  register from where the value originally came. Finally, it stores the result
  in memory.
*/
pair<LLIR_Register *, WIR_VirtualRegister *> castBackAndStore( const IR_Exp &exp,
                                                               WriteBackInfo &wb )
{
  DSTART(
    "pair<LLIR_Register*, WIR_VirtualRegister*> castBackAndStore(const IR_Exp&, WriteBackInfo&)" );

  const IR_Type &targetType = exp.getType();
  const bool useARegOperations =
    ( targetType.getType() == IR_Type::POINTER ) ||
    ( targetType.getType() == IR_Type::ARRAY );
  auto *aexp = dynamic_cast<const IR_AssignExp *>( &exp );

  // Check if conversion is needed (only in compound assignments).
  // WIR
  WIR_VirtualRegister &castedResult =
    ( aexp->getOperator() != IR_AssignExp::ASSIGN ) ?
      Cast::doCasting(
        targetType, getComputationLevelType( exp ), wb.mTmpReg,
        const_cast<IR_Exp *>( &exp ) ) :
      wb.mTmpReg.get();

  // LLIR
  LLIR_Register *castedRes = nullptr;
  if ( aexp->getOperator() != IR_AssignExp::ASSIGN ) {
    // Cast the temporary result back to the target type.
    const IR_Type &currentType = getComputationLevelType( exp );
    castedRes = Cast::doCasting( targetType, currentType, wb.getTempReg() );
  }
  if ( !castedRes )
    castedRes = wb.getTempReg();

  // After casting, we must move the result back to the variable's register (if
  // it is not already there).
  // WIR
  WIR_VirtualRegister &movedResult = *(wb.derefLocation.getResultReg());

  if ( movedResult != castedResult ) {
    if ( useARegOperations )
      TCINSTRUCTIONS.insertMOV_AA(
        dynamic_cast<TC_ARegV &>( movedResult ),
        dynamic_cast<TC_ARegV &>( castedResult ),
        const_cast<IR_Exp *>( &exp ) );
    else

    if ( movedResult.hasChilds() )
      TCINSTRUCTIONS.insertMOV(
        dynamic_cast<TC_ERegV &>( movedResult ),
        dynamic_cast<TC_ERegV &>( castedResult ),
        const_cast<IR_Exp *>( &exp ) );
    else
      TCINSTRUCTIONS.insertMOV(
        dynamic_cast<TC_DRegV &>( movedResult ),
        dynamic_cast<TC_DRegV &>( castedResult ),
        const_cast<IR_Exp *>( &exp ) );
  }

  // LLIR
  LLIR_Register *movedRes = wb.getResultReg();
  if ( string( movedRes->GetName() ) != string( castedRes->GetName() ) ) {
    if ( useARegOperations )
      TCINSTRUCTIONS.insertMOV_AA(
        movedRes, castedRes, const_cast<IR_Exp *>( &exp ) );
    else
      TCINSTRUCTIONS.insertMOV(
        movedRes, castedRes, const_cast<IR_Exp *>( &exp ) );
  }

  // Store the result if needed.
  if ( wb.writeBackToMemory )
    wb.derefLocation.storeBack( &getLHS( exp ), movedRes, &movedResult );

  return( make_pair( movedRes, &movedResult ) );
};
