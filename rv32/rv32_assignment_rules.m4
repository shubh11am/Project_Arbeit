#
#
#  This source file belongs to the
#
#           Hamburg University of Technology (TUHH)
#             WCC Compiler Framework
#
#  and is property of its respective copyright holder. It must neither be used
#  nor published even in parts without explicit written permission.
#
#  Copyright 2022
#
#  Hamburg University of Technology (TUHH)
#  Institute of Embedded Systems
#  21071 Hamburg
#  Germany
#
#  http://www.tuhh.de/es/esd/research/wcc
#
#


###############################################################################
#
#
# Simple Assignment Expressions
#
#
###############################################################################

ca_result_reg: tpm_AssignExpASSIGN( ca_lhs_reg, reg )
{
  $cost[0] = $cost[2] + $cost[3] + 2 * RV32I::OperationFormat::RR_1.getSize();
}
=
{
  RV32::DEBUG_RULE_ACTION(
    "ca_result_reg: tpm_AssignExpASSIGN( ca_lhs_reg, reg )", $1 );

  // Evaluate RHS first to account for side effects.
  auto &reg3 = $action[3]();
  auto wbInfo = $action[2]( false );

  RVINSTRUCTIONS.insertMOV(
    dynamic_cast<RV_RegV &>( wbInfo.mTmpReg.get() ), reg3, $1->getExp() );

  return( wbInfo );
};


############################################################################
#
# Rules for getting and casting (if needed) the LHS of assignments
#
############################################################################

ca_lhs_reg: reg
{
  // Direct conversion "reg" -> "ca_lhs_reg"
  $cost[0] = $cost[1] + RV32::lhsConversionCost( *$1->getExp() );
}
=
{
  RV32::DEBUG_RULE_ACTION( "ca_lhs_reg: reg", $1 );

  // Avoid compiler warning.
  (void) loadResult;

  auto &reg1 = $action[1]();

  RV32::RV32_WriteBackInfo wbInfo( reg1 );
  RV32::doLhsConversion( *$1->getExp(), wbInfo );

  return( wbInfo );
};


ca_lhs_reg: deref_reg
{
  if ( RV32::isAssignmentLHS( *$1->getExp() ) )
    // Casting is needed for different reg types (e.g., float/int).
    $cost[0] = $cost[1] + RV32::lhsConversionCost( *$1->getExp() );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION( "ca_lhs_reg: deref_reg", $1 );

  RV32::RV32_WriteBackInfo wbInfo( $action[1]( loadResult ) );
  RV32::doLhsConversion( *$1->getExp(), wbInfo );

  return( wbInfo );
};


############################################################################
#
# Rules for writing and casting (if needed) back the results of assignments
#
############################################################################

reg: ca_result_reg
{
  $cost[0] = $cost[1] + RV32::castBackAndStoreCost( *$1->getExp() );
}
=
{
  RV32::DEBUG_RULE_ACTION( "reg: ca_result_reg", $1 );

  auto wbInfo = $action[1]();

  auto p = RV32::castBackAndStore( *$1->getExp(), wbInfo );

  return( dynamic_cast<RV_RegV &>( *p ) );
};


###############################################################################
#
#
# Update-Assignment Expressions
#
#
###############################################################################

###############################################################################
#
# Update-Assignment +=
#
###############################################################################

ca_result_reg: tpm_AssignExpPLUS( ca_lhs_reg, reg )
{
  $cost[0] = $cost[2] + $cost[3] + RV32I::OperationFormat::RRR_1.getSize();
}
=
{
  RV32::DEBUG_RULE_ACTION(
    "ca_result_reg: tpm_AssignExpPLUS( ca_lhs_reg, reg )", $1 );

  // Evaluate RHS first to account for side effects and lower register pressure.
  auto &reg3 = $action[3]();
  auto wbInfo = $action[2]( true );

  auto &r = dynamic_cast<RV_RegV &>( wbInfo.mTmpReg.get() );
  RVINSTRUCTIONS.insertADD( r, r, reg3, $1->getExp() );

  return( wbInfo );
};


ca_result_reg: tpm_AssignExpPLUS( ca_lhs_reg, const12 )
{
  if ( $0->getExp()->getType().isIntegralType() )
    $cost[0] = $cost[2] + $cost[3] + RV32I::OperationFormat::RRC12_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION(
    "ca_result_reg: tpm_AssignExpPLUS( ca_lhs_reg, const12 )", $1 );

  auto wbInfo = $action[2]( true );
  auto v = $action[3]().getIntValue();

  auto &r = dynamic_cast<RV_RegV &>( wbInfo.mTmpReg.get() );
  RVINSTRUCTIONS.insertADDI( r, r, v, $1->getExp() );

  return( wbInfo );
};


###############################################################################
#
# Update-Assignment -=
#
###############################################################################

ca_result_reg: tpm_AssignExpMINUS( ca_lhs_reg, reg )
{
  $cost[0] =
    $cost[2] + $cost[3] + RV32I::OperationFormat::RRR_1.getSize();
}
=
{
  RV32::DEBUG_RULE_ACTION(
    "ca_result_reg: tpm_AssignExpMINUS( ca_lhs_reg, reg )", $1 );

  // Evaluate RHS first to account for side effects and lower register pressure.
  auto &reg3 = $action[3]();
  auto wbInfo = $action[2]( true );

  auto &r = dynamic_cast<RV_RegV &>( wbInfo.mTmpReg.get() );
  RVINSTRUCTIONS.insertSUB( r, r, reg3, $1->getExp() );

  return( wbInfo );
};


ca_result_reg: tpm_AssignExpMINUS( ca_lhs_reg, const12 )
{
  if ( $0->getExp()->getType().isIntegralType() )
    $cost[0] = $cost[2] + $cost[3] + RV32I::OperationFormat::RRC12_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION(
    "ca_result_reg: tpm_AssignExpMINUS( ca_lhs_reg, const12 )", $1 );

  auto wbInfo = $action[2]( true );
  auto v = $action[3]().getIntValue();

  auto &r = dynamic_cast<RV_RegV &>( wbInfo.mTmpReg.get() );
  RVINSTRUCTIONS.insertADDI( r, r, -v, $1->getExp() );

  return( wbInfo );
};


###############################################################################
#
# Update-Assignment *=
#
###############################################################################

ca_result_reg: tpm_AssignExpMULT( ca_lhs_reg, reg )
{
  $cost[0] =
    $cost[2] + $cost[3] + RV32I::OperationFormat::RRR_1.getSize();
}
=
{
  RV32::DEBUG_RULE_ACTION(
    "ca_result_reg: tpm_AssignExpMULT( ca_lhs_reg, reg )", $1 );

  // Evaluate RHS first to account for side effects and lower register pressure.
  auto &reg3 = $action[3]();
  auto wbInfo = $action[2]( true );

  auto &r = dynamic_cast<RV_RegV &>( wbInfo.mTmpReg.get() );
  RVINSTRUCTIONS.insertMUL( r, r, reg3, $1->getExp() );

  return( wbInfo );
};


###############################################################################
#
# Update-Assignment /=
#
###############################################################################

ca_result_reg: tpm_AssignExpDIV( ca_lhs_reg, reg )
{
  $cost[0] =
    $cost[2] + $cost[3] + RV32I::OperationFormat::RRR_1.getSize();
}
=
{
  RV32::DEBUG_RULE_ACTION(
    "ca_result_reg: tpm_AssignExpDIV( ca_lhs_reg, reg )", $1 );

  // Evaluate RHS first to account for side effects and lower register pressure.
  auto &reg3 = $action[3]();
  auto wbInfo = $action[2]( true );

  auto &r = dynamic_cast<RV_RegV &>( wbInfo.mTmpReg.get() );
  RVINSTRUCTIONS.insertDIV( r, r, reg3, $1->getExp() );

  return( wbInfo );
};


###############################################################################
#
# Update-Assignment %=
#
###############################################################################

ca_result_reg: tpm_AssignExpMOD( ca_lhs_reg, reg )
{
  $cost[0] =
    $cost[2] + $cost[3] + RV32I::OperationFormat::RRR_1.getSize();
}
=
{
  RV32::DEBUG_RULE_ACTION(
    "ca_result_reg: tpm_AssignExpMOD( ca_lhs_reg, reg )", $1 );

  // Evaluate RHS first to account for side effects and lower register pressure.
  auto &reg3 = $action[3]();
  auto wbInfo = $action[2]( true );

  const IR_Type &t = RV32::getComputationLevelType( *$1->getExp() );
  const bool isUnsigned = t.isUnsignedType();

  auto &r = dynamic_cast<RV_RegV &>( wbInfo.mTmpReg.get() );

  if ( isUnsigned )
    RVINSTRUCTIONS.insertREMU( r, r, reg3, $1->getExp() );
  else
    RVINSTRUCTIONS.insertREM( r, r, reg3, $1->getExp() );

  return( wbInfo );
};


###############################################################################
#
# Update-Assignment &=
#
###############################################################################

ca_result_reg: tpm_AssignExpAND( ca_lhs_reg, reg )
{
  $cost[0] = $cost[2] + $cost[3] + RV32I::OperationFormat::RRR_1.getSize();
}
=
{
  RV32::DEBUG_RULE_ACTION(
    "ca_result_reg: tpm_AssignExpAND( ca_lhs_reg, reg )", $1 );

  // Evaluate RHS first to account for side effects and lower register pressure.
  auto &reg3 = $action[3]();
  auto wbInfo = $action[2]( true );

  auto &r = dynamic_cast<RV_RegV &>( wbInfo.mTmpReg.get() );
  RVINSTRUCTIONS.insertAND( r, r, reg3, $1->getExp() );

  return( wbInfo );
};


ca_result_reg: tpm_AssignExpAND( ca_lhs_reg, const12 )
{
  $cost[0] = $cost[2] + $cost[3] + RV32I::OperationFormat::RRC12_1.getSize();
}
=
{
  RV32::DEBUG_RULE_ACTION(
    "ca_result_reg: tpm_AssignExpAND( ca_lhs_reg, const12 )", $1 );

  auto wbInfo = $action[2]( true );
  auto v = $action[3]().getIntValue();

  auto &r = dynamic_cast<RV_RegV &>( wbInfo.mTmpReg.get() );
  RVINSTRUCTIONS.insertANDI( r, r, v, $1->getExp() );

  return( wbInfo );
};


###############################################################################
#
# Update-Assignment |=
#
###############################################################################

ca_result_reg: tpm_AssignExpOR( ca_lhs_reg, reg )
{
  $cost[0] = $cost[2] + $cost[3] + RV32I::OperationFormat::RRR_1.getSize();
}
=
{
  RV32::DEBUG_RULE_ACTION(
    "ca_result_reg: tpm_AssignExpOR( ca_lhs_reg, reg )", $1 );

  // Evaluate RHS first to account for side effects and lower register pressure.
  auto &reg3 = $action[3]();
  auto wbInfo = $action[2]( true );

  auto &r = dynamic_cast<RV_RegV &>( wbInfo.mTmpReg.get() );
  RVINSTRUCTIONS.insertOR( r, r, reg3, $1->getExp() );

  return( wbInfo );
};


ca_result_reg: tpm_AssignExpOR( ca_lhs_reg, const12 )
{
  $cost[0] = $cost[2] + $cost[3] + RV32I::OperationFormat::RRC12_1.getSize();
}
=
{
  RV32::DEBUG_RULE_ACTION(
    "ca_result_reg: tpm_AssignExpOR( ca_lhs_reg, const12 )", $1 );

  auto wbInfo = $action[2]( true );
  auto v = $action[3]().getIntValue();

  auto &r = dynamic_cast<RV_RegV &>( wbInfo.mTmpReg.get() );
  RVINSTRUCTIONS.insertORI( r, r, v, $1->getExp() );

  return( wbInfo );
};


###############################################################################
#
# Update-Assignment ^=
#
###############################################################################

ca_result_reg: tpm_AssignExpXOR( ca_lhs_reg, reg )
{
  $cost[0] = $cost[2] + $cost[3] + RV32I::OperationFormat::RRR_1.getSize();
}
=
{
  RV32::DEBUG_RULE_ACTION(
    "ca_result_reg: tpm_AssignExpXOR( ca_lhs_reg, reg )", $1 );

  // Evaluate RHS first to account for side effects and lower register pressure.
  auto &reg3 = $action[3]();
  auto wbInfo = $action[2]( true );

  auto &r = dynamic_cast<RV_RegV &>( wbInfo.mTmpReg.get() );
  RVINSTRUCTIONS.insertXOR( r, r, reg3, $1->getExp() );

  return( wbInfo );
};


ca_result_reg: tpm_AssignExpXOR( ca_lhs_reg, const12 )
{
  $cost[0] = $cost[2] + $cost[3] + RV32I::OperationFormat::RRC12_1.getSize();
}
=
{
  RV32::DEBUG_RULE_ACTION(
    "ca_result_reg: tpm_AssignExpXOR( ca_lhs_reg, const12 )", $1 );

  auto wbInfo = $action[2]( true );
  auto v = $action[3]().getIntValue();

  auto &r = dynamic_cast<RV_RegV &>( wbInfo.mTmpReg.get() );
  RVINSTRUCTIONS.insertXORI( r, r, v, $1->getExp() );

  return( wbInfo );
};


###############################################################################
#
# Update-Assignment <<=
#
###############################################################################

ca_result_reg: tpm_AssignExpSHL( ca_lhs_reg, reg )
{
  if ( $2->getExp()->getType().isIntegralType() )
    $cost[0] = $cost[2] + $cost[3] + RV32I::OperationFormat::RRR_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION(
    "ca_result_reg: tpm_AssignExpSHL( ca_lhs_reg, reg )", $1 );

  // Evaluate RHS first to account for side effects and lower register pressure.
  auto &reg3 = $action[3]();
  auto wbInfo = $action[2]( true );

  auto &r = dynamic_cast<RV_RegV &>( wbInfo.mTmpReg.get() );
  RVINSTRUCTIONS.insertSLL( r, r, reg3, $1->getExp() );

  return( wbInfo );
};


ca_result_reg: tpm_AssignExpSHL( ca_lhs_reg, uconst5 )
{
  IR_Type &r = RV32::effectiveType( *$2->getExp() );

  if ( r.isIntegralType() )
    $cost[0] = $cost[2] + $cost[3] + RV32I::OperationFormat::RRC5_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION(
    "ca_result_reg: tpm_AssignExpSHL( ca_lhs_reg, uconst5 )", $1 );

  auto wbInfo = $action[2]( true );
  auto v = $action[3]().getIntValue();

  auto &r = dynamic_cast<RV_RegV &>( wbInfo.mTmpReg.get() );
  RVINSTRUCTIONS.insertSLLI( r, r, v, $1->getExp() );

  return( wbInfo );
};


###############################################################################
#
# Update-Assignment >>=
#
###############################################################################

ca_result_reg: tpm_AssignExpSHR( ca_lhs_reg, reg )
{
  $cost[0] = $cost[2] + $cost[3] + RV32I::OperationFormat::RRR_1.getSize();
}
=
{
  RV32::DEBUG_RULE_ACTION(
    "ca_result_reg: tpm_AssignExpSHR( ca_lhs_reg, reg )", $1 );

  // Evaluate RHS first to account for side effects and lower register pressure.
  auto &reg3 = $action[3]();
  auto wbInfo = $action[2]( true );

  auto t = RV32::effectiveType( *$2->getExp() ).getType();

  auto &r = dynamic_cast<RV_RegV &>( wbInfo.mTmpReg.get() );

  if ( RVIR_CONFIGURATION->arithmeticSHR )
    if ( t == IR_Type::UNSIGNED_INT )
      RVINSTRUCTIONS.insertSRL( r, r, reg3, $1->getExp() );
    else
      RVINSTRUCTIONS.insertSRA( r, r, reg3, $1->getExp() );
  else
    RVINSTRUCTIONS.insertSRL( r, r, reg3, $1->getExp() );

  return( wbInfo );
};


ca_result_reg: tpm_AssignExpSHR( ca_lhs_reg, uconst5 )
{
  $cost[0] = $cost[2] + $cost[3] + RV32I::OperationFormat::RRC5_1.getSize();
}
=
{
  RV32::DEBUG_RULE_ACTION(
    "ca_result_reg: tpm_AssignExpSHR( ca_lhs_reg, uconst5 )", $1 );

  auto wbInfo = $action[2]( true );
  auto v = $action[3]().getIntValue();

  auto t = RV32::effectiveType( *$2->getExp() ).getType();

  auto &r = dynamic_cast<RV_RegV &>( wbInfo.mTmpReg.get() );
  if ( RVIR_CONFIGURATION->arithmeticSHR )
    if ( t == IR_Type::UNSIGNED_INT )
      RVINSTRUCTIONS.insertSRLI( r, r, v, $1->getExp() );
    else
      RVINSTRUCTIONS.insertSRAI( r, r, v, $1->getExp() );
  else
    RVINSTRUCTIONS.insertSRLI( r, r, v, $1->getExp() );

  return( wbInfo );
};


###############################################################################
#
#
# pointer rules
#
#
###############################################################################


areg: ca_result_areg
{
  if ( RV32::isPointerType( $1->getExp()->getType() ) )
    $cost[0] = $cost[1] + RV32::castBackAndStoreCost( *$1->getExp() );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION( "areg: ca_result_areg", $1 );

  auto wbInfo = $action[1]();
  auto p = RV32::castBackAndStore( *$1->getExp(), wbInfo );

  return( dynamic_cast<RV_RegV &>( *p ) );
};


ca_result_areg: tpm_AssignExpASSIGN( ca_lhs_areg, areg )
{
  if ( RV32::isPointerType( *$2->getExp() ) &&
       dynamic_cast<IR_StringConstExp *>( $3->getExp() ) &&
       !RV32::isFunctionArgument( *$2->getExp() ) )
    $cost[0] = COST_INFINITY;
  else
    $cost[0] = $cost[2] + $cost[3] + RV32I::OperationFormat::RR_1.getSize();
}
=
{
  RV32::DEBUG_RULE_ACTION(
    "ca_result_areg: tpm_AssignExpASSIGN( ca_lhs_areg, areg )", $1 );

  // Evaluate RHS first to account for side effects.
  auto &p = $action[3]();
  auto wbInfo = $action[2]( false );

  RVINSTRUCTIONS.insertMOV(
    dynamic_cast<RV_RegV &>( wbInfo.mTmpReg.get() ), p, $1->getExp() );

  return( wbInfo );
};


ca_lhs_areg: areg
{
  // This rule performs the direct conversion "areg" -> "ca_lhs_areg".
  if ( RV32::isAssignmentLHS( *$1->getExp() ) &&
       RV32::isARegType( RV32::getComputationLevelType( *$1->getExp() ) ) )
    $cost[0] = $cost[1] + RV32::lhsConversionCost( *$1->getExp() );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION( "ca_lhs_areg: areg", $1 );
  (void) loadResult;
  // Avoid compiler warning.

  auto &reg1 = $action[1]();
  RV32::RV32_WriteBackInfo wbInfo( reg1 );
  RV32::doLhsConversion( *$1->getExp(), wbInfo );

  return( wbInfo );
};


ca_lhs_areg: deref_areg
{
  // Direct conversion "deref_areg" -> "ca_lhs_areg".

  if ( RV32::isAssignmentLHS( *$1->getExp() ) &&
       RV32::isARegType( RV32::getComputationLevelType( *$1->getExp() ) ) )
    $cost[0] = $cost[1] + RV32::lhsConversionCost( *$1->getExp() );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION( "ca_lhs_areg: deref_areg", $1 );

  RV32::RV32_WriteBackInfo wbInfo( $action[1]( loadResult ) );
  RV32::doLhsConversion( *$1->getExp(), wbInfo );

  return( wbInfo );
};


ca_result_areg: tpm_AssignExpPLUS( ca_lhs_areg, addrOffset )
{
  $cost[0] = $cost[2] + $cost[3] + RV32I::OperationFormat::RRC12_1.getSize();
}
=
{
  RV32::DEBUG_RULE_ACTION(
    "ca_result_areg: tpm_AssignExpPLUS( ca_lhs_areg, addrOffset )", $1 );

  auto wbInfo = $action[2]( true );
  auto *t = RV32::getBaseType( $2->getExp()->getType() );
  int off =
    $action[3]().getIntValue() * RV32::computeSizeOf( t );

  // done as MOVConstant + ADD since addi does not handle large numbers well
  auto &temp = RVINSTRUCTIONS.createReg();
  RVINSTRUCTIONS.insertMOVConstant( temp, off, $1->getExp() );

  auto &r = dynamic_cast<RV_RegV &>( wbInfo.mTmpReg.get() );
  RVINSTRUCTIONS.insertADD( r, r, temp, $1->getExp() );

  return( wbInfo );
};


ca_result_areg: tpm_AssignExpPLUS( ca_lhs_areg, reg )
{
  $cost[0] =
    $cost[2] + $cost[3];
}
=
{
  RV32::DEBUG_RULE_ACTION(
    "ca_result_areg: tpm_AssignExpPLUS( ca_lhs_areg, reg )", $1 );

  auto *t = RV32::getBaseType( $2->getExp()->getType() );
  const int byteSize = RV32::computeSizeOf( t );

  // Evaluate RHS first to account for side effects and lower register pressure.
  auto &p = $action[3]();
  auto wbInfo = $action[2]( true );

  auto &r = dynamic_cast<RV_RegV &>( wbInfo.mTmpReg.get() );

  auto &tmpReg1 = RVINSTRUCTIONS.createReg();
  auto &tmpReg2 = RVINSTRUCTIONS.createReg();

  // inefficient should be done via addressmodification, 
  // and use modified_areg, but this produces disfunct assembley
  RVINSTRUCTIONS.insertMOV(tmpReg1, p, $1->getExp());
  RVINSTRUCTIONS.insertMOVConstant(tmpReg2, byteSize, $1->getExp());

  RVINSTRUCTIONS.insertMUL(tmpReg1, tmpReg1, tmpReg2, $1->getExp());
  RVINSTRUCTIONS.insertADD(r, r, tmpReg1, $1->getExp());

  return( wbInfo );
};


ca_result_areg: tpm_AssignExpMINUS( ca_lhs_areg, addrOffset )
{
  $cost[0] = $cost[2] + $cost[3] + RV32I::OperationFormat::RRC12_1.getSize();
}
=
{
  RV32::DEBUG_RULE_ACTION(
    "ca_result_areg: tpm_AssignExpMINUS( ca_lhs_areg, addrOffset )", $1 );

  auto wbInfo = $action[2]( true );
 auto *t = RV32::getBaseType( $2->getExp()->getType() );
  int off =
    -$action[3]().getIntValue() * RV32::computeSizeOf( t );

  // done as MOVConstant + ADD since addi does not handle large numbers well
  auto &temp = RVINSTRUCTIONS.createReg();
  RVINSTRUCTIONS.insertMOVConstant( temp, off, $1->getExp() );

  auto &r = dynamic_cast<RV_RegV &>( wbInfo.mTmpReg.get() );
  RVINSTRUCTIONS.insertADD( r, r, temp, $1->getExp() );

  return( wbInfo );
};


ca_result_areg: tpm_AssignExpMINUS( ca_lhs_areg, reg )
{
  $cost[0] =
    $cost[2] + $cost[3];
}
=
{
  RV32::DEBUG_RULE_ACTION(
    "ca_result_areg: tpm_AssignExpMINUS( ca_lhs_areg, reg )", $1 );

  auto *t = RV32::getBaseType( $2->getExp()->getType() );
  const int byteSize = RV32::computeSizeOf( t );

  // Evaluate RHS first to account for side effects and lower register pressure.
  auto &p = $action[3]();
  auto wbInfo = $action[2]( true );

  auto &r = dynamic_cast<RV_RegV &>( wbInfo.mTmpReg.get() );

  auto &tmpReg1 = RVINSTRUCTIONS.createReg();
  auto &tmpReg2 = RVINSTRUCTIONS.createReg();

  // inefficient should be done via addressmodification, 
  // and use modified_areg, but this produces disfunct assembley
  RVINSTRUCTIONS.insertMOV(tmpReg1, p, $1->getExp());
  RVINSTRUCTIONS.insertMOVConstant(tmpReg2, byteSize, $1->getExp());

  RVINSTRUCTIONS.insertMUL(tmpReg1, tmpReg1, tmpReg2, $1->getExp());
  RVINSTRUCTIONS.insertSUB(r, r, tmpReg1, $1->getExp());

  return( wbInfo );
};