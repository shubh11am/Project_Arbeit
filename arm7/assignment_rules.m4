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
#  Copyright 2010 - 2022
#
#  Hamburg University of Technology (TUHH)
#  Institute of Embedded Systems
#  21071 Hamburg
#  Germany
#
#  http://www.tuhh.de/es/esd/research/wcc
#
#


ca_lhs_reg: dreg
{
  // Direct conversion "reg" -> "ca_lhs_reg"
  if ( isAssignmentLHS( *$1->getExp() ) )
    // Casting is needed for different reg types (e.g., float/int).
    $cost[0] = $cost[1] + lhsConversionCost( *$1->getExp() );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "ca_lhs_reg: dreg", $1 );

  auto p = $action[1]();

  WriteBackInfo wbInfo( p );
  doLhsConversion( *$1->getExp(), wbInfo );

  return( wbInfo );
};


ca_lhs_reg: deref_dreg
{
  if ( isAssignmentLHS( *$1->getExp() ) )
    // Casting is needed for different reg types (e.g., float/int).
    $cost[0] = $cost[1] + lhsConversionCost( *$1->getExp() );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "ca_lhs_reg: deref_dreg", $1 );

  WriteBackInfo wbInfo( $action[1]( loadResult, false ) );
  doLhsConversion( *$1->getExp(), wbInfo );

  return( wbInfo );
};


ca_result_reg: tpm_AssignExpPLUS( ca_lhs_reg, dreg )
{
  auto &t = getComputationLevelType( *$1->getExp() );

  if ( t.isIntegralType() )
    $cost[0] = $cost[2] + $cost[3] + CT( INS_ADD_32 );
  else

  if ( t.getType() == IR_Type::FLOAT )
    $cost[0] = $cost[2] + $cost[3] + CT( INS_BL_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION(
    "ca_result_reg: tpm_AssignExpPLUS( ca_lhs_reg, dreg )", $1 );

  // Evaluate the arguments.
  auto p = $action[3]();
  auto wbInfo = $action[2]( true );
  auto &t = getComputationLevelType( *$1->getExp() );

  auto *reg = wbInfo.getTempReg();

  // Generate the operation.
  if ( t.isIntegralType() )
    ARMINSTRUCTIONS.insertADD( reg, reg, p, $1->getExp() );
  else
    ARMINSTRUCTIONS.insertADD_F( OPER_AL, reg, reg, p, $1->getExp() );

  return( wbInfo );
};


ca_result_reg: tpm_AssignExpPLUS( ca_lhs_reg, const8 )
{
  $cost[0] = $cost[2] + $cost[3] + CT( INS_ADD_32 );
}
=
{
  DEBUG_RULE_ACTION(
    "ca_result_reg: tpm_AssignExpPLUS( ca_lhs_reg, const8 )", $1 );

  // Evaluate the arguments.
  auto rhs = $action[3]().getIntValue();
  auto wbInfo = $action[2]( true );
  auto *lhsReg = wbInfo.getTempReg();

  // Generate the operation.
  ARMINSTRUCTIONS.insertADD( lhsReg, lhsReg, rhs, $1->getExp() );

  return( wbInfo );
};


ca_result_reg: tpm_AssignExpMINUS( ca_lhs_reg, dreg )
{
  auto &t = getComputationLevelType( *$1->getExp() );

  if ( t.isIntegralType() )
    $cost[0] = $cost[2] + $cost[3] + CT( INS_SUB_32 );
  else

  if ( t.getType() == IR_Type::FLOAT )
    $cost[0] = $cost[2] + $cost[3] + CT( INS_BL_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION(
    "ca_result_reg: tpm_AssignExpMINUS( ca_lhs_reg, dreg )", $1 );

  // Evaluate the arguments.
  auto rhsReg = $action[3]();
  auto wbInfo = $action[2]( true );
  auto &t = getComputationLevelType( *$1->getExp() );

  auto *lhsReg = wbInfo.getTempReg();

  // Generate the operation.
  if ( t.isIntegralType() )
    ARMINSTRUCTIONS.insertSUB( lhsReg, lhsReg, rhsReg, $1->getExp() );
  else
    ARMINSTRUCTIONS.insertSUB_F( OPER_AL, lhsReg, lhsReg, rhsReg, $1->getExp() );

  return( wbInfo );
};


ca_result_reg: tpm_AssignExpMINUS( ca_lhs_reg, const8 )
{
  $cost[0] = $cost[2] + $cost[3] + CT( INS_SUB_32 );
}
=
{
  DEBUG_RULE_ACTION(
    "ca_result_reg: tpm_AssignExpMINUS( ca_lhs_reg, const8 )", $1 );

  // Evaluate the arguments.
  auto rhs = $action[3]().getIntValue();
  auto wbInfo = $action[2]( true );
  auto *lhsReg = wbInfo.getTempReg();

  // Generate the operation.
  ARMINSTRUCTIONS.insertSUB( lhsReg, lhsReg, rhs, $1->getExp() );

  return( wbInfo );
};


ca_result_reg: tpm_AssignExpMULT( ca_lhs_reg, dreg )
{
  auto &t = getComputationLevelType( *$1->getExp() );

  if ( t.isIntegralType() )
    $cost[0] = $cost[2] + $cost[3] + CT( INS_MUL_32 );
  else

  if ( t.getType() == IR_Type::FLOAT )
    $cost[0] = $cost[2] + $cost[3] + CT( INS_BL_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION(
    "ca_result_reg: tpm_AssignExpMULT( ca_lhs_reg, dreg )", $1 );

  // Evaluate the arguments.
  auto p = $action[3]();
  auto wbInfo = $action[2]( true );
  auto &t = getComputationLevelType( *$1->getExp() );

  auto *reg = wbInfo.getTempReg();

  // Generate the operation.
  if ( t.isIntegralType() )
    ARMINSTRUCTIONS.insertMUL( reg, reg, p, $1->getExp() );
  else
    ARMINSTRUCTIONS.insertMUL_F( OPER_AL, reg, reg, p, $1->getExp() );

  return( wbInfo );
};


ca_result_reg: tpm_AssignExpAND( ca_lhs_reg, dreg )
{
  $cost[0] = $cost[2] + $cost[3] + CT( INS_AND_32 );
}
=
{
  DEBUG_RULE_ACTION(
    "ca_result_reg: tpm_AssignExpAND( ca_lhs_reg, dreg )", $1 );

  // Evaluate the arguments.
  auto p = $action[3]();
  auto wbInfo = $action[2]( true );
  auto *lhsReg = wbInfo.getTempReg();

  // Generate the operation.
  ARMINSTRUCTIONS.insertAND( lhsReg, lhsReg, p, $1->getExp() );

  return( wbInfo );
};


ca_result_reg: tpm_AssignExpAND( ca_lhs_reg, const8 )
{
  $cost[0] = $cost[2] + $cost[3] + CT( INS_AND_32 );
}
=
{
  DEBUG_RULE_ACTION(
    "ca_result_reg: tpm_AssignExpAND( ca_lhs_reg, const8 )", $1 );

  // Evaluate the arguments.
  auto rhs = $action[3]().getIntValue();
  auto wbInfo = $action[2]( true );
  auto *lhsReg = wbInfo.getTempReg();

  // Generate the operation.
  ARMINSTRUCTIONS.insertAND( lhsReg, lhsReg, rhs, $1->getExp() );

  return( wbInfo );
};


ca_result_reg: tpm_AssignExpOR( ca_lhs_reg, dreg )
{
  $cost[0] = $cost[2] + $cost[3] + CT( INS_ORR_32 );
}
=
{
  DEBUG_RULE_ACTION(
    "ca_result_reg: tpm_AssignExpOR( ca_lhs_reg, dreg )", $1 );

  // Evaluate the arguments.
  auto rhsReg = $action[3]();
  auto wbInfo = $action[2]( true );
  auto *lhsReg = wbInfo.getTempReg();

  // Generate the operation.
  ARMINSTRUCTIONS.insertORR( lhsReg, lhsReg, rhsReg, $1->getExp() );

  return( wbInfo );
};


ca_result_reg: tpm_AssignExpOR( ca_lhs_reg, const8 )
{
  $cost[0] = $cost[2] + $cost[3] + CT( INS_ORR_32 );
}
=
{
  DEBUG_RULE_ACTION(
    "ca_result_reg: tpm_AssignExpOR( ca_lhs_reg, const8 )", $1 );

  // Evaluate the arguments.
  auto rhs = $action[3]().getIntValue();
  auto wbInfo = $action[2]( true );
  auto *lhsReg = wbInfo.getTempReg();

  // Generate the operation.
  ARMINSTRUCTIONS.insertORR( lhsReg, lhsReg, rhs, $1->getExp() );

  return( wbInfo );
};


ca_result_reg: tpm_AssignExpXOR( ca_lhs_reg, dreg )
{
  $cost[0] = $cost[2] + $cost[3] + CT( INS_EOR_32 );
}
=
{
  DEBUG_RULE_ACTION(
    "ca_result_reg: tpm_AssignExpXOR( ca_lhs_reg, dreg )", $1 );

  // Evaluate the arguments.
  auto rhsReg = $action[3]();
  auto wbInfo = $action[2]( true );
  auto *lhsReg = wbInfo.getTempReg();

  // Generate the operation.
  ARMINSTRUCTIONS.insertEOR( lhsReg, lhsReg, rhsReg, $1->getExp() );

  return( wbInfo );
};


ca_result_reg: tpm_AssignExpXOR( ca_lhs_reg, const8 )
{
  $cost[0] = $cost[2] + $cost[3] + CT( INS_EOR_32 );
}
=
{
  DEBUG_RULE_ACTION(
    "ca_result_reg: tpm_AssignExpXOR( ca_lhs_reg, const8 )", $1 );

  // Evaluate the arguments.
  auto rhs = $action[3]().getIntValue();
  auto wbInfo = $action[2]( true );
  auto *lhsReg = wbInfo.getTempReg();

  // Generate the operation.
  ARMINSTRUCTIONS.insertEOR( lhsReg, lhsReg, rhs, $1->getExp() );

  return( wbInfo );
};


ca_result_reg: tpm_AssignExpSHL( ca_lhs_reg, dreg )
{
  $cost[0] = $cost[2] + $cost[3] + CT( INS_MOV_32 );
}
=
{
  DEBUG_RULE_ACTION(
    "ca_result_reg: tpm_AssignExpSHL( ca_lhs_reg, dreg )", $1 );

  // Evaluate the arguments.
  auto rhsReg = $action[3]();
  auto wbInfo = $action[2]( true );
  auto *lhsReg = wbInfo.getTempReg();

  // Generate the operation.
  ARMINSTRUCTIONS.insertMOV(
    OPER_AL, "", lhsReg, lhsReg, OPER_LSL, rhsReg, $1->getExp() );

  return( wbInfo );
};


ca_result_reg: tpm_AssignExpSHL( ca_lhs_reg, const8 )
{
  $cost[0] = $cost[2] + $cost[3] + CT( INS_MOV_32 );
}
=
{
  DEBUG_RULE_ACTION(
    "ca_result_reg: tpm_AssignExpSHL( ca_lhs_reg, const8 )", $1 );

  // Evaluate the arguments.
  auto rhs = $action[3]().getIntValue();
  auto wbInfo = $action[2]( true );
  auto *lhsReg = wbInfo.getTempReg();

  // Generate the operation.
  ARMINSTRUCTIONS.insertMOV(
    OPER_AL, "", lhsReg, lhsReg, OPER_LSL, rhs, $1->getExp() );

  return( wbInfo );
};


ca_result_reg: tpm_AssignExpSHR( ca_lhs_reg, dreg )
{
  auto &t = getComputationLevelType( *$1->getExp() );

  if ( t.isUnsignedType() )
    $cost[0] = $cost[2] + $cost[3] + CT( INS_MOV_32 );
  else
    $cost[0] = $cost[2] + $cost[3] + 3 * CT( INS_MOV_32 );
}
=
{
  DEBUG_RULE_ACTION(
    "ca_result_reg: tpm_AssignExpSHR( ca_lhs_reg, dreg )", $1 );

  // Evaluate the arguments.
  auto rhsReg = $action[3]();
  auto wbInfo = $action[2]( true );
  auto &t = getComputationLevelType( *$1->getExp() );
  auto *lhsReg = wbInfo.getTempReg();

  // Generate the operation.
  // Check for signed/unsigned to generate arithmetic or logical shift.
  if ( t.isUnsignedType() )
    ARMINSTRUCTIONS.insertMOV(
      OPER_AL, "", lhsReg, lhsReg, OPER_LSR, rhsReg, $1->getExp() );
  else {
    int length = t.bitSize();

    // If the data type is a short/char, we need to position it at the upper
    // boundary to get the correct result with an arithmetic shift.
    if ( length < 32 )
      ARMINSTRUCTIONS.insertMOV(
        OPER_AL, lhsReg, lhsReg, OPER_LSL, 32 - length, $1->getExp() );

    ARMINSTRUCTIONS.insertMOV(
      OPER_AL, "", lhsReg, lhsReg, OPER_ASR, rhsReg, $1->getExp() );

    if ( length < 32 )
      ARMINSTRUCTIONS.insertMOV(
        OPER_AL, lhsReg, lhsReg, OPER_LSR, 32 - length, $1->getExp() );
  }

  return( wbInfo );
};


ca_result_reg: tpm_AssignExpSHR( ca_lhs_reg, const8 )
{
  auto &t = getComputationLevelType( *$1->getExp() );

  if ( t.isUnsignedType() )
    $cost[0] = $cost[2] + $cost[3] + CT( INS_MOV_32 );
  else
    $cost[0] = $cost[2] + $cost[3] + 3 * CT( INS_MOV_32 );
}
=
{
  DEBUG_RULE_ACTION(
    "ca_result_reg: tpm_AssignExpSHR( ca_lhs_reg, const8 )", $1 );

  // Evaluate the arguments.
  auto rhs = $action[3]().getIntValue();
  auto wbInfo = $action[2]( true );
  auto &t = getComputationLevelType( *$1->getExp() );

  auto *lhsReg = wbInfo.getTempReg();

  // Generate the operation.
  // Check for signed/unsigned to generate arithmetic or logical shift.
  if ( t.isUnsignedType() )
    ARMINSTRUCTIONS.insertMOV(
      OPER_AL, "", lhsReg, lhsReg, OPER_LSR, rhs, $1->getExp() );
  else {
    int length = t.bitSize();

    // If the data type is a short/char, we need to position it at the upper
    // boundary to get the correct result with an arithmetic shift.
    if ( length < 32 )
      ARMINSTRUCTIONS.insertMOV(
        OPER_AL, lhsReg, lhsReg, OPER_LSL, 32 - length, $1->getExp() );

    ARMINSTRUCTIONS.insertMOV(
      OPER_AL, "", lhsReg, lhsReg, OPER_ASR, rhs, $1->getExp() );

    if ( length < 32 )
      ARMINSTRUCTIONS.insertMOV(
        OPER_AL, lhsReg, lhsReg, OPER_LSR, 32 - length, $1->getExp() );
  }

  return( wbInfo );
};


ca_result_reg: tpm_AssignExpDIV( ca_lhs_reg, dreg )
{
  auto &t = getComputationLevelType( *$1->getExp() );

  if ( t.isIntegralType() || ( t.getType() == IR_Type::FLOAT ) )
    $cost[0] = $cost[2] + $cost[3] + CT( INS_BL_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION(
    "ca_result_reg: tpm_AssignExpDIV( ca_lhs_reg, dreg )", $1 );

  // Evaluate the arguments.
  auto p = $action[3]();
  auto wbInfo = $action[2]( true );
  auto &t = getComputationLevelType( *$1->getExp() );

  auto *reg = wbInfo.getTempReg();

  if ( t.isIntegralType() ) {
    // Generate a call to the integer divison.
    // Check if it is a signed or unsigned type.
    if ( t.isUnsignedType() )
      ARMINSTRUCTIONS.insertUDIVSI( OPER_AL, reg, reg, p, $1->getExp() );
    else
      ARMINSTRUCTIONS.insertSDIVSI( OPER_AL, reg, reg, p, $1->getExp() );
  } else
    ARMINSTRUCTIONS.insertDIV_F( OPER_AL, reg, reg, p, $1->getExp() );

  return( wbInfo );
};


ca_result_reg: tpm_AssignExpMOD( ca_lhs_reg, dreg )
{
  auto &t = getComputationLevelType( *$1->getExp() );

  if ( t.isIntegralType() )
    $cost[0] = $cost[2] + $cost[3] + CT( INS_BL_32 ) + CT( INS_MOV_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION(
    "ca_result_reg: tpm_AssignExpMOD( ca_lhs_reg, dreg )", $1 );

  // Evaluate the arguments.
  auto p = $action[3]();
  auto wbInfo = $action[2]( true );
  auto &t = getComputationLevelType( *$1->getExp() );
  auto *reg = wbInfo.getTempReg();

  // We need a separate result register, because insert{U,S}MODSI uses the first
  // parameter to save temporary values.
  auto *result = ARMINSTRUCTIONS.CreateRegister( "" );

  // Check if it is a signed or unsigned type.
  if ( t.isUnsignedType() )
    ARMINSTRUCTIONS.insertUMODSI( OPER_AL, result, reg, p, $1->getExp() );
  else
    ARMINSTRUCTIONS.insertSMODSI( OPER_AL, result, reg, p, $1->getExp() );

  // Finally, move the result into the TempReg.
  ARMINSTRUCTIONS.insertMOV( reg, result, $1->getExp() );

  return( wbInfo );
};


############################################################################
#
# Rules for writing and casting (if needed) back the results of assignments
#
############################################################################

dreg: ca_result_reg
{
  $cost[0] = $cost[1] + castBackAndStoreCost( *$1->getExp() );
}
=
{
  DEBUG_RULE_ACTION( "dreg: ca_result_reg", $1 );

  auto wbInfo = $action[1]();

  return( castBackAndStore( *$1->getExp(), wbInfo ) );
};
