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
#  Copyright 2021 - 2022
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
#  Less-than operations
#
#
###############################################################################

rel: tpm_BinaryExpLT( reg, reg )
{
  $cost[0] = $cost[2] + $cost[3];

  IR_Type &operandType = RV32::effectiveType( *$2->getExp() );

  if ( operandType.isIntegralType() )
    $cost[0] += RV32I::OperationFormat::RRL_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION( "rel: tpm_BinaryExpLT( reg, reg )", $1 );

  auto &reg2 = $action[2]();
  auto &reg3 = $action[3]();

  IR_Type &operandType = RV32::effectiveType( *$2->getExp() );

  if ( operandType.isUnsignedType() )
    RVINSTRUCTIONS.insertBLTU( reg2, reg3, b, $1->getExp());
  else
    RVINSTRUCTIONS.insertBLT( reg2, reg3, b, $1->getExp());

  if ( markLoopExit ) {
    WIR_Operation &jmp = RV32::RV32_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( false ) );
  }

  return;
};


nrel: tpm_BinaryExpLT( reg, reg )
{
  $cost[0] = $cost[2] + $cost[3];

  IR_Type &operandType = RV32::effectiveType( *$2->getExp() );

  if ( operandType.isIntegralType() )
    $cost[0] += RV32I::OperationFormat::RRL_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION( "nrel: tpm_BinaryExpLT( reg, reg )", $1 );

  auto &reg2 = $action[2]();
  auto &reg3 = $action[3]();

  IR_Type &operandType = RV32::effectiveType( *$2->getExp() );

  if ( operandType.isUnsignedType() )
    RVINSTRUCTIONS.insertBGEU( reg2, reg3, b, $1->getExp());
  else
    RVINSTRUCTIONS.insertBGE( reg2, reg3, b, $1->getExp());

  if ( markLoopExit ) {
    WIR_Operation &jmp = RV32::RV32_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( true ) );
  }

  return;
};


reg: tpm_BinaryExpLT( reg, reg )
{
  $cost[0] = $cost[2] + $cost[3];

  IR_Type &operandType = RV32::effectiveType( *$2->getExp() );

  if ( operandType.isIntegralType() )
    $cost[0] += RV32I::OperationFormat::RRR_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION( "reg: tpm_BinaryExpLT( reg, reg )", $1 );

  auto &reg2 = $action[2]();
  auto &reg3 = $action[3]();

  auto &r = RVINSTRUCTIONS.createReg();
  IR_Type &operandType = RV32::effectiveType( *$2->getExp() );

  if ( operandType.isUnsignedType() )
    RVINSTRUCTIONS.insertSLTU( r, reg2, reg3, $1->getExp() );
  else
    RVINSTRUCTIONS.insertSLT( r, reg2, reg3, $1->getExp() );

  return( r );
};


reg: tpm_BinaryExpLT( reg, const12 )
{
  if ( $2->getExp()->getType().isIntegralType() )
    $cost[0] = $cost[2] + $cost[3] + RV32I::OperationFormat::RRC12_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION( "reg: tpm_BinaryExpLT( reg, const12 )", $1 );

  auto &reg2 = $action[2]();
  auto v = $action[3]().getIntValue();

  auto &r = RVINSTRUCTIONS.createReg();
  IR_Type &operandType = RV32::effectiveType( *$2->getExp() );

  if ( operandType.isUnsignedType() )
    RVINSTRUCTIONS.insertSLTIU( r, reg2, v, $1->getExp() );
  else
    RVINSTRUCTIONS.insertSLTI( r, reg2, v, $1->getExp() );

  return( r );
};


###############################################################################
#
#
#  Greater-than operations
#
#
###############################################################################

rel: tpm_BinaryExpGT( reg, reg )
{
  $cost[0] = $cost[2] + $cost[3];

  IR_Type &operandType = RV32::effectiveType( *$2->getExp() );

  if ( operandType.isIntegralType() )
    $cost[0] += RV32I::OperationFormat::RRL_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION( "rel: tpm_BinaryExpGT( reg, reg )", $1 );

  auto &reg2 = $action[2]();
  auto &reg3 = $action[3]();

  IR_Type &operandType = RV32::effectiveType( *$2->getExp() );

  if ( operandType.isUnsignedType() )
    RVINSTRUCTIONS.insertBGTU( reg2, reg3, b, $1->getExp());
  else
    RVINSTRUCTIONS.insertBGT( reg2, reg3, b, $1->getExp());

  if ( markLoopExit ) {
    WIR_Operation &jmp = RV32::RV32_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( false ) );
  }

  return;
};


nrel: tpm_BinaryExpGT( reg, reg )
{
  $cost[0] = $cost[2] + $cost[3];

  IR_Type &operandType = RV32::effectiveType( *$2->getExp() );

  if ( operandType.isIntegralType() )
    $cost[0] += RV32I::OperationFormat::RRL_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION( "nrel: tpm_BinaryExpGT( reg, reg )", $1 );

  auto &reg2 = $action[2]();
  auto &reg3 = $action[3]();

  IR_Type &operandType = RV32::effectiveType( *$2->getExp() );

  if ( operandType.isUnsignedType() )
    RVINSTRUCTIONS.insertBLEU( reg2, reg3, b, $1->getExp());
  else
    RVINSTRUCTIONS.insertBLE( reg2, reg3, b, $1->getExp());

  if ( markLoopExit ) {
    WIR_Operation &jmp = RV32::RV32_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( true ) );
  }

  return;
};


reg: tpm_BinaryExpGT( reg, reg )
{
  $cost[0] = $cost[2] + $cost[3];

  IR_Type &operandType = RV32::effectiveType( *$2->getExp() );

  if ( operandType.isIntegralType() )
    $cost[0] += RV32I::OperationFormat::RRR_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION( "reg: tpm_BinaryExpGT( reg, reg )", $1 );

  auto &reg2 = $action[2]();
  auto &reg3 = $action[3]();

  auto &r = RVINSTRUCTIONS.createReg();
  IR_Type &operandType = RV32::effectiveType( *$2->getExp() );

  if ( operandType.isUnsignedType() )
    RVINSTRUCTIONS.insertSLTU( r, reg3, reg2, $1->getExp() );
  else
    RVINSTRUCTIONS.insertSLT( r, reg3, reg2, $1->getExp() );

  return( r );
};


reg: tpm_BinaryExpGT( const12, reg )
{
  if ( $3->getExp()->getType().isIntegralType() )
    $cost[0] = $cost[2] + $cost[3] + RV32I::OperationFormat::RRC12_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION( "reg: tpm_BinaryExpGT( const12, reg )", $1 );

  auto &reg3 = $action[3]();
  auto v = $action[2]().getIntValue();

  auto &r = RVINSTRUCTIONS.createReg();
  IR_Type &operandType = RV32::effectiveType( *$2->getExp() );

  if ( operandType.isUnsignedType() )
    RVINSTRUCTIONS.insertSLTIU( r, reg3, v, $1->getExp() );
  else
    RVINSTRUCTIONS.insertSLTI( r, reg3, v, $1->getExp() );

  return( r );
};


###############################################################################
#
#
#  Less-than-or-equal operations
#
#
###############################################################################

rel: tpm_BinaryExpLEQ( reg, reg )
{
  $cost[0] = $cost[2] + $cost[3];

  IR_Type &operandType = RV32::effectiveType( *$2->getExp() );

  if ( operandType.isIntegralType() )
    $cost[0] += RV32I::OperationFormat::RRL_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION( "rel: tpm_BinaryExpLEQ( reg, reg )", $1 );

  auto &reg2 = $action[2]();
  auto &reg3 = $action[3]();

  IR_Type &operandType = RV32::effectiveType( *$2->getExp() );

  if ( operandType.isUnsignedType() )
    RVINSTRUCTIONS.insertBLEU( reg2, reg3, b, $1->getExp());
  else
    RVINSTRUCTIONS.insertBLE( reg2, reg3, b, $1->getExp());

  if ( markLoopExit ) {
    WIR_Operation &jmp = RV32::RV32_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( false ) );
  }

  return;
};


nrel: tpm_BinaryExpLEQ( reg, reg )
{
  $cost[0] = $cost[2] + $cost[3];

  IR_Type &operandType = RV32::effectiveType( *$2->getExp() );

  if ( operandType.isIntegralType() )
    $cost[0] += RV32I::OperationFormat::RRL_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION( "nrel: tpm_BinaryExpLEQ( reg, reg )", $1 );

  auto &reg2 = $action[2]();
  auto &reg3 = $action[3]();

  IR_Type &operandType = RV32::effectiveType( *$2->getExp() );

  if ( operandType.isUnsignedType() )
    RVINSTRUCTIONS.insertBGTU( reg2, reg3, b, $1->getExp());
  else
    RVINSTRUCTIONS.insertBGT( reg2, reg3, b, $1->getExp());

  if ( markLoopExit ) {
    WIR_Operation &jmp = RV32::RV32_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( true ) );
  }

  return;
};


reg: tpm_BinaryExpLEQ( reg, reg )
{
  $cost[0] = $cost[2] + $cost[3];

  IR_Type &operandType = RV32::effectiveType( *$2->getExp() );

  if ( operandType.isIntegralType() )
    $cost[0] += RV32I::OperationFormat::RRR_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION( "reg: tpm_BinaryExpLEQ( reg, reg )", $1 );

  auto &reg2 = $action[2]();
  auto &reg3 = $action[3]();

  auto &r = RVINSTRUCTIONS.createReg();
  RVINSTRUCTIONS.insertADDI( r, reg3, 1, $1->getExp() );

  RVINSTRUCTIONS.insertSLT( r, reg2, r, $1->getExp() );

  return( r );
};


reg: tpm_BinaryExpLEQ( const12, reg )
{
  if ( $2->getExp()->getType().isIntegralType() )
    $cost[0] =
      $cost[2] + $cost[3] + 2 * RV32I::OperationFormat::RRC12_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION( "reg: tpm_BinaryExpLEQ( const12, reg )", $1 );

  auto &reg3 = $action[3]();
  auto v = $action[2]().getIntValue();

  auto &r = RVINSTRUCTIONS.createReg();
  IR_Type &operandType = RV32::effectiveType( *$2->getExp() );

  if ( operandType.isUnsignedType() )
    RVINSTRUCTIONS.insertSLTIU( r, reg3, v, $1->getExp() );
  else
    RVINSTRUCTIONS.insertSLTI( r, reg3, v, $1->getExp() );

  RVINSTRUCTIONS.insertXORI( r, r, 1, $1->getExp() );

  return( r );
};


reg: tpm_BinaryExpLEQ( reg, const12 )
{
  if ( $2->getExp()->getType().isIntegralType() )
    $cost[0] = $cost[2] + $cost[3] + RV32I::OperationFormat::RRC12_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION( "reg: tpm_BinaryExpLEQ( reg, const12 )", $1 );

  auto &reg2 = $action[2]();
  auto v = $action[3]().getIntValue() + 1;

  auto &r = RVINSTRUCTIONS.createReg();
  IR_Type &operandType = RV32::effectiveType( *$2->getExp() );

  if ( operandType.isUnsignedType() )
    RVINSTRUCTIONS.insertSLTIU( r, reg2, v, $1->getExp() );
  else
    RVINSTRUCTIONS.insertSLTI( r, reg2, v, $1->getExp() );

  return( r );
};


###############################################################################
#
#
#  Greater-than-or-equal operations
#
#
###############################################################################

rel: tpm_BinaryExpGEQ( reg, reg )
{
  $cost[0] = $cost[2] + $cost[3];

  IR_Type &operandType = RV32::effectiveType( *$2->getExp() );

  if ( operandType.isIntegralType() )
    $cost[0] += RV32I::OperationFormat::RRL_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION( "rel: tpm_BinaryExpGEQ( reg, reg )", $1 );

  auto &reg2 = $action[2]();
  auto &reg3 = $action[3]();

  IR_Type &operandType = RV32::effectiveType( *$2->getExp() );

  if ( operandType.isUnsignedType() )
    RVINSTRUCTIONS.insertBGEU( reg2, reg3, b, $1->getExp());
  else
    RVINSTRUCTIONS.insertBGE( reg2, reg3, b, $1->getExp());

  if ( markLoopExit ) {
    WIR_Operation &jmp = RV32::RV32_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( false ) );
  }

  return;
};


nrel: tpm_BinaryExpGEQ( reg, reg )
{
  $cost[0] = $cost[2] + $cost[3];

  IR_Type &operandType = RV32::effectiveType( *$2->getExp() );

  if ( operandType.isIntegralType() )
    $cost[0] += RV32I::OperationFormat::RRL_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION( "nrel: tpm_BinaryExpGEQ( reg, reg )", $1 );

  auto &reg2 = $action[2]();
  auto &reg3 = $action[3]();

  IR_Type &operandType = RV32::effectiveType( *$2->getExp() );

  if ( operandType.isUnsignedType() )
    RVINSTRUCTIONS.insertBLTU( reg2, reg3, b, $1->getExp());
  else
    RVINSTRUCTIONS.insertBLT( reg2, reg3, b, $1->getExp());

  if ( markLoopExit ) {
    WIR_Operation &jmp = RV32::RV32_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( true ) );
  }

  return;
};


reg: tpm_BinaryExpGEQ( reg, reg )
{
  $cost[0] = $cost[2] + $cost[3];

  IR_Type &operandType = RV32::effectiveType( *$2->getExp() );

  if ( operandType.isIntegralType() )
    $cost[0] +=
      RV32I::OperationFormat::RRR_1.getSize() +
      RV32I::OperationFormat::RRC12_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION( "reg: tpm_BinaryExpGEQ( reg, reg )", $1 );

  auto &reg2 = $action[2]();
  auto &reg3 = $action[3]();

  auto &r = RVINSTRUCTIONS.createReg();
  IR_Type &operandType = RV32::effectiveType( *$2->getExp() );

  if ( operandType.isUnsignedType() )
    RVINSTRUCTIONS.insertSLTU( r, reg2, reg3, $1->getExp() );
  else
    RVINSTRUCTIONS.insertSLT( r, reg2, reg3, $1->getExp() );

  RVINSTRUCTIONS.insertXORI( r, r, 1, $1->getExp() );

  return( r );
};


reg: tpm_BinaryExpGEQ( reg, const12 )
{
  if ( $2->getExp()->getType().isIntegralType() )
    $cost[0] =
      $cost[2] + $cost[3] + 2 * RV32I::OperationFormat::RRC12_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION( "reg: tpm_BinaryExpGEQ( reg, const12 )", $1 );

  auto &reg2 = $action[2]();
  auto v = $action[3]().getIntValue();

  auto &r = RVINSTRUCTIONS.createReg();
  IR_Type &operandType = RV32::effectiveType( *$2->getExp() );

  if ( operandType.isUnsignedType() )
    RVINSTRUCTIONS.insertSLTIU( r, reg2, v, $1->getExp() );
  else
    RVINSTRUCTIONS.insertSLTI( r, reg2, v, $1->getExp() );

  RVINSTRUCTIONS.insertXORI( r, r, 1, $1->getExp() );

  return( r );
};


reg: tpm_BinaryExpGEQ( const12, reg )
{
  if ( $3->getExp()->getType().isIntegralType() )
    $cost[0] = $cost[2] + $cost[3] + RV32I::OperationFormat::RRC12_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION( "reg: tpm_BinaryExpGEQ( const12, reg )", $1 );

  auto &reg3 = $action[3]();
  auto v = $action[2]().getIntValue() + 1;

  auto &r = RVINSTRUCTIONS.createReg();
  IR_Type &operandType = RV32::effectiveType( *$2->getExp() );

  if ( operandType.isUnsignedType() )
    RVINSTRUCTIONS.insertSLTIU( r, reg3, v, $1->getExp() );
  else
    RVINSTRUCTIONS.insertSLTI( r, reg3, v, $1->getExp() );

  return( r );
};


###############################################################################
#
#
#  Equal-to operations
#
#
###############################################################################

rel: tpm_BinaryExpEQ( reg, reg )
{
  $cost[0] = $cost[2] + $cost[3];

  IR_Type &operandType = RV32::effectiveType( *$2->getExp() );

  if ( operandType.isIntegralType() )
    $cost[0] += RV32I::OperationFormat::RRL_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION( "rel: tpm_BinaryExpEQ( reg, reg )", $1 );

  auto &reg2 = $action[2]();
  auto &reg3 = $action[3]();

  RVINSTRUCTIONS.insertBEQ( reg2, reg3, b, $1->getExp());

  if ( markLoopExit ) {
    WIR_Operation &jmp = RV32::RV32_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( false ) );
  }

  return;
};


nrel: tpm_BinaryExpEQ( reg, reg )
{
  $cost[0] = $cost[2] + $cost[3];

  IR_Type &operandType = RV32::effectiveType( *$2->getExp() );

  if ( operandType.isIntegralType() )
    $cost[0] += RV32I::OperationFormat::RRL_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION( "nrel: tpm_BinaryExpEQ( reg, reg )", $1 );

  auto &reg2 = $action[2]();
  auto &reg3 = $action[3]();

  RVINSTRUCTIONS.insertBNE( reg2, reg3, b, $1->getExp());

  if ( markLoopExit ) {
    WIR_Operation &jmp = RV32::RV32_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( true ) );
  }

  return;
};


reg: tpm_BinaryExpEQ( reg, reg )
{
  $cost[0] = $cost[2] + $cost[3];

  IR_Type &operandType = RV32::effectiveType( *$2->getExp() );

  if ( operandType.isIntegralType() )
    $cost[0] +=
      RV32I::OperationFormat::RRR_1.getSize() +
      RV32I::OperationFormat::RRC12_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION( "reg: tpm_BinaryExpEQ( reg, reg )", $1 );

  auto &reg2 = $action[2]();
  auto &reg3 = $action[3]();

  auto &r = RVINSTRUCTIONS.createReg();
  RVINSTRUCTIONS.insertSUB( r, reg2, reg3, $1->getExp() );

  RVINSTRUCTIONS.insertSLTIU( r, r, 1, $1->getExp() );

  return( r );
};


reg: tpm_BinaryExpEQ( reg, const12 )
{
  if ( $2->getExp()->getType().isIntegralType() )
    $cost[0] =
      $cost[2] + $cost[3] + 2 * RV32I::OperationFormat::RRC12_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION( "reg: tpm_BinaryExpEQ( reg, const12 )", $1 );

  auto &reg2 = $action[2]();
  auto v = $action[3]().getIntValue();

  auto &r = RVINSTRUCTIONS.createReg();
  RVINSTRUCTIONS.insertADDI( r, reg2, -v, $1->getExp() );

  RVINSTRUCTIONS.insertSLTIU( r, r, 1, $1->getExp() );

  return( r );
};


reg: tpm_BinaryExpEQ( const12, reg )
{
  if ( $3->getExp()->getType().isIntegralType() )
    $cost[0] =
      $cost[2] + $cost[3] + 2 * RV32I::OperationFormat::RRC12_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION( "reg: tpm_BinaryExpEQ( const12, reg )", $1 );

  auto &reg3 = $action[3]();
  auto v = $action[2]().getIntValue();

  auto &r = RVINSTRUCTIONS.createReg();

  RVINSTRUCTIONS.insertADDI( r, reg3, -v, $1->getExp() );

  RVINSTRUCTIONS.insertSLTIU( r, r, 1, $1->getExp() );

  return( r );
};

###############################################################################
#
#
#  Not-equal-to operations
#
#
###############################################################################

rel: tpm_BinaryExpNEQ( reg, reg )
{
  $cost[0] = $cost[2] + $cost[3];

  IR_Type &operandType = RV32::effectiveType( *$2->getExp() );

  if ( operandType.isIntegralType() )
    $cost[0] += RV32I::OperationFormat::RRL_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION( "rel: tpm_BinaryExpNEQ( reg, reg )", $1 );

  auto &reg2 = $action[2]();
  auto &reg3 = $action[3]();

  RVINSTRUCTIONS.insertBNE( reg2, reg3, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = RV32::RV32_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( false ) );
  }

  return;
};


nrel: tpm_BinaryExpNEQ( reg, reg )
{
  $cost[0] = $cost[2] + $cost[3];

  IR_Type &operandType = RV32::effectiveType( *$2->getExp() );

  if ( operandType.isIntegralType() )
    $cost[0] += RV32I::OperationFormat::RRL_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION( "nrel: tpm_BinaryExpNEQ( reg, reg )", $1 );

  auto &reg2 = $action[2]();
  auto &reg3 = $action[3]();

  RVINSTRUCTIONS.insertBEQ( reg2, reg3, b, $1->getExp());

  if ( markLoopExit ) {
    WIR_Operation &jmp = RV32::RV32_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( true ) );
  }

  return;
};


reg: tpm_BinaryExpNEQ( reg, reg )
{
  $cost[0] = $cost[2] + $cost[3];

  IR_Type &operandType = RV32::effectiveType( *$2->getExp() );

  if ( operandType.isIntegralType() )
    $cost[0] += 2 * RV32I::OperationFormat::RRR_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION( "reg: tpm_BinaryExpNEQ( reg, reg )", $1 );

  auto &reg2 = $action[2]();
  auto &reg3 = $action[3]();

  auto &r = RVINSTRUCTIONS.createReg();
  RVINSTRUCTIONS.insertSUB( r, reg2, reg3, $1->getExp() );

  auto &x0 = RVINSTRUCTIONS.createReg();
  RV32::bindToPHREG( x0, 0 );

  RVINSTRUCTIONS.insertSLTU( r, x0, r, $1->getExp() );

  return( r );
};


reg: tpm_BinaryExpNEQ( reg, const12 )
{
  if ( $2->getExp()->getType().isIntegralType() )
    $cost[0] =
      $cost[2] + $cost[3] + RV32I::OperationFormat::RRC12_1.getSize() +
      RV32I::OperationFormat::RRR_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION( "reg: tpm_BinaryExpNEQ( reg, const12 )", $1 );

  auto &reg2 = $action[2]();
  auto v = $action[3]().getIntValue();

  auto &r = RVINSTRUCTIONS.createReg();
  RVINSTRUCTIONS.insertADDI( r, reg2, -v, $1->getExp() );

  auto &x0 = RVINSTRUCTIONS.createReg();
  RV32::bindToPHREG( x0, 0 );

  RVINSTRUCTIONS.insertSLTU( r, x0, r, $1->getExp() );

  return( r );
};


reg: tpm_BinaryExpNEQ( const12, reg )
{
  if ( $2->getExp()->getType().isIntegralType() )
    $cost[0] =
      $cost[2] + $cost[3] + RV32I::OperationFormat::RRC12_1.getSize() +
      RV32I::OperationFormat::RRR_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION( "reg: tpm_BinaryExpNEQ( const12, reg )", $1 );

  auto &reg3 = $action[3]();
  auto v = $action[2]().getIntValue();

  auto &r = RVINSTRUCTIONS.createReg();
  RVINSTRUCTIONS.insertADDI( r, reg3, -v, $1->getExp() );

  auto &x0 = RVINSTRUCTIONS.createReg();
  RV32::bindToPHREG( x0, 0 );

  RVINSTRUCTIONS.insertSLTU( r, x0, r, $1->getExp() );

  return( r );
};
