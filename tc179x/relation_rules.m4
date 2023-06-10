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


###############################################################################
#
#
#  Relations (less-than)
#
#
###############################################################################

####################################################################
#
# Jump on positives
#
####################################################################

##########################################
#
#  Normalized relations (constants on the right), with implicit jump
#
##########################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
rel: tpm_BinaryExpLT( dreg, dreg )
{
  $cost[0] = $cost[2] + $cost[3];

  IR_Type &operandType = effectiveType( *$2->getExp() );

  if ( operandType.isIntegralType() )
    $cost[0] += TC13::OperationFormat::DDL_1.getSize();
  else

  if ( operandType.getType() == IR_Type::FLOAT ) {
    if ( TCCODESEL->getConfig()->getEmitFpuInstructions() )
      $cost[0] +=
        TC13::OperationFormat::DDD_1.getSize() +
        2 * TC13::OperationFormat::DC5L.getSize();
    else
      $cost[0] += SOFTFLOAT_COST + TC13::OperationFormat::DC4L_1.getSize();
  } else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpLT( dreg, dreg )", $1 );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  IR_Type &operandType = effectiveType( *$2->getExp() );

  // LLIR
  if ( operandType.isIntegralType() )
    if ( operandType.isUnsignedType() )
      TCINSTRUCTIONS.insertJLT_U( p1.first, p2.first, block, $1->getExp() );
    else
      TCINSTRUCTIONS.insertJLT( p1.first, p2.first, block, $1->getExp() );
  else

  if ( operandType.getType() == IR_Type::FLOAT ) {
    LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );

    if ( TCCODESEL->getConfig()->getEmitFpuInstructions() ) {
      TCINSTRUCTIONS.insertCMP_F( regTmp, p1.first, p2.first, $1->getExp() );
      TCINSTRUCTIONS.insertJZ_T( regTmp, 1, block, $1->getExp() );
      TCINSTRUCTIONS.insertJZ_T( regTmp, 2, block, $1->getExp() );
    } else {
      TCINSTRUCTIONS.insertGE_F( regTmp, p1.first, p2.first );
      TCINSTRUCTIONS.insertJLT( regTmp, 0, block, $1->getExp() );
    }
  }

  // WIR
  if ( operandType.isIntegralType() ) {
    if ( operandType.isUnsignedType() )
      TCINSTRUCTIONS.insertJLT_U( p1.second, p2.second, b, $1->getExp() );
    else
      TCINSTRUCTIONS.insertJLT( p1.second, p2.second, b, $1->getExp() );

    if ( markLoopExit ) {
      WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
      jmp.insertContainer( WIR_LoopExit( false ) );
    }
  } else

  if ( operandType.getType() == IR_Type::FLOAT ) {
    auto &tmpReg = TCINSTRUCTIONS.createDReg();

    if ( TCCODESEL->getConfig()->getEmitFpuInstructions() ) {
      TCINSTRUCTIONS.insertCMP_F( tmpReg, p1.second, p2.second, $1->getExp() );
      TCINSTRUCTIONS.insertJZ_T( tmpReg, 1, b, $1->getExp() );
      TCINSTRUCTIONS.insertJZ_T( tmpReg, 2, b, $1->getExp() );
    } else {
      TCINSTRUCTIONS.insertGE_F( tmpReg, p1.second, p2.second, $1->getExp() );
      TCINSTRUCTIONS.insertJLT( tmpReg, 0, b, $1->getExp() );
    }

    if ( markLoopExit ) {
      WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
      jmp.insertContainer( WIR_LoopExit( false ) );
    }
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
rel: tpm_BinaryExpLT( dreg, constant0 )
{
  if ( $2->getExp()->getType().isIntegralType() &&
       effectiveType( *$2->getExp() ).isSignedType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpLT( dreg, constant0 )", $1 );

  auto p = $action[2]();

  // LLIR
  TCINSTRUCTIONS.insertJLT( p.first, 0, block, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertJLT( p.second, 0, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( false ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
rel: tpm_BinaryExpLT( dreg, const4 )
{
  if ( $2->getExp()->getType().isIntegralType() &&
       effectiveType( *$2->getExp() ).isSignedType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpLT( dreg, const4 )", $1 );

  auto p = $action[2]();
  auto v = $action[3]().getIntValue();

  // LLIR
  TCINSTRUCTIONS.insertJLT( p.first, v, block, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertJLT( p.second, v, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( false ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
rel: tpm_BinaryExpLT( dreg, uconst4 )
{
  if ( $2->getExp()->getType().isIntegralType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DC4L_2.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpLT( dreg, uconst4 )", $1 );

  auto p = $action[2]();
  auto v = $action[3]().getIntValue();

  // LLIR
  TCINSTRUCTIONS.insertJLT_U( p.first, v, block, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertJLT_U( p.second, v, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( false ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
rel: tpm_BinaryExpLT( dreg, const9 )
{
  if ( $2->getExp()->getType().isIntegralType() &&
       effectiveType( *$2->getExp() ).isSignedType() )
    $cost[0] =
      $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_1.getSize() +
      TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpLT( dreg, const9 )", $1 );

  auto p = $action[2]();
  auto v = $action[3]().getIntValue();

  // LLIR
  LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertLT( regTmp, p.first, v, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( regTmp, 0, block, $1->getExp() );

  // WIR
  auto &tmpReg = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertLT( tmpReg, p.second, v, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( tmpReg, 0, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( false ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
rel: tpm_BinaryExpLT( dreg, uconst9 )
{
  if ( $2->getExp()->getType().isIntegralType() )
    $cost[0] =
      $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_2.getSize() +
      TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpLT( dreg, uconst9 )", $1 );

  auto p = $action[2]();
  auto v = $action[3]().getIntValue();

  // LLIR
  LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertLT_U( regTmp, p.first, v, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( regTmp, 0, block, $1->getExp() );

  // WIR
  auto &tmpReg = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertLT_U( tmpReg, p.second, v, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( tmpReg, 0, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( false ) );
  }

  return;
};


##############################################################################
#
#  Denormal relations (constants on the left)
#
#  NOTE: TriCore operations (J)LE and (J)GT do not exist.
#
##############################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
rel: tpm_BinaryExpLT( constant0, dreg )
{
  if ( $3->getExp()->getType().isIntegralType() &&
       effectiveType( *$3->getExp() ).isSignedType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpLT( constant0, dreg )", $1 );

  auto p = $action[3]();

  // LLIR
  TCINSTRUCTIONS.insertJGE( p.first, 1, block, $1->getExp());

  // WIR
  TCINSTRUCTIONS.insertJGE( p.second, 1, b, $1->getExp());

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( false ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
rel: tpm_BinaryExpLT( const4, dreg )
{
  if ( $3->getExp()->getType().isIntegralType() &&
       effectiveType( *$3->getExp() ).isSignedType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;

  // Since JGT is missing, we need JGE. This means we can only use values < 0x7.
  if ( getConstIntValue( $2 ) >= TC_Const4_Signed::getMaxValue( 4 ) )
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpLT( const4, dreg )", $1 );

  auto v = $action[2]().getIntValue() + 1;
  auto p = $action[3]();

  // LLIR
  TCINSTRUCTIONS.insertJGE( p.first, v, block, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertJGE( p.second, v, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( false ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
rel: tpm_BinaryExpLT( uconst4, dreg )
{
  if ( $3->getExp()->getType().isIntegralType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DC4L_2.getSize();
  else
    $cost[0] = COST_INFINITY;

  // Since JGT_U is missing, we need JGE_U. This means we can only use
  // values < 0xf.
  if ( getConstIntValue( $2 ) >= (int) TC_Const4_Unsigned::getMaxValue( 4 ) )
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpLT( uconst4, dreg )", $1 );

  auto v = $action[2]().getIntValue() + 1;
  auto p = $action[3]();

  // LLIR
  TCINSTRUCTIONS.insertJGE_U( p.first, v, block, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertJGE_U( p.second, v, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( false ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
rel: tpm_BinaryExpLT( const9, dreg )
{
  if ( $3->getExp()->getType().isIntegralType() &&
       effectiveType( *$3->getExp() ).isSignedType() )
    $cost[0] =
      $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_1.getSize() +
      TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;

  // Since GT is missing, we need GE. This means we can only use values < 0xff.
  if ( getConstIntValue( $2 ) >= TC_Const9_Signed::getMaxValue( 9 ) )
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpLT( const9, dreg )", $1 );

  auto v = $action[2]().getIntValue() + 1;
  auto p = $action[3]();

  // LLIR
  LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertGE( regTmp, p.first, v, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( regTmp, 0, block, $1->getExp() );

  // WIR
  auto &tmpReg = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertGE( tmpReg, p.second, v, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( tmpReg, 0, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( false ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
rel: tpm_BinaryExpLT( uconst9, dreg )
{
  if ( $3->getExp()->getType().isIntegralType() )
    $cost[0] =
      $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_2.getSize()  +
      TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;

  // Since GT_U is missing, we need GE_U. This means we can only use
  // values < 0x1ff.
  if ( getConstIntValue( $2 ) >= (int) TC_Const9_Unsigned::getMaxValue( 9 ) )
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpLT( uconst9, dreg )", $1 );

  auto v = $action[2]().getIntValue() + 1;
  auto p = $action[3]();

  // LLIR
  LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertGE_U( regTmp, p.first, v, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( regTmp, 0, block, $1->getExp() );

  // WIR
  auto &tmpReg = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertGE_U( tmpReg, p.second, v, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( tmpReg, 0, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( false ) );
  }

  return;
};


####################################################################
#
# Jump on negatives
#
####################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
nrel: tpm_BinaryExpLT( dreg, dreg )
{
  $cost[0] = $cost[2] + $cost[3];

  IR_Type &operandType = effectiveType( *$2->getExp() );

  if ( operandType.isIntegralType() )
    $cost[0] += TC13::OperationFormat::DDL_1.getSize();
  else

  if ( operandType.getType() == IR_Type::FLOAT ) {
    if ( TCCODESEL->getConfig()->getEmitFpuInstructions() )
      $cost[0] +=
        TC13::OperationFormat::DDD_1.getSize() +
        TC13::OperationFormat::DC5L.getSize();
    else
      $cost[0] += SOFTFLOAT_COST + TC13::OperationFormat::DC4L_1.getSize();
  } else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "nrel: tpm_BinaryExpLT( dreg, dreg )", $1 );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  IR_Type &operandType = effectiveType( *$2->getExp() );

  // LLIR
  if ( operandType.isIntegralType() )
    if ( operandType.isUnsignedType() )
      TCINSTRUCTIONS.insertJGE_U( p1.first, p2.first, block, $1->getExp() );
    else
      TCINSTRUCTIONS.insertJGE( p1.first, p2.first, block, $1->getExp() );
  else

  if ( operandType.getType() == IR_Type::FLOAT ) {
    LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );

    if ( TCCODESEL->getConfig()->getEmitFpuInstructions() ) {
      TCINSTRUCTIONS.insertCMP_F( regTmp, p1.first, p2.first, $1->getExp() );
      TCINSTRUCTIONS.insertJZ_T( regTmp, 0, block, $1->getExp() );
    } else {
      TCINSTRUCTIONS.insertLT_F( regTmp, p1.first, p2.first );
      TCINSTRUCTIONS.insertJGE( regTmp, 0, block, $1->getExp() );
    }
  }

  // WIR
  if ( operandType.isIntegralType() ) {
    if ( operandType.isUnsignedType() )
      TCINSTRUCTIONS.insertJGE_U( p1.second, p2.second, b, $1->getExp() );
    else
      TCINSTRUCTIONS.insertJGE( p1.second, p2.second, b, $1->getExp() );

    if ( markLoopExit ) {
      WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
      jmp.insertContainer( WIR_LoopExit( true ) );
    }
  } else

  if ( operandType.getType() == IR_Type::FLOAT ) {
    auto &tmpReg = TCINSTRUCTIONS.createDReg();

    if ( TCCODESEL->getConfig()->getEmitFpuInstructions() ) {
      TCINSTRUCTIONS.insertCMP_F( tmpReg, p1.second, p2.second, $1->getExp() );
      TCINSTRUCTIONS.insertJZ_T( tmpReg, 0, b, $1->getExp() );
    } else {
      TCINSTRUCTIONS.insertLT_F( tmpReg, p1.second, p2.second, $1->getExp() );
      TCINSTRUCTIONS.insertJGE( tmpReg, 0, b, $1->getExp() );
    }

    if ( markLoopExit ) {
      WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
      jmp.insertContainer( WIR_LoopExit( true ) );
    }
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
nrel: tpm_BinaryExpLT( dreg, constant0 )
{
  if ( $2->getExp()->getType().isIntegralType() &&
       effectiveType( *$2->getExp() ).isSignedType() )
    $cost[0] += $cost[2] + $cost[3] + TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "nrel: tpm_BinaryExpLT( dreg, constant0 )", $1 );

  auto p = $action[2]();

  // LLIR
  TCINSTRUCTIONS.insertJGE( p.first, 0, block, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertJGE( p.second, 0, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( true ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
nrel: tpm_BinaryExpLT( dreg, const4 )
{
  if ( $2->getExp()->getType().isIntegralType() &&
       effectiveType( *$2->getExp() ).isSignedType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "nrel: tpm_BinaryExpLT( dreg, const4 )", $1 );

  auto p = $action[2]();
  auto v = $action[3]().getIntValue();

  // LLIR
  TCINSTRUCTIONS.insertJGE( p.first, v, block, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertJGE( p.second, v, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( true ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
nrel: tpm_BinaryExpLT( dreg, uconst4 )
{
  if ( $2->getExp()->getType().isIntegralType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DC4L_2.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "nrel: tpm_BinaryExpLT( dreg, uconst4 )", $1 );

  auto p = $action[2]();
  auto v = $action[3]().getIntValue();

  // LLIR
  TCINSTRUCTIONS.insertJGE_U( p.first, v, block, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertJGE_U( p.second, v, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( true ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
nrel: tpm_BinaryExpLT( dreg, const9 )
{
  if ( $2->getExp()->getType().isIntegralType() &&
       effectiveType( *$2->getExp() ).isSignedType() )
    $cost[0] =
      $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_1.getSize() +
      TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "nrel: tpm_BinaryExpLT( dreg, const9 )", $1 );

  auto p = $action[2]();
  auto v = $action[3]().getIntValue();

  // LLIR
  LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertLT( regTmp, p.first, v, $1->getExp() );
  TCINSTRUCTIONS.insertJEQ( regTmp, 0, block, $1->getExp() );

  // WIR
  auto &tmpReg = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertLT( tmpReg, p.second, v, $1->getExp() );
  TCINSTRUCTIONS.insertJEQ( tmpReg, 0, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( true ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
nrel: tpm_BinaryExpLT( dreg, uconst9 )
{
  if ( $2->getExp()->getType().isIntegralType() )
    $cost[0] =
      $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_2.getSize() +
      TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "nrel: tpm_BinaryExpLT( dreg, uconst9 )", $1 );

  auto p = $action[2]();
  auto v = $action[3]().getIntValue();

  // LLIR
  LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertLT_U( regTmp, p.first, v, $1->getExp() );
  TCINSTRUCTIONS.insertJEQ( regTmp, 0, block, $1->getExp() );

  // WIR
  auto &tmpReg = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertLT_U( tmpReg, p.second, v, $1->getExp() );
  TCINSTRUCTIONS.insertJEQ( tmpReg, 0, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( true ) );
  }

  return;
};


##############################################################################
#
#  Denormal relations (constants on the left)
#
#  NOTE: TriCore operations (J)LE and (J)GT do not exist.
#
##############################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
nrel: tpm_BinaryExpLT( constant0, dreg )
{
  if ( $3->getExp()->getType().isIntegralType() &&
       effectiveType( *$3->getExp() ).isSignedType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "nrel: tpm_BinaryExpLT( constant0, dreg )", $1 );

  auto p = $action[3]();

  // LLIR
  TCINSTRUCTIONS.insertJLT( p.first, 0, block, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertJLT( p.second, 0, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( true ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
nrel: tpm_BinaryExpLT( const4, dreg )
{
  if ( $3->getExp()->getType().isIntegralType() &&
       effectiveType( *$3->getExp() ).isSignedType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;

  // Since JLE is missing, we need JLT. This means we can only use values < 0x7.
  if ( getConstIntValue( $2 ) >= TC_Const4_Signed::getMaxValue( 4 ) )
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "nrel: tpm_BinaryExpLT( const4, dreg )", $1 );

  auto v = $action[2]().getIntValue() + 1;
  auto p = $action[3]();

  // LLIR
  TCINSTRUCTIONS.insertJLT( p.first, v, block, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertJLT( p.second, v, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( true ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
nrel: tpm_BinaryExpLT( uconst4, dreg )
{
  if ( $3->getExp()->getType().isIntegralType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DC4L_2.getSize();
  else
    $cost[0] = COST_INFINITY;

  // Since JLE is missing, we need JLT. This means we can only use values < 0xf.
  if ( getConstIntValue( $2 ) >= (int) TC_Const4_Unsigned::getMaxValue( 4 ) )
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "nrel: tpm_BinaryExpLT( uconst4, dreg )", $1 );

  auto v = $action[2]().getIntValue() + 1;
  auto p = $action[3]();

  // LLIR
  TCINSTRUCTIONS.insertJLT_U( p.first, v, block, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertJLT_U( p.second, v, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( true ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
nrel: tpm_BinaryExpLT( const9, dreg )
{
  if ( $3->getExp()->getType().isIntegralType() &&
       effectiveType( *$3->getExp() ).isSignedType() )
    $cost[0] =
      $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_1.getSize() +
      TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;

  // Since LE is missing, we need LT. This means we can only use values < 0xff.
  if ( getConstIntValue( $2 ) >= TC_Const9_Signed::getMaxValue( 9 ) )
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "nrel: tpm_BinaryExpLT( const9, dreg )", $1 );

  auto v = $action[2]().getIntValue() + 1;
  auto p = $action[3]();

  // LLIR
  LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertLT( regTmp, p.first, v, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( regTmp, 0, block, $1->getExp() );

  // WIR
  auto &tmpReg = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertLT( tmpReg, p.second, v, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( tmpReg, 0, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( true ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
nrel: tpm_BinaryExpLT( uconst9, dreg )
{
  if ( $3->getExp()->getType().isIntegralType() )
    $cost[0] =
      $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_2.getSize() +
      TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;

  // Since LE_U is missing, we need LT_U. This means we can only use
  // values < 0x1ff.
  if ( getConstIntValue( $2 ) >= (int) TC_Const9_Unsigned::getMaxValue( 9 ) )
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "nrel: tpm_BinaryExpLT( uconst9, dreg )", $1 );

  auto v = $action[2]().getIntValue() + 1;
  auto p = $action[3]();

  // LLIR
  LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertLT_U( regTmp, p.first, v, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( regTmp, 0, block, $1->getExp() );

  // WIR
  auto &tmpReg = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertLT_U( tmpReg, p.second, v, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( tmpReg, 0, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( true ) );
  }

  return;
};


##############################################################################
#
#  Normalized relations (constants on the right), as values
#
##############################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpLT( ereg, ereg )
{
  $cost[0] =
    $cost[2] + $cost[3] + SOFTFLOAT_COST +
    TC13::OperationFormat::DDC9_1.getSize();
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpLT( ereg, ereg )", $1 );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertLT_D( reg, p1.first, p2.first, $1->getExp() );
  TCINSTRUCTIONS.insertLT( reg, reg, 0, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertLT_D( r, p1.second, p2.second, $1->getExp() );
  TCINSTRUCTIONS.insertLT( r, r, 0, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpLT( dreg, dreg )
{
  $cost[0] = $cost[2] + $cost[3];

  auto &t = effectiveType( *$2->getExp() );
  if ( t.isIntegralType() )
    $cost[0] += TC13::OperationFormat::DDD_1.getSize();
  else
  if ( t.getType() == IR_Type::FLOAT ) {
    if ( TCCODESEL->getConfig()->getEmitFpuInstructions() )
      $cost[0] +=
        TC13::OperationFormat::DDD_1.getSize() +
        TC13::OperationFormat::DDC5DC5_1.getSize();
    else
      $cost[0] += SOFTFLOAT_COST + TC13::OperationFormat::DDC9_1.getSize();
  } else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpLT( dreg, dreg )", $1 );

  auto p1 = $action[2]();
  auto p2 = $action[3]();
  auto &t = effectiveType( *$2->getExp() );

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  if ( t.isIntegralType() )
    if ( t.isUnsignedType() )
      TCINSTRUCTIONS.insertLT_U( reg, p1.first, p2.first, $1->getExp() );
    else
      TCINSTRUCTIONS.insertLT( reg, p1.first, p2.first, $1->getExp() );
  else
  if ( t.getType() == IR_Type::FLOAT ) {
    LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );
    if ( TCCODESEL->getConfig()->getEmitFpuInstructions() ) {
      TCINSTRUCTIONS.insertCMP_F( regTmp, p1.first, p2.first, $1->getExp() );
      TCINSTRUCTIONS.insertOR_T( reg, regTmp, 0, regTmp, 0, $1->getExp() );
    } else {
      TCINSTRUCTIONS.insertLT_F( regTmp, p1.first, p2.first, $1->getExp() );
      TCINSTRUCTIONS.insertLT( reg, regTmp, 0, $1->getExp() );
    }
  }

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  if ( t.isIntegralType() )
    if ( t.isUnsignedType() )
      TCINSTRUCTIONS.insertLT_U( r, p1.second, p2.second, $1->getExp() );
    else
      TCINSTRUCTIONS.insertLT( r, p1.second, p2.second, $1->getExp() );
  else
  if ( t.getType() == IR_Type::FLOAT ) {
    auto &tmpReg = TCINSTRUCTIONS.createDReg();
    if ( TCCODESEL->getConfig()->getEmitFpuInstructions() ) {
      TCINSTRUCTIONS.insertCMP_F( tmpReg, p1.second, p2.second, $1->getExp() );
      TCINSTRUCTIONS.insertOR_T( r, tmpReg, 0, tmpReg, 0, $1->getExp() );
    } else {
      TCINSTRUCTIONS.insertLT_F( tmpReg, p1.second, p2.second, $1->getExp() );
      TCINSTRUCTIONS.insertLT( r, tmpReg, 0, $1->getExp() );
    }
  }

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpLT( dreg, constant0 )
{
  if ( $2->getExp()->getType().isIntegralType() &&
       effectiveType( *$2->getExp() ).isSignedType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpLT( dreg, constant0 )", $1 );

  auto p = $action[2]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertSH( reg, p.first, -31, $1->getExp() ); // Isolate sign-bit

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertSH( r, p.second, -31, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpLT( dreg, const9 )
{
  if ( $2->getExp()->getType().isIntegralType() &&
       effectiveType( *$2->getExp() ).isSignedType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpLT( dreg, const9 )", $1 );

  auto p = $action[2]();
  auto v = $action[3]().getIntValue();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertLT( reg, p.first, v, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertLT( r, p.second, v, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpLT( dreg, uconst9 )
{
  if ( $2->getExp()->getType().isIntegralType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_2.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpLT( dreg, uconst9 )", $1 );

  auto p = $action[2]();
  auto v = $action[3]().getIntValue();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertLT_U( reg, p.first, v, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertLT_U( r, p.second, v, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpLT( areg, areg )
{
  $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DAA.getSize();
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpLT( areg, areg )", $1 );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertLT_A( reg, p1.first, p2.first, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertLT_A( r, p1.second, p2.second, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


##############################################################################
#
#  Denormal relations (constants on the left), as values
#
#  NOTE: Instructions (J)LE and (J)GT do not exist.
#
##############################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpLT( constant0, dreg )
{
  if ( $3->getExp()->getType().isIntegralType() &&
       effectiveType( *$3->getExp() ).isSignedType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpLT( constant0, dreg )", $1 );

  auto p = $action[3]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertGE( reg, p.first, 1, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertGE( r, p.second, 1, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpLT( const9, dreg )
{
  if ( $3->getExp()->getType().isIntegralType() &&
       effectiveType( *$3->getExp() ).isSignedType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_1.getSize();
  else
    $cost[0] = COST_INFINITY;

  // Since JLE is missing, we need JLT. Means we can only use values < 0xff.
  if ( getConstIntValue( $2 ) >= 0xff )
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpLT( const9, dreg )", $1 );

  auto p = $action[3]();
  auto v = $action[2]().getIntValue();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertGE( reg, p.first, v + 1, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertGE( r, p.second, v + 1, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpLT( uconst9, dreg )
{
  if ( $3->getExp()->getType().isIntegralType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_2.getSize();
  else
    $cost[0] = COST_INFINITY;

  // Since JLE is missing, we need JLT. Means we can only use values < 0x1ff.
  if ( getConstIntValue( $2 ) >= 0x1ff )
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpLT( uconst9, dreg )", $1 );

  auto p = $action[3]();
  auto v = $action[2]().getIntValue();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertGE_U( reg, p.first, v + 1, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertGE_U( r, p.second, v + 1, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


###############################################################################
#
#
#  Relations (greater-than)
#
#
###############################################################################

####################################################################
#
# Jump on negatives
#
####################################################################

######################################################
#
#  Normalized relations (constants on the right), with implicit jump
#
######################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
rel: tpm_BinaryExpGT( dreg, dreg )
{
  $cost[0] = $cost[2] + $cost[3];

  IR_Type &operandType = effectiveType( *$2->getExp() );

  if ( operandType.isIntegralType() )
    $cost[0] += TC13::OperationFormat::DDL_1.getSize();
  else

  if ( operandType.getType() == IR_Type::FLOAT ) {
    if ( TCCODESEL->getConfig()->getEmitFpuInstructions() )
      $cost[0] +=
        TC13::OperationFormat::DDD_1.getSize() +
        2 * TC13::OperationFormat::DC5L.getSize();
    else
      $cost[0] += SOFTFLOAT_COST + TC13::OperationFormat::DC4L_1.getSize();
  } else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpGT( dreg, dreg )", $1 );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  IR_Type &operandType = effectiveType( *$2->getExp() );

  // LLIR
  if ( operandType.isIntegralType() )
    if ( operandType.isUnsignedType() )
      TCINSTRUCTIONS.insertJLT_U( p2.first, p1.first, block, $1->getExp() );
    else
      TCINSTRUCTIONS.insertJLT( p2.first, p1.first, block, $1->getExp() );
  else

  if ( operandType.getType() == IR_Type::FLOAT ) {
    LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );

    if ( TCCODESEL->getConfig()->getEmitFpuInstructions() ) {
      TCINSTRUCTIONS.insertCMP_F( regTmp, p1.first, p2.first, $1->getExp() );
      TCINSTRUCTIONS.insertJZ_T( regTmp, 0, block, $1->getExp() );
      TCINSTRUCTIONS.insertJZ_T( regTmp, 1, block, $1->getExp() );
    } else {
      TCINSTRUCTIONS.insertLE_F( regTmp, p1.first, p2.first );
      TCINSTRUCTIONS.insertJGE( regTmp, 1, block, $1->getExp() );
    }
  }

  // WIR
  if ( operandType.isIntegralType() ) {
    if ( operandType.isUnsignedType() )
      TCINSTRUCTIONS.insertJLT_U( p2.second, p1.second, b, $1->getExp() );
    else
      TCINSTRUCTIONS.insertJLT( p2.second, p1.second, b, $1->getExp() );

    if ( markLoopExit ) {
      WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
      jmp.insertContainer( WIR_LoopExit( false ) );
    }
  } else

  if ( operandType.getType() == IR_Type::FLOAT ) {
    auto &tmpReg = TCINSTRUCTIONS.createDReg();

    if ( TCCODESEL->getConfig()->getEmitFpuInstructions() ) {
      TCINSTRUCTIONS.insertCMP_F( tmpReg, p1.second, p2.second, $1->getExp() );
      TCINSTRUCTIONS.insertJZ_T( tmpReg, 0, b, $1->getExp() );
      TCINSTRUCTIONS.insertJZ_T( tmpReg, 1, b, $1->getExp() );
    } else {
      TCINSTRUCTIONS.insertLE_F( tmpReg, p1.second, p2.second, $1->getExp() );
      TCINSTRUCTIONS.insertJGE( tmpReg, 1, b, $1->getExp() );
    }

    if ( markLoopExit ) {
      WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
      jmp.insertContainer( WIR_LoopExit( false ) );
    }
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
rel: tpm_BinaryExpGT( dreg, constant0 )
{
  if ( $2->getExp()->getType().isIntegralType() &&
       effectiveType( *$2->getExp() ).isSignedType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpGT( dreg, constant0 )", $1 );

  auto p = $action[2]();

  // LLIR
  TCINSTRUCTIONS.insertJGE( p.first, 1, block, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertJGE( p.second, 1, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( false ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
rel: tpm_BinaryExpGT( dreg, const4 )
{
  if ( $2->getExp()->getType().isIntegralType() &&
       effectiveType( *$2->getExp() ).isSignedType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;

  // Since JGT is missing, we need JGE. This means we can only use values < 0x7.
  if ( getConstIntValue( $3 ) >= TC_Const4_Signed::getMaxValue( 4 ) )
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpGT( dreg, const4 )", $1 );

  auto p = $action[2]();
  auto v = $action[3]().getIntValue() + 1;

  // LLIR
  TCINSTRUCTIONS.insertJGE( p.first, v, block, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertJGE( p.second, v, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( false ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
rel: tpm_BinaryExpGT( dreg, uconst4 )
{
  if ( $2->getExp()->getType().isIntegralType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DC4L_2.getSize();
  else
    $cost[0] = COST_INFINITY;

  // Since JGT_U is missing, we need JGE_U. This means we can only use
  // values < 0xf.
  if ( getConstIntValue( $3 ) >= (int) TC_Const4_Unsigned::getMaxValue( 4 ) )
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpGT( dreg, uconst4 )", $1 );

  auto p = $action[2]();
  auto v = $action[3]().getIntValue() + 1;

  // LLIR
  TCINSTRUCTIONS.insertJGE_U( p.first, v, block, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertJGE_U( p.second, v, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( false ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
rel: tpm_BinaryExpGT( dreg, const9 )
{
  if ( $2->getExp()->getType().isIntegralType() &&
       effectiveType( *$2->getExp() ).isSignedType() )
    $cost[0] =
      $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_1.getSize() +
      TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;

  // Since GT is missing, we need GE. This means we can only use values < 0xff.
  if ( getConstIntValue( $3 ) >= TC_Const9_Signed::getMaxValue( 9 ) )
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpGT( dreg, const9 )", $1 );

  auto p = $action[2]();
  auto v = $action[3]().getIntValue() + 1;

  // LLIR
  LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertGE( regTmp, p.first, v, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( regTmp, 0, block, $1->getExp() );

  // WIR
  auto &tmpReg = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertGE( tmpReg, p.second, v, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( tmpReg, 0, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( false ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
rel: tpm_BinaryExpGT( dreg, uconst9 )
{
  if ( $2->getExp()->getType().isIntegralType() )
    $cost[0] =
      $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_2.getSize() +
      TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;

  // Since GT_U is missing, we need GE_U. This means we can only use
  // values < 0x1ff.
  if ( getConstIntValue( $3 ) >= (int) TC_Const9_Unsigned::getMaxValue( 9 ) )
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpGT( dreg, uconst9 )", $1 );

  auto p = $action[2]();
  auto v = $action[3]().getIntValue() + 1;

  // LLIR
  LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertGE_U( regTmp, p.first, v, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( regTmp, 0, block, $1->getExp() );

  // WIR
  auto &tmpReg = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertGE_U( tmpReg, p.second, v, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( tmpReg, 0, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( false ) );
  }

  return;
};


##############################################################################
#
#  Denormal relations (constants on the left)
#
#  NOTE: Instructions (J)LE and (J)GT do not exist.
#
##############################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
rel: tpm_BinaryExpGT( constant0, dreg )
{
  if ( $3->getExp()->getType().isIntegralType() &&
       effectiveType( *$3->getExp() ).isSignedType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpGT( constant0, dreg )", $1 );

  auto p = $action[3]();

  // LLIR
  TCINSTRUCTIONS.insertJLT( p.first, 0, block, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertJLT( p.second, 0, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( false ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
rel: tpm_BinaryExpGT( const4, dreg )
{
  if ( $3->getExp()->getType().isIntegralType() &&
       effectiveType( *$3->getExp() ).isSignedType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpGT( const4, dreg )", $1 );

  auto v = $action[2]().getIntValue();
  auto p = $action[3]();

  // LLIR
  TCINSTRUCTIONS.insertJLT( p.first, v, block, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertJLT( p.second, v, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( false ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
rel: tpm_BinaryExpGT( uconst4, dreg )
{
  if ( $3->getExp()->getType().isIntegralType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DC4L_2.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpGT( uconst4, dreg )", $1 );

  auto v = $action[2]().getIntValue();
  auto p = $action[3]();

  // LLIR
  TCINSTRUCTIONS.insertJLT_U( p.first, v, block, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertJLT_U( p.second, v, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( false ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
rel: tpm_BinaryExpGT( const9, dreg )
{
  if ( $3->getExp()->getType().isIntegralType() &&
       effectiveType( *$3->getExp() ).isSignedType() )
    $cost[0] =
      $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_1.getSize() +
      TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpGT( const9, dreg )", $1 );

  auto v = $action[2]().getIntValue();
  auto p = $action[3]();

  // LLIR
  LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertLT( regTmp, p.first, v, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( regTmp, 0, block, $1->getExp() );

  // WIR
  auto &tmpReg = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertLT( tmpReg, p.second, v, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( tmpReg, 0, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( false ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
rel: tpm_BinaryExpGT( uconst9, dreg )
{
  if ( $3->getExp()->getType().isIntegralType() )
    $cost[0] =
      $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_2.getSize() +
      TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpGT( uconst9, dreg )", $1 );

  auto v = $action[2]().getIntValue();
  auto p = $action[3]();

  // LLIR
  LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertLT_U( regTmp, p.first, v, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( regTmp, 0, block, $1->getExp() );

  // WIR
  auto &tmpReg = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertLT_U( tmpReg, p.second, v, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( tmpReg, 0, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( false ) );
  }

  return;
};


####################################################################
#
# Jump on negatives
#
####################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
nrel: tpm_BinaryExpGT( dreg, dreg )
{
  $cost[0] = $cost[2] + $cost[3];

  IR_Type &operandType = effectiveType( *$2->getExp() );

  if ( operandType.isIntegralType() )
    $cost[0] += TC13::OperationFormat::DDL_1.getSize();
  else

  if ( operandType.getType() == IR_Type::FLOAT ) {
    if ( TCCODESEL->getConfig()->getEmitFpuInstructions() )
      $cost[0] +=
        TC13::OperationFormat::DDD_1.getSize() +
        TC13::OperationFormat::DC5L.getSize();
    else
      $cost[0] += SOFTFLOAT_COST + TC13::OperationFormat::DC4L_1.getSize();
  } else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "nrel: tpm_BinaryExpGT( dreg, dreg )", $1 );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  IR_Type &operandType = effectiveType( *$2->getExp() );

  // LLIR
  if ( operandType.isIntegralType() )
    if ( operandType.isUnsignedType() )
      TCINSTRUCTIONS.insertJGE_U( p2.first, p1.first, block, $1->getExp() );
    else
      TCINSTRUCTIONS.insertJGE( p2.first, p1.first, block, $1->getExp() );
  else

  if ( operandType.getType() == IR_Type::FLOAT ) {
    LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );

    if ( TCCODESEL->getConfig()->getEmitFpuInstructions() ) {
      TCINSTRUCTIONS.insertCMP_F( regTmp, p1.first, p2.first, $1->getExp() );
      TCINSTRUCTIONS.insertJZ_T( regTmp, 2, block, $1->getExp() );
    } else {
      TCINSTRUCTIONS.insertGT_F( regTmp, p1.first, p2.first );
      TCINSTRUCTIONS.insertJLT( regTmp, 1, block, $1->getExp() );
    }
  }

  // WIR
  if ( operandType.isIntegralType() ) {
    if ( operandType.isUnsignedType() )
      TCINSTRUCTIONS.insertJGE_U( p2.second, p1.second, b, $1->getExp() );
    else
      TCINSTRUCTIONS.insertJGE( p2.second, p1.second, b, $1->getExp() );

    if ( markLoopExit ) {
      WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
      jmp.insertContainer( WIR_LoopExit( true ) );
    }
  } else

  if ( operandType.getType() == IR_Type::FLOAT ) {
    auto &tmpReg = TCINSTRUCTIONS.createDReg();

    if ( TCCODESEL->getConfig()->getEmitFpuInstructions() ) {
      TCINSTRUCTIONS.insertCMP_F( tmpReg, p1.second, p2.second, $1->getExp() );
      TCINSTRUCTIONS.insertJZ_T( tmpReg, 2, b, $1->getExp() );
    } else {
      TCINSTRUCTIONS.insertGT_F( tmpReg, p1.second, p2.second, $1->getExp() );
      TCINSTRUCTIONS.insertJLT( tmpReg, 1, b, $1->getExp() );
    }

    if ( markLoopExit ) {
      WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
      jmp.insertContainer( WIR_LoopExit( true ) );
    }
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
nrel: tpm_BinaryExpGT( dreg, constant0 )
{
  if ( $2->getExp()->getType().isIntegralType() &&
       effectiveType( *$2->getExp() ).isSignedType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "nrel: tpm_BinaryExpGT( dreg, constant0 )", $1 );

  auto p = $action[2]();

  // LLIR
  TCINSTRUCTIONS.insertJLT( p.first, 1, block, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertJLT( p.second, 1, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( true ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
nrel: tpm_BinaryExpGT( dreg, const4 )
{
  if ( $2->getExp()->getType().isIntegralType() &&
       effectiveType( *$2->getExp() ).isSignedType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;

  // Since JGT is missing, we need JLT. This means we can only use values < 0x7.
  if ( getConstIntValue( $3 ) >= TC_Const4_Signed::getMaxValue( 4 ) )
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "nrel: tpm_BinaryExpGT( dreg, const4 )", $1 );

  auto p = $action[2]();
  auto v = $action[3]().getIntValue() + 1;

  // LLIR
  TCINSTRUCTIONS.insertJLT( p.first, v, block, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertJLT( p.second, v, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( true ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
nrel: tpm_BinaryExpGT( dreg, uconst4 )
{
  if ( $2->getExp()->getType().isIntegralType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DC4L_2.getSize();
  else
    $cost[0] = COST_INFINITY;

  // Since JGT_U is missing, we need JLT_U. This means we can only use
  // values < 0xf.
  if ( getConstIntValue( $3 ) >= (int) TC_Const4_Unsigned::getMaxValue( 4 ) )
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "nrel: tpm_BinaryExpGT( dreg, uconst4 )", $1 );

  auto p = $action[2]();
  auto v = $action[3]().getIntValue() + 1;

  // LLIR
  TCINSTRUCTIONS.insertJLT_U( p.first, v, block, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertJLT_U( p.second, v, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( true ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
nrel: tpm_BinaryExpGT( dreg, const9 )
{
  if ( $2->getExp()->getType().isIntegralType() &&
       effectiveType( *$2->getExp() ).isSignedType() )
    $cost[0] =
      $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_1.getSize() +
      TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;

  // Since GT is missing, we need LT. This means we can only use values < 0xff.
  if ( getConstIntValue( $3 ) >= TC_Const9_Signed::getMaxValue( 9 ) )
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "nrel: tpm_BinaryExpGT( dreg, const9 )", $1 );

  auto p = $action[2]();
  auto v = $action[3]().getIntValue() + 1;

  // LLIR
  LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertLT( regTmp, p.first, v, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( regTmp, 0, block, $1->getExp() );

  // WIR
  auto &tmpReg = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertLT( tmpReg, p.second, v, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( tmpReg, 0, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( true ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
nrel: tpm_BinaryExpGT( dreg, uconst9 )
{
  if ( $2->getExp()->getType().isIntegralType() )
    $cost[0] =
      $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_2.getSize() +
      TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;

  // Since GT_U is missing, we need LT_U. This means we can only use
  // values < 0x1ff.
  if ( getConstIntValue( $3 ) >= (int) TC_Const9_Unsigned::getMaxValue( 9 ) )
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "nrel: tpm_BinaryExpGT( dreg, uconst9 )", $1 );

  auto p = $action[2]();
  auto v = $action[3]().getIntValue() + 1;

  // LLIR
  LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertLT_U( regTmp, p.first, v, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( regTmp, 0, block, $1->getExp() );

  // WIR
  auto &tmpReg = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertLT_U( tmpReg, p.second, v, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( tmpReg, 0, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( true ) );
  }

  return;
};


##############################################################################
#
#  Denormal relations (constants on the left)
#
#  NOTE: TriCore operations (J)LE and (J)GT do not exist.
#
##############################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
nrel: tpm_BinaryExpGT( constant0, dreg )
{
  if ( $3->getExp()->getType().isIntegralType() &&
       effectiveType( *$3->getExp() ).isSignedType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "nrel: tpm_BinaryExpGT( constant0, dreg )", $1 );

  auto p = $action[3]();

  // LLIR
  TCINSTRUCTIONS.insertJGE( p.first, 0, block, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertJGE( p.second, 0, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( true ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
nrel: tpm_BinaryExpGT( const4, dreg )
{
  if ( $3->getExp()->getType().isIntegralType() &&
       effectiveType( *$3->getExp() ).isSignedType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "nrel: tpm_BinaryExpGT( const4, dreg )", $1 );

  auto v = $action[2]().getIntValue();
  auto p = $action[3]();

  // LLIR
  TCINSTRUCTIONS.insertJGE( p.first, v, block, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertJGE( p.second, v, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( true ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
nrel: tpm_BinaryExpGT( uconst4, dreg )
{
  if ( $3->getExp()->getType().isIntegralType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DC4L_2.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "nrel: tpm_BinaryExpGT( uconst4, dreg )", $1 );

  auto v = $action[2]().getIntValue();
  auto p = $action[3]();

  // LLIR
  TCINSTRUCTIONS.insertJGE_U( p.first, v, block, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertJGE_U( p.second, v, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( true ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
nrel: tpm_BinaryExpGT( const9, dreg )
{
  if ( $3->getExp()->getType().isIntegralType() &&
       effectiveType( *$3->getExp() ).isSignedType() )
    $cost[0] =
      $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_1.getSize() +
      TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "nrel: tpm_BinaryExpGT( const9, dreg )", $1 );

  auto v = $action[2]().getIntValue();
  auto p = $action[3]();

  // LLIR
  LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertGE( regTmp, p.first, v, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( regTmp, 0, block, $1->getExp() );

  // WIR
  auto &tmpReg = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertGE( tmpReg, p.second, v, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( tmpReg, 0, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( true ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
nrel: tpm_BinaryExpGT( uconst9, dreg )
{
  if ( $3->getExp()->getType().isIntegralType() )
    $cost[0] =
      $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_2.getSize() +
      TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "nrel: tpm_BinaryExpGT( uconst9, dreg )", $1 );

  auto v = $action[2]().getIntValue();
  auto p = $action[3]();

  // LLIR
  LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertGE_U( regTmp, p.first, v, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( regTmp, 0, block, $1->getExp() );

  // WIR
  auto &tmpReg = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertGE_U( tmpReg, p.second, v, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( tmpReg, 0, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( true ) );
  }

  return;
};



##############################################################################
#
#  Normalized relations (constants on the right), as values
#
##############################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpGT( ereg, ereg )
{
  $cost[0] =
    $cost[2] + $cost[3] + SOFTFLOAT_COST +
    TC13::OperationFormat::DDC9_1.getSize();
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpGT( ereg, ereg )", $1 );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertGT_D( reg, p1.first, p2.first, $1->getExp() );
  TCINSTRUCTIONS.insertGE( reg, reg, 1, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertGT_D( r, p1.second, p2.second, $1->getExp() );
  TCINSTRUCTIONS.insertGE( r, r, 1, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpGT( dreg, dreg )
{
  $cost[0] = $cost[2] + $cost[3];

  auto &t = effectiveType( *$2->getExp() );
  if ( t.isIntegralType() )
    $cost[0] += TC13::OperationFormat::DDD_1.getSize();
  else
  if ( t.getType() == IR_Type::FLOAT ) {
    if ( TCCODESEL->getConfig()->getEmitFpuInstructions() )
      $cost[0] +=
        TC13::OperationFormat::DDD_1.getSize() +
        TC13::OperationFormat::DDC5DC5_1.getSize();
    else
      $cost[0] += SOFTFLOAT_COST + TC13::OperationFormat::DDC9_1.getSize();
  }
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpGT( dreg, dreg )", $1 );

  auto &t = effectiveType( *$2->getExp() );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  if ( t.isIntegralType() )
    if ( t.isUnsignedType() )
      TCINSTRUCTIONS.insertLT_U( reg, p2.first, p1.first, $1->getExp() );
    else
      TCINSTRUCTIONS.insertLT( reg, p2.first, p1.first, $1->getExp() );
  else
  if ( t.getType() == IR_Type::FLOAT ) {
    LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );
    if ( TCCODESEL->getConfig()->getEmitFpuInstructions() ) {
      TCINSTRUCTIONS.insertCMP_F( regTmp, p1.first, p2.first, $1->getExp() );
      TCINSTRUCTIONS.insertOR_T( reg, regTmp, 2, regTmp, 2, $1->getExp() );
    } else {
      TCINSTRUCTIONS.insertGT_F( regTmp, p1.first, p2.first );
      TCINSTRUCTIONS.insertGE( reg, regTmp, 1, $1->getExp() );
    }
  }

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  if ( t.isIntegralType() )
    if ( t.isUnsignedType() )
      TCINSTRUCTIONS.insertLT_U( r, p2.second, p1.second, $1->getExp() );
    else
      TCINSTRUCTIONS.insertLT( r, p2.second, p1.second, $1->getExp() );
  else
  if ( t.getType() == IR_Type::FLOAT ) {
    auto &tmpReg = TCINSTRUCTIONS.createDReg();
    if ( TCCODESEL->getConfig()->getEmitFpuInstructions() ) {
      TCINSTRUCTIONS.insertCMP_F( tmpReg, p1.second, p2.second, $1->getExp() );
      TCINSTRUCTIONS.insertOR_T( r, tmpReg, 2, tmpReg, 2, $1->getExp() );
    } else {
      TCINSTRUCTIONS.insertGT_F( tmpReg, p1.second, p2.second, $1->getExp() );
      TCINSTRUCTIONS.insertGE( r, tmpReg, 1, $1->getExp() );
    }
  }

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpGT( dreg, constant0 )
{
  if ( $2->getExp()->getType().isIntegralType() &&
       effectiveType( *$2->getExp() ).isSignedType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpGT( dreg, constant0 )", $1 );

  auto p = $action[2]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertGE( reg, p.first, 1, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertGE( r, p.second, 1, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpGT( dreg, const9 )
{
  if ( $2->getExp()->getType().isIntegralType() &&
       effectiveType( *$2->getExp() ).isSignedType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_1.getSize();
  else
    $cost[0] = COST_INFINITY;

  // Since JGT is missing, we need JGE. Means we can only use values < 0xff.
  if ( getConstIntValue( $3 ) >= 0xff )
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpGT( dreg, const9 )", $1 );

  auto p = $action[2]();
  auto v = $action[3]().getIntValue();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertGE( reg, p.first, v + 1, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertGE( r, p.second, v + 1, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpGT( dreg, uconst9 )
{
  if ( $2->getExp()->getType().isIntegralType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_2.getSize();
  else
    $cost[0] = COST_INFINITY;

  // Since JGT is missing, we need JGE. Means we can only use values < 0xff.
  if ( getConstIntValue( $3 ) >= 0x1ff )
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpGT( dreg, uconst9 )", $1 );

  auto p = $action[2]();
  auto v = $action[3]().getIntValue();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertGE_U( reg, p.first, v + 1, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertGE_U( r, p.second, v + 1, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpGT( areg, areg )
{
  $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DAA.getSize();
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpGT( areg, areg )", $1 );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertLT_A( reg, p2.first, p1.first, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertLT_A( r, p2.second, p1.second, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


##############################################################################
#
#  Denormal relations (constants on the left), as values
#
#  NOTE: Instructions (J)LE and (J)GT do not exist.
#
##############################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpGT( constant0, dreg )
{
  if ( $3->getExp()->getType().isIntegralType() &&
       effectiveType( *$3->getExp() ).isSignedType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpGT( constant0, dreg )", $1 );

  auto p = $action[3]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertSH( reg, p.first, -31, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertSH( r, p.second, -31, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpGT( const9, dreg )
{
  if ( $3->getExp()->getType().isIntegralType() &&
       effectiveType( *$3->getExp() ).isSignedType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpGT( const9, dreg )", $1 );

  auto v = $action[2]().getIntValue();
  auto p = $action[3]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertLT( reg, p.first, v, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertLT( r, p.second, v, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpGT( uconst9, dreg )
{
  if ( $3->getExp()->getType().isIntegralType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_2.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpGT( uconst9, dreg )", $1 );

  auto v = $action[2]().getIntValue();
  auto p = $action[3]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertLT_U( reg, p.first, v, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertLT_U( r, p.second, v, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


###############################################################################
#
#
#  Relations (lower-or-equal-than)
#
#
###############################################################################

####################################################################
#
# Jump on positives
#
####################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
rel: tpm_BinaryExpLEQ( dreg, dreg )
{
  $cost[0] = $cost[2] + $cost[3];

  IR_Type &operandType = effectiveType( *$2->getExp() );

  if ( operandType.isIntegralType() )
    $cost[0] += TC13::OperationFormat::DDL_1.getSize();
  else

  if ( operandType.getType() == IR_Type::FLOAT ) {
    if ( TCCODESEL->getConfig()->getEmitFpuInstructions() )
      $cost[0] +=
        TC13::OperationFormat::DDD_1.getSize() +
        TC13::OperationFormat::DC5L.getSize();
    else
      $cost[0] += SOFTFLOAT_COST + TC13::OperationFormat::DC4L_1.getSize();
  } else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpLEQ( dreg, dreg )", $1 );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  IR_Type &operandType = effectiveType( *$2->getExp() );

  // LLIR
  if ( operandType.isIntegralType() )
    if ( operandType.isUnsignedType() )
      TCINSTRUCTIONS.insertJGE_U( p2.first, p1.first, block, $1->getExp() );
    else
      TCINSTRUCTIONS.insertJGE( p2.first, p1.first, block, $1->getExp() );
  else

  if ( operandType.getType() == IR_Type::FLOAT ) {
    LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );

    if ( TCCODESEL->getConfig()->getEmitFpuInstructions() ) {
      TCINSTRUCTIONS.insertCMP_F( regTmp, p1.first, p2.first, $1->getExp() );
      TCINSTRUCTIONS.insertJZ_T( regTmp, 2, block, $1->getExp() );
    } else {
      TCINSTRUCTIONS.insertGT_F( regTmp, p1.first, p2.first );
      TCINSTRUCTIONS.insertJLT( regTmp, 1, block, $1->getExp() );
    }
  }

  // WIR
  if ( operandType.isIntegralType() ) {
    if ( operandType.isUnsignedType() )
      TCINSTRUCTIONS.insertJGE_U( p2.second, p1.second, b, $1->getExp() );
    else
      TCINSTRUCTIONS.insertJGE( p2.second, p1.second, b, $1->getExp() );

    if ( markLoopExit ) {
      WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
      jmp.insertContainer( WIR_LoopExit( false ) );
    }
  } else

  if ( operandType.getType() == IR_Type::FLOAT ) {
    auto &tmpReg = TCINSTRUCTIONS.createDReg();

    if ( TCCODESEL->getConfig()->getEmitFpuInstructions() ) {
      TCINSTRUCTIONS.insertCMP_F( tmpReg, p1.second, p2.second, $1->getExp() );
      TCINSTRUCTIONS.insertJZ_T( tmpReg, 2, b, $1->getExp() );
    } else {
      TCINSTRUCTIONS.insertGT_F( tmpReg, p1.second, p2.second, $1->getExp() );
      TCINSTRUCTIONS.insertJLT( tmpReg, 1, b, $1->getExp() );
    }

    if ( markLoopExit ) {
      WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
      jmp.insertContainer( WIR_LoopExit( false ) );
    }
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
rel: tpm_BinaryExpLEQ( dreg, constant0 )
{
  if ( $2->getExp()->getType().isIntegralType() &&
       effectiveType( *$2->getExp() ).isSignedType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpLEQ( dreg, constant0 )", $1 );

  auto p = $action[2]();

  // LLIR
  TCINSTRUCTIONS.insertJLT( p.first, 1, block, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertJLT( p.second, 1, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( false ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
rel: tpm_BinaryExpLEQ( dreg, const4 )
{
  if ( $2->getExp()->getType().isIntegralType() &&
       effectiveType( *$2->getExp() ).isSignedType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;

  // Since JLE is missing, we need JLT. This means we can only use values < 0x7.
  if ( getConstIntValue( $3 ) >= TC_Const4_Signed::getMaxValue( 4 ) )
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpLEQ( dreg, const4 )", $1 );

  auto p = $action[2]();
  auto v = $action[3]().getIntValue() + 1;

  // LLIR
  TCINSTRUCTIONS.insertJLT( p.first, v, block, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertJLT( p.second, v, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( false ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
rel: tpm_BinaryExpLEQ( dreg, uconst4 )
{
  if ( $2->getExp()->getType().isIntegralType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DC4L_2.getSize();
  else
    $cost[0] = COST_INFINITY;

  // Since JLE_U is missing, we need JLT_U. This means we can only use
  // values < 0xf.
  if ( getConstIntValue( $3 ) >= (int) TC_Const4_Unsigned::getMaxValue( 4 ) )
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpLEQ( dreg, uconst4 )", $1 );

  auto p = $action[2]();
  auto v = $action[3]().getIntValue() + 1;

  // LLIR
  TCINSTRUCTIONS.insertJLT_U( p.first, v, block, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertJLT_U( p.second, v, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( false ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
rel: tpm_BinaryExpLEQ( dreg, const9 )
{
  if ( $2->getExp()->getType().isIntegralType() &&
       effectiveType( *$2->getExp() ).isSignedType() )
    $cost[0] =
      $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_1.getSize() +
      TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;

  // Since LE is missing, we need LT. This means we can only use values < 0xff.
  if ( getConstIntValue( $3 ) >= TC_Const9_Signed::getMaxValue( 9 ) )
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpLEQ( dreg, const9 )", $1 );

  auto p = $action[2]();
  auto v = $action[3]().getIntValue() + 1;

  // LLIR
  LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertLT( regTmp, p.first, v, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( regTmp, 0, block, $1->getExp() );

  // WIR
  auto &tmpReg = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertLT( tmpReg, p.second, v, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( tmpReg, 0, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( false ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
rel: tpm_BinaryExpLEQ( dreg, uconst9 )
{
  if ( $2->getExp()->getType().isIntegralType() )
    $cost[0] =
      $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_2.getSize() +
      TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;

  // Since LE_U is missing, we need LT_U. This means we can only use
  // values < 0x1ff.
  if ( getConstIntValue( $3 ) >= (int) TC_Const9_Unsigned::getMaxValue( 9 ) )
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpLEQ( dreg, uconst9 )", $1 );

  auto p = $action[2]();
  auto v = $action[3]().getIntValue() + 1;

  // LLIR
  LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertLT_U( regTmp, p.first, v, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( regTmp, 0, block, $1->getExp() );

  // WIR
  auto &tmpReg = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertLT_U( tmpReg, p.second, v, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( tmpReg, 0, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( false ) );
  }

  return;
};


##############################################################################
#
#  Denormal relations (constants on the left)
#
#  NOTE: Instructions (J)LE and (J)GT do not exist.
#
##############################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
rel: tpm_BinaryExpLEQ( constant0, dreg )
{
  if ( $3->getExp()->getType().isIntegralType() &&
       effectiveType( *$3->getExp() ).isSignedType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpLEQ( constant0, dreg )", $1 );

  auto p = $action[3]();

  // LLIR
  TCINSTRUCTIONS.insertJGE( p.first, 0, block, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertJGE( p.second, 0, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( false ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
rel: tpm_BinaryExpLEQ( const4, dreg )
{
  if ( $3->getExp()->getType().isIntegralType() &&
       effectiveType( *$3->getExp() ).isSignedType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpLEQ( const4, dreg )", $1 );

  auto v = $action[2]().getIntValue();
  auto p = $action[3]();

  // LLIR
  TCINSTRUCTIONS.insertJGE( p.first, v, block, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertJGE( p.second, v, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( false ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
rel: tpm_BinaryExpLEQ( uconst4, dreg )
{
  if ( $3->getExp()->getType().isIntegralType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DC4L_2.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpLEQ( uconst4, dreg )", $1 );

  auto v = $action[2]().getIntValue();
  auto p = $action[3]();

  // LLIR
  TCINSTRUCTIONS.insertJGE_U( p.first, v, block, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertJGE_U( p.second, v, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( false ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
rel: tpm_BinaryExpLEQ( const9, dreg )
{
  if ( $3->getExp()->getType().isIntegralType() &&
       effectiveType( *$3->getExp() ).isSignedType() )
    $cost[0] =
      $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_1.getSize() +
      TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpLEQ( const9, dreg )", $1 );

  auto v = $action[2]().getIntValue();
  auto p = $action[3]();

  // LLIR
  LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertGE( regTmp, p.first, v, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( regTmp, 0, block, $1->getExp() );

  // WIR
  auto &tmpReg = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertGE( tmpReg, p.second, v, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( tmpReg, 0, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( false ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
rel: tpm_BinaryExpLEQ( uconst9, dreg )
{
  if ( $3->getExp()->getType().isIntegralType() )
    $cost[0] =
      $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_2.getSize() +
      TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpLEQ( uconst9, dreg )", $1 );

  auto v = $action[2]().getIntValue();
  auto p = $action[3]();

  // LLIR
  LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertGE_U( regTmp, p.first, v, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( regTmp, 0, block, $1->getExp() );

  // WIR
  auto &tmpReg = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertGE_U( tmpReg, p.second, v, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( tmpReg, 0, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( false ) );
  }

  return;
};


####################################################################
#
# Jump on negatives
#
####################################################################

##########################################
#
#  Normalized relations (constants on the right), with implicit jump
#
#########################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
nrel: tpm_BinaryExpLEQ( dreg, dreg )
{
  $cost[0] = $cost[2] + $cost[3];

  IR_Type &operandType = effectiveType( *$2->getExp() );

  if ( operandType.isIntegralType() )
    if ( operandType.isUnsignedType() )
      $cost[0] += TC13::OperationFormat::DC4L_2.getSize();
    else
      $cost[0] += TC13::OperationFormat::DC4L_1.getSize();
  else

  if ( operandType.getType() == IR_Type::FLOAT ) {
    if ( TCCODESEL->getConfig()->getEmitFpuInstructions() )
      $cost[0] +=
        TC13::OperationFormat::DDD_1.getSize() +
        2 * TC13::OperationFormat::DC5L.getSize();
    else
      $cost[0] += SOFTFLOAT_COST + TC13::OperationFormat::DC4L_1.getSize();
  } else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "nrel: tpm_BinaryExpLEQ( dreg, dreg )", $1 );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  IR_Type &operandType = effectiveType( *$2->getExp() );

  // LLIR
  if ( operandType.isIntegralType() )
    if ( operandType.isUnsignedType() )
      TCINSTRUCTIONS.insertJLT_U( p2.first, p1.first, block, $1->getExp() );
    else
      TCINSTRUCTIONS.insertJLT( p2.first, p1.first, block, $1->getExp() );
  else

  if ( operandType.getType() == IR_Type::FLOAT ) {
    LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );

    if ( TCCODESEL->getConfig()->getEmitFpuInstructions() ) {
      TCINSTRUCTIONS.insertCMP_F( regTmp, p1.first, p2.first, $1->getExp() );
      TCINSTRUCTIONS.insertJZ_T( regTmp, 0, block, $1->getExp() );
      TCINSTRUCTIONS.insertJZ_T( regTmp, 1, block, $1->getExp() );
    } else {
      TCINSTRUCTIONS.insertLE_F( regTmp, p1.first, p2.first );
      TCINSTRUCTIONS.insertJGE( regTmp, 1, block, $1->getExp() );
    }
  }

  // WIR
  if ( operandType.isIntegralType() ) {
    if ( operandType.isUnsignedType() )
      TCINSTRUCTIONS.insertJLT_U( p2.second, p1.second, b, $1->getExp() );
    else
      TCINSTRUCTIONS.insertJLT( p2.second, p1.second, b, $1->getExp() );

    if ( markLoopExit ) {
      WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
      jmp.insertContainer( WIR_LoopExit( true ) );
    }
  } else

  if ( operandType.getType() == IR_Type::FLOAT ) {
    auto &tmpReg = TCINSTRUCTIONS.createDReg();

    if ( TCCODESEL->getConfig()->getEmitFpuInstructions() ) {
      TCINSTRUCTIONS.insertCMP_F( tmpReg, p1.second, p2.second, $1->getExp() );
      TCINSTRUCTIONS.insertJZ_T( tmpReg, 0, b, $1->getExp() );
      TCINSTRUCTIONS.insertJZ_T( tmpReg, 1, b, $1->getExp() );
    } else {
      TCINSTRUCTIONS.insertLE_F( tmpReg, p1.second, p2.second, $1->getExp() );
      TCINSTRUCTIONS.insertJGE( tmpReg, 1, b, $1->getExp() );
    }

    if ( markLoopExit ) {
      WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
      jmp.insertContainer( WIR_LoopExit( true ) );
    }
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
nrel: tpm_BinaryExpLEQ( dreg, constant0 )
{
  if ( $2->getExp()->getType().isIntegralType() &&
       effectiveType( *$2->getExp() ).isSignedType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "nrel: tpm_BinaryExpLEQ( dreg, constant0 )", $1 );

  auto p = $action[2]();

  // LLIR
  TCINSTRUCTIONS.insertJGE( p.first, 1, block, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertJGE( p.second, 1, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( true ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
nrel: tpm_BinaryExpLEQ( dreg, const4 )
{
  if ( $2->getExp()->getType().isIntegralType() &&
       effectiveType( *$2->getExp() ).isSignedType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;

  // Since JLEQ is missing, we need JGE. This means we can only use
  // values < 0x7.
  if ( getConstIntValue( $3 ) >= TC_Const4_Signed::getMaxValue( 4 ) )
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "nrel: tpm_BinaryExpLEQ( dreg, const4 )", $1 );

  auto p = $action[2]();
  auto v = $action[3]().getIntValue() + 1;

  // LLIR
  TCINSTRUCTIONS.insertJGE( p.first, v, block, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertJGE( p.second, v, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( true ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
nrel: tpm_BinaryExpLEQ( dreg, uconst4 )
{
  if ( $2->getExp()->getType().isIntegralType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DC4L_2.getSize();
  else
    $cost[0] = COST_INFINITY;

  // Since JLEQ_U is missing, we need JGE_U. This means we can only use
  // values < 0xf.
  if ( getConstIntValue( $3 ) >= (int) TC_Const4_Unsigned::getMaxValue( 4 ) )
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "nrel: tpm_BinaryExpLEQ( dreg, uconst4 )", $1 );

  auto p = $action[2]();
  auto v = $action[3]().getIntValue() + 1;

  // LLIR
  TCINSTRUCTIONS.insertJGE_U( p.first, v, block, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertJGE_U( p.second, v, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( true ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
nrel: tpm_BinaryExpLEQ( dreg, const9 )
{
  if ( $2->getExp()->getType().isIntegralType() &&
       effectiveType( *$2->getExp() ).isSignedType() )
    $cost[0] =
      $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_1.getSize() +
      TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;

  // Since LEQ is missing, we need GE. Thie means we can only use values < 0xff.
  if ( getConstIntValue( $3 ) >= TC_Const9_Signed::getMaxValue( 9 ) )
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "nrel: tpm_BinaryExpLEQ( dreg, const9 )", $1 );

  auto p = $action[2]();
  auto v = $action[3]().getIntValue() + 1;

  // LLIR
  LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertGE( regTmp, p.first, v, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( regTmp, 0, block, $1->getExp() );

  // WIR
  auto &tmpReg = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertGE( tmpReg, p.second, v, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( tmpReg, 0, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( true ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
nrel: tpm_BinaryExpLEQ( dreg, uconst9 )
{
  if ( $2->getExp()->getType().isIntegralType() )
    $cost[0] =
      $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_2.getSize() +
      TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;

  // Since LEQ_U is missing, we need GE_U. This means we can only use
  // values < 0x1ff.
  if ( getConstIntValue( $3 ) >= (int) TC_Const9_Unsigned::getMaxValue( 9 ) )
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "nrel: tpm_BinaryExpLEQ( dreg, uconst9 )", $1 );

  auto p = $action[2]();
  auto v = $action[3]().getIntValue() + 1;

  // LLIR
  LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertGE_U( regTmp, p.first, v, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( regTmp, 0, block, $1->getExp() );

  // WIR
  auto &tmpReg = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertGE_U( tmpReg, p.second, v, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( tmpReg, 0, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( true ) );
  }

  return;
};


##############################################################################
#
#  Denormal relations (constants on the left)
#
#  NOTE: Instructions (J)LE and (J)GT do not exist.
#
##############################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
nrel: tpm_BinaryExpLEQ( constant0, dreg )
{
  if ( $3->getExp()->getType().isIntegralType() &&
       effectiveType( *$3->getExp() ).isSignedType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "nrel: tpm_BinaryExpLEQ( constant0, dreg )", $1 );

  auto p = $action[3]();

  // LLIR
  TCINSTRUCTIONS.insertJLT( p.first, 0, block, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertJLT( p.second, 0, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( true ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
nrel: tpm_BinaryExpLEQ( const4, dreg )
{
  if ( $3->getExp()->getType().isIntegralType() &&
       effectiveType( *$3->getExp() ).isSignedType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "nrel: tpm_BinaryExpLEQ( const4, dreg )", $1 );

  auto v = $action[2]().getIntValue();
  auto p = $action[3]();

  // LLIR
  TCINSTRUCTIONS.insertJLT( p.first, v, block, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertJLT( p.second, v, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( true ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
nrel: tpm_BinaryExpLEQ( uconst4, dreg )
{
  if ( $3->getExp()->getType().isIntegralType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DC4L_2.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "nrel: tpm_BinaryExpLEQ( uconst4, dreg )", $1 );

  auto v = $action[2]().getIntValue();
  auto p = $action[3]();

  // LLIR
  TCINSTRUCTIONS.insertJLT_U( p.first, v, block, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertJLT_U( p.second, v, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( true ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
nrel: tpm_BinaryExpLEQ( const9, dreg )
{
  if ( $3->getExp()->getType().isIntegralType() &&
       effectiveType( *$3->getExp() ).isSignedType() )
    $cost[0] =
      $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_1.getSize() +
      TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "nrel: tpm_BinaryExpLEQ( const9, dreg )", $1 );

  auto v = $action[2]().getIntValue();
  auto p = $action[3]();

  // LLIR
  LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertLT( regTmp, p.first, v, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( regTmp, 0, block, $1->getExp() );

  // WIR
  auto &tmpReg = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertLT( tmpReg, p.second, v, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( tmpReg, 0, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( true ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
nrel: tpm_BinaryExpLEQ( uconst9, dreg )
{
  if ( $3->getExp()->getType().isIntegralType() )
    $cost[0] =
      $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_2.getSize() +
      TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "nrel: tpm_BinaryExpLEQ( uconst9, dreg )", $1 );

  auto v = $action[2]().getIntValue();
  auto p = $action[3]();

  // LLIR
  LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertLT_U( regTmp, p.first, v, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( regTmp, 0, block, $1->getExp() );

  // WIR
  auto &tmpReg = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertLT_U( tmpReg, p.second, v, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( tmpReg, 0, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( true ) );
  }

  return;
};


##############################################################################
#
#  Normalized relations (constants on the right), as values
#
##############################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpLEQ( ereg, ereg )
{
  $cost[0] =
    $cost[2] + $cost[3] + SOFTFLOAT_COST +
    TC13::OperationFormat::DDC9_1.getSize();
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpLEQ( ereg, ereg )", $1 );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertLE_D( reg, p1.first, p2.first, $1->getExp() );
  TCINSTRUCTIONS.insertLT( reg, reg, 1, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertLE_D( r, p1.second, p2.second, $1->getExp() );
  TCINSTRUCTIONS.insertLT( r, r, 1, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpLEQ( dreg, dreg )
{
  $cost[0] = $cost[2] + $cost[3];

  auto &t = effectiveType( *$2->getExp() );
  if ( t.isIntegralType() )
    $cost[0] += TC13::OperationFormat::DDD_1.getSize();
  else
  if ( t.getType() == IR_Type::FLOAT ) {
    if ( TCCODESEL->getConfig()->getEmitFpuInstructions() )
      $cost[0] +=
        TC13::OperationFormat::DDD_1.getSize() +
        TC13::OperationFormat::DDC5DC5_1.getSize();
    else
      $cost[0] += SOFTFLOAT_COST + TC13::OperationFormat::DDC9_1.getSize();
  }
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpLEQ( dreg, dreg )", $1 );

  auto &t = effectiveType( *$2->getExp() );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  if ( t.isIntegralType() )
    if ( t.isUnsignedType() )
      TCINSTRUCTIONS.insertGE_U( reg, p2.first, p1.first, $1->getExp() );
    else
      TCINSTRUCTIONS.insertGE( reg, p2.first, p1.first, $1->getExp() );
  else
  if ( t.getType() == IR_Type::FLOAT ) {
    LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );
    if ( TCCODESEL->getConfig()->getEmitFpuInstructions() ) {
      TCINSTRUCTIONS.insertCMP_F( regTmp, p1.first, p2.first, $1->getExp() );
      TCINSTRUCTIONS.insertOR_T( reg, regTmp, 0, regTmp, 1, $1->getExp() );
    } else {
      TCINSTRUCTIONS.insertLE_F( regTmp, p1.first, p2.first );
      TCINSTRUCTIONS.insertLT( reg, regTmp, 1, $1->getExp() );
    }
  }

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  if ( t.isIntegralType() )
    if ( t.isUnsignedType() )
      TCINSTRUCTIONS.insertGE_U( r, p2.second, p1.second, $1->getExp() );
    else
      TCINSTRUCTIONS.insertGE( r, p2.second, p1.second, $1->getExp() );
  else
  if ( t.getType() == IR_Type::FLOAT ) {
    auto &tmpReg = TCINSTRUCTIONS.createDReg();
    if ( TCCODESEL->getConfig()->getEmitFpuInstructions() ) {
      TCINSTRUCTIONS.insertCMP_F( tmpReg, p1.second, p2.second, $1->getExp() );
      TCINSTRUCTIONS.insertOR_T( r, tmpReg, 0, tmpReg, 1, $1->getExp() );
    } else {
      TCINSTRUCTIONS.insertLE_F( tmpReg, p1.second, p2.second, $1->getExp());
      TCINSTRUCTIONS.insertLT( r, tmpReg, 1, $1->getExp() );
    }
  }

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpLEQ( dreg, constant0 )
{
  if ( $2->getExp()->getType().isIntegralType() &&
       effectiveType( *$2->getExp() ).isSignedType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpLEQ( dreg, constant0 )", $1 );

  auto p = $action[2]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertLT( reg, p.first, 1, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertLT( r, p.second, 1, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpLEQ( dreg, const9 )
{
  if ( $2->getExp()->getType().isIntegralType() &&
       effectiveType( *$2->getExp() ).isSignedType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_1.getSize();
  else
    $cost[0] = COST_INFINITY;

  // Since JLEQ is missing, we need JLT. Means we can only use values < 0xff.
  if ( getConstIntValue( $3 ) >= 0xff )
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpLEQ( dreg, const9 )", $1 );

  auto p = $action[2]();
  auto v = $action[3]().getIntValue();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertLT( reg, p.first, v + 1, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertLT( r, p.second, v + 1, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpLEQ( dreg, uconst9 )
{
  if ( $2->getExp()->getType().isIntegralType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_2.getSize();
  else
    $cost[0] = COST_INFINITY;

  // Since JLEQ is missing, we need JLT. Means we can only use values < 0x1ff.
  if ( getConstIntValue( $3 ) >= 0x1ff )
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpLEQ( dreg, uconst9 )", $1 );

  auto p = $action[2]();
  auto v = $action[3]().getIntValue();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertLT_U( reg, p.first, v + 1, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertLT_U( r, p.second, v + 1, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpLEQ( areg, areg )
{
  $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DAA.getSize();
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpLEQ( areg, areg )", $1 );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertGE_A( reg, p2.first, p1.first, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertGE_A( r, p2.second, p1.second, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


##############################################################################
#
#  Denormal relations (constants on the left), as values
#
#  NOTE: Instructions (J)LE and (J)GT do not exist.
#
##############################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpLEQ( constant0, dreg )
{
  if ( $3->getExp()->getType().isIntegralType() &&
       effectiveType( *$3->getExp() ).isSignedType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpLEQ( constant0, dreg )", $1 );

  auto p = $action[3]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertGE( reg, p.first, 0, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertGE( r, p.second, 0, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpLEQ( const9, dreg )
{
  if ( $3->getExp()->getType().isIntegralType() &&
       effectiveType( *$3->getExp() ).isSignedType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpLEQ( const9, dreg )", $1 );

  auto v = $action[2]().getIntValue();
  auto p = $action[3]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertGE( reg, p.first, v, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertGE( r, p.second, v, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpLEQ( uconst9, dreg )
{
  if ( $3->getExp()->getType().isIntegralType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_2.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpLEQ( uconst9, dreg )", $1 );

  auto v = $action[2]().getIntValue();
  auto p = $action[3]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertGE_U( reg, p.first, v, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertGE_U( r, p.second, v, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


###############################################################################
#
#
#  Relations (greater-or-equal-than)
#
#
###############################################################################

####################################################################
#
# Jump on positives
#
####################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
rel: tpm_BinaryExpGEQ( dreg, dreg )
{
  $cost[0] = $cost[2] + $cost[3];

  IR_Type &operandType = effectiveType( *$2->getExp() );

  if ( operandType.isIntegralType() )
    $cost[0] += TC13::OperationFormat::DDL_1.getSize();
  else

  if ( operandType.getType() == IR_Type::FLOAT ) {
    if ( TCCODESEL->getConfig()->getEmitFpuInstructions() )
      $cost[0] +=
        TC13::OperationFormat::DDD_1.getSize() +
        TC13::OperationFormat::DC5L.getSize();
    else
      $cost[0] += SOFTFLOAT_COST + TC13::OperationFormat::DC4L_1.getSize();
  } else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpGEQ( dreg, dreg )", $1 );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  IR_Type &operandType = effectiveType( *$2->getExp() );

  // LLIR
  if ( operandType.isIntegralType() )
    if ( operandType.isUnsignedType() )
      TCINSTRUCTIONS.insertJGE_U( p1.first, p2.first, block, $1->getExp() );
    else
      TCINSTRUCTIONS.insertJGE( p1.first, p2.first, block, $1->getExp() );
  else

  if ( operandType.getType() == IR_Type::FLOAT ) {
    LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );

    if ( TCCODESEL->getConfig()->getEmitFpuInstructions() ) {
      TCINSTRUCTIONS.insertCMP_F( regTmp, p1.first, p2.first, $1->getExp() );
      TCINSTRUCTIONS.insertJZ_T( regTmp, 0, block, $1->getExp() );
    } else {
      TCINSTRUCTIONS.insertLT_F( regTmp, p1.first, p2.first );
      TCINSTRUCTIONS.insertJGE( regTmp, 0, block, $1->getExp() );
    }
  }

  // WIR
  if ( operandType.isIntegralType() ) {
    if ( operandType.isUnsignedType() )
      TCINSTRUCTIONS.insertJGE_U( p1.second, p2.second, b, $1->getExp() );
    else
      TCINSTRUCTIONS.insertJGE( p1.second, p2.second, b, $1->getExp() );

    if ( markLoopExit ) {
      WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
      jmp.insertContainer( WIR_LoopExit( false ) );
    }
  } else

  if ( operandType.getType() == IR_Type::FLOAT ) {
    auto &tmpReg = TCINSTRUCTIONS.createDReg();

    if ( TCCODESEL->getConfig()->getEmitFpuInstructions() ) {
      TCINSTRUCTIONS.insertCMP_F( tmpReg, p1.second, p2.second, $1->getExp() );
      TCINSTRUCTIONS.insertJZ_T( tmpReg, 0, b, $1->getExp() );
    } else {
      TCINSTRUCTIONS.insertLT_F( tmpReg, p1.second, p2.second, $1->getExp() );
      TCINSTRUCTIONS.insertJGE( tmpReg, 0, b, $1->getExp() );
    }

    if ( markLoopExit ) {
      WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
      jmp.insertContainer( WIR_LoopExit( false ) );
    }
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
rel: tpm_BinaryExpGEQ( dreg, constant0 )
{
  if ( $2->getExp()->getType().isIntegralType() &&
       effectiveType( *$2->getExp() ).isSignedType() )
    $cost[0] += $cost[2] + $cost[3] + TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpGEQ( dreg, constant0 )", $1 );

  auto p = $action[2]();

  // LLIR
  TCINSTRUCTIONS.insertJGE( p.first, 0, block, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertJGE( p.second, 0, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( false ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
rel: tpm_BinaryExpGEQ( dreg, const4 )
{
  if ( $2->getExp()->getType().isIntegralType() &&
       effectiveType( *$2->getExp() ).isSignedType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpGEQ( dreg, const4 )", $1 );

  auto p = $action[2]();
  auto v = $action[3]().getIntValue();

  // LLIR
  TCINSTRUCTIONS.insertJGE( p.first, v, block, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertJGE( p.second, v, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( false ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
rel: tpm_BinaryExpGEQ( dreg, uconst4 )
{
  if ( $2->getExp()->getType().isIntegralType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DC4L_2.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpGEQ( dreg, uconst4 )", $1 );

  auto p = $action[2]();
  auto v = $action[3]().getIntValue();

  // LLIR
  TCINSTRUCTIONS.insertJGE_U( p.first, v, block, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertJGE_U( p.second, v, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( false ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
rel: tpm_BinaryExpGEQ( dreg, const9 )
{
  if ( $2->getExp()->getType().isIntegralType() &&
       effectiveType( *$2->getExp() ).isSignedType() )
    $cost[0] =
      $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_1.getSize() +
      TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpGEQ( dreg, const9 )", $1 );

  auto p = $action[2]();
  auto v = $action[3]().getIntValue();

  // LLIR
  LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertLT( regTmp, p.first, v, $1->getExp() );
  TCINSTRUCTIONS.insertJEQ( regTmp, 0, block, $1->getExp() );

  // WIR
  auto &tmpReg = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertLT( tmpReg, p.second, v, $1->getExp() );
  TCINSTRUCTIONS.insertJEQ( tmpReg, 0, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( false ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
rel: tpm_BinaryExpGEQ( dreg, uconst9 )
{
  if ( $2->getExp()->getType().isIntegralType() )
    $cost[0] =
      $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_2.getSize() +
      TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpGEQ( dreg, uconst9 )", $1 );

  auto p = $action[2]();
  auto v = $action[3]().getIntValue();

  // LLIR
  LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertLT_U( regTmp, p.first, v, $1->getExp() );
  TCINSTRUCTIONS.insertJEQ( regTmp, 0, block, $1->getExp() );

  // WIR
  auto &tmpReg = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertLT_U( tmpReg, p.second, v, $1->getExp() );
  TCINSTRUCTIONS.insertJEQ( tmpReg, 0, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( false ) );
  }

  return;
};


##############################################################################
#
#  Denormal relations (constants on the left)
#
#  NOTE: Instructions (J)LE and (J)GT do not exist.
#
##############################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
rel: tpm_BinaryExpGEQ( constant0, dreg )
{
  if ( $3->getExp()->getType().isIntegralType() &&
       effectiveType( *$3->getExp() ).isSignedType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpGEQ( constant0, dreg )", $1 );

  auto p = $action[3]();

  // LLIR
  TCINSTRUCTIONS.insertJLT( p.first, 1, block, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertJLT( p.second, 1, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( false ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
rel: tpm_BinaryExpGEQ( const4, dreg )
{
  if ( $3->getExp()->getType().isIntegralType() &&
       effectiveType( *$3->getExp() ).isSignedType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;

  // Since JLE is missing, we need JLT. This means we can only use values < 0x7.
  if ( getConstIntValue( $2 ) >= TC_Const4_Signed::getMaxValue( 4 ) )
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpGEQ( const4, dreg )", $1 );

  auto v = $action[2]().getIntValue() + 1;
  auto p = $action[3]();

  // LLIR
  TCINSTRUCTIONS.insertJLT( p.first, v, block, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertJLT( p.second, v, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( false ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
rel: tpm_BinaryExpGEQ( uconst4, dreg )
{
  if ( $3->getExp()->getType().isIntegralType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DC4L_2.getSize();
  else
    $cost[0] = COST_INFINITY;

  // Since JLE_U is missing, we need JLT_U. This means we can only use
  // values < 0xf.
  if ( getConstIntValue( $2 ) >= (int) TC_Const4_Unsigned::getMaxValue( 4 ) )
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpGEQ( uconst4, dreg )", $1 );

  auto v = $action[2]().getIntValue() + 1;
  auto p = $action[3]();

  // LLIR
  TCINSTRUCTIONS.insertJLT_U( p.first, v, block, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertJLT_U( p.second, v, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( false ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
rel: tpm_BinaryExpGEQ( const9, dreg )
{
  if ( $3->getExp()->getType().isIntegralType() &&
       effectiveType( *$3->getExp() ).isSignedType() )
    $cost[0] =
      $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_1.getSize() +
      TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;

  // Since LE is missing, we need LT. This means we can only use values < 0xff.
  if ( getConstIntValue( $2 ) >= TC_Const9_Signed::getMaxValue( 9 ) )
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpGEQ( const9, dreg )", $1 );

  auto v = $action[2]().getIntValue() + 1;
  auto p = $action[3]();

  // LLIR
  LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertLT( regTmp, p.first, v, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( regTmp, 0, block, $1->getExp() );

  // WIR
  auto &tmpReg = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertLT( tmpReg, p.second, v, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( tmpReg, 0, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( false ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
rel: tpm_BinaryExpGEQ( uconst9, dreg )
{
  if ( $3->getExp()->getType().isIntegralType() )
    $cost[0] =
      $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_2.getSize() +
      TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;

  // Since LE_U is missing, we need LT_U. This means we can only use
  // values < 0x1ff.
  if ( getConstIntValue( $2 ) >= (int) TC_Const9_Unsigned::getMaxValue( 9 ) )
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpGEQ( uconst9, dreg )", $1 );

  auto v = $action[2]().getIntValue() + 1;
  auto p = $action[3]();

  // LLIR
  LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertLT_U( regTmp, p.first, v, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( regTmp, 0, block, $1->getExp() );

  // WIR
  auto &tmpReg = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertLT_U( tmpReg, p.second, v, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( tmpReg, 0, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( false ) );
  }

  return;
};


####################################################################
#
# Jump on negatives
#
####################################################################

##########################################
#
#  Normalized relations (constants on the right), with implicit jump
#
##########################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
nrel: tpm_BinaryExpGEQ( dreg, dreg )
{
  $cost[0] = $cost[2] + $cost[3];

  IR_Type &operandType = effectiveType( *$2->getExp() );

  if ( operandType.isIntegralType() )
    $cost[0] += TC13::OperationFormat::DDL_1.getSize();
  else

  if ( operandType.getType() == IR_Type::FLOAT ) {
    if ( TCCODESEL->getConfig()->getEmitFpuInstructions() )
      $cost[0] +=
        TC13::OperationFormat::DDD_1.getSize() +
        2 * TC13::OperationFormat::DC5L.getSize();
    else
      $cost[0] += SOFTFLOAT_COST + TC13::OperationFormat::DC4L_1.getSize();
  } else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "nrel: tpm_BinaryExpGEQ( dreg, dreg )", $1 );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  IR_Type &operandType = effectiveType( *$2->getExp() );

  // LLIR
  if ( operandType.isIntegralType() )
    if ( operandType.isUnsignedType() )
      TCINSTRUCTIONS.insertJLT_U( p1.first, p2.first, block, $1->getExp() );
    else
      TCINSTRUCTIONS.insertJLT( p1.first, p2.first, block, $1->getExp() );
  else

  if ( operandType.getType() == IR_Type::FLOAT ) {
    LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );

    if ( TCCODESEL->getConfig()->getEmitFpuInstructions() ) {
      TCINSTRUCTIONS.insertCMP_F( regTmp, p1.first, p2.first, $1->getExp() );
      TCINSTRUCTIONS.insertJZ_T( regTmp, 1, block, $1->getExp() );
      TCINSTRUCTIONS.insertJZ_T( regTmp, 2, block, $1->getExp() );
    } else {
      TCINSTRUCTIONS.insertGE_F( regTmp, p1.first, p2.first );
      TCINSTRUCTIONS.insertJLT( regTmp, 0, block, $1->getExp() );
    }
  }

  // WIR
  if ( operandType.isIntegralType() ) {
    if ( operandType.isUnsignedType() )
      TCINSTRUCTIONS.insertJLT_U( p1.second, p2.second, b, $1->getExp() );
    else
      TCINSTRUCTIONS.insertJLT( p1.second, p2.second, b, $1->getExp() );

    if ( markLoopExit ) {
      WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
      jmp.insertContainer( WIR_LoopExit( true ) );
    }
  } else

  if ( operandType.getType() == IR_Type::FLOAT ) {
    auto &tmpReg = TCINSTRUCTIONS.createDReg();

    if ( TCCODESEL->getConfig()->getEmitFpuInstructions() ) {
      TCINSTRUCTIONS.insertCMP_F( tmpReg, p1.second, p2.second, $1->getExp() );
      TCINSTRUCTIONS.insertJZ_T( tmpReg, 1, b, $1->getExp() );
      TCINSTRUCTIONS.insertJZ_T( tmpReg, 2, b, $1->getExp() );
    } else {
      TCINSTRUCTIONS.insertGE_F( tmpReg, p1.second, p2.second, $1->getExp() );
      TCINSTRUCTIONS.insertJLT( tmpReg, 0, b, $1->getExp() );
    }

    if ( markLoopExit ) {
      WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
      jmp.insertContainer( WIR_LoopExit( true ) );
    }
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
nrel: tpm_BinaryExpGEQ( dreg, constant0 )
{
  if ( $2->getExp()->getType().isIntegralType() &&
       effectiveType( *$2->getExp() ).isSignedType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "nrel: tpm_BinaryExpGEQ( dreg, constant0 )", $1 );

  auto p = $action[2]();

  // LLIR
  TCINSTRUCTIONS.insertJLT( p.first, 0, block, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertJLT( p.second, 0, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( true ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
nrel: tpm_BinaryExpGEQ( dreg, const4 )
{
  if ( $2->getExp()->getType().isIntegralType() &&
       effectiveType( *$2->getExp() ).isSignedType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "nrel: tpm_BinaryExpGEQ( dreg, const4 )", $1 );

  auto p = $action[2]();
  auto v = $action[3]().getIntValue();

  // LLIR
  TCINSTRUCTIONS.insertJLT( p.first, v, block, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertJLT( p.second, v, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( true ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
nrel: tpm_BinaryExpGEQ( dreg, uconst4 )
{
  if ( $2->getExp()->getType().isIntegralType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DC4L_2.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "nrel: tpm_BinaryExpGEQ( dreg, uconst4 )", $1 );

  auto p = $action[2]();
  auto v = $action[3]().getIntValue();

  // LLIR
  TCINSTRUCTIONS.insertJLT_U( p.first, v, block, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertJLT_U( p.second, v, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( true ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
nrel: tpm_BinaryExpGEQ( dreg, const9 )
{
  if ( $2->getExp()->getType().isIntegralType() &&
       effectiveType( *$2->getExp() ).isSignedType() )
    $cost[0] =
      $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_1.getSize() +
      TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "nrel: tpm_BinaryExpGEQ( dreg, const9 )", $1 );

  auto p = $action[2]();
  auto v = $action[3]().getIntValue();

  // LLIR
  LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertLT( regTmp, p.first, v, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( regTmp, 0, block, $1->getExp() );

  // WIR
  auto &tmpReg = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertLT( tmpReg, p.second, v, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( tmpReg, 0, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( true ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
nrel: tpm_BinaryExpGEQ( dreg, uconst9 )
{
  if ( $2->getExp()->getType().isIntegralType() )
    $cost[0] =
      $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_2.getSize() +
      TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "nrel: tpm_BinaryExpGEQ( dreg, uconst9 )", $1 );

  auto p = $action[2]();
  auto v = $action[3]().getIntValue();

  // LLIR
  LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertLT_U( regTmp, p.first, v, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( regTmp, 0, block, $1->getExp() );

  // WIR
  auto &tmpReg = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertLT_U( tmpReg, p.second, v, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( tmpReg, 0, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( true ) );
  }

  return;
};


##############################################################################
#
#  Denormal relations (constants on the left)
#
#  NOTE: Instructions (J)LE and (J)GT do not exist.
#
##############################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
nrel: tpm_BinaryExpGEQ( constant0, dreg )
{
  if ( $3->getExp()->getType().isIntegralType() &&
       effectiveType( *$3->getExp() ).isSignedType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "nrel: tpm_BinaryExpGEQ( constant0, dreg )", $1 );

  auto p = $action[3]();

  // LLIR
  TCINSTRUCTIONS.insertJGE( p.first, 1, block, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertJGE( p.second, 1, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( true ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
nrel: tpm_BinaryExpGEQ( const4, dreg )
{
  if ( $3->getExp()->getType().isIntegralType() &&
       effectiveType( *$3->getExp() ).isSignedType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;

  // Since JGT is missing, we need JGE. Thie means we can only use values < 0x7.
  if ( getConstIntValue( $2 ) >= TC_Const4_Signed::getMaxValue( 4 ) )
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "nrel: tpm_BinaryExpGEQ( const4, dreg )", $1 );

  auto v = $action[2]().getIntValue() + 1;
  auto p = $action[3]();

  // LLIR
  TCINSTRUCTIONS.insertJGE( p.first, v, block, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertJGE( p.second, v, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( true ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
nrel: tpm_BinaryExpGEQ( uconst4, dreg )
{
  if ( $3->getExp()->getType().isIntegralType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DC4L_2.getSize();
  else
    $cost[0] = COST_INFINITY;

  // Since JGT is missing, we need JGE_U. Thie means we can only use
  // values < 0xf.
  if ( getConstIntValue( $2 ) >= (int) TC_Const4_Unsigned::getMaxValue( 4 ) )
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "nrel: tpm_BinaryExpGEQ( uconst4, dreg )", $1 );

  auto v = $action[2]().getIntValue() + 1;
  auto p = $action[3]();

  // LLIR
  TCINSTRUCTIONS.insertJGE_U( p.first, v, block, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertJGE_U( p.second, v, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( true ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
nrel: tpm_BinaryExpGEQ( const9, dreg )
{
  if ( $3->getExp()->getType().isIntegralType() &&
       effectiveType( *$3->getExp() ).isSignedType() )
    $cost[0] =
      $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_1.getSize() +
      TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;

  // Since JGT is missing, we need GE. Thie means we can only use values < 0xff.
  if ( getConstIntValue( $2 ) >= TC_Const9_Signed::getMaxValue( 9 ) )
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "nrel: tpm_BinaryExpGEQ( const9, dreg )", $1 );

  auto v = $action[2]().getIntValue() + 1;
  auto p = $action[3]();

  // LLIR
  LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertGE( regTmp, p.first, v, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( regTmp, 0, block, $1->getExp() );

  // WIR
  auto &tmpReg = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertGE( tmpReg, p.second, v, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( tmpReg, 0, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( true ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
nrel: tpm_BinaryExpGEQ( uconst9, dreg )
{
  if ( $3->getExp()->getType().isIntegralType() )
    $cost[0] =
      $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_2.getSize() +
      TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;

  // Since JGT is missing, we need GE_U. Thie means we can only use
  // values < 0x1ff.
  if ( getConstIntValue( $2 ) >= (int) TC_Const9_Unsigned::getMaxValue( 9 ) )
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "nrel: tpm_BinaryExpGEQ( uconst9, dreg )", $1 );

  auto v = $action[2]().getIntValue() + 1;
  auto p = $action[3]();

  // LLIR
  LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertGE_U( regTmp, p.first, v, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( regTmp, 0, block, $1->getExp() );

  // WIR
  auto &tmpReg = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertGE_U( tmpReg, p.second, v, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( tmpReg, 0, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( true ) );
  }

  return;
};


##############################################################################
#
#  Normalized relations (constants on the right), as values
#
##############################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpGEQ( ereg, ereg )
{
  $cost[0] =
    $cost[2] + $cost[3] + SOFTFLOAT_COST +
    TC13::OperationFormat::DDC9_1.getSize();
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpGEQ( ereg, ereg )", $1 );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertGE_D( reg, p1.first, p2.first, $1->getExp() );
  TCINSTRUCTIONS.insertGE( reg, reg, 0, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertGE_D( r, p1.second, p2.second, $1->getExp() );
  TCINSTRUCTIONS.insertGE( r, r, 0, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpGEQ( dreg, dreg )
{
  $cost[0] = $cost[2] + $cost[3];

  auto &t = effectiveType( *$2->getExp() );
  if ( t.isIntegralType() )
    $cost[0] += TC13::OperationFormat::DDD_1.getSize();
  else
  if ( t.getType() == IR_Type::FLOAT ) {
    if ( TCCODESEL->getConfig()->getEmitFpuInstructions() )
      $cost[0] +=
        TC13::OperationFormat::DDD_1.getSize() +
        TC13::OperationFormat::DDC5DC5_1.getSize();
    else
      $cost[0] += SOFTFLOAT_COST + TC13::OperationFormat::DDC9_1.getSize();
  }
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpGEQ( dreg, dreg )", $1 );

  auto &t = effectiveType( *$2->getExp() );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  if ( t.isIntegralType() )
    if ( t.isUnsignedType() )
      TCINSTRUCTIONS.insertGE_U( reg, p1.first, p2.first, $1->getExp() );
    else
      TCINSTRUCTIONS.insertGE( reg, p1.first, p2.first, $1->getExp() );
  else
  if ( t.getType() == IR_Type::FLOAT ) {
    LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );
    if ( TCCODESEL->getConfig()->getEmitFpuInstructions() ) {
      TCINSTRUCTIONS.insertCMP_F( regTmp, p1.first, p2.first );
      TCINSTRUCTIONS.insertOR_T( reg, regTmp, 2, regTmp, 1, $1->getExp() );
    } else {
      TCINSTRUCTIONS.insertGE_F( regTmp, p1.first, p2.first );
      TCINSTRUCTIONS.insertGE( reg, regTmp, 0, $1->getExp() );
    }
  }

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  if ( t.isIntegralType() )
    if ( t.isUnsignedType() )
      TCINSTRUCTIONS.insertGE_U( r, p1.second, p2.second, $1->getExp() );
    else
      TCINSTRUCTIONS.insertGE( r, p1.second, p2.second, $1->getExp() );
  else
  if ( t.getType() == IR_Type::FLOAT ) {
    auto &tmpReg = TCINSTRUCTIONS.createDReg();
    if ( TCCODESEL->getConfig()->getEmitFpuInstructions() ) {
      TCINSTRUCTIONS.insertCMP_F( tmpReg, p1.second, p2.second, $1->getExp() );
      TCINSTRUCTIONS.insertOR_T( r, tmpReg, 2, tmpReg, 1, $1->getExp() );
    } else {
      TCINSTRUCTIONS.insertGE_F( tmpReg, p1.second, p2.second, $1->getExp() );
      TCINSTRUCTIONS.insertGE( r, tmpReg, 0, $1->getExp() );
    }
  }

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpGEQ( dreg, constant0 )
{
  if ( $2->getExp()->getType().isIntegralType() &&
       effectiveType( *$2->getExp() ).isSignedType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpGEQ( dreg, constant0 )", $1 );

  auto p = $action[2]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertGE( reg, p.first, 0, $1->getExp() );

  // WIR
  auto & r= TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertGE( r, p.second, 0, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpGEQ( dreg, const9 )
{
  if ( $2->getExp()->getType().isIntegralType() &&
       effectiveType( *$2->getExp() ).isSignedType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpGEQ( dreg, const9 )", $1 );

  auto p = $action[2]();
  auto v = $action[3]().getIntValue();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertGE( reg, p.first, v, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertGE( r, p.second, v, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpGEQ( dreg, uconst9 )
{
  if ( $2->getExp()->getType().isIntegralType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_2.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpGEQ( dreg, uconst9 )", $1 );

  auto p = $action[2]();
  auto v = $action[3]().getIntValue();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertGE_U( reg, p.first, v, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertGE_U( r, p.second, v, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpGEQ( areg, areg )
{
  $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DAA.getSize();
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpGEQ( areg, areg )", $1 );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertGE_A( reg, p1.first, p2.first, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertGE_A( r, p1.second, p2.second, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


##############################################################################
#
#  Denormal relations (constants on the left), as values
#
#  NOTE: Instructions (J)LE and (J)GT do not exist.
#
##############################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpGEQ( constant0, dreg )
{
  if ( $3->getExp()->getType().isIntegralType() &&
       effectiveType( *$3->getExp() ).isSignedType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpGEQ( constant0, dreg )", $1 );

  auto p = $action[3]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertLT( reg, p.first, 1, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertLT( r, p.second, 1, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpGEQ( const9, dreg )
{
  if ( $3->getExp()->getType().isIntegralType() &&
       effectiveType( *$3->getExp() ).isSignedType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_1.getSize();
  else
    $cost[0] = COST_INFINITY;

  // Since LE is missing, we need LT. Means we can only use values < 0xff.
  if ( getConstIntValue( $2 ) >= 0xff )
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpGEQ( const9, dreg )", $1 );

  auto p = $action[3]();
  auto v = $action[2]().getIntValue();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertLT( reg, p.first, v + 1, $1->getExp() );

  // WIR
  auto & r= TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertLT( r, p.second, v + 1, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpGEQ( uconst9, dreg )
{
  if ( $3->getExp()->getType().isIntegralType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_2.getSize();
  else
    $cost[0] = COST_INFINITY;

  // Since LE is missing, we need LT. Means we can only use values < 0x1ff.
  if ( getConstIntValue( $2 ) >= 0x1ff )
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpGEQ( uconst9, dreg )", $1 );

  auto p = $action[3]();
  auto v = $action[2]().getIntValue();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertLT_U( reg, p.first, v + 1, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertLT_U( r, p.second, v + 1, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


###############################################################################
#
#  Relations (equal-to)
#
#  Note: Rules are commutative
#
###############################################################################

####################################################################
#
# Jump on positives
#
####################################################################

##################################################
#
#  Normalized relations (constants on the right), with implicit jump
#
##################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
rel: tpm_BinaryExpEQ( dreg, dreg )
{
  $cost[0] = $cost[2] + $cost[3];

  IR_Type &operandType = effectiveType( *$2->getExp() );

  if ( operandType.isIntegralType() )
    $cost[0] += TC13::OperationFormat::DDL_1.getSize();
  else

  if ( operandType.getType() == IR_Type::FLOAT ) {
    if ( TCCODESEL->getConfig()->getEmitFpuInstructions() )
      $cost[0] +=
        TC13::OperationFormat::DDD_1.getSize() +
        TC13::OperationFormat::DC5L.getSize();
    else
      $cost[0] += SOFTFLOAT_COST + TC13::OperationFormat::DC4L_1.getSize();
  } else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpEQ( dreg, dreg )", $1 );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  IR_Type &operandType = effectiveType( *$2->getExp() );

  // LLIR
  if ( operandType.isIntegralType() )
    TCINSTRUCTIONS.insertJEQ( p1.first, p2.first, block, $1->getExp() );
  else

  if ( operandType.getType() == IR_Type::FLOAT ) {
    LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );

    if ( TCCODESEL->getConfig()->getEmitFpuInstructions() ) {
      TCINSTRUCTIONS.insertCMP_F( regTmp, p1.first, p2.first );
      TCINSTRUCTIONS.insertJNZ_T( regTmp, 1, block, $1->getExp() );
    } else {
      TCINSTRUCTIONS.insertNE_F( regTmp, p1.first, p2.first );
      TCINSTRUCTIONS.insertJEQ( regTmp, 0, block, $1->getExp() );
    }
  }

  // WIR
  if ( operandType.isIntegralType() ) {
    TCINSTRUCTIONS.insertJEQ( p1.second, p2.second, b, $1->getExp() );

    if ( markLoopExit ) {
      WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
      jmp.insertContainer( WIR_LoopExit( false ) );
    }
  } else

  if ( operandType.getType() == IR_Type::FLOAT ) {
    auto &tmpReg = TCINSTRUCTIONS.createDReg();

    if ( TCCODESEL->getConfig()->getEmitFpuInstructions() ) {
      TCINSTRUCTIONS.insertCMP_F( tmpReg, p1.second, p2.second, $1->getExp() );
      TCINSTRUCTIONS.insertJNZ_T( tmpReg, 1, b, $1->getExp() );
    } else {
      TCINSTRUCTIONS.insertNE_F( tmpReg, p1.second, p2.second, $1->getExp() );
      TCINSTRUCTIONS.insertJEQ( tmpReg, 0, b, $1->getExp() );
    }

    if ( markLoopExit ) {
      WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
      jmp.insertContainer( WIR_LoopExit( false ) );
    }
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
rel: tpm_BinaryExpEQ( dreg, constant0 )
{
  if ( $2->getExp()->getType().isIntegralType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpEQ( dreg, constant0 )", $1 );

  auto p = $action[2]();

  // LLIR
  TCINSTRUCTIONS.insertJEQ( p.first, 0, block, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertJEQ( p.second, 0, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( false ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
rel: tpm_BinaryExpEQ( dreg, const4 )
{
  if ( $2->getExp()->getType().isIntegralType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpEQ( dreg, const4 )", $1 );

  auto p = $action[2]();
  auto v = $action[3]().getIntValue();

  // LLIR
  TCINSTRUCTIONS.insertJEQ( p.first, v, block, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertJEQ( p.second, v, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( false ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
rel: tpm_BinaryExpEQ( dreg, uconst4 )
{
  if ( $2->getExp()->getType().isIntegralType() )
    $cost[0] =
      $cost[2] + $cost[3] + TC13::OperationFormat::DC16_1.getSize() +
      TC13::OperationFormat::DDL_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpEQ( dreg, uconst4 )", $1 );

  auto p = $action[2]();
  auto v = $action[3]().getIntValue();

  // LLIR
  LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertMOV( regTmp, v, $1->getExp() );
  TCINSTRUCTIONS.insertJEQ( p.first, regTmp, block, $1->getExp() );

  // WIR
  auto &tmpReg = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertMOVConstant( tmpReg, v, $1->getExp() );
  TCINSTRUCTIONS.insertJEQ( p.second, tmpReg, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( false ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
rel: tpm_BinaryExpEQ( dreg, const9 )
{
  if ( $2->getExp()->getType().isIntegralType() )
    $cost[0] =
      $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_1.getSize() +
      TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpEQ( dreg, const9 )", $1 );

  auto p = $action[2]();
  auto v = $action[3]().getIntValue();

  // LLIR
  LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertEQ( regTmp, p.first, v, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( regTmp, 0, block, $1->getExp() );

  // WIR
  auto &tmpReg = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertEQ( tmpReg, p.second, v, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( tmpReg, 0, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( false ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
rel: tpm_BinaryExpEQ( areg, areg )
{
  $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::AAL.getSize();
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpEQ( areg, areg )", $1 );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  // LLIR
  TCINSTRUCTIONS.insertJEQ_A( p1.first, p2.first, block, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertJEQ_A( p1.second, p2.second, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( false ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
rel: tpm_BinaryExpEQ( areg, constant0 )
{
  $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::AL_2.getSize();
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpEQ( areg, constant0 )", $1 );

  auto p = $action[2]();

  // LLIR
  TCINSTRUCTIONS.insertJZ_A( p.first, block, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertJZ_A( p.second, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( false ) );
  }

  return;
};


####################################################################
#
# Jump on negatives
#
####################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
nrel: tpm_BinaryExpEQ( dreg, dreg )
{
  $cost[0] = $cost[2] + $cost[3];

  IR_Type &operandType = effectiveType( *$2->getExp() );

  if ( operandType.isIntegralType() )
    $cost[0] += TC13::OperationFormat::DDL_1.getSize();
  else

  if ( operandType.getType() == IR_Type::FLOAT ) {
    if ( TCCODESEL->getConfig()->getEmitFpuInstructions() )
      $cost[0] +=
        TC13::OperationFormat::DDD_1.getSize() +
        TC13::OperationFormat::DC5L.getSize();
    else
      $cost[0] += SOFTFLOAT_COST + TC13::OperationFormat::DC4L_1.getSize();
  } else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "nrel: tpm_BinaryExpEQ( dreg, dreg )", $1 );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  IR_Type &operandType = effectiveType( *$2->getExp() );

  // LLIR
  if ( operandType.isIntegralType() )
    TCINSTRUCTIONS.insertJNE( p1.first, p2.first, block, $1->getExp() );
  else

  if ( operandType.getType() == IR_Type::FLOAT ) {
    LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );

    if ( TCCODESEL->getConfig()->getEmitFpuInstructions() ) {
      TCINSTRUCTIONS.insertCMP_F( regTmp, p1.first, p2.first );
      TCINSTRUCTIONS.insertJZ_T( regTmp, 1, block, $1->getExp() );
    } else {
      TCINSTRUCTIONS.insertEQ_F( regTmp, p1.first, p2.first );
      TCINSTRUCTIONS.insertJNE( regTmp, 0, block, $1->getExp() );
    }
  }

  // WIR
  if ( operandType.isIntegralType() ) {
    TCINSTRUCTIONS.insertJNE( p1.second, p2.second, b, $1->getExp() );

    if ( markLoopExit ) {
      WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
      jmp.insertContainer( WIR_LoopExit( true ) );
    }
  } else

  if ( operandType.getType() == IR_Type::FLOAT ) {
    auto &tmpReg = TCINSTRUCTIONS.createDReg();

    if ( TCCODESEL->getConfig()->getEmitFpuInstructions() ) {
      TCINSTRUCTIONS.insertCMP_F( tmpReg, p1.second, p2.second, $1->getExp() );
      TCINSTRUCTIONS.insertJZ_T( tmpReg, 1, b, $1->getExp() );
    } else {
      TCINSTRUCTIONS.insertEQ_F( tmpReg, p1.second, p2.second, $1->getExp() );
      TCINSTRUCTIONS.insertJNE( tmpReg, 0, b, $1->getExp() );
    }

    if ( markLoopExit ) {
      WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
      jmp.insertContainer( WIR_LoopExit( true ) );
    }
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
nrel: tpm_BinaryExpEQ( dreg, constant0 )
{
  if ( $2->getExp()->getType().isIntegralType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "nrel: tpm_BinaryExpEQ( dreg, constant0 )", $1 );

  auto p = $action[2]();

  // LLIR
  TCINSTRUCTIONS.insertJNE( p.first, 0, block, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertJNE( p.second, 0, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( true ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
nrel: tpm_BinaryExpEQ( dreg, const4 )
{
  if ( $2->getExp()->getType().isIntegralType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "nrel: tpm_BinaryExpEQ( dreg, const4 )", $1 );

  auto p = $action[2]();
  auto v = $action[3]().getIntValue();

  // LLIR
  TCINSTRUCTIONS.insertJNE( p.first, v, block, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertJNE( p.second, v, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( true ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
nrel: tpm_BinaryExpEQ( dreg, uconst4 )
{
  if ( $2->getExp()->getType().isIntegralType() )
    $cost[0] =
      $cost[2] + $cost[3] + TC13::OperationFormat::DC16_1.getSize() +
      TC13::OperationFormat::DDL_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "nrel: tpm_BinaryExpEQ( dreg, uconst4 )", $1 );

  auto p = $action[2]();
  auto v = $action[3]().getIntValue();

  // LLIR
  LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertMOV( regTmp, v, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( p.first, regTmp, block, $1->getExp() );

  // WIR
  auto &tmpReg = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertMOVConstant( tmpReg, v, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( p.second, tmpReg, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( true ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
nrel: tpm_BinaryExpEQ( dreg, const9 )
{
  if ( $2->getExp()->getType().isIntegralType() )
    $cost[0] =
      $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_1.getSize() +
      TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "nrel: tpm_BinaryExpEQ( dreg, const9 )", $1 );

  auto p = $action[2]();
  auto v = $action[3]().getIntValue();

  // LLIR
  LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertNE( regTmp, p.first, v, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( regTmp, 0, block, $1->getExp() );

  // WIR
  auto &tmpReg = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertNE( tmpReg, p.second, v, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( tmpReg, 0, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( true ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
nrel: tpm_BinaryExpEQ( areg, areg )
{
  $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::AAL.getSize();
}
=
{
  DEBUG_RULE_ACTION( "nrel: tpm_BinaryExpEQ( areg, areg )", $1 );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  // LLIR
  TCINSTRUCTIONS.insertJNE_A( p1.first, p2.first, block, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertJNE_A( p1.second, p2.second, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( true ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
nrel: tpm_BinaryExpEQ( areg, constant0 )
{
  $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::AL_2.getSize();
}
=
{
  DEBUG_RULE_ACTION( "nrel: tpm_BinaryExpEQ( areg, constant0 )", $1 );

  auto p = $action[2]();

  // LLIR
  TCINSTRUCTIONS.insertJNZ_A( p.first, block, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertJNZ_A( p.second, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( true ) );
  }

  return;
};


##############################################################################
#
#  Normalized relations (constants on the right), as values
#
##############################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpEQ( ereg, ereg )
{
  $cost[0] =
    $cost[2] + $cost[3] + SOFTFLOAT_COST +
    TC13::OperationFormat::DDC9_1.getSize();
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpEQ( ereg, ereg )", $1 );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertEQ_D( reg, p1.first, p2.first, $1->getExp() );
  TCINSTRUCTIONS.insertEQ( reg, reg, 0, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertEQ_D( r, p1.second, p2.second, $1->getExp() );
  TCINSTRUCTIONS.insertEQ( r, r, 0, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpEQ( dreg, dreg )
{
  $cost[0] = $cost[2] + $cost[3];

  auto &t = effectiveType( *$2->getExp() );
  if ( t.isIntegralType() )
    $cost[0] += TC13::OperationFormat::DDD_1.getSize();
  else
  if ( t.getType() == IR_Type::FLOAT ) {
    if ( TCCODESEL->getConfig()->getEmitFpuInstructions() )
      $cost[0] +=
        TC13::OperationFormat::DDD_1.getSize() +
        TC13::OperationFormat::DDC5DC5_1.getSize();
    else
      $cost[0] += SOFTFLOAT_COST + TC13::OperationFormat::DDC9_1.getSize();
  }
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpEQ( dreg, dreg )", $1 );

  auto &t = effectiveType( *$2->getExp() );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  if ( t.isIntegralType() )
    TCINSTRUCTIONS.insertEQ( reg, p1.first, p2.first, $1->getExp() );
  else
  if ( t.getType() == IR_Type::FLOAT ) {
    LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );
    if ( TCCODESEL->getConfig()->getEmitFpuInstructions() ) {
      TCINSTRUCTIONS.insertCMP_F( regTmp, p1.first, p2.first );
      TCINSTRUCTIONS.insertOR_T( reg, regTmp, 1, regTmp, 1, $1->getExp() );
    } else {
      TCINSTRUCTIONS.insertEQ_F( regTmp, p1.first, p2.first );
      TCINSTRUCTIONS.insertEQ( reg, regTmp, 0, $1->getExp() );
    }
  }

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  if ( t.isIntegralType() )
    TCINSTRUCTIONS.insertEQ( r, p1.second, p2.second, $1->getExp() );
  else
  if ( t.getType() == IR_Type::FLOAT ) {
    auto &tmpReg = TCINSTRUCTIONS.createDReg();
    if ( TCCODESEL->getConfig()->getEmitFpuInstructions() ) {
      TCINSTRUCTIONS.insertCMP_F( tmpReg, p1.second, p2.second, $1->getExp() );
      TCINSTRUCTIONS.insertOR_T( r, tmpReg, 1, tmpReg, 1, $1->getExp() );
    } else {
      TCINSTRUCTIONS.insertEQ_F( tmpReg, p1.second, p2.second, $1->getExp() );
      TCINSTRUCTIONS.insertEQ( r, tmpReg, 0, $1->getExp() );
    }
  }

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpEQ( dreg, constant0 )
{
  if ( $2->getExp()->getType().isIntegralType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpEQ( dreg, constant0 )", $1 );

  auto p = $action[2]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertEQ( reg, p.first, 0, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertEQ( r, p.second, 0, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpEQ( dreg, const9 )
{
  if ( $2->getExp()->getType().isIntegralType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpEQ( dreg, const9 )", $1 );

  auto p = $action[2]();
  auto v = $action[3]().getIntValue();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertEQ( reg, p.first, v, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertEQ( r, p.second, v, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpEQ( areg, areg )
{
  $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DAA.getSize();
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpEQ( areg, areg )", $1 );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertEQ_A( reg, p1.first, p2.first, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertEQ_A( r, p1.second, p2.second, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpEQ( areg, constant0 )
{
  $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DA.getSize();
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpEQ( areg, constant0 )", $1 );

  auto p = $action[2]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertEQZ_A( reg, p.first, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertEQZ_A( r, p.second, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


###############################################################################
#
#  Relations (not-equal-to)
#
#  Note: Rules are commutative.
#
###############################################################################

####################################################################
#
# Jump on positives
#
####################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
rel: tpm_BinaryExpNEQ( dreg, dreg )
{
  $cost[0] = $cost[2] + $cost[3];

  IR_Type &operandType = effectiveType( *$2->getExp() );

  if ( operandType.isIntegralType() )
    $cost[0] += TC13::OperationFormat::DDL_1.getSize();
  else

  if ( operandType.getType() == IR_Type::FLOAT ) {
    if ( TCCODESEL->getConfig()->getEmitFpuInstructions() )
      $cost[0] +=
        TC13::OperationFormat::DDD_1.getSize() +
        TC13::OperationFormat::DC5L.getSize();
    else
      $cost[0] += SOFTFLOAT_COST + TC13::OperationFormat::DC4L_1.getSize();
  } else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpNEQ( dreg, dreg )", $1 );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  IR_Type &operandType = effectiveType( *$2->getExp() );

  // LLIR
  if ( operandType.isIntegralType() )
    TCINSTRUCTIONS.insertJNE( p1.first, p2.first, block, $1->getExp() );
  else

  if ( operandType.getType() == IR_Type::FLOAT ) {
    LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );

    if ( TCCODESEL->getConfig()->getEmitFpuInstructions() ) {
      TCINSTRUCTIONS.insertCMP_F( regTmp, p1.first, p2.first );
      TCINSTRUCTIONS.insertJZ_T( regTmp, 1, block, $1->getExp() );
    } else {
      TCINSTRUCTIONS.insertEQ_F( regTmp, p1.first, p2.first );
      TCINSTRUCTIONS.insertJNE( regTmp, 0, block, $1->getExp() );
    }
  }

  // WIR
  if ( operandType.isIntegralType() ) {
    TCINSTRUCTIONS.insertJNE( p1.second, p2.second, b, $1->getExp() );

    if ( markLoopExit ) {
      WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
      jmp.insertContainer( WIR_LoopExit( false ) );
    }
  } else

  if ( operandType.getType() == IR_Type::FLOAT ) {
    auto &tmpReg = TCINSTRUCTIONS.createDReg();

    if ( TCCODESEL->getConfig()->getEmitFpuInstructions() ) {
      TCINSTRUCTIONS.insertCMP_F( tmpReg, p1.second, p2.second, $1->getExp() );
      TCINSTRUCTIONS.insertJZ_T( tmpReg, 1, b, $1->getExp() );
    } else {
      TCINSTRUCTIONS.insertEQ_F( tmpReg, p1.second, p2.second, $1->getExp() );
      TCINSTRUCTIONS.insertJNE( tmpReg, 0, b, $1->getExp() );
    }

    if ( markLoopExit ) {
      WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
      jmp.insertContainer( WIR_LoopExit( false ) );
    }
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
rel: tpm_BinaryExpNEQ( dreg, constant0 )
{
  if ( $2->getExp()->getType().isIntegralType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpNEQ( dreg, constant0 )", $1 );

  auto p = $action[2]();

  // LLIR
  TCINSTRUCTIONS.insertJNE( p.first, 0, block, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertJNE( p.second, 0, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( false ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
rel: tpm_BinaryExpNEQ( dreg, const4 )
{
  if ( $2->getExp()->getType().isIntegralType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpNEQ( dreg, const4 )", $1 );

  auto p = $action[2]();
  auto v = $action[3]().getIntValue();

  // LLIR
  TCINSTRUCTIONS.insertJNE( p.first, v, block, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertJNE( p.second, v, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( false ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
rel: tpm_BinaryExpNEQ( dreg, uconst4 )
{
  if ( $2->getExp()->getType().isIntegralType() )
    $cost[0] =
      $cost[2] + $cost[3] + TC13::OperationFormat::DC16_1.getSize() +
      TC13::OperationFormat::DDL_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpNEQ( dreg, uconst4 )", $1 );

  auto p = $action[2]();
  auto v = $action[3]().getIntValue();

  // LLIR
  LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertMOV( regTmp, v, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( p.first, regTmp, block, $1->getExp() );

  // WIR
  auto &tmpReg = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertMOVConstant( tmpReg, v, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( p.second, tmpReg, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( false ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
rel: tpm_BinaryExpNEQ( dreg, const9 )
{
  if ( $2->getExp()->getType().isIntegralType() )
    $cost[0] =
      $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_1.getSize() +
      TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpNEQ( dreg, const9 )", $1 );

  auto p = $action[2]();
  auto v = $action[3]().getIntValue();

  // LLIR
  LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertNE( regTmp, p.first, v, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( regTmp, 0, block, $1->getExp() );

  // WIR
  auto &tmpReg = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertNE( tmpReg, p.second, v, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( tmpReg, 0, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( false ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
rel: tpm_BinaryExpNEQ( areg, areg )
{
  $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::AAL.getSize();
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpNEQ( areg, areg )", $1 );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  // LLIR
  TCINSTRUCTIONS.insertJNE_A( p1.first, p2.first, block, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertJNE_A( p1.second, p2.second, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( false ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
rel: tpm_BinaryExpNEQ( areg, constant0 )
{
  $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::AL_2.getSize();
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpNEQ( areg, constant0 )", $1 );

  auto p = $action[2]();

  // LLIR
  TCINSTRUCTIONS.insertJNZ_A( p.first, block, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertJNZ_A( p.second, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( false ) );
  }

  return;
};


####################################################################
#
# Jump on positives
#
####################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
nrel: tpm_BinaryExpNEQ( dreg, dreg )
{
  $cost[0] = $cost[2] + $cost[3];

  IR_Type &operandType = effectiveType( *$2->getExp() );

  if ( operandType.isIntegralType() )
    $cost[0] += TC13::OperationFormat::DDL_1.getSize();
  else

  if ( operandType.getType() == IR_Type::FLOAT ) {
    if ( TCCODESEL->getConfig()->getEmitFpuInstructions() )
      $cost[0] +=
        TC13::OperationFormat::DDD_1.getSize() +
        TC13::OperationFormat::DC5L.getSize();
    else
      $cost[0] += SOFTFLOAT_COST + TC13::OperationFormat::DC4L_1.getSize();
  } else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "nrel: tpm_BinaryExpNEQ( dreg, dreg )", $1 );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  IR_Type &operandType = effectiveType( *$2->getExp() );

  // LLIR
  if ( operandType.isIntegralType() )
    TCINSTRUCTIONS.insertJEQ( p1.first, p2.first, block, $1->getExp() );
  else

  if ( operandType.getType() == IR_Type::FLOAT ) {
    LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );

    if ( TCCODESEL->getConfig()->getEmitFpuInstructions() ) {
      TCINSTRUCTIONS.insertCMP_F( regTmp, p1.first, p2.first );
      TCINSTRUCTIONS.insertJNZ_T( regTmp, 1, block, $1->getExp() );
    } else {
      TCINSTRUCTIONS.insertNE_F( regTmp, p1.first, p2.first );
      TCINSTRUCTIONS.insertJEQ( regTmp, 0, block, $1->getExp() );
    }
  }

  // WIR
  if ( operandType.isIntegralType() ) {
    TCINSTRUCTIONS.insertJEQ( p1.second, p2.second, b, $1->getExp() );

    if ( markLoopExit ) {
      WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
      jmp.insertContainer( WIR_LoopExit( true ) );
    }
  } else

  if ( operandType.getType() == IR_Type::FLOAT ) {
    auto &tmpReg = TCINSTRUCTIONS.createDReg();

    if ( TCCODESEL->getConfig()->getEmitFpuInstructions() ) {
      TCINSTRUCTIONS.insertCMP_F( tmpReg, p1.second, p2.second, $1->getExp() );
      TCINSTRUCTIONS.insertJNZ_T( tmpReg, 1, b, $1->getExp() );
    } else {
      TCINSTRUCTIONS.insertNE_F( tmpReg, p1.second, p2.second, $1->getExp() );
      TCINSTRUCTIONS.insertJEQ( tmpReg, 0, b, $1->getExp() );
    }

    if ( markLoopExit ) {
      WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
      jmp.insertContainer( WIR_LoopExit( true ) );
    }
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
nrel: tpm_BinaryExpNEQ( dreg, constant0 )
{
  if ( $2->getExp()->getType().isIntegralType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "nrel: tpm_BinaryExpNEQ( dreg, constant0 )", $1 );

  auto p = $action[2]();

  // LLIR
  TCINSTRUCTIONS.insertJEQ( p.first, 0, block, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertJEQ( p.second, 0, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( true ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
nrel: tpm_BinaryExpNEQ( dreg, const4 )
{
  if ( $2->getExp()->getType().isIntegralType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "nrel: tpm_BinaryExpNEQ( dreg, const4 )", $1 );

  auto p = $action[2]();
  auto v = $action[3]().getIntValue();

  // LLIR
  TCINSTRUCTIONS.insertJEQ( p.first, v, block, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertJEQ( p.second, v, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( true ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
nrel: tpm_BinaryExpNEQ( dreg, uconst4 )
{
  if ( $2->getExp()->getType().isIntegralType() )
    $cost[0] =
      $cost[2] + $cost[3] + TC13::OperationFormat::DC16_1.getSize() +
      TC13::OperationFormat::DDL_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "nrel: tpm_BinaryExpNEQ( dreg, uconst4 )", $1 );

  auto p = $action[2]();
  auto v = $action[3]().getIntValue();

  // LLIR
  LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertMOV( regTmp, v, $1->getExp() );
  TCINSTRUCTIONS.insertJEQ( p.first, regTmp, block, $1->getExp() );

  // WIR
  auto &tmpReg = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertMOVConstant( tmpReg, v, $1->getExp() );
  TCINSTRUCTIONS.insertJEQ( p.second, tmpReg, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( true ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
nrel: tpm_BinaryExpNEQ( dreg, const9 )
{
  if ( $2->getExp()->getType().isIntegralType() )
    $cost[0] =
      $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_1.getSize() +
      TC13::OperationFormat::DC4L_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "nrel: tpm_BinaryExpNEQ( dreg, const9 )", $1 );

  auto p = $action[2]();
  auto v = $action[3]().getIntValue();

  // LLIR
  LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertEQ( regTmp, p.first, v, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( regTmp, 0, block, $1->getExp() );

  // WIR
  auto &tmpReg = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertEQ( tmpReg, p.second, v, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( tmpReg, 0, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( true ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
nrel: tpm_BinaryExpNEQ( areg, areg )
{
  $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::AAL.getSize();
}
=
{
  DEBUG_RULE_ACTION( "nrel: tpm_BinaryExpNEQ( areg, areg )", $1 );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  // LLIR
  TCINSTRUCTIONS.insertJEQ_A( p1.first, p2.first, block, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertJEQ_A( p1.second, p2.second, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( true ) );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
nrel: tpm_BinaryExpNEQ( areg, constant0 )
{
  $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::AL_2.getSize();
}
=
{
  DEBUG_RULE_ACTION( "nrel: tpm_BinaryExpNEQ( areg, constant0 )", $1 );

  auto p = $action[2]();

  // LLIR
  TCINSTRUCTIONS.insertJZ_A( p.first, block, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertJZ_A( p.second, b, $1->getExp() );

  if ( markLoopExit ) {
    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( true ) );
  }

  return;
};


##############################################################################
#
#  Normalized relations (constants on the right), as values
#
##############################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpNEQ( ereg, ereg )
{
  $cost[0] =
    $cost[2] + $cost[3] + SOFTFLOAT_COST +
    TC13::OperationFormat::DDC9_1.getSize();
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpNEQ( ereg, ereg )", $1 );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertNE_D( reg, p1.first, p2.first, $1->getExp() );
  TCINSTRUCTIONS.insertNE( reg, reg, 0, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertNE_D( r, p1.second, p2.second, $1->getExp() );
  TCINSTRUCTIONS.insertNE( r, r, 0, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpNEQ( dreg, dreg )
{
  $cost[0] = $cost[2] + $cost[3];

  auto &t = effectiveType( *$2->getExp() );
  if ( t.isIntegralType() )
    $cost[0] += TC13::OperationFormat::DDD_1.getSize();
  else
  if ( t.getType() == IR_Type::FLOAT ) {
    if ( TCCODESEL->getConfig()->getEmitFpuInstructions() )
      $cost[0] +=
        TC13::OperationFormat::DDD_1.getSize() +
        TC13::OperationFormat::DDC5DC5_1.getSize();
    else
      $cost[0] += SOFTFLOAT_COST + TC13::OperationFormat::DDC9_1.getSize();
  }
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpNEQ( dreg, dreg )", $1 );

  auto &t = effectiveType( *$2->getExp() );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  if ( t.isIntegralType() )
    TCINSTRUCTIONS.insertNE( reg, p1.first, p2.first, $1->getExp() );
  else
  if ( t.getType() == IR_Type::FLOAT ) {
    LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );
    if ( TCCODESEL->getConfig()->getEmitFpuInstructions() ) {
      TCINSTRUCTIONS.insertCMP_F( regTmp, p1.first, p2.first );
      TCINSTRUCTIONS.insertOR_T( reg, regTmp, 2, regTmp, 0 , $1->getExp() );
    } else {
      TCINSTRUCTIONS.insertNE_F( regTmp, p1.first, p2.first );
      TCINSTRUCTIONS.insertNE( reg, regTmp, 0, $1->getExp() );
    }
  }

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  if ( t.isIntegralType() )
    TCINSTRUCTIONS.insertNE( r, p1.second, p2.second, $1->getExp() );
  else
  if ( t.getType() == IR_Type::FLOAT ) {
    auto &tmpReg = TCINSTRUCTIONS.createDReg();
    if ( TCCODESEL->getConfig()->getEmitFpuInstructions() ) {
      TCINSTRUCTIONS.insertCMP_F( tmpReg, p1.second, p2.second, $1->getExp() );
      TCINSTRUCTIONS.insertOR_T( r, tmpReg, 2, tmpReg, 0 , $1->getExp() );
    } else {
      TCINSTRUCTIONS.insertNE_F( tmpReg, p1.second, p2.second, $1->getExp() );
      TCINSTRUCTIONS.insertNE( r, tmpReg, 0, $1->getExp() );
    }
  }

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpNEQ( dreg, constant0 )
{
  if ( $2->getExp()->getType().isIntegralType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpNEQ( dreg, constant0 )", $1 );

  auto p = $action[2]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertNE( reg, p.first, 0, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertNE( r, p.second, 0, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpNEQ( dreg, const9 )
{
  if ( $2->getExp()->getType().isIntegralType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpNEQ( dreg, const9 )", $1 );

  auto p = $action[2]();
  auto v = $action[3]().getIntValue();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertNE( reg, p.first, v, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertNE( r, p.second, v, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpNEQ( areg, areg )
{
  $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DAA.getSize();
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpNEQ( areg, areg )", $1 );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertNE_A( reg, p1.first, p2.first, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertNE_A( r, p1.second, p2.second, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpNEQ( areg, constant0 )
{
  $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DA.getSize();
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpNEQ( areg, constant0 )", $1 );

  auto p = $action[2]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertNEZ_A( reg, p.first, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertNEZ_A( r, p.second, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};
