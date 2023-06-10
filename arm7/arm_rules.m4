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
#  Copyright 2005 - 2022
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
# Statement rules
#
#
###############################################################################

stmt: tpm_ExpStmt( any_reg )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "stmt: tpm_ExpStmt( any_reg )", $1 );

  // Evaluate the argument.
  $action[2]();
};


###############################################################################
#
#
# Simple Assignment Expressions
#
#
###############################################################################

dreg: tpm_AssignExpASSIGN( dreg, const8 )
{
  $cost[0] = $cost[2] + $cost[3] + CT( INS_MOV_32 );
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_AssignExpASSIGN( dreg, const8 )", $1 );

  // Evaluate the arguments.
  auto rhs = $action[3]().getIntValue();
  auto *lhsReg = $action[2]();

  // Generate the operation.
  ARMINSTRUCTIONS.insertMOV( lhsReg, rhs, $1->getExp() );

  return( lhsReg );
};


dreg: tpm_AssignExpASSIGN( dreg, dreg )
{
  $cost[0] = $cost[2] + $cost[3] + CT( INS_MOV_32 );
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_AssignExpASSIGN( dreg, dreg )", $1 );

  // Evaluate the arguments.
  auto *rhsReg = $action[3]();
  auto *lhsReg = $action[2]();

  // Generate the operation.
  ARMINSTRUCTIONS.insertMOV( lhsReg, rhsReg, $1->getExp() );

  return( lhsReg );
};


ereg: tpm_AssignExpASSIGN( ereg, ereg )
{
  $cost[0] = $cost[2] + $cost[3] + 2 * CT( INS_MOV_32 );
}
=
{
  DEBUG_RULE_ACTION( "ereg: tpm_AssignExpASSIGN( ereg, ereg )", $1 );

  auto *rhsEReg = $action[3]();
  auto *lhsEReg = $action[2]();

  // Evaluate the arguments.
  auto *rhsLowReg = rhsEReg->GetFirstChild();
  auto *rhsHighReg = rhsEReg->GetNextChild( rhsEReg->GetFirstChild() );
  auto *lhsLowReg = lhsEReg->GetFirstChild();
  auto *lhsHighReg = lhsEReg->GetNextChild( lhsEReg->GetFirstChild() );

  // Generate the operations.
  ARMINSTRUCTIONS.insertMOV( lhsHighReg, rhsHighReg, $1->getExp() );
  ARMINSTRUCTIONS.insertMOV( lhsLowReg, rhsLowReg, $1->getExp() );

  return( lhsEReg );
};


#######
# PLUS
#######

ereg: tpm_AssignExpPLUS( ereg, ereg )
{
  $cost[0] = $cost[2] + $cost[3];

  if ( $0->getExp()->getType().isIntegralType() &&
       ( $0->getExp()->getType().bitSize() > 32 ) )
    $cost[0] += CT( INS_ADD_32 ) + CT( INS_ADC_32 );
  else

  if ( ( $0->getExp()->getType().getType() == IR_Type::DOUBLE ) ||
       ( $0->getExp()->getType().getType() == IR_Type::LONG_DOUBLE ) )
    $cost[0] += CT( INS_BL_32 ) + 6 * CT( INS_MOV_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "ereg: tpm_AssignExpPLUS( ereg, ereg )", $1 );

  // Evaluate the arguments.
  auto *lhsEReg = $action[2]();
  auto *rhsEReg = $action[3]();

  // Generate the operation.
  if ( $0->getExp()->getType().isIntegralType() &&
       ( $0->getExp()->getType().bitSize() > 32 ) )
    ARMINSTRUCTIONS.insertADD_LL(
      OPER_AL, lhsEReg, lhsEReg, rhsEReg, $1->getExp() );
  else
    ARMINSTRUCTIONS.insertADD_D(
      OPER_AL, lhsEReg, lhsEReg, rhsEReg, $1->getExp() );

  return( lhsEReg );
};


#######
# MINUS
#######

ereg: tpm_AssignExpMINUS( ereg, ereg )
{
  $cost[0] = $cost[2] + $cost[3];

  if ( $0->getExp()->getType().isIntegralType() &&
       ( $0->getExp()->getType().bitSize() > 32 ) )
    $cost[0] += CT( INS_SUB_32 ) + CT( INS_SBC_32 );
  else

  if ( ( $0->getExp()->getType().getType() == IR_Type::DOUBLE ) ||
       ( $0->getExp()->getType().getType() == IR_Type::LONG_DOUBLE ) )
    $cost[0] += CT( INS_BL_32 ) + 6 * CT( INS_MOV_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "ereg: tpm_AssignExpMINUS( ereg, ereg )", $1 );

  // Evaluate the arguments.
  auto *lhsEReg = $action[2]();
  auto *rhsEReg = $action[3]();

  // Generate the operation.
  if ( $0->getExp()->getType().isIntegralType() &&
       ( $0->getExp()->getType().bitSize() > 32 ) )
    ARMINSTRUCTIONS.insertSUB_LL(
      OPER_AL, lhsEReg, lhsEReg, rhsEReg, $1->getExp() );
  else
    ARMINSTRUCTIONS.insertSUB_D(
      OPER_AL, lhsEReg, lhsEReg, rhsEReg, $1->getExp() );

  return( lhsEReg );
};


#####
# MUL
#####

ereg: tpm_AssignExpMULT( ereg, ereg )
{
  $cost[0] = $cost[2] + $cost[3];

  if ( ( $0->getExp()->getType().getType() == IR_Type::LONG_LONG ) ||
       ( $0->getExp()->getType().getType() == IR_Type::UNSIGNED_LONG_LONG ) )
    $cost[0] +=
      8 * CT( INS_MOV_32 ) + CT( INS_UMULL_32 ) + CT( INS_MLA_32 ) +
      CT( INS_MUL_32 ) + CT( INS_ADD_32 );
  else

  if ( ( $0->getExp()->getType().getType() == IR_Type::DOUBLE ) ||
       ( $0->getExp()->getType().getType() == IR_Type::LONG_DOUBLE ) )
    $cost[0] += CT( INS_BL_32 ) + 6 * CT( INS_MOV_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "ereg: tpm_AssignExpMULT( ereg, ereg )", $1 );

  // Evaluate the arguments.
  auto *rhsEReg = $action[3]();
  auto *lhsEReg = $action[2]();

  // Generate the operation.
  if ( ( $0->getExp()->getType().getType() == IR_Type::LONG_LONG ) ||
       ( $0->getExp()->getType().getType() == IR_Type::UNSIGNED_LONG_LONG ) )
    ARMINSTRUCTIONS.insertMUL_LL(
      OPER_AL, lhsEReg, lhsEReg, rhsEReg, $1->getExp() );
  else
    ARMINSTRUCTIONS.insertMUL_D(
      OPER_AL, lhsEReg, lhsEReg, rhsEReg, $1->getExp() );

  return( lhsEReg );
};


#####
# DIV
#####

ereg: tpm_AssignExpDIV( ereg, ereg )
{
  $cost[0] = $cost[2] + $cost[3];

  if ( ( $0->getExp()->getType().getType() == IR_Type::LONG_LONG ) ||
       ( $0->getExp()->getType().getType() == IR_Type::UNSIGNED_LONG_LONG ) )
    $cost[0] += CT( INS_BL_32 ) + 6 * CT( INS_MOV_32 );
  else

  if ( ( $0->getExp()->getType().getType() == IR_Type::DOUBLE ) ||
       ( $0->getExp()->getType().getType() == IR_Type::LONG_DOUBLE ) )
    $cost[0] += CT( INS_BL_32 ) + 6 * CT( INS_MOV_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "ereg: tpm_AssignExpDIV( ereg, ereg )", $1 );

  // Evaluate the arguments.
  auto *rhsEReg = $action[3]();
  auto *lhsEReg = $action[2]();

  // Generate the operation.
  if ( $0->getExp()->getType().getType() == IR_Type::LONG_LONG )
    ARMINSTRUCTIONS.insertDIV_LL(
      OPER_AL, lhsEReg, lhsEReg, rhsEReg, $1->getExp() );
  else

  if ( $0->getExp()->getType().getType() == IR_Type::UNSIGNED_LONG_LONG )
    ARMINSTRUCTIONS.insertDIV_ULL(
      OPER_AL, lhsEReg, lhsEReg, rhsEReg, $1->getExp() );
  else
    ARMINSTRUCTIONS.insertDIV_D(
      OPER_AL, lhsEReg, lhsEReg, rhsEReg, $1->getExp() );

  return( lhsEReg );
};


#####
# MOD
#####

ereg: tpm_AssignExpMOD( ereg, ereg )
{
  $cost[0] = $cost[2] + $cost[3];

  if ( ( $0->getExp()->getType().getType() == IR_Type::LONG_LONG ) ||
       ( $0->getExp()->getType().getType() == IR_Type::UNSIGNED_LONG_LONG ) )
    $cost[0] += CT( INS_BL_32 ) + 6 * CT( INS_MOV_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "ereg: tpm_AssignExpMOD( ereg, ereg )", $1 );

  // Evaluate the arguments.
  auto *rhsEReg = $action[3]();
  auto *lhsEReg = $action[2]();

  // Generate the operation.
  if ( $0->getExp()->getType().getType() == IR_Type::LONG_LONG )
    ARMINSTRUCTIONS.insertMOD_LL(
      OPER_AL, lhsEReg, lhsEReg, rhsEReg, $1->getExp() );
  else
    ARMINSTRUCTIONS.insertMOD_ULL(
      OPER_AL, lhsEReg, lhsEReg, rhsEReg, $1->getExp() );

  return( lhsEReg );
};


#####
# AND
#####

ereg: tpm_AssignExpAND( ereg, ereg )
{
  if ( ( $1->getExp()->getType().getType() == IR_Type::LONG_LONG ) ||
       ( $1->getExp()->getType().getType() == IR_Type::UNSIGNED_LONG_LONG ) )
    $cost[0] = $cost[2] + $cost[3] + 2 * CT( INS_AND_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "ereg: tpm_AssignExpAND( ereg, ereg )", $1 );

  // Evaluate the arguments.
  auto *rhsEReg = $action[3]();
  auto *lhsEReg = $action[2]();

  // Generate the operation.
  ARMINSTRUCTIONS.insertAND(
    lhsEReg->GetFirstChild(), lhsEReg->GetFirstChild(),
    rhsEReg->GetFirstChild(), $1->getExp() );
  ARMINSTRUCTIONS.insertAND(
    lhsEReg->GetNextChild( lhsEReg->GetFirstChild() ),
    lhsEReg->GetNextChild( lhsEReg->GetFirstChild() ),
    rhsEReg->GetNextChild( rhsEReg->GetFirstChild() ), $1->getExp() );

  return( lhsEReg );
};


#####
# XOR
#####

ereg: tpm_AssignExpXOR( ereg, ereg )
{
  if ( ( $1->getExp()->getType().getType() == IR_Type::LONG_LONG ) ||
       ( $1->getExp()->getType().getType() == IR_Type::UNSIGNED_LONG_LONG ) )
    $cost[0] = $cost[2] + $cost[3] + 2 * CT( INS_EOR_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "ereg: tpm_AssignExpXOR( ereg, ereg )", $1 );

  // Evaluate the arguments.
  auto *rhsEReg = $action[3]();
  auto *lhsEReg = $action[2]();

  // Generate the operation.
  ARMINSTRUCTIONS.insertEOR(
    lhsEReg->GetFirstChild(), lhsEReg->GetFirstChild(),
    rhsEReg->GetFirstChild(), $1->getExp() );
  ARMINSTRUCTIONS.insertEOR(
    lhsEReg->GetNextChild( lhsEReg->GetFirstChild() ),
    lhsEReg->GetNextChild( lhsEReg->GetFirstChild() ),
    rhsEReg->GetNextChild( rhsEReg->GetFirstChild() ), $1->getExp() );

  return( lhsEReg );
};


#####
# OR
#####

ereg: tpm_AssignExpOR( ereg, ereg )
{
  if ( ( $1->getExp()->getType().getType() == IR_Type::LONG_LONG ) ||
       ( $1->getExp()->getType().getType() == IR_Type::UNSIGNED_LONG_LONG ) )
    $cost[0] = $cost[2] + $cost[3] + 2 * CT( INS_ORR_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "ereg: tpm_AssignExpOR( ereg, ereg )", $1 );

  // Evaluate the arguments.
  auto *rhsEReg = $action[3]();
  auto *lhsEReg = $action[2]();

  // Generate the operation.
  ARMINSTRUCTIONS.insertORR(
    lhsEReg->GetFirstChild(), lhsEReg->GetFirstChild(),
    rhsEReg->GetFirstChild(), $1->getExp() );
  ARMINSTRUCTIONS.insertORR(
    lhsEReg->GetNextChild( lhsEReg->GetFirstChild() ),
    lhsEReg->GetNextChild( lhsEReg->GetFirstChild() ),
    rhsEReg->GetNextChild( rhsEReg->GetFirstChild() ), $1->getExp() );

  return( lhsEReg );
};


###############################################################################
#
# Shift operators
#
###############################################################################

ereg: tpm_AssignExpSHL( ereg, dreg )
{
  $cost[0] = $cost[2] + $cost[3];
  if ( ( $0->getExp()->getType().getType() == IR_Type::LONG_LONG ) ||
       ( $0->getExp()->getType().getType() == IR_Type::UNSIGNED_LONG_LONG ) )
    $cost[0] += CT( INS_BL_32 ) + 5 * CT( INS_MOV_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "ereg: tpm_AssignExpSHL( ereg, dreg )", $1 );

  // Evaluate the arguments.
  auto *lhsEReg = $action[2]();
  auto *rhsReg = $action[3]();

  // Generate the operation.
  ARMINSTRUCTIONS.insertSHL_LL( OPER_AL, lhsEReg, lhsEReg, rhsReg, $1->getExp() );

  return( lhsEReg );
};


ereg: tpm_AssignExpSHR( ereg, dreg )
{
  $cost[0] = $cost[2] + $cost[3];
  if ( ( $0->getExp()->getType().getType() == IR_Type::LONG_LONG ) ||
       ( $0->getExp()->getType().getType() == IR_Type::UNSIGNED_LONG_LONG ) )
    $cost[0] += CT( INS_BL_32 ) + 5 * CT( INS_MOV_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "ereg: tpm_AssignExpSHR( ereg, dreg )", $1 );

  // Evaluate the arguments.
  auto *lhsEReg = $action[2]();
  auto *rhsReg = $action[3]();

  // Generate the operation.
  if ( $0->getExp()->getType().getType() == IR_Type::LONG_LONG )
    ARMINSTRUCTIONS.insertSHR_LL(
      OPER_AL, lhsEReg, lhsEReg, rhsReg, $1->getExp() );
  else
    ARMINSTRUCTIONS.insertSHR_ULL(
      OPER_AL, lhsEReg, lhsEReg, rhsReg, $1->getExp() );

  return( lhsEReg );
};


###############################################################################
#
#
# Integer constants of various bit widths, string constants and floats
#
#
###############################################################################

addrOffset: tpm_IntConstExp
{
  if ( isAddrOffset( *$1->getExp() ) )
    $cost[0] = 0;
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "addrOffset: tpm_IntConstExp", $1 );

  auto *icExp = dynamic_cast<IR_IntConstExp*>( $1->getExp() );

  return( icExp->getValue() );
};


const8: tpm_IntConstExp
{
  if ( isConst8( *$1->getExp() ) )
    $cost[0] = 100;
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "const8: tpm_IntConstExp", $1 );

  auto *icExp = dynamic_cast<IR_IntConstExp*>( $1->getExp() );

  return( icExp->getValue() );
};


dreg: tpm_IntConstExp
{
  if ( $1->getExp()->getType().isIntegralType() &&
       ( $1->getExp()->getType().bitSize() <= 32 ) ) {
    auto val =
      dynamic_cast<IR_IntConstExp *>( $1->getExp() )->getValue().getIntValue();
    int countORR = 0;
    for ( int chunks = 1; chunks < 4; chunks++ )
      if ( ( val & (0xFF << ( 8*chunks ) ) ) != 0 )
        countORR++;
    $cost[0] = 10000 + CT( INS_MOV_32 ) + ( countORR * CT( INS_ORR_32 ) );
  } else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_IntConstExp", $1 );

  auto val =
    dynamic_cast<IR_IntConstExp *>( $1->getExp() )->getValue().getIntValue();

  auto *reg = ARMINSTRUCTIONS.CreateRegister( "" );
  ARMINSTRUCTIONS.insertMOV_ORR( reg, val, $1->getExp() );

  return( reg );
};


areg: tpm_StringConstExp
{
  $cost[0] = CT( INS_LDR_32 );
}
=
{
  DEBUG_RULE_ACTION( "areg: tpm_StringConstExp", $1 );

  // Generate operand trees.
  auto *stringConstExp = dynamic_cast<IR_StringConstExp *>( $1->getExp() );
  string stringConst = stringConstExp->getValue();

  // Generate target register holding the result of the current operation.
  auto *reg = ARMINSTRUCTIONS.CreateRegister( "", true );

  // Obtain label in section .rodata.
  string label = ARMCODESEL->getRODataLabel( stringConst );

  // Check if a literal pool was already created.
  auto *f = ARMCODESEL->getLastLLIRFunction();
  string poolLabel = f->getDataSectionAddressLabel( label );

  // If not, create one.
  if ( poolLabel.empty() ) {
    auto *addrBB = createLiteralPool( label, $1->getExp() );
    poolLabel = addrBB->GetLabel();
  }

  // Generate the operation.
  ARMINSTRUCTIONS.insertLDR( OPER_AL, reg, false, poolLabel, $1->getExp() );

  return( reg );
};


dreg: tpm_ImplicitCast( constAddress )
{
  // This is not really a casting rule, it just loads constant addresses to an
  // address register. But until this casting operator is met, it is not clear
  // that an integer constant really is a constant address.
  if ( isRegType( *$1->getExp()->getImplicitCastType() ) )
    $cost[0] = CT( INS_MOV_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_ImplicitCast( constAddress )", $1 );

  auto *reg = ARMINSTRUCTIONS.CreateRegister( "", true );

  // Generate the operation.
  ARMINSTRUCTIONS.insertMOV( reg, $action[2]().getIntValue() );

  return( reg );
};


constantf: tpm_FloatConstExp
{
  if ( $1->getExp()->getType().getType() == IR_Type::FLOAT )
    $cost[0] = 0;
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "constantf: tpm_FloatConstExp", $1 );

  auto *exp = dynamic_cast<IR_FloatConstExp *>( $1->getExp() );

  return( exp->getValue().getSingleValue() );
};


dreg: constantf
{
  $cost[0] = $cost[1] + CT( INS_MOV_32 ) + 3 * CT( INS_ORR_32 );
}
=
{
  DEBUG_RULE_ACTION( "dreg: constantf", $1 );

  Float f = $action[1]();

  // LLIR
  auto *reg = ARMINSTRUCTIONS.CreateRegister( "" );
  ARMINSTRUCTIONS.insertMOV_ORR( reg, f.getValue().getComposed(), $1->getExp() );

  return( reg );
};


constantd: tpm_FloatConstExp
{
  if ( ( $1->getExp()->getType().getType() == IR_Type::DOUBLE ) ||
       ( $1->getExp()->getType().getType() == IR_Type::LONG_DOUBLE ) )
    $cost[0] = 0;
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "constantd: tpm_FloatConstExp", $1 );

  auto f = dynamic_cast<IR_FloatConstExp *>( $1->getExp() )->getValue();

  if ( ( f.getType() == IR_Type::DOUBLE ) ||
       ( f.getType() == IR_Type::LONG_DOUBLE ) )
    return( f.getDoubleValue() );
  else
    return( f.getLongDoubleValue() );
};


ereg: constantd
{
  $cost[0] = $cost[1] + 2 * CT( INS_MOV_32 ) + 6 * CT( INS_ORR_32 );
}
=
{
  DEBUG_RULE_ACTION( "ereg: constantd", $1 );

  Double v = $action[1]();

  typedef Double::Value::Composed Bits;
  Bits bits = v.getValue().getComposed();

  unsigned long low = bits & 0xffffffff;
  unsigned long high = (bits >> 32) & 0xffffffff;

  // LLIR
  auto *ereg = ARMINSTRUCTIONS.CreateERegister( "" );

  // MSB in the first child, LSB in the second one.
  ARMINSTRUCTIONS.insertMOV_ORR( ereg->GetFirstChild(), high, $1->getExp() );
  ARMINSTRUCTIONS.insertMOV_ORR(
    ereg->GetNextChild( ereg->GetFirstChild() ), low, $1->getExp() );

  return( ereg );
};


constantll: tpm_IntConstExp
{
  if ( ( $1->getExp()->getType().getType() == IR_Type::LONG_LONG ) ||
       ( $1->getExp()->getType().getType() == IR_Type::UNSIGNED_LONG_LONG ) )
    $cost[0] = 0;
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "constantll: tpm_IntConstExp", $1 );

  auto l = dynamic_cast<IR_IntConstExp *>( $1->getExp() )->getValue();

  return( l );
};


ereg: constantll
{
  $cost[0] = $cost[1] + 2 * CT( INS_MOV_32 ) + 6 * CT( INS_ORR_32 );
}
=
{
  DEBUG_RULE_ACTION( "ereg: constantll", $1 );

  auto v = $action[1]();

  unsigned int low = getLowerLongLongWord( v );
  unsigned int high = getUpperLongLongWord( v );

  // Create extended LLIR_Register.
  auto *ereg = ARMINSTRUCTIONS.CreateERegister( "" );

  // LSB is the first child, MSB is the second one.
  ARMINSTRUCTIONS.insertMOV_ORR( ereg->GetFirstChild(), low, $1->getExp() );
  ARMINSTRUCTIONS.insertMOV_ORR(
    ereg->GetNextChild( ereg->GetFirstChild() ), high, $1->getExp() );

  return( ereg );
};


###############################################################################
#
#
# Simple expressions
#
#
###############################################################################

dreg: tpm_SymbolExp
{
  // Handles register 'reg' symbols (non-stack, non-global).
  auto &symExp = dynamic_cast<IR_SymbolExp &>( *$1->getExp() );
  auto &sym = symExp.getSymbol();

  if ( isRegType( sym ) && !sym.isGlobal() && !sym.getEnumType() &&
       ( ARMCODESEL->getStack()->getSymbolOffset( &sym ) < 0 ) )
    $cost[0] = loadRegisterSymbolCost( symExp );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_SymbolExp", $1 );

  auto *symExp = dynamic_cast<IR_SymbolExp *>( $1->getExp() );
  return( loadRegisterSymbol( *symExp ) );
};


ereg: tpm_SymbolExp
{
  // Handles pseudo 'ereg' symbols (non-stack, non-global).
  auto &symExp = dynamic_cast<IR_SymbolExp &>( *$1->getExp() );
  auto &sym = symExp.getSymbol();

  if ( isERegType( sym ) && !sym.isGlobal() &&
       ( ARMCODESEL->getStack()->getSymbolOffset( &sym ) < 0 ) )
    $cost[0] = loadRegisterSymbolCost( symExp );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "ereg: tpm_SymbolExp", $1 );

  auto *symExp = dynamic_cast<IR_SymbolExp *>( $1->getExp() );
  return( loadRegisterSymbol( *symExp ) );
};


deref_dreg: tpm_SymbolExp
{
  // Handles local stack 'reg' symbols.
  auto &symExp = dynamic_cast<IR_SymbolExp &>( *$1->getExp() );
  auto &sym = symExp.getSymbol();

  if ( isRegType( sym ) && !sym.isGlobal() && !sym.getEnumType() &&
       ( ARMCODESEL->getStack()->getSymbolOffset( &sym ) >= 0 ) )
    $cost[0] = loadStackSymbolCost( symExp );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "deref_dreg: tpm_SymbolExp", $1 );

  auto *symExp = dynamic_cast<IR_SymbolExp *>( $1->getExp() );
  auto &sym = symExp->getSymbol();

  // In case of a dry run, only return the offset.
  if ( dryRun ) {
    int offset = ARMCODESEL->getStack()->getSymbolOffset( &sym );
    return(
      ARM_LValue {
        nullptr,
        ARM_AddressModification {
          static_cast<LLIR_Register *>( nullptr ), offset, &sym.getType(),
          AddressModification::ModTime::NONE, AddressModification::ModOper::ADD,
          true, true },
        true } );
  }

  return( loadStackSymbol( *symExp, loadResult ) );
};


deref_ereg: tpm_SymbolExp
{
  // Handles local stack 'ereg' symbols.
  auto &symExp = dynamic_cast<IR_SymbolExp &>( *$1->getExp() );
  auto &sym = symExp.getSymbol();

  if ( isERegType( sym ) && !sym.isGlobal() && !sym.getEnumType() &&
       ( ARMCODESEL->getStack()->getSymbolOffset( &sym ) >= 0 ) )
    $cost[0] = loadStackSymbolCost( symExp );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "deref_ereg: tpm_SymbolExp", $1 );

  auto *symExp = dynamic_cast<IR_SymbolExp *>( $1->getExp() );
  auto &sym = symExp->getSymbol();

  // In case of a dry run, only return the offset.
  if ( dryRun ) {
    int offset = ARMCODESEL->getStack()->getSymbolOffset( &sym );
    return(
      ARM_LValue {
        nullptr,
        ARM_AddressModification {
          static_cast<LLIR_Register *>( nullptr ), offset, &sym.getType(),
          AddressModification::ModTime::NONE, AddressModification::ModOper::ADD,
          true, true },
        true } );
  }

  return( loadStackSymbol( *symExp, loadResult ) );
};


deref_dreg: tpm_SymbolExp
{
  // Handles global 'reg' symbols (except enums).
  auto &symExp = dynamic_cast<IR_SymbolExp &>( *$1->getExp() );
  auto &sym = symExp.getSymbol();

  if ( ( isRegType( sym ) || isERegType( sym ) ) && sym.isGlobal() &&
       !sym.getEnumType() )
    $cost[0] = loadGlobalSymbolCost( symExp );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "deref_dreg: tpm_SymbolExp", $1 );

  auto *symExp = dynamic_cast<IR_SymbolExp *>( $1->getExp() );
  return( loadGlobalSymbol( *symExp, loadResult ) );
};


deref_ereg: tpm_SymbolExp
{
  // Handles global 'ereg' symbols (except enums).
  auto &symExp = dynamic_cast<IR_SymbolExp &>( *$1->getExp() );
  auto &sym = symExp.getSymbol();

  if ( isERegType( sym ) && sym.isGlobal() && !sym.getEnumType() )
    $cost[0] = loadGlobalSymbolCost( symExp );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "deref_ereg: tpm_SymbolExp", $1 );

  auto *symExp = dynamic_cast<IR_SymbolExp *>( $1->getExp() );
  return( loadGlobalSymbol( *symExp, loadResult ) );
};


###############################################################################
#
#
# Arithmetic Operations
#
#
###############################################################################

#######
# PLUS
#######

dreg: tpm_BinaryExpPLUS( dreg, dreg )
{
  $cost[0] = $cost[2] + $cost[3];

  if ( $0->getExp()->getType().isIntegralType() )
    $cost[0] += CT( INS_ADD_32 );
  else

  if ( $0->getExp()->getType().getType() == IR_Type::FLOAT )
    $cost[0] += CT( INS_BL_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpPLUS( dreg, dreg )", $1 );

  auto *result = ARMINSTRUCTIONS.CreateRegister( "" );

  // Evaluate the arguments.
  auto *lhsReg = $action[2]();
  auto *rhsReg = $action[3]();

  // Generate the operation.
  if ( $0->getExp()->getType().isIntegralType() )
    ARMINSTRUCTIONS.insertADD( result, rhsReg, lhsReg, $1->getExp() );
  else
    ARMINSTRUCTIONS.insertADD_F( OPER_AL, result, lhsReg, rhsReg, $1->getExp() );

  return( result );
};


dreg: tpm_BinaryExpPLUS( dreg, const8 )
{
  if ( $0->getExp()->getType().isIntegralType() )
    $cost[0] = $cost[2] + $cost[3] + CT( INS_ADD_32 );
  else

  if ( $0->getExp()->getType().isRealType() )
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpPLUS( dreg, const8 )", $1 );

  auto *result = ARMINSTRUCTIONS.CreateRegister( "" );

  // Evaluate the arguments.
  auto *lhsReg = $action[2]();
  int rhs = $action[3]().getIntValue();

  // Generate the operation.
  ARMINSTRUCTIONS.insertADD( result, lhsReg, rhs, $1->getExp() );

  return( result );
};


ereg: tpm_BinaryExpPLUS( ereg, ereg )
{
  $cost[0] = $cost[2] + $cost[3];

  if ( $0->getExp()->getType().isIntegralType() &&
       ( $0->getExp()->getType().bitSize() > 32 ) )
    $cost[0] += CT( INS_ADD_32 ) + CT( INS_ADC_32 );
  else

  if ( ( $0->getExp()->getType().getType() == IR_Type::DOUBLE ) ||
       ( $0->getExp()->getType().getType() == IR_Type::LONG_DOUBLE ) )
    $cost[0] += CT( INS_BL_32 ) + 6 * CT( INS_MOV_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "ereg: tpm_BinaryExpPLUS( ereg, ereg )", $1 );

  auto *eresult = ARMINSTRUCTIONS.CreateERegister( "" );

  // Evaluate the arguments.
  auto *lhsEReg = $action[2]();
  auto *rhsEReg = $action[3]();

  // Generate the operation.
  if ( $0->getExp()->getType().isIntegralType() &&
       ( $0->getExp()->getType().bitSize() > 32 ) )
    ARMINSTRUCTIONS.insertADD_LL(
      OPER_AL, eresult, lhsEReg, rhsEReg, $1->getExp() );
  else
    ARMINSTRUCTIONS.insertADD_D(
      OPER_AL, eresult, lhsEReg, rhsEReg, $1->getExp() );

  return( eresult );
};


#######
# MINUS
#######

dreg: tpm_BinaryExpMINUS( dreg, dreg )
{
  $cost[0] = $cost[2] + $cost[3];

  if ( $0->getExp()->getType().isIntegralType() )
    $cost[0] += CT( INS_SUB_32 );

  else

  if ( $0->getExp()->getType().getType() == IR_Type::FLOAT )
    $cost[0] += CT( INS_BL_32 );

  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpMINUS( dreg, dreg )", $1 );

  auto *result = ARMINSTRUCTIONS.CreateRegister( "" );

  // Evaluate the arguments.
  auto *lhsReg = $action[2]();
  auto *rhsReg = $action[3]();

  // Generate the operation.
  if ( $0->getExp()->getType().isIntegralType() )
    ARMINSTRUCTIONS.insertSUB( result, lhsReg, rhsReg, $1->getExp() );
  else
    ARMINSTRUCTIONS.insertSUB_F( OPER_AL, result, lhsReg, rhsReg, $1->getExp() );

  return( result );
};


dreg: tpm_BinaryExpMINUS( dreg, const8 )
{
  if ( $0->getExp()->getType().isIntegralType() )
    $cost[0] = $cost[2] + $cost[3] + CT( INS_SUB_32 );
  else

  if ( $0->getExp()->getType().isRealType() )
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpMINUS( dreg, const8 )", $1 );

  auto *result = ARMINSTRUCTIONS.CreateRegister( "" );

  // Evaluate the arguments.
  auto *lhsReg = $action[2]();
  int rhs = $action[3]().getIntValue();

  // Generate the operation.
  ARMINSTRUCTIONS.insertSUB( result, lhsReg, rhs, $1->getExp() );

  return( result );
};


dreg: tpm_BinaryExpMINUS( const8, dreg )
{
  if ( $0->getExp()->getType().isIntegralType() )
    $cost[0] = $cost[2] + $cost[3] + CT( INS_RSB_32 );
  else

  if ( $0->getExp()->getType().isRealType() )
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpMINUS( const8, dreg )", $1 );

  auto *result = ARMINSTRUCTIONS.CreateRegister( "" );

  // Evaluate the arguments.
  int lhs = $action[2]().getIntValue();
  auto *rhsReg = $action[3]();

  // Generate the operation.
  ARMINSTRUCTIONS.insertRSB( result, rhsReg, lhs, $1->getExp() );

  return( result );
};


ereg: tpm_BinaryExpMINUS( ereg, ereg )
{
  $cost[0] = $cost[2] + $cost[3];

  if ( $0->getExp()->getType().isIntegralType() &&
       ( $0->getExp()->getType().bitSize() > 32 ) )
    $cost[0] += CT( INS_SUB_32 ) + CT( INS_SBC_32 );
  else

  if ( ( $0->getExp()->getType().getType() == IR_Type::DOUBLE ) ||
       ( $0->getExp()->getType().getType() == IR_Type::LONG_DOUBLE ) )
    $cost[0] += CT( INS_BL_32 ) + 6 * CT( INS_MOV_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "ereg: tpm_BinaryExpMINUS( ereg, ereg )", $1 );

  auto *eresult = ARMINSTRUCTIONS.CreateERegister( "" );

  // Evaluate the arguments.
  auto *lhsEReg = $action[2]();
  auto *rhsEReg = $action[3]();

  // Generate the operation.
  if ( $0->getExp()->getType().isIntegralType() &&
       ( $0->getExp()->getType().bitSize() > 32 ) )
    ARMINSTRUCTIONS.insertSUB_LL(
      OPER_AL, eresult, lhsEReg, rhsEReg, $1->getExp() );
  else
    ARMINSTRUCTIONS.insertSUB_D(
      OPER_AL, eresult, lhsEReg, rhsEReg, $1->getExp() );

  return( eresult );
};


#######
# MULT
#######

dreg: tpm_BinaryExpMULT( dreg, dreg )
{
  $cost[0] = $cost[2] + $cost[3];

  if ( $0->getExp()->getType().isIntegralType() )
    $cost[0] += CT ( INS_MUL_32 );
  else

  if ( $0->getExp()->getType().getType() == IR_Type::FLOAT )
    $cost[0] += CT( INS_BL_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpMULT( dreg, dreg )", $1 );

  auto *result = ARMINSTRUCTIONS.CreateRegister( "" );

  // Evaluate the arguments.
  auto *lhsReg = $action[2]();
  auto *rhsReg = $action[3]();

  // Generate the operation.
  if ( $0->getExp()->getType().isIntegralType() )
    ARMINSTRUCTIONS.insertMUL( result, lhsReg, rhsReg, $1->getExp() );
  else
    ARMINSTRUCTIONS.insertMUL_F( OPER_AL, result, lhsReg, rhsReg, $1->getExp() );

  return( result );
};


ereg: tpm_BinaryExpMULT( ereg, ereg )
{
  $cost[0] = $cost[2] + $cost[3];

  if ( ( $0->getExp()->getType().getType() == IR_Type::LONG_LONG ) ||
       ( $0->getExp()->getType().getType() == IR_Type::UNSIGNED_LONG_LONG ) )
    $cost[0] +=
      8 * CT( INS_MOV_32 ) + CT( INS_UMULL_32 ) + CT( INS_MLA_32 ) +
      CT( INS_MUL_32 ) + CT( INS_ADD_32 );
  else

  if ( ( $0->getExp()->getType().getType() == IR_Type::DOUBLE ) ||
       ( $0->getExp()->getType().getType() == IR_Type::LONG_DOUBLE ) )
    $cost[0] += CT( INS_BL_32 ) + 6 * CT( INS_MOV_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "ereg: tpm_BinaryExpMULT( ereg, ereg )", $1 );

  auto *eresult = ARMINSTRUCTIONS.CreateERegister( "" );

  // Evaluate the arguments.
  auto *lhsEReg = $action[2]();
  auto *rhsEReg = $action[3]();

  // Generate the operation.
  if ( ( $0->getExp()->getType().getType() == IR_Type::LONG_LONG ) ||
       ( $0->getExp()->getType().getType() == IR_Type::UNSIGNED_LONG_LONG ) )
    ARMINSTRUCTIONS.insertMUL_LL(
      OPER_AL, eresult, lhsEReg, rhsEReg, $1->getExp() );
  else
    ARMINSTRUCTIONS.insertMUL_D(
      OPER_AL, eresult, lhsEReg, rhsEReg, $1->getExp() );

  return( eresult );
};


#######
# MOD
#######

dreg: tpm_BinaryExpMOD( dreg, dreg )
{
  $cost[0] = $cost[2] + $cost[3];

  if ( $0->getExp()->getType().isIntegralType() )
    // DIV Call, MULT + Sub
    $cost[0] += CT( INS_BL_32 ) + CT( INS_MUL_32 ) + CT( INS_SUB_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpMOD( dreg, dreg )", $1 );

  auto *result = ARMINSTRUCTIONS.CreateRegister( "" );

  // Evaluate the arguments.
  auto *lhsReg = $action[2]();
  auto *rhsReg = $action[3]();

  // Generate the operation.
  if ( $0->getExp()->getType().isSignedType() )
    ARMINSTRUCTIONS.insertSMODSI( OPER_AL, result, lhsReg, rhsReg, $1->getExp() );
  else
    ARMINSTRUCTIONS.insertUMODSI( OPER_AL, result, lhsReg, rhsReg, $1->getExp() );

  return( result );
};


ereg: tpm_BinaryExpMOD( ereg, ereg )
{
  $cost[0] = $cost[2] + $cost[3];

  if ( ( $0->getExp()->getType().getType() == IR_Type::LONG_LONG ) ||
       ( $0->getExp()->getType().getType() == IR_Type::UNSIGNED_LONG_LONG ) )
    $cost[0] += CT( INS_BL_32 ) + 6 * CT( INS_MOV_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "ereg: tpm_BinaryExpMOD( ereg, ereg )", $1 );

  auto *eresult = ARMINSTRUCTIONS.CreateERegister( "" );

  // Evaluate the arguments.
  auto *lhsEReg = $action[2]();
  auto *rhsEReg = $action[3]();

  // Generate the operation.
  if ( $0->getExp()->getType().getType() == IR_Type::LONG_LONG )
    ARMINSTRUCTIONS.insertMOD_LL(
      OPER_AL, eresult, lhsEReg, rhsEReg, $1->getExp() );
  else
    ARMINSTRUCTIONS.insertMOD_ULL(
      OPER_AL, eresult, lhsEReg, rhsEReg, $1->getExp() );

  return( eresult );
};


#######
# DIV
#######

dreg: tpm_BinaryExpDIV( dreg, dreg )
{
  $cost[0] = $cost[2] + $cost[3];

  if ( $0->getExp()->getType().isIntegralType() )
    $cost[0] += CT ( INS_BL_32 );
  else

  if ( $0->getExp()->getType().getType() == IR_Type::FLOAT )
    $cost[0] += CT( INS_BL_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpDIV( dreg, dreg )", $1 );

  auto *result = ARMINSTRUCTIONS.CreateRegister( "" );

  // Evaluate the arguments.
  auto *lhsReg = $action[2]();
  auto *rhsReg = $action[3]();

  if ( $0->getExp()->getType().isIntegralType() ) {
    // Generate a call to the integer divison.
    // Check if it is a signed or unsigned type.
    if ( $0->getExp()->getType().isUnsignedType() )
      ARMINSTRUCTIONS.insertUDIVSI(
        OPER_AL, result, lhsReg, rhsReg, $1->getExp() );
    else
      ARMINSTRUCTIONS.insertSDIVSI(
        OPER_AL, result, lhsReg, rhsReg, $1->getExp() );

  } else
    ARMINSTRUCTIONS.insertDIV_F( OPER_AL, result, lhsReg, rhsReg, $1->getExp() );

  return( result );
};


ereg: tpm_BinaryExpDIV( ereg, ereg )
{
  $cost[0] = $cost[2] + $cost[3];

  if ( ( $0->getExp()->getType().getType() == IR_Type::LONG_LONG ) ||
       ( $0->getExp()->getType().getType() == IR_Type::UNSIGNED_LONG_LONG ) )
    $cost[0] += CT( INS_BL_32 ) + 6 * CT( INS_MOV_32 );
  else

  if ( ( $0->getExp()->getType().getType() == IR_Type::DOUBLE ) ||
       ( $0->getExp()->getType().getType() == IR_Type::LONG_DOUBLE ) )
    $cost[0] += CT( INS_BL_32 ) + 6 * CT( INS_MOV_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "ereg: tpm_BinaryExpDIV( ereg, ereg )", $1 );

  auto *eresult = ARMINSTRUCTIONS.CreateERegister( "" );

  // Evaluate the arguments.
  auto *lhsEReg = $action[2]();
  auto *rhsEReg = $action[3]();

  // Generate the operation.
  if ( $0->getExp()->getType().getType() == IR_Type::LONG_LONG )
    ARMINSTRUCTIONS.insertDIV_LL(
      OPER_AL, eresult, lhsEReg, rhsEReg, $1->getExp() );
  else

  if ( $0->getExp()->getType().getType() == IR_Type::UNSIGNED_LONG_LONG )
    ARMINSTRUCTIONS.insertDIV_ULL(
      OPER_AL, eresult, lhsEReg, rhsEReg, $1->getExp() );
  else
    ARMINSTRUCTIONS.insertDIV_D(
      OPER_AL, eresult, lhsEReg, rhsEReg, $1->getExp() );

  return( eresult );
};


###############################################################################
#
#
# Bitwise logical operators
#
#
###############################################################################

dreg: tpm_BinaryExpAND( dreg, dreg )
{
  $cost[0] = $cost[2] + $cost[3] + CT( INS_AND_32 );
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpAND( dreg, dreg )", $1 );

  auto *result = ARMINSTRUCTIONS.CreateRegister( "" );

  // Evaluate the arguments.
  LLIR_Register *lhsReg = $action[2]();
  LLIR_Register *rhsReg = $action[3]();

  // Generate the operation.
  ARMINSTRUCTIONS.insertAND( result, lhsReg, rhsReg, $1->getExp() );

  return( result );
};


dreg: tpm_BinaryExpAND( dreg, const8 )
{
  $cost[0] = $cost[2] + $cost[3] + CT( INS_AND_32 );
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpAND( dreg, const8 )", $1 );

  auto *result = ARMINSTRUCTIONS.CreateRegister( "" );

  // Evaluate the arguments.
  auto *lhsReg = $action[2]();
  int rhs = $action[3]().getIntValue();

  // Generate the operation.
  ARMINSTRUCTIONS.insertAND( result, lhsReg, rhs, $1->getExp() );

  return( result );
};


ereg: tpm_BinaryExpAND( ereg, ereg )
{
  if ( ( $1->getExp()->getType().getType() == IR_Type::LONG_LONG ) ||
       ( $1->getExp()->getType().getType() == IR_Type::UNSIGNED_LONG_LONG ) )
    $cost[0] = $cost[2] + $cost[3] + 2 * CT( INS_AND_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "ereg: tpm_BinaryExpAND( ereg, ereg )", $1 );

  // Evaluate the arguments.
  auto *rhsEReg = $action[3]();
  auto *lhsEReg = $action[2]();

  // Create an extended result register.
  auto *eresult = ARMINSTRUCTIONS.CreateERegister( "" );

  // Generate the operation.
  ARMINSTRUCTIONS.insertAND(
    eresult->GetFirstChild(), lhsEReg->GetFirstChild(),
    rhsEReg->GetFirstChild(), $1->getExp() );
  ARMINSTRUCTIONS.insertAND(
    eresult->GetNextChild( eresult->GetFirstChild() ),
    lhsEReg->GetNextChild( lhsEReg->GetFirstChild() ),
    rhsEReg->GetNextChild( rhsEReg->GetFirstChild() ), $1->getExp() );

  return( eresult );
};


dreg: tpm_BinaryExpOR( dreg, dreg )
{
  $cost[0] = $cost[2] + $cost[3] + CT( INS_ORR_32 );
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpOR( dreg, dreg )", $1 );

  auto *result = ARMINSTRUCTIONS.CreateRegister( "" );

  // Evaluate the arguments.
  auto *lhsReg = $action[2]();
  auto *rhsReg = $action[3]();

  // Generate the operation.
  ARMINSTRUCTIONS.insertORR( result, lhsReg, rhsReg, $1->getExp() );

  return( result );
};


dreg: tpm_BinaryExpOR( dreg, const8 )
{
  $cost[0] = $cost[2] + $cost[3] + CT( INS_ORR_32 );
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpOR( dreg, const8 )", $1 );

  auto *result = ARMINSTRUCTIONS.CreateRegister( "" );

  // Evaluate the arguments.
  auto *lhsReg = $action[2]();
  int rhs = $action[3]().getIntValue();

  // Generate the operation.
  ARMINSTRUCTIONS.insertORR( result, lhsReg, rhs , $1->getExp() );

  return( result );
};


ereg: tpm_BinaryExpOR( ereg, ereg )
{
  if ( ( $1->getExp()->getType().getType() == IR_Type::LONG_LONG ) ||
       ( $1->getExp()->getType().getType() == IR_Type::UNSIGNED_LONG_LONG ) )
    $cost[0] = $cost[2] + $cost[3] + 2 * CT( INS_ORR_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "ereg: tpm_BinaryExpOR( ereg, ereg )", $1 );

  // Evaluate the arguments.
  auto *rhsEReg = $action[3]();
  auto *lhsEReg = $action[2]();

  // Create an extended result register.
  auto *eresult = ARMINSTRUCTIONS.CreateERegister( "" );

  // Generate the operation.
  ARMINSTRUCTIONS.insertORR(
    eresult->GetFirstChild(), lhsEReg->GetFirstChild(),
    rhsEReg->GetFirstChild(), $1->getExp() );
  ARMINSTRUCTIONS.insertORR(
    eresult->GetNextChild( eresult->GetFirstChild() ),
    lhsEReg->GetNextChild( lhsEReg->GetFirstChild() ),
    rhsEReg->GetNextChild( rhsEReg->GetFirstChild() ), $1->getExp() );

  return( eresult );
};


dreg: tpm_BinaryExpXOR( dreg, dreg )
{
  $cost[0] = $cost[2] + $cost[3] + CT( INS_EOR_32 );
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpXOR( dreg, dreg )", $1 );

  auto *result = ARMINSTRUCTIONS.CreateRegister( "" );

  // Evaluate the arguments.
  auto *lhsReg = $action[2]();
  auto *rhsReg = $action[3]();

  // Generate the operation.
  ARMINSTRUCTIONS.insertEOR( result, lhsReg, rhsReg, $1->getExp() );

  return( result );
};


dreg: tpm_BinaryExpXOR( dreg, const8 )
{
  $cost[0] = $cost[2] + $cost[3] + CT( INS_EOR_32 );
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpXOR( dreg, const8 )", $1 );

  auto *result = ARMINSTRUCTIONS.CreateRegister( "" );

  // Evaluate the arguments.
  auto *lhsReg = $action[2]();
  int rhs = $action[3]().getIntValue();

  // Generate the operation.
  ARMINSTRUCTIONS.insertEOR( result, lhsReg, rhs, $1->getExp() );

  return( result );
};


ereg: tpm_BinaryExpXOR( ereg, ereg )
{
  if ( ( $1->getExp()->getType().getType() == IR_Type::LONG_LONG ) ||
       ( $1->getExp()->getType().getType() == IR_Type::UNSIGNED_LONG_LONG ) )
    $cost[0] = $cost[2] + $cost[3] + 2 * CT( INS_EOR_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "ereg: tpm_BinaryExpXOR( ereg, ereg )", $1 );

  // Evaluate the arguments.
  auto *rhsEReg = $action[3]();
  auto *lhsEReg = $action[2]();

  // Create an extended result register.
  auto *eresult = ARMINSTRUCTIONS.CreateERegister( "" );

  // Generate the operation.
  ARMINSTRUCTIONS.insertEOR(
    eresult->GetFirstChild(), lhsEReg->GetFirstChild(),
    rhsEReg->GetFirstChild(), $1->getExp() );
  ARMINSTRUCTIONS.insertEOR(
    eresult->GetNextChild( eresult->GetFirstChild() ),
    lhsEReg->GetNextChild( lhsEReg->GetFirstChild() ),
    rhsEReg->GetNextChild( rhsEReg->GetFirstChild() ), $1->getExp() );

  return( eresult );
};


###############################################################################
#
#
#  Relations
#
#
###############################################################################

rel: tpm_BinaryExpEQ( dreg, dreg )
{
  $cost[0] = $cost[2] + $cost[3] + CT( INS_TEQ_32 );
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpEQ( dreg, dreg )", $1 );

  // Evaluate the arguments.
  auto *lhsReg = $action[2]();
  auto *rhsReg = $action[3]();

  // Generate the operation.
  ARMINSTRUCTIONS.insertTEQ( lhsReg, rhsReg, $1->getExp() );

  return( &effectiveType( *$2->getExp() ) );
};


dreg: tpm_BinaryExpEQ( dreg, dreg )
{
  $cost[0] = $cost[2] + $cost[3] + CT( INS_TEQ_32 ) + 2 * CT( INS_MOV_32 );
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpEQ( dreg, dreg )", $1 );

  auto *result = ARMINSTRUCTIONS.CreateRegister( "" );

  // Evaluate the arguments.
  auto *lhsReg = $action[2]();
  auto *rhsReg = $action[3]();

  // Generate the operation.
  ARMINSTRUCTIONS.insertTEQ( lhsReg, rhsReg, $1->getExp() );

  // Insert a 1 or a 0.
  IR_Type &t = effectiveType( *$2->getExp() );
  bool sgn = t.isSignedType() || t.isRealType();

  ARMINSTRUCTIONS.insertMOV( OPER_AL, result, 0, $1->getExp() );
  ARMINSTRUCTIONS.insertMOV( getOper( $1, sgn ), result, 1, $1->getExp() );

  return( result );
};


rel: tpm_BinaryExpEQ( dreg, const8 )
{
  $cost[0] = $cost[2] + $cost[3] + CT( INS_TEQ_32 );
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpEQ( dreg, const8 )", $1 );

  // Evaluate the arguments.
  auto *lhsReg = $action[2]();
  int rhs = $action[3]().getIntValue();

  // Generate the operation.
  ARMINSTRUCTIONS.insertTEQ( lhsReg, rhs, $1->getExp() );

  return( &effectiveType( *$2->getExp() ) );
};


dreg: tpm_BinaryExpEQ( dreg, const8 )
{
  $cost[0] = $cost[2] + $cost[3] + CT( INS_TEQ_32 );
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpEQ( dreg, const8 )", $1 );

  auto *result = ARMINSTRUCTIONS.CreateRegister( "" );

  // Evaluate the arguments.
  auto *lhsReg = $action[2]();
  int rhs = $action[3]().getIntValue();

  // Generate the operation.
  ARMINSTRUCTIONS.insertTEQ( lhsReg, rhs, $1->getExp() );

  // Insert a 1 or a 0.
  ARMINSTRUCTIONS.insertMOV( OPER_AL, result, 0, $1->getExp() );
  auto &t = effectiveType( *$2->getExp() );
  bool sgn = t.isSignedType() || t.isRealType();
  ARMINSTRUCTIONS.insertMOV( getOper( $1, sgn ), result, 1, $1->getExp() );

  return( result );
};


dreg: tpm_BinaryExpEQ( ereg, ereg )
{
  $cost[0] = $cost[2] + $cost[3] + 2 * CT( INS_MOV_32 );

  if ( effectiveType( *$2->getExp() ).getType() == IR_Type::UNSIGNED_LONG_LONG ||
       effectiveType( *$2->getExp() ).getType() == IR_Type::LONG_LONG )
    $cost[0] += 2 * CT( INS_TEQ_32 );
  else

  if ( effectiveType( *$2->getExp() ).getType() == IR_Type::DOUBLE ||
       effectiveType( *$2->getExp() ).getType() == IR_Type::LONG_DOUBLE )
    $cost[0] += CT( INS_BL_32 ) + 5 * CT( INS_MOV_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpEQ( ereg, ereg )", $1 );

  // Evaluate the arguments.
  auto *rhsEReg = $action[3]();
  auto *lhsEReg = $action[2]();

  // Create a result register to represent an integer value of 0 or 1.
  auto *result = ARMINSTRUCTIONS.CreateRegister( "" );

  // Generate the operation.
  if ( effectiveType( *$2->getExp() ).getType() == IR_Type::UNSIGNED_LONG_LONG ||
       effectiveType( *$2->getExp() ).getType() == IR_Type::LONG_LONG ) {
    ARMINSTRUCTIONS.insertTEQ(
      lhsEReg->GetFirstChild(), rhsEReg->GetFirstChild(), $1->getExp() );
    ARMINSTRUCTIONS.insertTEQ(
      OPER_EQ, lhsEReg->GetNextChild( lhsEReg->GetFirstChild() ),
      rhsEReg->GetNextChild( rhsEReg->GetFirstChild() ), $1->getExp() );
  } else {
    ARMINSTRUCTIONS.insertEQ_D( OPER_AL, result, lhsEReg, rhsEReg, $1->getExp() );
    // Compare result of softfloat routine against zero.
    ARMINSTRUCTIONS.insertCMP( OPER_AL, result, 0, $1->getExp() );
  }

  // Insert a 1 or a 0.
  ARMINSTRUCTIONS.insertMOV( OPER_AL, result, 0, $1->getExp() );
  auto &t = effectiveType( *$2->getExp() );
  bool sgn = t.isSignedType() || t.isRealType();
  ARMINSTRUCTIONS.insertMOV( getOper( $1, sgn ), result, 1, $1->getExp() );

  return( result );
};


rel: tpm_BinaryExpEQ( ereg, ereg )
{
  $cost[0] = $cost[2] + $cost[3];

  if ( effectiveType( *$2->getExp() ).getType() == IR_Type::UNSIGNED_LONG_LONG ||
       effectiveType( *$2->getExp() ).getType() == IR_Type::LONG_LONG )
    $cost[0] += 2 * CT( INS_TEQ_32 );
  else

  if ( effectiveType( *$2->getExp() ).getType() == IR_Type::DOUBLE ||
       effectiveType( *$2->getExp() ).getType() == IR_Type::LONG_DOUBLE )
    $cost[0] += CT( INS_BL_32 ) + 5 * CT( INS_MOV_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpEQ( ereg, ereg )", $1 );

  // Evaluate the arguments.
  auto *rhsEReg = $action[3]();
  auto *lhsEReg = $action[2]();

  // Generate the operation.
  if ( effectiveType( *$2->getExp() ).getType() == IR_Type::UNSIGNED_LONG_LONG ||
       effectiveType( *$2->getExp() ).getType() == IR_Type::LONG_LONG ) {
    ARMINSTRUCTIONS.insertTEQ(
      lhsEReg->GetFirstChild(), rhsEReg->GetFirstChild(), $1->getExp() );
    ARMINSTRUCTIONS.insertTEQ(
      OPER_EQ, lhsEReg->GetNextChild( lhsEReg->GetFirstChild() ),
      rhsEReg->GetNextChild( rhsEReg->GetFirstChild() ), $1->getExp() );
  } else {
    auto *result = ARMINSTRUCTIONS.CreateRegister( "" );
    ARMINSTRUCTIONS.insertEQ_D( OPER_AL, result, lhsEReg, rhsEReg, $1->getExp() );
    // Compare result of softfloat routine against zero.
    ARMINSTRUCTIONS.insertCMP( OPER_AL, result, 0, $1->getExp() );
  }

  return( &effectiveType( *$2->getExp() ) );
};


rel: tpm_BinaryExpLT( dreg, dreg )
{
  auto &t = effectiveType( *$2->getExp() );

  if ( t.isIntegralType() || t.isPointerType() )
    $cost[0] = $cost[2] + $cost[3] + CT( INS_CMP_32 );
  else

  if ( t.getType() == IR_Type::FLOAT )
    $cost[0] = $cost[2] + $cost[3] + CT( INS_BL_32 ) + CT( INS_CMP_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpLT( dreg, dreg )", $1 );

  // Evaluate the arguments.
  auto *lhsReg = $action[2]();
  auto *rhsReg = $action[3]();

  auto &t = effectiveType( *$2->getExp() );

  // Generate the operation.
  if ( t.isIntegralType() || t.isPointerType() )
    ARMINSTRUCTIONS.insertCMP( OPER_AL, lhsReg, rhsReg, $1->getExp() );
  else {
    // Compare the floats.
    auto *result = ARMINSTRUCTIONS.CreateRegister( "" );
    ARMINSTRUCTIONS.insertLT_F( OPER_AL, result, lhsReg, rhsReg, $1->getExp() );
    // And now compare it against zero.
    ARMINSTRUCTIONS.insertCMP( OPER_AL, result, 0, $1->getExp() );
  }

  return( &t );
};


dreg: tpm_BinaryExpLT( dreg, dreg )
{
  auto &t = effectiveType( *$2->getExp() );

  if ( t.isIntegralType() || t.isPointerType() )
    $cost[0] = $cost[2] + $cost[3] + CT( INS_CMP_32 ) + 2 * CT( INS_MOV_32 );
  else

  if ( t.getType() == IR_Type::FLOAT )
    $cost[0] =
      $cost[2] + $cost[3] + CT( INS_BL_32 ) + CT( INS_CMP_32 ) +
      2 * CT( INS_MOV_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpLT( dreg, dreg )", $1 );

  // Evaluate the arguments.
  auto *lhsReg = $action[2]();
  auto *rhsReg = $action[3]();

  auto &t = effectiveType( *$2->getExp() );

  // Generate the operation.
  if ( t.isIntegralType() || t.isPointerType() )
    ARMINSTRUCTIONS.insertCMP( OPER_AL, lhsReg, rhsReg, $1->getExp() );
  else {
    // Compare the floats.
    auto *reg = ARMINSTRUCTIONS.CreateRegister( "" );
    ARMINSTRUCTIONS.insertLT_F( OPER_AL, reg, lhsReg, rhsReg, $1->getExp() );
    // And now compare it against zero.
    ARMINSTRUCTIONS.insertCMP( OPER_AL, reg, 0, $1->getExp() );
  }

  // Insert a 1 or a 0.
  auto *result = ARMINSTRUCTIONS.CreateRegister( "" );
  ARMINSTRUCTIONS.insertMOV( OPER_AL, result, 0, $1->getExp() );
  bool sgn = t.isSignedType() || t.isRealType();
  ARMINSTRUCTIONS.insertMOV( getOper( $1, sgn ), result, 1, $1->getExp() );

  return( result );
};


rel: tpm_BinaryExpLT( dreg, const8 )
{
  $cost[0] = $cost[2] + $cost[3] + CT( INS_CMP_32 );
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpLT( dreg, const8 )", $1 );

  // Evaluate the arguments.
  auto *lhsReg = $action[2]();
  int rhs = $action[3]().getIntValue();

  // Generate the operation.
  ARMINSTRUCTIONS.insertCMP( OPER_AL, lhsReg, rhs, $1->getExp() );

  return( &effectiveType( *$2->getExp() ) );
};


dreg: tpm_BinaryExpLT( dreg, const8 )
{
  $cost[0] = $cost[2] + $cost[3] + CT( INS_CMP_32 );
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpLT( dreg, const8 )", $1 );

  auto *result = ARMINSTRUCTIONS.CreateRegister( "" );

  // Evaluate the arguments.
  auto *lhsReg = $action[2]();
  int rhs = $action[3]().getIntValue();

  // Generate the operation.
  ARMINSTRUCTIONS.insertCMP( OPER_AL, lhsReg, rhs, $1->getExp() );

  // Insert a 1 or a 0.
  ARMINSTRUCTIONS.insertMOV( OPER_AL, result, 0, $1->getExp() );
  auto &t = effectiveType( *$2->getExp() );
  bool sgn = t.isSignedType() || t.isRealType();
  ARMINSTRUCTIONS.insertMOV( getOper( $1, sgn ), result, 1, $1->getExp() );

  return( result );
};


dreg: tpm_BinaryExpLT( ereg, ereg )
{
  $cost[0] = $cost[2] + $cost[3] + 2 * CT( INS_MOV_32 );

  if ( effectiveType( *$2->getExp() ).getType() == IR_Type::UNSIGNED_LONG_LONG ||
       effectiveType( *$2->getExp() ).getType() == IR_Type::LONG_LONG )
    $cost[0] += 2 * CT( INS_CMP_32 );
  else

  if ( effectiveType( *$2->getExp() ).getType() == IR_Type::DOUBLE ||
       effectiveType( *$2->getExp() ).getType() == IR_Type::LONG_DOUBLE )
    $cost[0] += CT( INS_BL_32 ) + 5 * CT( INS_MOV_32 ) + CT( INS_CMP_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpLT( ereg, ereg )", $1 );

  // Evaluate the arguments.
  auto *rhsEReg = $action[3]();
  auto *lhsEReg = $action[2]();

  // Create a result register to represent an integer value of 0 or 1.
  auto *result = ARMINSTRUCTIONS.CreateRegister( "" );

  // Generate the operation.
  if ( effectiveType( *$2->getExp() ).getType() == IR_Type::UNSIGNED_LONG_LONG ||
       effectiveType( *$2->getExp() ).getType() == IR_Type::LONG_LONG ) {
    ARMINSTRUCTIONS.insertCMP(
      OPER_AL, lhsEReg->GetNextChild( lhsEReg->GetFirstChild() ),
      rhsEReg->GetNextChild( rhsEReg->GetFirstChild() ), $1->getExp() );
    // Check the lower children iff the higher children are the same.
    ARMINSTRUCTIONS.insertCMP(
      OPER_EQ, lhsEReg->GetFirstChild(), rhsEReg->GetFirstChild(),
      $1->getExp() );
  } else {
    ARMINSTRUCTIONS.insertLT_D( OPER_AL, result, lhsEReg, rhsEReg, $1->getExp() );
    // And now compare it against zero.
    ARMINSTRUCTIONS.insertCMP( OPER_AL, result, 0, $1->getExp() );
  }

  // Insert a 1 or a 0.
  ARMINSTRUCTIONS.insertMOV( OPER_AL, result, 0, $1->getExp() );
  auto &t = effectiveType( *$2->getExp() );
  bool sgn = t.isSignedType() || t.isRealType();
  ARMINSTRUCTIONS.insertMOV( getOper( $1, sgn ), result, 1, $1->getExp() );

  return( result );
};


rel: tpm_BinaryExpLT( ereg, ereg )
{
  $cost[0] = $cost[2] + $cost[3];

  if ( effectiveType( *$2->getExp() ).getType() == IR_Type::UNSIGNED_LONG_LONG ||
       effectiveType( *$2->getExp() ).getType() == IR_Type::LONG_LONG )
    $cost[0] += 2 * CT( INS_CMP_32 );
  else

  if ( effectiveType( *$2->getExp() ).getType() == IR_Type::DOUBLE ||
       effectiveType( *$2->getExp() ).getType() == IR_Type::LONG_DOUBLE )
    $cost[0] += CT( INS_BL_32 ) + 5 * CT( INS_MOV_32 ) + CT( INS_CMP_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpLT( ereg, ereg )", $1 );

  // Evaluate the arguments.
  auto *rhsEReg = $action[3]();
  auto *lhsEReg = $action[2]();

  // Generate the operation.
  if ( effectiveType( *$2->getExp() ).getType() == IR_Type::UNSIGNED_LONG_LONG ||
       effectiveType( *$2->getExp() ).getType() == IR_Type::LONG_LONG ) {
    ARMINSTRUCTIONS.insertCMP(
      OPER_AL, lhsEReg->GetNextChild( lhsEReg->GetFirstChild() ),
      rhsEReg->GetNextChild( rhsEReg->GetFirstChild() ), $1->getExp() );
    // Check the lower children iff the higher children are the same.
    ARMINSTRUCTIONS.insertCMP(
      OPER_EQ, lhsEReg->GetFirstChild(), rhsEReg->GetFirstChild(),
      $1->getExp() );
  } else {
    auto *result = ARMINSTRUCTIONS.CreateRegister( "" );
    ARMINSTRUCTIONS.insertLT_D( OPER_AL, result, lhsEReg, rhsEReg, $1->getExp() );
    // And now compare it against zero.
    ARMINSTRUCTIONS.insertCMP( OPER_AL, result, 0, $1->getExp() );
  }

  return( &effectiveType( *$2->getExp() ) );
};


rel: tpm_BinaryExpGT( dreg, dreg )
{
  auto &t = effectiveType( *$2->getExp() );

  if ( t.isIntegralType() || t.isPointerType() )
    $cost[0] = $cost[2] + $cost[3] + CT( INS_CMP_32 );
  else

  if ( t.getType() == IR_Type::FLOAT )
    $cost[0] = $cost[2] + $cost[3] + CT( INS_BL_32 ) + CT( INS_CMP_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpGT( dreg, dreg )", $1 );

  // Evaluate the arguments.
  auto *lhsReg = $action[2]();
  auto *rhsReg = $action[3]();

  auto &t = effectiveType( *$2->getExp() );

  // Generate the operation.
  if ( t.isIntegralType() || t.isPointerType() )
    ARMINSTRUCTIONS.insertCMP( OPER_AL, lhsReg, rhsReg, $1->getExp() );
  else {
    // Compare the floats.
    auto *result = ARMINSTRUCTIONS.CreateRegister( "" );
    ARMINSTRUCTIONS.insertGT_F( OPER_AL, result, lhsReg, rhsReg, $1->getExp() );
    // And now compare it against zero.
    ARMINSTRUCTIONS.insertCMP( OPER_AL, result, 0, $1->getExp() );
  }

  return( &t );
};


dreg: tpm_BinaryExpGT( dreg, dreg )
{
  auto &t = effectiveType( *$2->getExp() );

  if ( t.isIntegralType() || t.isPointerType() )
    $cost[0] = $cost[2] + $cost[3] + CT( INS_CMP_32 ) + 2 * CT( INS_MOV_32 );
  else

  if ( t.getType() == IR_Type::FLOAT )
    $cost[0] =
      $cost[2] + $cost[3] + CT( INS_BL_32 ) + CT( INS_CMP_32 ) +
      2 * CT( INS_MOV_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpGT( dreg, dreg )", $1 );

  auto &t = effectiveType( *$2->getExp() );

  // Evaluate the arguments.
  auto *lhsReg = $action[2]();
  auto *rhsReg = $action[3]();

  // Generate the operation.
  if ( t.isIntegralType() || t.isPointerType() )
    ARMINSTRUCTIONS.insertCMP( OPER_AL, lhsReg, rhsReg, $1->getExp() );
  else  {
    // Compare the floats.
    auto *result = ARMINSTRUCTIONS.CreateRegister( "" );
    ARMINSTRUCTIONS.insertGT_F( OPER_AL, result, lhsReg, rhsReg, $1->getExp() );
    // And now compare it against zero.
    ARMINSTRUCTIONS.insertCMP( OPER_AL, result, 0, $1->getExp() );
  }

  // Insert a 1 or a 0.
  auto *result = ARMINSTRUCTIONS.CreateRegister( "" );
  ARMINSTRUCTIONS.insertMOV( OPER_AL, result, 0, $1->getExp() );
  bool sgn = t.isSignedType() || t.isRealType();
  ARMINSTRUCTIONS.insertMOV( getOper( $1, sgn ), result, 1, $1->getExp() );

  return( result );
};


rel: tpm_BinaryExpGT( dreg, const8 )
{
  $cost[0] = $cost[2] + $cost[3] + CT( INS_CMP_32 );
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpGT( dreg, const8 )", $1 );

  // Evaluate the arguments.
  auto *lhsReg = $action[2]();
  int rhs = $action[3]().getIntValue();

  // Generate the operation.
  ARMINSTRUCTIONS.insertCMP( OPER_AL, lhsReg, rhs, $1->getExp() );

  return( &effectiveType( *$2->getExp() ) );
};


dreg: tpm_BinaryExpGT( dreg, const8 )
{
  $cost[0] = $cost[2] + $cost[3] + CT( INS_CMP_32 );
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpGT( dreg, const8 )", $1 );

  // Evaluate the arguments.
  auto *lhsReg = $action[2]();
  int rhs = $action[3]().getIntValue();

  // Generate the operation.
  ARMINSTRUCTIONS.insertCMP( OPER_AL, lhsReg, rhs, $1->getExp() );

  // Insert a 1 or a 0.
  auto *result = ARMINSTRUCTIONS.CreateRegister( "" );
  ARMINSTRUCTIONS.insertMOV( OPER_AL, result, 0, $1->getExp() );
  auto &t = effectiveType( *$2->getExp() );
  bool sgn = t.isSignedType() || t.isRealType();
  ARMINSTRUCTIONS.insertMOV( getOper( $1, sgn ), result, 1, $1->getExp() );

  return( result );
};


dreg: tpm_BinaryExpGT( ereg, ereg )
{
  $cost[0] = $cost[2] + $cost[3] + 2 * CT( INS_MOV_32 );

  if ( effectiveType( *$2->getExp() ).getType() == IR_Type::UNSIGNED_LONG_LONG ||
       effectiveType( *$2->getExp() ).getType() == IR_Type::LONG_LONG )
    $cost[0] += 2 * CT( INS_CMP_32 );
  else

  if ( effectiveType( *$2->getExp() ).getType() == IR_Type::DOUBLE ||
       effectiveType( *$2->getExp() ).getType() == IR_Type::LONG_DOUBLE )
    $cost[0] += CT( INS_BL_32 ) + 5 * CT( INS_MOV_32 ) + CT( INS_CMP_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpGT( ereg, ereg )", $1 );

  // Evaluate the arguments.
  auto *rhsEReg = $action[3]();
  auto *lhsEReg = $action[2]();

  // Create a result register to represent an integer value of 0 or 1.
  auto *result = ARMINSTRUCTIONS.CreateRegister( "" );

  // Generate the operation.
  if ( effectiveType( *$2->getExp() ).getType() == IR_Type::UNSIGNED_LONG_LONG ||
       effectiveType( *$2->getExp() ).getType() == IR_Type::LONG_LONG ) {
    ARMINSTRUCTIONS.insertCMP(
      OPER_AL, lhsEReg->GetNextChild( lhsEReg->GetFirstChild() ),
      rhsEReg->GetNextChild( rhsEReg->GetFirstChild() ), $1->getExp() );
    // Check the lower children iff the higher children are the same.
    ARMINSTRUCTIONS.insertCMP(
      OPER_EQ, lhsEReg->GetFirstChild(), rhsEReg->GetFirstChild(),
      $1->getExp() );
  } else {
    ARMINSTRUCTIONS.insertGT_D( OPER_AL, result, lhsEReg, rhsEReg, $1->getExp() );
    // And now compare it against zero.
    ARMINSTRUCTIONS.insertCMP( OPER_AL, result, 0, $1->getExp() );
  }

  // Insert a 1 or a 0.
  ARMINSTRUCTIONS.insertMOV( OPER_AL, result, 0, $1->getExp() );
  auto &t = effectiveType( *$2->getExp() );
  bool sgn = t.isSignedType() || t.isRealType();
  ARMINSTRUCTIONS.insertMOV( getOper( $1, sgn ), result, 1, $1->getExp() );

  return( result );
};


rel: tpm_BinaryExpGT( ereg, ereg )
{
  $cost[0] = $cost[2] + $cost[3];

  if ( effectiveType( *$2->getExp() ).getType() == IR_Type::UNSIGNED_LONG_LONG ||
       effectiveType( *$2->getExp() ).getType() == IR_Type::LONG_LONG )
    $cost[0] += 2 * CT( INS_CMP_32 );
  else

  if ( effectiveType( *$2->getExp() ).getType() == IR_Type::DOUBLE ||
       effectiveType( *$2->getExp() ).getType() == IR_Type::LONG_DOUBLE )
    $cost[0] += CT( INS_BL_32 ) + 5 * CT( INS_MOV_32 ) + CT( INS_CMP_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpGT( ereg, ereg )", $1 );

  // Evaluate the arguments.
  auto *rhsEReg = $action[3]();
  auto *lhsEReg = $action[2]();

  // Generate the operation.
  if ( effectiveType( *$2->getExp() ).getType() == IR_Type::UNSIGNED_LONG_LONG ||
       effectiveType( *$2->getExp() ).getType() == IR_Type::LONG_LONG ) {
    ARMINSTRUCTIONS.insertCMP(
      OPER_AL, lhsEReg->GetNextChild( lhsEReg->GetFirstChild() ),
      rhsEReg->GetNextChild( rhsEReg->GetFirstChild() ), $1->getExp() );
    // Check the lower children iff the higher children are the same.
    ARMINSTRUCTIONS.insertCMP(
      OPER_EQ, lhsEReg->GetFirstChild(), rhsEReg->GetFirstChild(),
      $1->getExp() );
  } else {
    auto *result = ARMINSTRUCTIONS.CreateRegister( "" );
    ARMINSTRUCTIONS.insertGT_D( OPER_AL, result, lhsEReg, rhsEReg, $1->getExp() );
    // And now compare it against zero.
    ARMINSTRUCTIONS.insertCMP( OPER_AL, result, 0, $1->getExp() );
  }

  return( &effectiveType( *$2->getExp() ) );
};


rel: tpm_BinaryExpLEQ( dreg, dreg )
{
  auto &t = effectiveType( *$2->getExp() );

  if ( t.isIntegralType() || t.isPointerType() )
    $cost[0] = $cost[2] + $cost[3] + CT( INS_CMP_32 );
  else

  if ( t.getType() == IR_Type::FLOAT )
    $cost[0] = $cost[2] + $cost[3] + CT( INS_BL_32 ) + CT( INS_CMP_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpLEQ( dreg, dreg )", $1 );

  // Evaluate the arguments.
  auto *lhsReg = $action[2]();
  auto *rhsReg = $action[3]();

  auto &t = effectiveType( *$2->getExp() );

  // Generate the operation.
  if ( t.isIntegralType() || t.isPointerType() )
    ARMINSTRUCTIONS.insertCMP( OPER_AL, lhsReg, rhsReg, $1->getExp() );
  else {
    // Compare the floats.
    auto *result = ARMINSTRUCTIONS.CreateRegister( "" );
    ARMINSTRUCTIONS.insertLE_F( OPER_AL, result, lhsReg, rhsReg, $1->getExp() );
    // And now compare it against zero.
    ARMINSTRUCTIONS.insertCMP( OPER_AL, result, 0, $1->getExp() );
  }

  return( &t );
};


dreg: tpm_BinaryExpLEQ( dreg, dreg )
{
  auto &t = effectiveType( *$2->getExp() );

  if ( t.isIntegralType() || t.isPointerType() )
    $cost[0] = $cost[2] + $cost[3] + CT( INS_CMP_32 ) + 2 * CT( INS_MOV_32 );
  else

  if ( t.getType() == IR_Type::FLOAT )
    $cost[0] =
      $cost[2] + $cost[3] + CT( INS_BL_32 ) + CT( INS_CMP_32 ) +
      2 * CT( INS_MOV_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpLEQ( dreg, dreg )", $1 );

  auto &t = effectiveType( *$2->getExp() );

  // Evaluate the arguments.
  auto *lhsReg = $action[2]();
  auto *rhsReg = $action[3]();

  // Generate the operation.
  if ( t.isIntegralType() || t.isPointerType() )
    ARMINSTRUCTIONS.insertCMP( OPER_AL, lhsReg, rhsReg, $1->getExp() );
  else {
    // Compare the floats.
    auto *result = ARMINSTRUCTIONS.CreateRegister( "" );
    ARMINSTRUCTIONS.insertLE_F( OPER_AL, result, lhsReg, rhsReg, $1->getExp() );
    // And now compare it against zero.
    ARMINSTRUCTIONS.insertCMP( OPER_AL, result, 0, $1->getExp() );
  }

  // Insert a 1 or a 0.
  auto *result = ARMINSTRUCTIONS.CreateRegister( "" );
  ARMINSTRUCTIONS.insertMOV( OPER_AL, result, 0, $1->getExp() );
  bool sgn = t.isSignedType() || t.isRealType();
  ARMINSTRUCTIONS.insertMOV( getOper( $1, sgn ), result, 1, $1->getExp() );

  return( result );
};


rel: tpm_BinaryExpLEQ( dreg, const8 )
{
  $cost[0] = $cost[2] + $cost[3] + CT( INS_CMP_32 );
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpLEQ( dreg, const8 )", $1 );

  // Evaluate the arguments.
  auto *lhsReg = $action[2]();
  int rhs = $action[3]().getIntValue();

  // Generate the operation.
  ARMINSTRUCTIONS.insertCMP( OPER_AL, lhsReg, rhs, $1->getExp() );

  return( &effectiveType( *$2->getExp() ) );
};


dreg: tpm_BinaryExpLEQ( ereg, ereg )
{
  $cost[0] = $cost[2] + $cost[3] + 2 * CT( INS_MOV_32 );

  if ( effectiveType( *$2->getExp() ).getType() == IR_Type::UNSIGNED_LONG_LONG ||
       effectiveType( *$2->getExp() ).getType() == IR_Type::LONG_LONG )
    $cost[0] += 2 * CT( INS_CMP_32 );
  else

  if ( effectiveType( *$2->getExp() ).getType() == IR_Type::DOUBLE ||
       effectiveType( *$2->getExp() ).getType() == IR_Type::LONG_DOUBLE )
    $cost[0] += CT( INS_BL_32 ) + 5 * CT( INS_MOV_32 ) + CT( INS_CMP_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpLEQ( ereg, ereg )", $1 );

  // Evaluate the arguments.
  auto *rhsEReg = $action[3]();
  auto *lhsEReg = $action[2]();

  // Create a result register to represent an integer value of 0 or 1.
  auto *result = ARMINSTRUCTIONS.CreateRegister( "" );

  // Generate the operation.
  if ( effectiveType( *$2->getExp() ).getType() == IR_Type::UNSIGNED_LONG_LONG ||
       effectiveType( *$2->getExp() ).getType() == IR_Type::LONG_LONG ) {
    ARMINSTRUCTIONS.insertCMP(
      OPER_AL, lhsEReg->GetNextChild( lhsEReg->GetFirstChild() ),
      rhsEReg->GetNextChild( rhsEReg->GetFirstChild() ), $1->getExp() );
    // Check the lower children iff the higher children are the same.
    ARMINSTRUCTIONS.insertCMP(
      OPER_EQ, lhsEReg->GetFirstChild(), rhsEReg->GetFirstChild(),
      $1->getExp() );
  } else {
    ARMINSTRUCTIONS.insertLEQ_D(
      OPER_AL, result, lhsEReg, rhsEReg, $1->getExp() );
    // Compare result of softfloat routine against zero.
    ARMINSTRUCTIONS.insertCMP( OPER_AL, result, 0, $1->getExp() );
  }

  // Insert a 1 or a 0.
  ARMINSTRUCTIONS.insertMOV( OPER_AL, result, 0, $1->getExp() );
  auto &t = effectiveType( *$2->getExp() );
  bool sgn = t.isSignedType() || t.isRealType();
  ARMINSTRUCTIONS.insertMOV( getOper( $1, sgn ), result, 1, $1->getExp() );

  return( result );
};


rel: tpm_BinaryExpLEQ( ereg, ereg )
{
  $cost[0] = $cost[2] + $cost[3];

  if ( effectiveType( *$2->getExp() ).getType() == IR_Type::UNSIGNED_LONG_LONG ||
       effectiveType( *$2->getExp() ).getType() == IR_Type::LONG_LONG )
    $cost[0] += 2 * CT( INS_CMP_32 );
  else

  if ( effectiveType( *$2->getExp() ).getType() == IR_Type::DOUBLE ||
       effectiveType( *$2->getExp() ).getType() == IR_Type::LONG_DOUBLE )
    $cost[0] += CT( INS_BL_32 ) + 5 * CT( INS_MOV_32 ) + CT( INS_CMP_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpLEQ( ereg, ereg )", $1 );

  // Evaluate the arguments.
  auto *rhsEReg = $action[3]();
  auto *lhsEReg = $action[2]();

  // Generate the operation.
  if ( effectiveType( *$2->getExp() ).getType() == IR_Type::UNSIGNED_LONG_LONG ||
       effectiveType( *$2->getExp() ).getType() == IR_Type::LONG_LONG ) {
    ARMINSTRUCTIONS.insertCMP(
      OPER_AL, lhsEReg->GetNextChild( lhsEReg->GetFirstChild() ),
      rhsEReg->GetNextChild( rhsEReg->GetFirstChild() ), $1->getExp() );
    // Check the lower children iff the higher children are the same.
    ARMINSTRUCTIONS.insertCMP(
      OPER_EQ, lhsEReg->GetFirstChild(), rhsEReg->GetFirstChild(),
      $1->getExp() );
  } else {
    auto *result = ARMINSTRUCTIONS.CreateRegister( "" );
    ARMINSTRUCTIONS.insertLEQ_D(
      OPER_AL, result, lhsEReg, rhsEReg, $1->getExp() );
    // Compare result of softfloat routine against zero.
    ARMINSTRUCTIONS.insertCMP( OPER_AL, result, 0, $1->getExp() );
  }

  return( &effectiveType( *$2->getExp() ) );
};


rel: tpm_BinaryExpGEQ( dreg, dreg )
{
  auto &t = effectiveType( *$2->getExp() );

  if ( t.isIntegralType() || t.isPointerType() )
    $cost[0] = $cost[2] + $cost[3] + CT( INS_CMP_32 );
  else

  if ( t.getType() == IR_Type::FLOAT )
    $cost[0] = $cost[2] + $cost[3] + CT( INS_BL_32 ) + CT( INS_CMP_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpGEQ( dreg, dreg )", $1 );

  auto &t = effectiveType( *$2->getExp() );

  // Evaluate the arguments.
  auto *lhsReg = $action[2]();
  auto *rhsReg = $action[3]();

  // Generate the operation.
  if ( t.isIntegralType() || t.isPointerType() )
    ARMINSTRUCTIONS.insertCMP( OPER_AL, lhsReg, rhsReg, $1->getExp() );
  else {
    // Compare the floats.
    auto *result = ARMINSTRUCTIONS.CreateRegister( "" );
    ARMINSTRUCTIONS.insertGE_F( OPER_AL, result, lhsReg, rhsReg, $1->getExp() );
    // And now compare it against zero.
    ARMINSTRUCTIONS.insertCMP( OPER_AL, result, 0, $1->getExp() );
  }

  return( &effectiveType( *$2->getExp() ) );
};


dreg: tpm_BinaryExpGEQ( dreg, dreg )
{
  auto &t = effectiveType( *$2->getExp() );

  if ( t.isIntegralType() || t.isPointerType() )
    $cost[0] = $cost[2] + $cost[3] + CT( INS_CMP_32 ) + 2 * CT( INS_MOV_32 );
  else

  if ( t.getType() == IR_Type::FLOAT )
    $cost[0] =
      $cost[2] + $cost[3] + CT( INS_BL_32 ) + CT( INS_CMP_32 ) +
      2 * CT( INS_MOV_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpGEQ( dreg, dreg )", $1 );

  auto &t = effectiveType( *$2->getExp() );

  // Evaluate the arguments.
  auto *lhsReg = $action[2]();
  auto *rhsReg = $action[3]();

  // Generate the operation.
  if ( t.isIntegralType() || t.isPointerType() )
    ARMINSTRUCTIONS.insertCMP( OPER_AL, lhsReg, rhsReg, $1->getExp() );
  else {
    // Compare the floats.
    auto *result = ARMINSTRUCTIONS.CreateRegister( "" );
    ARMINSTRUCTIONS.insertGE_F( OPER_AL, result, lhsReg, rhsReg, $1->getExp() );
    // And now compare it against zero.
    ARMINSTRUCTIONS.insertCMP( OPER_AL, result, 0, $1->getExp() );
  }

  // Insert a 1 or a 0.
  auto *result = ARMINSTRUCTIONS.CreateRegister( "" );
  ARMINSTRUCTIONS.insertMOV( OPER_AL, result, 0, $1->getExp() );
  bool sgn = t.isSignedType() || t.isRealType();
  ARMINSTRUCTIONS.insertMOV( getOper( $1, sgn ), result, 1, $1->getExp() );

  return( result );
};


rel: tpm_BinaryExpGEQ( dreg, const8 )
{
  $cost[0] = $cost[2] + $cost[3] + CT( INS_CMP_32 );
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpGEQ( dreg, const8 )", $1 );

  // Evaluate the arguments.
  auto *lhsReg = $action[2]();
  int rhs = $action[3]().getIntValue();

  // Generate the operation.
  ARMINSTRUCTIONS.insertCMP( OPER_AL, lhsReg, rhs, $1->getExp() );

  return( &effectiveType( *$2->getExp() ) );
};


dreg: tpm_BinaryExpGEQ( ereg, ereg )
{
  $cost[0] = $cost[2] + $cost[3] + 2 * CT( INS_MOV_32 );

if ( effectiveType( *$2->getExp() ).getType() == IR_Type::UNSIGNED_LONG_LONG ||
     effectiveType( *$2->getExp() ).getType() == IR_Type::LONG_LONG )
    $cost[0] += 2 * CT( INS_CMP_32 );
  else

  if ( effectiveType( *$2->getExp() ).getType() == IR_Type::DOUBLE ||
       effectiveType( *$2->getExp() ).getType() == IR_Type::LONG_DOUBLE )
    $cost[0] += CT( INS_BL_32 ) + 5 * CT( INS_MOV_32 ) + CT( INS_CMP_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpGEQ( ereg, ereg )", $1 );

  // Create a result register to represent an integer value of 0 or 1.
  LLIR_Register *result = ARMINSTRUCTIONS.CreateRegister( "" );

  // Evaluate the arguments.
  auto *rhsEReg = $action[3]();
  auto *lhsEReg = $action[2]();

  // Generate the operation.
  if ( effectiveType( *$2->getExp() ).getType() == IR_Type::UNSIGNED_LONG_LONG ||
       effectiveType( *$2->getExp() ).getType() == IR_Type::LONG_LONG ) {
    ARMINSTRUCTIONS.insertCMP(
      OPER_AL, lhsEReg->GetNextChild( lhsEReg->GetFirstChild() ),
      rhsEReg->GetNextChild( rhsEReg->GetFirstChild() ), $1->getExp() );
    // Check the lower children iff the higher children are the same.
    ARMINSTRUCTIONS.insertCMP(
      OPER_EQ, lhsEReg->GetFirstChild(), rhsEReg->GetFirstChild(),
      $1->getExp() );
  } else {
    ARMINSTRUCTIONS.insertGEQ_D(
      OPER_AL, result, lhsEReg, rhsEReg, $1->getExp() );
    // And now compare it against zero.
    ARMINSTRUCTIONS.insertCMP( OPER_AL, result, 0, $1->getExp() );
  }

  // Insert a 1 or a 0.
  ARMINSTRUCTIONS.insertMOV( OPER_AL, result, 0, $1->getExp() );
  auto &t = effectiveType( *$2->getExp() );
  bool sgn = t.isSignedType() || t.isRealType();
  ARMINSTRUCTIONS.insertMOV( getOper( $1, sgn ), result, 1, $1->getExp() );

  return( result );
};


rel: tpm_BinaryExpGEQ( ereg, ereg )
{
  $cost[0] = $cost[2] + $cost[3];

  if ( effectiveType( *$2->getExp() ).getType() == IR_Type::UNSIGNED_LONG_LONG ||
       effectiveType( *$2->getExp() ).getType() == IR_Type::LONG_LONG )
    $cost[0] += 2 * CT( INS_CMP_32 );
  else

  if ( effectiveType( *$2->getExp() ).getType() == IR_Type::DOUBLE ||
       effectiveType( *$2->getExp() ).getType() == IR_Type::LONG_DOUBLE )
    $cost[0] += CT( INS_BL_32 ) + 5 * CT( INS_MOV_32 ) + CT( INS_CMP_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpGEQ( ereg, ereg )", $1 );

  // Evaluate the arguments.
  auto *rhsEReg = $action[3]();
  auto *lhsEReg = $action[2]();

  // Generate the operation.
  if ( effectiveType( *$2->getExp() ).getType() == IR_Type::UNSIGNED_LONG_LONG ||
       effectiveType( *$2->getExp() ).getType() == IR_Type::LONG_LONG ) {
    ARMINSTRUCTIONS.insertCMP(
      OPER_AL, lhsEReg->GetNextChild( lhsEReg->GetFirstChild() ),
      rhsEReg->GetNextChild( rhsEReg->GetFirstChild() ), $1->getExp() );
    // Check the lower children iff the higher children are the same.
    ARMINSTRUCTIONS.insertCMP(
      OPER_EQ, lhsEReg->GetFirstChild(), rhsEReg->GetFirstChild(),
      $1->getExp() );
  } else {
    auto *result = ARMINSTRUCTIONS.CreateRegister( "" );
    ARMINSTRUCTIONS.insertGEQ_D(
      OPER_AL, result, lhsEReg, rhsEReg, $1->getExp() );
    // And now compare it against zero.
    ARMINSTRUCTIONS.insertCMP( OPER_AL, result, 0, $1->getExp() );
  }

  return( &effectiveType( *$2->getExp() ) );
};


rel: tpm_BinaryExpNEQ( dreg, dreg )
{
  $cost[0] = $cost[2] + $cost[3] + CT( INS_CMP_32 );
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpNEQ( dreg, dreg )", $1 );

  // Evaluate the arguments.
  auto *lhsReg = $action[2]();
  auto *rhsReg = $action[3]();

  // Generate the operation.
  ARMINSTRUCTIONS.insertCMP( OPER_AL, lhsReg, rhsReg, $1->getExp() );

  return( &effectiveType( *$2->getExp() ) );
};


dreg: tpm_BinaryExpNEQ( dreg, dreg )
{
  auto &t = effectiveType( *$2->getExp() );

  if ( t.isIntegralType() || t.isPointerType() )
    $cost[0] = $cost[2] + $cost[3] + CT( INS_CMP_32 ) + 2 * CT( INS_MOV_32 );
  else

  if ( t.getType() == IR_Type::FLOAT )
    $cost[0] =
      $cost[2] + $cost[3] + CT( INS_BL_32 ) + CT( INS_CMP_32 ) +
      2 * CT( INS_MOV_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpNEQ( dreg, dreg )", $1 );

  auto &t = effectiveType( *$2->getExp() );

  // Evaluate the arguments.
  LLIR_Register *lhsReg = $action[2]();
  LLIR_Register *rhsReg = $action[3]();

  // Generate the operation.
  if ( t.isIntegralType() || t.isPointerType() )
    ARMINSTRUCTIONS.insertCMP( OPER_AL, lhsReg, rhsReg, $1->getExp() );
  else {
    // Compare the floats.
    auto *result = ARMINSTRUCTIONS.CreateRegister( "" );
    ARMINSTRUCTIONS.insertNE_F( OPER_AL, result, lhsReg, rhsReg, $1->getExp() );
    // And now compare it against zero.
    ARMINSTRUCTIONS.insertCMP( OPER_AL, result, 0, $1->getExp() );
  }

  // Insert a 1 or a 0.
  auto *result = ARMINSTRUCTIONS.CreateRegister( "" );
  ARMINSTRUCTIONS.insertMOV( OPER_AL, result, 0, $1->getExp() );
  bool sgn = t.isSignedType() || t.isRealType();
  ARMINSTRUCTIONS.insertMOV( getOper( $1, sgn ), result, 1, $1->getExp() );

  return( result );
};


rel: tpm_BinaryExpNEQ( dreg, const8 )
{
  $cost[0] = $cost[2] + $cost[3] + CT( INS_CMP_32 );
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpNEQ( dreg, const8 )", $1 );

  // Evaluate the arguments.
  auto *lhsReg = $action[2]();
  int rhs = $action[3]().getIntValue();

  // Generate the operation.
  ARMINSTRUCTIONS.insertCMP( OPER_AL, lhsReg, rhs, $1->getExp() );

  return( &effectiveType( *$2->getExp() ) );
};


dreg: tpm_BinaryExpNEQ( ereg, ereg )
{
  $cost[0] = $cost[2] + $cost[3] + 2 * CT( INS_MOV_32 );

  if ( effectiveType( *$2->getExp() ).getType() == IR_Type::UNSIGNED_LONG_LONG ||
       effectiveType( *$2->getExp() ).getType() == IR_Type::LONG_LONG )
    $cost[0] += 2 * CT( INS_TEQ_32 );
  else

  if ( effectiveType( *$2->getExp() ).getType() == IR_Type::DOUBLE ||
       effectiveType( *$2->getExp() ).getType() == IR_Type::LONG_DOUBLE )
    $cost[0] += CT( INS_BL_32 ) + 5 * CT( INS_MOV_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpNEQ( ereg, ereg )", $1 );

  // Evaluate the arguments.
  auto *rhsEReg = $action[3]();
  auto *lhsEReg = $action[2]();

  // Create a result register to represent an integer value of 0 or 1.
  auto *result = ARMINSTRUCTIONS.CreateRegister( "" );

  // Generate the operation.
  if ( effectiveType( *$2->getExp() ).getType() == IR_Type::UNSIGNED_LONG_LONG ||
       effectiveType( *$2->getExp() ).getType() == IR_Type::LONG_LONG ) {
    ARMINSTRUCTIONS.insertTEQ(
      lhsEReg->GetFirstChild(), rhsEReg->GetFirstChild(), $1->getExp() );
    ARMINSTRUCTIONS.insertTEQ(
      OPER_EQ, lhsEReg->GetNextChild( lhsEReg->GetFirstChild() ),
      rhsEReg->GetNextChild( rhsEReg->GetFirstChild() ), $1->getExp() );
  } else {
    ARMINSTRUCTIONS.insertNEQ_D(
      OPER_AL, result, lhsEReg, rhsEReg, $1->getExp() );
    // Compare result of softfloat routine against zero.
    ARMINSTRUCTIONS.insertCMP( OPER_AL, result, 0, $1->getExp() );
  }

  // Insert a 1 or a 0.
  ARMINSTRUCTIONS.insertMOV( OPER_AL, result, 0, $1->getExp() );
  auto &t = effectiveType( *$2->getExp() );
  bool sgn = t.isSignedType() || t.isRealType();
  ARMINSTRUCTIONS.insertMOV( getOper( $1, sgn ), result, 1, $1->getExp() );

  return( result );
};


rel: tpm_BinaryExpNEQ( ereg, ereg )
{
  $cost[0] = $cost[2] + $cost[3];

  if ( effectiveType( *$2->getExp() ).getType() == IR_Type::UNSIGNED_LONG_LONG ||
       effectiveType( *$2->getExp() ).getType() == IR_Type::LONG_LONG )
    $cost[0] += 2 * CT( INS_TEQ_32 );
  else

  if ( effectiveType( *$2->getExp() ).getType() == IR_Type::DOUBLE ||
       effectiveType( *$2->getExp() ).getType() == IR_Type::LONG_DOUBLE )
    $cost[0] += CT( INS_BL_32 ) + 5 * CT( INS_MOV_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "rel: tpm_BinaryExpNEQ( ereg, ereg )", $1 );

  // Evaluate the arguments.
  auto *rhsEReg = $action[3]();
  auto *lhsEReg = $action[2]();

  // Generate the operation.
  if ( effectiveType( *$2->getExp() ).getType() == IR_Type::UNSIGNED_LONG_LONG ||
       effectiveType( *$2->getExp() ).getType() == IR_Type::LONG_LONG ) {
    ARMINSTRUCTIONS.insertTEQ(
      lhsEReg->GetFirstChild(), rhsEReg->GetFirstChild(), $1->getExp() );
    ARMINSTRUCTIONS.insertTEQ(
      OPER_EQ, lhsEReg->GetNextChild( lhsEReg->GetFirstChild() ),
      rhsEReg->GetNextChild( rhsEReg->GetFirstChild() ), $1->getExp() );
  } else {
    auto *result = ARMINSTRUCTIONS.CreateRegister( "" );
    ARMINSTRUCTIONS.insertNEQ_D(
      OPER_AL, result, lhsEReg, rhsEReg, $1->getExp() );
    // Compare result of softfloat routine against zero.
    ARMINSTRUCTIONS.insertCMP( OPER_AL, result, 0, $1->getExp() );
  }

  return( &effectiveType( *$2->getExp() ) );
};


rel: dreg
{
  $cost[0] = CT( INS_CMP_32 ) + $cost[1];
}
=
{
  DEBUG_RULE_ACTION( "rel: dreg", $1 );

  auto *r = $action[1]();

  // Generate the operation.
  ARMINSTRUCTIONS.insertCMP( OPER_AL, r, 0, $1->getExp() );

  return( &effectiveType( *$1->getExp() ) );
};


###############################################################################
#
#
# Miscellaneous Expressions
#
#
###############################################################################

dreg: tpm_BinaryExpCOMMA( any_reg, dreg )
{
  // This rule does not do anything by itself.
  $cost[0] = $cost[2] + $cost[3];
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpCOMMA( any_reg, dreg )", $1 );

  // Evaluate both expressions, always starting with the left side.
  $action[2]();
  return( $action[3]() );
};


ereg: tpm_BinaryExpCOMMA( any_reg, ereg )
{
  // This rule does not do anything by itself.
  $cost[0] = $cost[2] + $cost[3];
}
=
{
  DEBUG_RULE_ACTION( "ereg: tpm_BinaryExpCOMMA( any_reg, ereg )", $1 );

  // Evaluate both expressions, always starting with the left side.
  $action[2]();
  return( $action[3]() );
};


areg: tpm_BinaryExpCOMMA( any_reg, areg )
{
  // This rule does not do anything by itself.
  $cost[0] = $cost[2] + $cost[3];
}
=
{
  DEBUG_RULE_ACTION( "areg: tpm_BinaryExpCOMMA( any_reg, areg )", $1 );

  // Evaluate both expressions, always starting with the left side.
  $action[2]();
  return( $action[3]() );
};


areg: tpm_CondExp( dreg, any_reg, any_reg )
{
  $cost[0] =
    $cost[2] + $cost[3] + $cost[4] + CT( INS_CMP_32 ) + 3 * CT(INS_B_32 ) +
    2 * CT( INS_MOV_32 );
}
=
{
  DEBUG_RULE_ACTION( "areg: tpm_CondExp( dreg, any_reg, any_reg )", $1 );

  // Create Labels for the new Basic Blocks
  string labelFalse = LLIR::getUniqueLabel();
  string labelAfter = LLIR::getUniqueLabel();

  auto *result = ARMINSTRUCTIONS.CreateRegister( "" );

  auto *condReg = $action[2]();

  // Check the value of the condition and branch accordingly.
  ARMINSTRUCTIONS.insertCMP( OPER_AL, condReg, 0, $1->getExp() );
  ARMINSTRUCTIONS.insertB( OPER_EQ, labelFalse );

  // Create basic block for LHS argument of ':'.
  beginNewLLIRBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );

  // Create code for LHS of ':'.
  auto *regTrue = $action[3]();

  if ( $0->getExp()->getType().getType() != IR_Type::VOID )
    ARMINSTRUCTIONS.insertMOV( result, regTrue, $1->getExp() );
  ARMINSTRUCTIONS.insertB( OPER_AL, labelAfter, $1->getExp() );

  // Create basic block for RHS argument of ':'.
  beginNewLLIRBasicBlock(
    labelFalse.c_str(), *$1->getExp()->getStmt().getBasicBlock() );

  // Create code for RHS of ':'.
  auto *regFalse = $action[4]();

  if ( $0->getExp()->getType().getType() != IR_Type::VOID )
    ARMINSTRUCTIONS.insertMOV( result, regFalse, $1->getExp() );

  // Create basic block for the next instructions.
  beginNewLLIRBasicBlock(
    labelAfter.c_str(), *$1->getExp()->getStmt().getBasicBlock() );

  return( result );
};


dreg: tpm_CondExp( dreg, any_reg, any_reg )
{
  $cost[0] =
    $cost[2] + $cost[3] + $cost[4] + CT( INS_CMP_32 ) + 3 * CT(INS_B_32 ) +
    2 * CT( INS_MOV_32 );
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_CondExp( dreg, any_reg, any_reg )", $1 );

  // Create Labels for the new Basic Blocks
  string labelFalse = LLIR::getUniqueLabel();
  string labelAfter = LLIR::getUniqueLabel();

  auto *result = ARMINSTRUCTIONS.CreateRegister( "" );

  auto *condReg = $action[2]();

  // Check the value of the condition and branch accordingly.
  ARMINSTRUCTIONS.insertCMP( OPER_AL, condReg, 0, $1->getExp() );
  ARMINSTRUCTIONS.insertB( OPER_EQ, labelFalse );

  // Create basic block for LHS argument of ':'.
  beginNewLLIRBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );

  // Create code for LHS of ':'.
  auto *regTrue = $action[3]();

  if ( $0->getExp()->getType().getType() != IR_Type::VOID )
    ARMINSTRUCTIONS.insertMOV( result, regTrue, $1->getExp() );
  ARMINSTRUCTIONS.insertB( OPER_AL, labelAfter, $1->getExp() );

  // Create basic block for RHS argument of ':'.
  beginNewLLIRBasicBlock(
    labelFalse.c_str(), *$1->getExp()->getStmt().getBasicBlock() );

  // Create code for RHS of ':'.
  auto *regFalse = $action[4]();

  if ( $0->getExp()->getType().getType() != IR_Type::VOID )
    ARMINSTRUCTIONS.insertMOV( result, regFalse, $1->getExp() );

  // Create basic block for the next instructions.
  beginNewLLIRBasicBlock(
    labelAfter.c_str(), *$1->getExp()->getStmt().getBasicBlock() );

  return( result );
};


dreg: tpm_BinaryExpSHL( dreg, dreg )
{
  $cost[0] = $cost[2] + $cost[3] + CT( INS_MOV_32 );
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpSHL( dreg, dreg )", $1 );

  auto *result = ARMINSTRUCTIONS.CreateRegister( "" );

  // Evaluate the arguments.
  auto *lhsReg = $action[2]();
  auto *rhsReg = $action[3]();

  // Generate the operation.
  ARMINSTRUCTIONS.insertMOV(
    OPER_AL, "", result, lhsReg, OPER_LSL, rhsReg, $1->getExp() );

  return( result );
};


ereg: tpm_BinaryExpSHL( ereg, dreg )
{
  if ( ( $0->getExp()->getType().getType() == IR_Type::LONG_LONG ) ||
       ( $0->getExp()->getType().getType() == IR_Type::UNSIGNED_LONG_LONG ) )
    $cost[0] = $cost[2] + $cost[3] + CT( INS_BL_32 ) + 5 * CT( INS_MOV_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "ereg: tpm_BinaryExpSHL( ereg, dreg )", $1 );

  auto *eresult = ARMINSTRUCTIONS.CreateERegister( "" );

  // Evaluate the arguments.
  auto *lhsEReg = $action[2]();
  auto *rhsReg = $action[3]();

  // Generate the operation.
  ARMINSTRUCTIONS.insertSHL_LL( OPER_AL, eresult, lhsEReg, rhsReg, $1->getExp() );

  return( eresult );
};


dreg: tpm_BinaryExpSHR( dreg, dreg )
{
  $cost[0] = $cost[2] + $cost[3] + CT( INS_MOV_32 );
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpSHR( dreg, dreg )", $1 );

  auto *result = ARMINSTRUCTIONS.CreateRegister( "" );

  // Evaluate the arguments.
  auto *lhsReg = $action[2]();
  auto *rhsReg = $action[3]();

  // Generate the operation.
  // Check for signed/unsigned to generate arithmetic or logical shift.
  if ( $0->getExp()->getType().isUnsignedType() )
    ARMINSTRUCTIONS.insertMOV(
      OPER_AL, "", result, lhsReg, OPER_LSR, rhsReg, $1->getExp() );
  else
    ARMINSTRUCTIONS.insertMOV(
      OPER_AL, "", result, lhsReg, OPER_ASR, rhsReg, $1->getExp() );

  return( result );
};


ereg: tpm_BinaryExpSHR( ereg, dreg )
{
  $cost[0] = $cost[2] + $cost[3];

  if ( ( $0->getExp()->getType().getType() == IR_Type::LONG_LONG ) ||
       ( $0->getExp()->getType().getType() == IR_Type::UNSIGNED_LONG_LONG ) )
    $cost[0] = $cost[2] + $cost[3] + CT( INS_BL_32 ) + 5 * CT( INS_MOV_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "ereg: tpm_BinaryExpSHR( ereg, dreg )", $1 );

  auto *eresult = ARMINSTRUCTIONS.CreateERegister( "" );

  // Evaluate the arguments.
  auto *lhsEReg = $action[2]();
  auto *rhsReg = $action[3]();

  // Generate the operation.
  if ( $0->getExp()->getType().getType() == IR_Type::LONG_LONG )
    ARMINSTRUCTIONS.insertSHR_LL(
      OPER_AL, eresult, lhsEReg, rhsReg, $1->getExp() );
  else
    ARMINSTRUCTIONS.insertSHR_ULL(
      OPER_AL, eresult, lhsEReg, rhsReg, $1->getExp() );

  return( eresult );
};


###############################################################################
#
#
# Unary expressions
#
#
###############################################################################

#######
# PLUS
#######

const8: tpm_UnaryExpPLUS( const8 )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "const8: tpm_UnaryExpPLUS( const8 )", $1 );

  // Generate operand trees.
  return( $action[2]() );
};


dreg: tpm_UnaryExpPLUS( dreg )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_UnaryExpPLUS( dreg )", $1 );

  // Generate operand trees.
  return( $action[2]() );
};


ereg: tpm_UnaryExpPLUS( ereg )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "ereg: tpm_UnaryExpPLUS( ereg )", $1 );

  // Generate operand trees.
  return( $action[2]() );
};


#######
# MINUS
#######

const8: tpm_UnaryExpMINUS( const8 )
{
  if ( isNegConst8( $2->getExp() ) )
    $cost[0] = $cost[2];
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "const8: tpm_UnaryExpMINUS( const8 )", $1 );

  // Generate operand trees.
  return( -$action[2]() );
};


dreg: tpm_UnaryExpMINUS( dreg )
{
  if ( $0->getExp()->getType().isIntegralType() )
    $cost[0] = $cost[2] + CT( INS_RSB_32 );
  else
    $cost[0] = $cost[2] + CT( INS_EOR_32 );
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_UnaryExpMINUS( dreg )", $1 );

  // Generate operand trees.
  auto *reg = $action[2]();

  // Create a new register.
  auto *res = ARMINSTRUCTIONS.CreateRegister( "" );

  // Check if it is a float.
  if ( $0->getExp()->getType().isIntegralType() )
   // Emit subtraction from zero.
   ARMINSTRUCTIONS.insertRSB( res, reg, 0, $1->getExp() );
  else {
    // Negate the 32bit float value by flipping the first bit.
    const uint32_t val = ( 1 << 31 );
    ARMINSTRUCTIONS.insertEOR( res, reg, val, $1->getExp() );
  }

  return( res );
};


constantd: tpm_UnaryExpMINUS( constantd )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "constantd: tpm_UnaryExpMINUS( constantd )", $1 );

  return( -$action[2]() );
};


constantll: tpm_UnaryExpMINUS( constantll )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "constantll: tpm_UnaryExpMINUS( constantll )", $1 );

  return( -$action[2]() );
};


ereg: tpm_UnaryExpMINUS( ereg )
{
  if ( $0->getExp()->getType().isIntegralType() &&
       ( $0->getExp()->getType().bitSize() > 32 ) )
    $cost[0] =
      $cost[2] + CT( INS_MVN_32 ) + 2 * CT( INS_EOR_32 ) + CT( INS_ADD_32 ) +
      CT( INS_ADC_32 );
  else

  if ( ( $0->getExp()->getType().getType() == IR_Type::DOUBLE ) ||
       ( $0->getExp()->getType().getType() == IR_Type::LONG_DOUBLE ) )
    $cost[0] = $cost[2] + CT( INS_EOR_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "ereg: tpm_UnaryExpMINUS( ereg )", $1 );

  // Generate operand trees.
  auto *ereg = $action[2]();

  // Check if it is an integer > 32 bit.
  if ( $0->getExp()->getType().isIntegralType() &&
       ( $0->getExp()->getType().bitSize() > 32 ) ) {
   // Negate the long long integer value by flipping all bits of both registers
   // via eor with 0xffffffff, adding 1 to the LS/first/lower register while
   // updating the carry flag and adding the MS/second/higher register and carry
   // bit.
   auto *reg = ARMINSTRUCTIONS.CreateRegister( "" );

   ARMINSTRUCTIONS.insertMVN( OPER_AL, reg, 0, $1->getExp() );
   // flip all bits of both child registers via eor 0xffffffff.
   ARMINSTRUCTIONS.insertEOR(
    ereg->GetFirstChild(), ereg->GetFirstChild(), reg, $1->getExp() );
   ARMINSTRUCTIONS.insertEOR(
    ereg->GetNextChild( ereg->GetFirstChild() ),
    ereg->GetNextChild( ereg->GetFirstChild() ), reg, $1->getExp() );

   // Add 1 to the LS/first/lower child and set S-bit to update condition flags.
   ARMINSTRUCTIONS.insertADD(
    OPER_AL, OPER_SBIT, ereg->GetFirstChild(), ereg->GetFirstChild(), 1,
    $1->getExp() );

   // Add MS/second/higher child and carry bit.
   ARMINSTRUCTIONS.insertADC(
    ereg->GetNextChild( ereg->GetFirstChild() ),
    ereg->GetNextChild( ereg->GetFirstChild() ), 0, $1->getExp() );
  } else {
    // Negate the 64bit double value by flipping the first bit of the most
    // significant register, which is the first/lower register for doubles.
    const uint32_t val = ( 1 << 31 );
    ARMINSTRUCTIONS.insertEOR(
      ereg->GetFirstChild(), ereg->GetFirstChild(), val, $1->getExp() );
  }

  return( ereg );
};


##########
# INC,DECR
##########

dreg: tpm_UnaryExpPREINC( dreg )
{
  if ( $0->getExp()->getType().isIntegralType() ) {
    $cost[0] = $cost[2] + CT( INS_ADD_32 );
    if ( $0->getExp()->getType().bitSize() != 32 )
      $cost[0] += Cast::truncateCosts( $0->getExp()->getType() );
  } else

  if ( $0->getExp()->getType().getType() == IR_Type::FLOAT )
    $cost[0] = $cost[2] + CT( INS_BL_32 ) + 3 * CT( INS_MOV_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_UnaryExpPREINC( dreg )", $1 );

  // Generate operand trees.
  auto *reg = $action[2]();

  // Generate the operation.
  if ( $0->getExp()->getType().isIntegralType() ) {
    ARMINSTRUCTIONS.insertADD( reg, reg, 1, $1->getExp() );

    // Truncate if needed.
    if ( $0->getExp()->getType().bitSize() != 32 )
      Cast::truncate( reg, reg, $0->getExp()->getType(), $1->getExp() );
  } else {
    // Move a float 1 to a new register.
    auto *reg1 = ARMINSTRUCTIONS.CreateRegister( "" );

    Float f( 1.0f );
    ARMINSTRUCTIONS.insertMOV_ORR(
      reg1, f.getValue().getComposed(), $1->getExp() );

    // And add them.
    ARMINSTRUCTIONS.insertADD_F( OPER_AL, reg, reg, reg1, $1->getExp() );
  }

  return( reg );
};


ereg: tpm_UnaryExpPREINC( ereg )
{
  $cost[0] = $cost[2];

  if ( ( $0->getExp()->getType().getType() == IR_Type::LONG_LONG ) ||
       ( $0->getExp()->getType().getType() == IR_Type::UNSIGNED_LONG_LONG ) )
    $cost[0] += CT( INS_ADD_32 ) + CT( INS_ADC_32 ) + 2 * CT( INS_MOV_32 );
  else

  if ( ( $0->getExp()->getType().getType() == IR_Type::DOUBLE ) ||
       ( $0->getExp()->getType().getType() == IR_Type::LONG_DOUBLE ) )
    $cost[0] += CT( INS_BL_32 ) + 8 * CT( INS_MOV_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "ereg: tpm_UnaryExpPREINC( ereg )", $1 );

  // Create registers.
  auto *ereg = $action[2]();
  auto *one = ARMINSTRUCTIONS.CreateERegister( "" );

  if ( ( $0->getExp()->getType().getType() == IR_Type::LONG_LONG ) ||
       ( $0->getExp()->getType().getType() == IR_Type::UNSIGNED_LONG_LONG ) ) {
    // Move a long long int 1 to the new register.
    ARMINSTRUCTIONS.insertMOV(
      one->GetNextChild( one->GetFirstChild() ), 0, $1->getExp() );
    ARMINSTRUCTIONS.insertMOV( one->GetFirstChild(), 1, $1->getExp() );

    // Add them.
    ARMINSTRUCTIONS.insertADD_LL( OPER_AL, ereg, ereg, one, $1->getExp() );
  } else {
    // Move a double 1 to the new register. For double, the first child holds
    // high, the second low.
    unsigned long low = 0x00000000;
    unsigned long high = 0x3FF00000;

    ARMINSTRUCTIONS.insertMOV_ORR(
      one->GetNextChild( one->GetFirstChild() ), low, $1->getExp() );
    ARMINSTRUCTIONS.insertMOV_ORR( one->GetFirstChild(), high, $1->getExp() );

    // And add them.
    ARMINSTRUCTIONS.insertADD_D( OPER_AL, ereg, ereg, one, $1->getExp() );
  }

  return( ereg );
};


dreg: tpm_UnaryExpPREDEC( dreg )
{
  if ( $0->getExp()->getType().isIntegralType() ) {
    $cost[0] = $cost[2] + CT( INS_SUB_32 );
    if ( $0->getExp()->getType().bitSize() != 32 )
      $cost[0] += Cast::truncateCosts( $0->getExp()->getType() );
  } else

  if ( $0->getExp()->getType().getType() == IR_Type::FLOAT )
    $cost[0] = $cost[2] + CT( INS_BL_32 ) + 3 * CT( INS_MOV_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_UnaryExpPREDEC( dreg )", $1 );

  // Generate operand trees.
  auto *reg = $action[2]();

  // Generate the operation.
  if ( $0->getExp()->getType().isIntegralType() ) {
    ARMINSTRUCTIONS.insertSUB( reg, reg, 1, $1->getExp() );

    // Truncate if needed.
    if ( $0->getExp()->getType().bitSize() != 32 )
      Cast::truncate( reg, reg, $0->getExp()->getType(), $1->getExp() );
  } else {
    // Move a float 1 to a new register.
    auto *reg1 = ARMINSTRUCTIONS.CreateRegister( "" );

    Float f( 1.0f );
    ARMINSTRUCTIONS.insertMOV_ORR(
      reg1, f.getValue().getComposed(), $1->getExp() );

    // And add them.
    ARMINSTRUCTIONS.insertSUB_F( OPER_AL, reg, reg, reg1, $1->getExp() );
  }

  return( reg );
};


ereg: tpm_UnaryExpPREDEC( ereg )
{
  $cost[0] = $cost[2];

  if ( ( $0->getExp()->getType().getType() == IR_Type::LONG_LONG ) ||
       ( $0->getExp()->getType().getType() == IR_Type::UNSIGNED_LONG_LONG ) )
    $cost[0] += CT( INS_SUB_32 ) + CT( INS_SBC_32 ) + 2 * CT( INS_MOV_32 );
  else

  if ( ( $0->getExp()->getType().getType() == IR_Type::DOUBLE ) ||
       ( $0->getExp()->getType().getType() == IR_Type::LONG_DOUBLE ) )
    $cost[0] += CT( INS_BL_32 ) + 8 * CT( INS_MOV_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "ereg: tpm_UnaryExpPREDEC( ereg )", $1 );

  // Create registers.
  auto *ereg = $action[2]();
  auto *one = ARMINSTRUCTIONS.CreateERegister( "" );

  if ( ( $0->getExp()->getType().getType() == IR_Type::LONG_LONG ) ||
       ( $0->getExp()->getType().getType() == IR_Type::UNSIGNED_LONG_LONG ) ) {
    // Move a long long int 1 to the new register.
    ARMINSTRUCTIONS.insertMOV(
      one->GetNextChild( one->GetFirstChild() ), 0, $1->getExp() );
    ARMINSTRUCTIONS.insertMOV( one->GetFirstChild(), 1, $1->getExp() );

    // Subtract 1.
    ARMINSTRUCTIONS.insertSUB_LL( OPER_AL, ereg, ereg, one, $1->getExp() );
  } else {
    // Move a double 1 to the new register. For double, the first child holds
    // high, the second low.
    unsigned long low = 0x00000000;
    unsigned long high = 0x3FF00000;

    ARMINSTRUCTIONS.insertMOV_ORR(
      one->GetNextChild( one->GetFirstChild() ), low, $1->getExp() );
    ARMINSTRUCTIONS.insertMOV_ORR( one->GetFirstChild(), high, $1->getExp() );

    // Subtract 1.
    ARMINSTRUCTIONS.insertSUB_D( OPER_AL, ereg, ereg, one, $1->getExp() );
  }

  return( ereg );
};


dreg: tpm_UnaryExpPOSTINC( dreg )
{
  if ( $0->getExp()->getType().isIntegralType() ) {
    $cost[0] = $cost[2] + CT( INS_MOV_32 ) + CT( INS_ADD_32 );
    if ( $0->getExp()->getType().bitSize() != 32 )
      $cost[0] += Cast::truncateCosts( $0->getExp()->getType() );
  } else

  if ( $0->getExp()->getType().getType() == IR_Type::FLOAT )
    $cost[0] = $cost[2] + CT( INS_BL_32 ) + 3 * CT( INS_MOV_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_UnaryExpPOSTINC( dreg )", $1 );

  auto *result = ARMINSTRUCTIONS.CreateRegister( "" );

  // Generate operand trees.
  auto *reg = $action[2]();

  // Generate the operation. Copy operand value.
  ARMINSTRUCTIONS.insertMOV( result, reg, $1->getExp() );

  if ( $0->getExp()->getType().isIntegralType() ) {
    // Emit increment instruction on operand.
    ARMINSTRUCTIONS.insertADD( reg, reg, 1, $1->getExp() );

    // Truncate if needed.
    if ( $0->getExp()->getType().bitSize() != 32 )
      Cast::truncate( reg, reg, $0->getExp()->getType(), $1->getExp() );
  } else {
    // Move a float 1 to a new register.
    auto *reg1 = ARMINSTRUCTIONS.CreateRegister( "" );

    Float f( 1.0f );
    ARMINSTRUCTIONS.insertMOV_ORR(
      reg1, f.getValue().getComposed(), $1->getExp() );

    // And add them.
    ARMINSTRUCTIONS.insertADD_F( OPER_AL, reg, reg, reg1, $1->getExp() );
  }

  return( result );
};


ereg: tpm_UnaryExpPOSTINC( ereg )
{
  $cost[0] = $cost[2];

  if ( ( $0->getExp()->getType().getType() == IR_Type::LONG_LONG ) ||
       ( $0->getExp()->getType().getType() == IR_Type::UNSIGNED_LONG_LONG ) )
    $cost[0] += CT( INS_ADD_32 ) + CT( INS_ADC_32 ) + 2 * CT( INS_MOV_32 );
  else

  if ( ( $0->getExp()->getType().getType() == IR_Type::DOUBLE ) ||
       ( $0->getExp()->getType().getType() == IR_Type::LONG_DOUBLE ) )
    $cost[0] += CT( INS_BL_32 ) + 8 * CT( INS_MOV_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "ereg: tpm_UnaryExpPOSTINC( ereg )", $1 );

  // Create registers.
  auto *eresult = ARMINSTRUCTIONS.CreateERegister( "" );
  auto *ereg = $action[2]();
  auto *one = ARMINSTRUCTIONS.CreateERegister( "" );

  // Copy operand value.
  ARMINSTRUCTIONS.insertMOV(
    eresult->GetFirstChild(), ereg->GetFirstChild(), $1->getExp() );
  ARMINSTRUCTIONS.insertMOV(
    eresult->GetNextChild( eresult->GetFirstChild() ),
    ereg->GetNextChild( ereg->GetFirstChild() ), $1->getExp() );

  if ( ( $0->getExp()->getType().getType() == IR_Type::LONG_LONG ) ||
       ( $0->getExp()->getType().getType() == IR_Type::UNSIGNED_LONG_LONG ) ) {
    // Move a long long int 1 to the new register.
    ARMINSTRUCTIONS.insertMOV(
      one->GetNextChild( one->GetFirstChild() ), 0, $1->getExp() );
    ARMINSTRUCTIONS.insertMOV( one->GetFirstChild(), 1, $1->getExp() );

    // Increment operand.
    ARMINSTRUCTIONS.insertADD_LL( OPER_AL, ereg, ereg, one, $1->getExp() );
  } else {
    // Move a double 1 to the new register. For double, the first child holds
    // high, the second low.
    unsigned long low = 0x00000000;
    unsigned long high = 0x3FF00000;

    ARMINSTRUCTIONS.insertMOV_ORR(
      one->GetNextChild( one->GetFirstChild() ), low, $1->getExp() );
    ARMINSTRUCTIONS.insertMOV_ORR( one->GetFirstChild(), high, $1->getExp() );

    // And add them.
    ARMINSTRUCTIONS.insertADD_D( OPER_AL, ereg, ereg, one, $1->getExp() );
  }

  return( eresult );
};


dreg: tpm_UnaryExpPOSTDEC( dreg )
{

  if ( $0->getExp()->getType().isIntegralType() ) {
    $cost[0] = $cost[2] + CT( INS_MOV_32 ) + CT( INS_SUB_32 );
    if ( $0->getExp()->getType().bitSize() != 32 )
      $cost[0] += Cast::truncateCosts( $0->getExp()->getType() );
  } else

  if ( $0->getExp()->getType().getType() == IR_Type::FLOAT )
    $cost[0] = $cost[2] + CT( INS_BL_32 ) + 3 * CT( INS_MOV_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_UnaryExpPOSTDEC( dreg )", $1 );

  auto *result = ARMINSTRUCTIONS.CreateRegister( "" );

  // Generate operand trees.
  auto *reg = $action[2]();

  // Generate the operation. Copy operand value.
  ARMINSTRUCTIONS.insertMOV( result, reg, $1->getExp() );

  if ( $0->getExp()->getType().isIntegralType() ) {
    // Emit decrement instruction on operand.
    ARMINSTRUCTIONS.insertSUB( reg, reg, 1, $1->getExp() );

    // Truncate if needed.
    if ( $0->getExp()->getType().bitSize() != 32 )
      Cast::truncate( reg, reg, $0->getExp()->getType(), $1->getExp() );
  } else {
    // Move a float 1 to a new register.
    auto *reg1 = ARMINSTRUCTIONS.CreateRegister( "" );

    Float f( 1.0f );
    ARMINSTRUCTIONS.insertMOV_ORR(
      reg1, f.getValue().getComposed(), $1->getExp() );

    // And add them.
    ARMINSTRUCTIONS.insertSUB_F( OPER_AL, reg, reg, reg1, $1->getExp() );
  }

  return( result );
};


ereg: tpm_UnaryExpPOSTDEC( ereg )
{
  $cost[0] = $cost[2];

  if ( ( $0->getExp()->getType().getType() == IR_Type::LONG_LONG ) ||
       ( $0->getExp()->getType().getType() == IR_Type::UNSIGNED_LONG_LONG ) )
    $cost[0] += CT( INS_SUB_32 ) + CT( INS_SBC_32 ) + 2 * CT( INS_MOV_32 );
  else

  if ( ( $0->getExp()->getType().getType() == IR_Type::DOUBLE ) ||
       ( $0->getExp()->getType().getType() == IR_Type::LONG_DOUBLE ) )
    $cost[0] += CT( INS_BL_32 ) + 8 * CT( INS_MOV_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "ereg: tpm_UnaryExpPOSTDEC( ereg )", $1 );

  // Create registers.
  auto *eresult = ARMINSTRUCTIONS.CreateERegister( "" );
  auto *ereg = $action[2]();
  auto *one = ARMINSTRUCTIONS.CreateERegister( "" );

  // Copy operand value.
  ARMINSTRUCTIONS.insertMOV(
    eresult->GetFirstChild(), ereg->GetFirstChild(), $1->getExp() );
  ARMINSTRUCTIONS.insertMOV(
    eresult->GetNextChild( eresult->GetFirstChild() ),
    ereg->GetNextChild( ereg->GetFirstChild() ), $1->getExp() );

  if ( ( $0->getExp()->getType().getType() == IR_Type::LONG_LONG ) ||
       ( $0->getExp()->getType().getType() == IR_Type::UNSIGNED_LONG_LONG ) ) {
    // Move a long long int 1 to the new register.
    ARMINSTRUCTIONS.insertMOV(
      one->GetNextChild( one->GetFirstChild() ), 0, $1->getExp() );
    ARMINSTRUCTIONS.insertMOV( one->GetFirstChild(), 1, $1->getExp() );

    // Decrement operand.
    ARMINSTRUCTIONS.insertSUB_LL( OPER_AL, ereg, ereg, one, $1->getExp() );
  } else {
    // Move a double 1 to the new register. For double, the first child holds
    // high, the second low.
    unsigned long low = 0x00000000;
    unsigned long high = 0x3FF00000;

    ARMINSTRUCTIONS.insertMOV_ORR(
      one->GetNextChild( one->GetFirstChild() ), low, $1->getExp() );
    ARMINSTRUCTIONS.insertMOV_ORR( one->GetFirstChild(), high, $1->getExp() );

    // Decrement operand.
    ARMINSTRUCTIONS.insertSUB_D( OPER_AL, ereg, ereg, one, $1->getExp() );
  }

  return( eresult );
};


dreg: tpm_UnaryExpBITNOT( dreg )
{
  $cost[0] = $cost[2] + CT( INS_MVN_32 );
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_UnaryExpBITNOT( dreg )", $1 );

  auto *result = ARMINSTRUCTIONS.CreateRegister( "" );

  // Generate operand trees.
  auto *reg = $action[2]();

  ARMINSTRUCTIONS.insertMVN( OPER_AL, result, reg, $1->getExp() );

  return( result );
};


dreg: tpm_UnaryExpBITNOT( const8 )
{
  $cost[0] = $cost[2] + CT( INS_MVN_32 );
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_UnaryExpBITNOT( const8 )", $1 );

  auto *result = ARMINSTRUCTIONS.CreateRegister( "" );

  // Generate operand trees.
  int rhs = $action[2]().getIntValue();

  ARMINSTRUCTIONS.insertMVN( OPER_AL, result, rhs, $1->getExp() );

  return( result );
};


#####################################################################
#
#
# If-then-else statements
#
#
#####################################################################

stmt: tpm_IfStmt( rel )
{
  $cost[0] = $cost[2] + CT( INS_B_32 );
}
=
{
  DEBUG_RULE_ACTION( "stmt: tpm_IfStmt( rel )", $1 );

  // Run the action of the if-condition in order to set the flags, which are
  // then checked with the OPER code.
  auto *t = $action[2]();
  bool sgn = t->isSignedType() || t->isRealType();

  stringstream ss;
  ss << dynamic_cast<IR_IfStmt *>( $1->getStmt() )->getContinueBasicBlock();
  string bb = ARMCODESEL->getBlockLabel( ss.str() );

  // Get the right condition for this TPM tree element.
  string condition = getInvOper( $2, sgn );

  // Generate the branch operation.
  ARMINSTRUCTIONS.insertB(
    condition, bb, $1->getExp(), InstructionFactory::IF_STMT );
};


stmt: tpm_IfElseStmt( rel )
{
  $cost[0] = $cost[2] + CT( INS_B_32 );
}
=
{
  DEBUG_RULE_ACTION( "stmt: tpm_IfElseStmt( rel )", $1 );

  // Run the action of the if-condition in order to set the flags, which are
  // then checked with the OPER code.
  auto *t = $action[2]();
  bool sgn = t->isSignedType() || t->isRealType();

  stringstream ss;
  ss << dynamic_cast<IR_IfElseStmt *>( $1->getStmt() )->getFalseBasicBlock();
  string bb = ARMCODESEL->getBlockLabel( ss.str() );

  // Get the right condition for this TPM tree element.
  string condition = getInvOper( $2, sgn );

  // Generate the branch operation.
  ARMINSTRUCTIONS.insertB(
    condition, bb, $1->getExp(), InstructionFactory::IFELSE_STMT );
};


#####################################################################
#
#
# For-loop
#
#
#####################################################################

stmt: tpm_ForStmt( rel )
{
  $cost[0] = $cost[2] + CT( INS_B_32 );
}
=
{
  DEBUG_RULE_ACTION( "stmt: tpm_ForStmt( rel )", $1 );

  // Run the action of the loop condition in order to set the flags, which are
  // then checked with the OPER code.
  auto *t = $action[2]();
  bool sgn = t->isSignedType() || t->isRealType();

  // Generate new basic block label.
  stringstream tmpStr, ss;
  tmpStr << $1->getStmt()->getBasicBlock();
  string bb = ARMCODESEL->getBlockLabel( tmpStr.str() );

  // Generate new basic block for loop condition, if not yet existing.
  if ( !ARMCODESEL->containsBB( bb ) )
    beginNewLLIRBasicBlock( bb.c_str(), *$1->getStmt()->getBasicBlock() );
  ARMCODESEL->setCurrentInstruction( nullptr );

  auto *loop = dynamic_cast<IR_LoopStmt *>( $1->getStmt()->getParent() );
  ss << loop->getFalseBasicBlock();
  bb = ARMCODESEL->getBlockLabel( ss.str() );

  // Get the right condition for this TPM tree element.
  string condition = getInvOper( $2, sgn );

  // Generate the branch operation.
  ARMINSTRUCTIONS.insertB(
    condition, bb, $1->getExp(), InstructionFactory::FOR_STMT );
  ARMCODESEL->getLastLLIRBB()->AddPragma(
    new LLIR_Pragma( "Loop condition: FOR", true ) );
};


#####################################################################
#
#
# While-loop
#
#
#####################################################################

stmt: tpm_WhileStmt( rel )
{
  $cost[0] = $cost[2] + CT( INS_B_32 );
}
=
{
  DEBUG_RULE_ACTION( "stmt: tpm_WhileStmt( rel )", $1 );

  // Run the action of the loop condition in order to set the flags, which are
  // then checked with the OPER code.
  auto *t = $action[2]();
  bool sgn = t->isSignedType() || t->isRealType();

  // Generate new basic block label.
  stringstream ss;
  auto *loop = dynamic_cast<IR_LoopStmt *>( $1->getStmt() );
  ss << loop->getFalseBasicBlock();
  string bb = ARMCODESEL->getBlockLabel( ss.str() );

  // Get the right condition for this TPM tree element.
  string condition = getInvOper( $2, sgn );

  // Generate the branch operation.
  ARMINSTRUCTIONS.insertB(
    condition, bb, $1->getExp(), InstructionFactory::WHILE_STMT );
  ARMCODESEL->getLastLLIRBB()->AddPragma(
    new LLIR_Pragma( "Loop condition: WHILE", true ) );
};


#####################################################################
#
#
# doWhile-loop
#
#
#####################################################################

stmt: tpm_DoWhileStmt( rel )
{
  $cost[0] = $cost[2] + CT( INS_B_32 );
}
=
{
  DEBUG_RULE_ACTION( "stmt: tpm_DoWhileStmt( rel )", $1 );

  // Run the action of the loop condition in order to set the flags, which are
  // then checked with the OPER code.
  auto *t = $action[2]();
  bool sgn = t->isSignedType() || t->isRealType();

  // Get first block within the loop body.
  stringstream ss;
  auto *loop = dynamic_cast<IR_LoopStmt *>( $1->getStmt() );
  ss << loop->getTrueBasicBlock();
  string bb = ARMCODESEL->getBlockLabel( ss.str() );

  // Get the right condition for this TPM tree element.
  string condition = getOper( $2, sgn );

  // Generate the branch operation.
  ARMINSTRUCTIONS.insertB(
    condition, bb, $1->getExp(), InstructionFactory::DOWHILE_STMT );
  ARMCODESEL->getLastLLIRBB()->AddPragma(
    new LLIR_Pragma( "Loop condition: DOWHILE", true ) );
};


#####################################################################
#
#
# Jump statements
#
#
#####################################################################

stmt: tpm_ImplicitJump
{
  $cost[0] = CT( INS_B_32 );
}
=
{
  DEBUG_RULE_ACTION( "stmt: tpm_ImplicitJump", $1 );

  // Generate new internal basic block for this implicit jump.
  beginNewLLIRBasicBlock( *$1->getBasicBlock() );

  // Generate unconditional jump to target block.
  stringstream ss;
  ss << $1->getBasicBlock()->getImplicitSucc();
  string targetLabel = ARMCODESEL->getBlockLabel( ss.str() );

  // Generate the branch operation.
  ARMINSTRUCTIONS.insertB(
    OPER_AL, targetLabel, $1->getExp(), InstructionFactory::JUMP_STMT );
};


##############################################################################
#
#
# Function calls, argument passing and returns
#
#
###############################################################################

called_function: tpm_SymbolExp
{
  // This rule generates the BL instructions for direct function calls.
  if ( isFunctionType( *$1->getExp() ) )
    $cost[0] = $cost[1] + CT( INS_BL_32 ) + CT( INS_MOV_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "called_function: tpm_SymbolExp", $1 );

  // Generate the BRANCHLINK instruction to the label of the function.
  string label =
    dynamic_cast<IR_SymbolExp *>( $1->getExp() )->getSymbol().getName();

  // Generate the branch with link operation.
  ARMINSTRUCTIONS.insertBL(
    OPER_AL, label, &lhsRegs, $1->getExp(), InstructionFactory::JUMP_STMT );
  auto *insBL = ARMCODESEL->getCurrentInstruction();

  // aiT expects that CRL2 basic blocks end with a CALL instruction, since it is
  // unknown if the called function will ever return. An example are the paired
  // C library functions setjmp/longjmp, where the control flow from the longjmp
  // returns to the point of the dynamically enclosing setjmp() call (see
  // Muchnick p.175). Thus, split current basic block after a call.
  ARMCODESEL->beginNewLLIRBasicBlock();

  // Allocate new register to keep the function call result.
  LLIR_Register *reg0 = nullptr;
  LLIR_Register *reg1 = nullptr;

  if ( returnBehaviour == EXTENDED_REGISTER ) {
    reg0 = ARMINSTRUCTIONS.CreateERegister( "" );
    reg1 = ARMINSTRUCTIONS.CreateERegister( "" );
    bindToPHREG( *reg1, 0 );

    ARMINSTRUCTIONS.insertMOV(
      reg0->GetFirstChild(), reg1->GetFirstChild(), $1->getExp() );
    ARMINSTRUCTIONS.insertMOV(
      reg0->GetNextChild( reg0->GetFirstChild() ) ,
      reg1->GetNextChild( reg1->GetFirstChild() ), $1->getExp() );
  } else

  if ( returnBehaviour == DATA_REGISTER ) {
    reg0 = ARMINSTRUCTIONS.CreateRegister( "" );
    reg1 = ARMINSTRUCTIONS.CreateRegister( PHREG_R0 );

    ARMINSTRUCTIONS.insertMOV( reg0, reg1, $1->getExp() );
  }

  // Register the result register as an implicit operand of the CALL.
  if ( returnBehaviour != NO_REGISTER ) {
    auto *opBL = insBL->GetFirstOp();

    if ( returnBehaviour == EXTENDED_REGISTER ) {
      auto *implicitParam =
        new LLIR_Parameter( reg1->GetFirstChild(), USAGE_DEF, 1 );
      opBL->AddParameter( implicitParam );
      implicitParam =
        new LLIR_Parameter(
          reg1->GetNextChild( reg1->GetFirstChild() ), USAGE_DEF, 1 );
      opBL->AddParameter( implicitParam );
    } else {
      auto *implicitParam = new LLIR_Parameter( reg1, USAGE_DEF, 1 );
      opBL->AddParameter( implicitParam );
    }
  }

  // Return result register.
  return( reg0 ? reg0 : nullptr );
};


called_function: areg
{
  auto &t = $1->getExp()->getType();
  auto *pointerType = dynamic_cast<IR_PointerType *>( &t );

  // This rule should map to (*fnptr)() and fnptr() function pointer calls.
  if ( isFunctionType( t ) ||
       ( pointerType && isFunctionType( pointerType->getBaseType() ) ) )
      $cost[0] = $cost[1] + CT( INS_BL_32 ) + 2 * CT( INS_MOV_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "called_function: areg", $1 );

  auto *reg = $action[1]();

  // First move the register with the function address into the IP register.
  // This way, we make sure that no parameter passing register is used for it
  // and that a possible unspill happens before and not between the LR setting
  // and the actual call.
  auto *ip = ARMINSTRUCTIONS.CreateRegister( PHREG_IP );
  ARMINSTRUCTIONS.insertMOV( ip, reg, $1->getExp() );

  // Then, move the pc into the link register.
  auto *pc = ARMINSTRUCTIONS.CreateRegister( PHREG_PC );
  auto *lr = ARMINSTRUCTIONS.CreateRegister( PHREG_LR );
  ARMINSTRUCTIONS.insertMOV( lr, pc, $1->getExp() );

  // Then, create a BX instruction to branch to the function address.
  ARMINSTRUCTIONS.insertBX( OPER_AL, ip, &lhsRegs, $1->getExp() );
  auto *insBX = ARMCODESEL->getCurrentInstruction();

  // aiT expects that CRL2 basic blocks end with a CALL instruction, since it is
  // unknown if the called function will ever return. An example are the paired
  // C library functions setjmp/longjmp, where the control flow from the longjmp
  // returns to the point of the dynamically enclosing setjmp() call (see
  // Muchnick p.175). Thus, split current basic block after a call.
  beginNewLLIRBasicBlock();

  // Allocate new register to keep the function call result.
  LLIR_Register *reg0 = nullptr;
  LLIR_Register *reg1 = nullptr;

  if ( returnBehaviour == DATA_REGISTER ) {
    reg0 = ARMINSTRUCTIONS.CreateRegister( "" );
    reg1 = ARMINSTRUCTIONS.CreateRegister( PHREG_R0 );
    ARMINSTRUCTIONS.insertMOV( reg0, reg1, $1->getExp() );
  }

  // Register the result register as an implicit operand of the CALL.
  if ( returnBehaviour != NO_REGISTER ) {
    auto *opBX = insBX->GetFirstOp();
    auto *implicitParam = new LLIR_Parameter( reg1, USAGE_DEF, 1 );
    opBX->AddParameter( implicitParam );
  }

  // Return result register.
  return( reg0 ? reg0 : nullptr );
};


dreg: tpm_CallExp( called_function, arg )
{
  // Acquire the type of this expression.
  auto *t = &($1->getExp()->getType());

  // Only match this rule if the return value is *not* of pointer type and a
  // single reg type.
  if ( !dynamic_cast<IR_PointerType *>( t ) && !isERegType( *t ) )
    $cost[0] = $cost[2] + $cost[3];
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_CallExp( called_function, arg )", $1 );

  // Do parameter passing.
  auto *theCall = dynamic_cast<IR_CallExp *>( $1->getExp() );

  regptr_list rhsRegs;
  regptr_list lhsRegs;

  $action[3]( true, 0, 0, theCall, &lhsRegs, &rhsRegs );
  lhsRegs.clear();
  $action[3]( false, 0, 0, theCall, &lhsRegs, &rhsRegs );

  if ( $0->getExp()->getType().getType() == IR_Type::VOID )
    return( $action[2]( lhsRegs, NO_REGISTER ) );
  else
    return( $action[2]( lhsRegs, DATA_REGISTER ) );
};


ereg: tpm_CallExp( called_function, arg )
{
  // Acquire the type of this expression.
  auto *t = &($1->getExp()->getType());

  // Only match this rule if the return value is *not* of pointer type and an
  // extended reg type.
  if ( !dynamic_cast<IR_PointerType *>( t ) && isERegType( *t ) )
    $cost[0] = $cost[2] + $cost[3];
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "ereg: tpm_CallExp( called_function, arg )", $1 );

  // Do parameter passing.
  auto *theCall = dynamic_cast<IR_CallExp *>( $1->getExp() );

  regptr_list rhsRegs;
  regptr_list lhsRegs;

  $action[3]( true, 0, 0, theCall, &lhsRegs, &rhsRegs );
  lhsRegs.clear();
  $action[3]( false, 0, 0, theCall, &lhsRegs, &rhsRegs );

  if ( $0->getExp()->getType().getType() == IR_Type::VOID )
    return( $action[2]( lhsRegs, NO_REGISTER ) );
  else
    return( $action[2]( lhsRegs, EXTENDED_REGISTER ) );
};


areg: tpm_CallExp( called_function, arg )
{
  // Acquire the type of this expression.
  auto *t = &($1->getExp()->getType());

  // Only match this rule if the return value *is* of pointer type.
  if ( dynamic_cast<IR_PointerType *>( t ) )
    $cost[0] = $cost[2] + $cost[3];
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "areg: tpm_CallExp( called_function, arg )", $1 );

  // Do parameter passing.
  auto *theCall = dynamic_cast<IR_CallExp *>( $1->getExp() );

  regptr_list rhsRegs;
  regptr_list lhsRegs;

  $action[3]( true, 0, 0, theCall, &lhsRegs, &rhsRegs );
  lhsRegs.clear();
  $action[3]( false, 0, 0, theCall, &lhsRegs, &rhsRegs );

  if ( $0->getExp()->getType().getType() == IR_Type::VOID )
    return( $action[2]( lhsRegs, NO_REGISTER ) );
  else
    return( $action[2]( lhsRegs, DATA_REGISTER ) );
};


arg: tpm_CallExpARG( any_reg, arg )
{
 $cost[0] = $cost[2] + $cost[3] + CT( INS_MOV_32 ) + CT( INS_STR_32 );
}
=
{
  DEBUG_RULE_ACTION( "arg: tpm_CallExpARG( any_reg, arg )", $1 );

  int incr = 0;

  if ( dryrun )
    rhsRegs->push_back( $action[2]() );
  else {
    ufAssert( !rhsRegs->empty() );

    auto *rhsreg = rhsRegs->front();
    rhsRegs->pop_front();

    int myRegister =
     Stack::isPassedThroughRegister( theCall->getFunctionType(), index );

    if ( myRegister != -1 ) {
      auto *lhsreg = &getFunctionArgumentRegister( *theCall, index, myRegister );

      // If an extended reg.
      if ( lhsreg->IsHierarchical() ) {
        // Move the two registers.
        ARMINSTRUCTIONS.insertMOV(
          lhsreg->GetFirstChild(), rhsreg->GetFirstChild(), $1->getExp() );
        ARMINSTRUCTIONS.insertMOV(
          lhsreg->GetNextChild( lhsreg->GetFirstChild() ),
          rhsreg->GetNextChild( rhsreg->GetFirstChild() ), $1->getExp() );

          if ( lhsRegs != nullptr ) {
            lhsRegs->push_back( lhsreg->GetFirstChild() );
            lhsRegs->push_back(
              lhsreg->GetNextChild( lhsreg->GetFirstChild() ) );
          }
      } else {
        // Generate the operation.
        ARMINSTRUCTIONS.insertMOV( lhsreg, rhsreg, $1->getExp() );

        if ( lhsRegs != nullptr )
          lhsRegs->push_back( lhsreg );
      }
    } else {
      // Pass argument via the stack.
      auto *sp = ARMINSTRUCTIONS.CreateRegister( PHREG_SP );

      // TODO: We could save stack space here if we'd not use 4 bytes for each
      //       char/short too, but this would also require adapting the symbol
      //       load rules.
      incr = intBytes;

      // Check if we do a simply STR or two.
      if ( rhsreg->IsHierarchical() ) {
        // Genererate the first instruction, store the LSB.
        ARMINSTRUCTIONS.insertSTR(
          OPER_AL, OPER_IMMOFF, rhsreg->GetFirstChild(), sp, offset,
          $1->getExp() );
        ARMCODESEL->getLastLLIRBB()->GetLastIns()->AddPragma(
          new LLIR_Pragma( "Passing overflow function parameter", true ) );

        // And the second one, store the MSB.
        ARMINSTRUCTIONS.insertSTR(
          OPER_AL, OPER_IMMOFF, rhsreg->GetNextChild( rhsreg->GetFirstChild() ),
          sp, offset - 4, $1->getExp() );
        ARMCODESEL->getLastLLIRBB()->GetLastIns()->AddPragma(
          new LLIR_Pragma( "Passing overflow function parameter", true ) );
      } else {
        // Generate the operation.
        ARMINSTRUCTIONS.insertSTR(
          OPER_AL, OPER_IMMOFF, rhsreg, sp, offset, $1->getExp() );
        ARMCODESEL->getLastLLIRBB()->GetLastIns()->AddPragma(
          new LLIR_Pragma( "Passing overflow function parameter", true ) );
      }
    }
  }

  $action[3]( dryrun, index + 1, offset + incr, theCall, lhsRegs, rhsRegs );
};


arg: tpm_CallExpNOARG
{
  $cost[0] = 0;
}
=
{
  DEBUG_RULE_ACTION( "arg: tpm_CallExpNOARG", $1 );

  // Do nothing.
};


stmt: tpm_ReturnStmtVOID
{
  $cost[0] = CT( INS_SUB_32 ) + CT( INS_LDM_32 );
}
=
{
  DEBUG_RULE_ACTION( "stmt: tpm_ReturnStmtVOID", $1 );

  auto *sp = ARMINSTRUCTIONS.CreateRegister( PHREG_SP );
  auto *fp = ARMINSTRUCTIONS.CreateRegister( PHREG_FP );
  auto *pc = ARMINSTRUCTIONS.CreateRegister( PHREG_PC );

  // Save all registers in a queue.
  deque<LLIR_Register *> registers;
  registers.push_back( fp );
  registers.push_back( sp );
  registers.push_back( pc );

  ARMINSTRUCTIONS.insertSUB( sp, fp, 12, $1->getExp() );
  ARMINSTRUCTIONS.insertLDM(
    OPER_FD, OPER_AL, sp, OPER_NOWRITEBACK, &registers, $1->getExp(),
    InstructionFactory::RETURN_STMT );
};


stmt: tpm_ReturnStmt( any_reg )
{
  $cost[0] = $cost[2] + CT( INS_MOV_32 ) + CT( INS_SUB_32 ) + CT( INS_LDM_32 );
}
=
{
  DEBUG_RULE_ACTION( "stmt: tpm_ReturnStmt( any_reg )", $1 );

  auto *sp = ARMINSTRUCTIONS.CreateRegister( PHREG_SP );
  auto *fp = ARMINSTRUCTIONS.CreateRegister( PHREG_FP );
  auto *pc = ARMINSTRUCTIONS.CreateRegister( PHREG_PC );
  LLIR_Register *final;

  auto *srcReg = $action[2]();

  // Check if it's a simple reg or an extended one.
  if ( isEReg( srcReg ) )
    final = ARMINSTRUCTIONS.CreateERegister( "" );
  else
    final = ARMINSTRUCTIONS.CreateRegister( "" );

  // Save all registers in a queue.
  deque<LLIR_Register *> registers;
  registers.push_back( fp );
  registers.push_back( sp );
  registers.push_back( pc );

  // Return result in register R0.
  bindToPHREG( *final, 0 );

  // Copy result to the return register(s).
  if ( isEReg( srcReg ) ) {
    ARMINSTRUCTIONS.insertMOV(
      final->GetFirstChild(), srcReg->GetFirstChild(), $1->getExp() );
    ARMINSTRUCTIONS.insertMOV(
      final->GetNextChild( final->GetFirstChild() ),
      srcReg->GetNextChild( srcReg->GetFirstChild() ), $1->getExp() );
  } else
    ARMINSTRUCTIONS.insertMOV( final, srcReg, $1->getExp() );

  // Set new position of SP.
  ARMINSTRUCTIONS.insertSUB( sp, fp, 12, $1->getExp() );
  ARMINSTRUCTIONS.insertLDM(
    OPER_FD, OPER_AL, sp, OPER_NOWRITEBACK, &registers, $1->getExp(),
    InstructionFactory::RETURN_STMT );
};


###############################################################################
#
#
# Logical AND, OR and NOT operators
#
#
###############################################################################

dreg: tpm_BinaryExpLOGAND( dreg, dreg )
{
  $cost[0] =
    $cost[2] + $cost[3] + 2 * CT( INS_MOV_32 ) + CT( INS_CMP_32 ) +
    CT( INS_B_32 );
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpLOGAND( dreg, dreg )", $1 );

  // Involves splitting of basic blocks. ANSI-C standards have been followed. If
  // the LHS expression is false, the entire AND expression is treated as false
  // and the RHS expression is never evaluated.

  auto *bexp = dynamic_cast<IR_BinaryExp *>( $2->getExp() );
  bool isNestedLogAND = bexp && ( bexp->getOperator() == IR_BinaryExp::LOGAND );

  // LLIR
  // Generate label for the basic block after the current operation.
  string label0 = LLIR::getUniqueLabel();
  string jmpLabel = ( logAndEndLabel != "" ? logAndEndLabel : label0 );
  if ( isNestedLogAND ) {
    if ( logAndEndLabel == "" )
      logAndEndLabel = label0;
  } else
    logAndEndLabel = "";

  // Generate LHS operand.
  auto p1 = $action[2]();

  // Small hack: The tree pattern matcher will match the rightmost operator
  //             first if confronted with a chain like
  //             a && b && c && d
  //             so the first match will be
  //             ( a && b && c ) && ( d )
  // Therefore, we can reuse the result register that is delivered by the LHS
  // as our target register, if the LHS is produced by a logical operator.
  // In the future this could be replaced by a more involved rule framework that
  // uses special nonterminals to handle the situation. Then also the global
  // variable communication above could be eliminated.
  LLIR_Register *reg = nullptr;
  if ( isNestedLogAND )
    reg = p1;
  else {
    reg = ARMINSTRUCTIONS.CreateRegister( "" );
    ARMINSTRUCTIONS.insertMOV( reg, p1, $1->getExp() );
  }

  // Compare reg with 0.
  ARMINSTRUCTIONS.insertCMP( OPER_AL, reg, 0, $1->getExp() );
  ARMINSTRUCTIONS.insertB( OPER_EQ, jmpLabel, $1->getExp() );

  // LLIR
  // Insert new basic block for RHS operand.
  beginNewLLIRBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );

  // Generate RHS operand.
  auto p2 = $action[3]();

  // LLIR
  ARMINSTRUCTIONS.insertMOV( reg, p2, $1->getExp() );

  // Insert new basic block after the AND operation.
  beginNewLLIRBasicBlock(
    label0.c_str(), *$1->getExp()->getStmt().getBasicBlock() );

  return( reg );
};


dreg: tpm_BinaryExpLOGAND( ereg, ereg )
{
  $cost[0] = $cost[2] + $cost[3] + 2 * CT( INS_MOV_32 );

  if ( $2->getExp()->getType().isIntegralType() &&
       ( $2->getExp()->getType().bitSize() > 32 ) )
    $cost[0] += 4 * CT( INS_CMP_32 );
  else

  if ( ( $2->getExp()->getType().getType() == IR_Type::DOUBLE ) ||
       ( $2->getExp()->getType().getType() == IR_Type::LONG_DOUBLE ) )
    $cost[0] += 2 * CT( INS_BL_32 ) + 10 * CT( INS_MOV_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpLOGAND( ereg, ereg )", $1 );

  // Evaluate the arguments.
  auto *rhsEReg = $action[3]();
  auto *lhsEReg = $action[2]();

  // Create a result register to represent an integer value of 0 or 1.
  auto *result = ARMINSTRUCTIONS.CreateRegister( "" );

  // Generate the operation.
  if ( $2->getExp()->getType().isIntegralType() &&
       ( $2->getExp()->getType().bitSize() > 32 ) ) {
    // Compare the LS child of the lhs against zero.
    ARMINSTRUCTIONS.insertCMP(
      OPER_AL, lhsEReg->GetFirstChild(), 0, $1->getExp() );

    // Check the MS child of the lhs iff the LS child compared equal to zero.
    ARMINSTRUCTIONS.insertCMP(
      OPER_EQ, lhsEReg->GetNextChild( lhsEReg->GetFirstChild() ), 0,
      $1->getExp() );

    // Check the LS child of the rhs iff the lhs didn't compare equal to zero.
    ARMINSTRUCTIONS.insertCMP(
      OPER_NE, rhsEReg->GetFirstChild(), 0, $1->getExp() );

    // Check the MS child of the rhs iff the LS child compared equal to zero.
    ARMINSTRUCTIONS.insertCMP(
      OPER_EQ, rhsEReg->GetNextChild( rhsEReg->GetFirstChild() ), 0,
      $1->getExp() );
  } else {
    // Create temporary extended register to represent zero.
    auto *eregTmp = ARMINSTRUCTIONS.CreateERegister( "" );

    ARMINSTRUCTIONS.insertMOV( eregTmp->GetFirstChild(), 0, $1->getExp() );
    ARMINSTRUCTIONS.insertMOV(
      eregTmp->GetNextChild( eregTmp->GetFirstChild() ), 0, $1->getExp() );

    // Compare input double value of lhs against zero.
    ARMINSTRUCTIONS.insertEQ_D( OPER_AL, result, lhsEReg, eregTmp, $1->getExp() );

    // Compare result of softfloat routine against zero.
    ARMINSTRUCTIONS.insertCMP( OPER_AL, result, 0, $1->getExp() );

    // Iff the lhs compared unequal to zero, compare rhs against zero.
    ARMINSTRUCTIONS.insertEQ_D( OPER_NE, result, rhsEReg, eregTmp, $1->getExp() );

    // Compare result of softfloat routine against zero.
    ARMINSTRUCTIONS.insertCMP( OPER_AL, result, 0, $1->getExp() );
  }

  // Insert a 1 or a 0.
  ARMINSTRUCTIONS.insertMOV( OPER_AL, result, 0, $1->getExp() );
  ARMINSTRUCTIONS.insertMOV( OPER_NE, result, 1, $1->getExp() );

  return( result );
};


dreg: tpm_BinaryExpLOGOR( dreg, dreg )
{
  $cost[0] =
    $cost[2] + $cost[3] + 2 * CT( INS_MOV_32 ) + CT( INS_CMP_32 ) +
    CT( INS_B_32 );
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpLOGOR( dreg, dreg )", $1 );

  // Involves splitting of basic blocks. ANSI-C standards have been followed. If
  // the LHS expression is true, the entire OR expression is treated as true and
  // the RHS expression is never evaluated.

  auto *bexp = dynamic_cast<IR_BinaryExp *>( $2->getExp() );
  bool isNestedLogOR = bexp && ( bexp->getOperator() == IR_BinaryExp::LOGOR );

  // LLIR
  // Generate label for the basic block after the current operation.
  string label0 = LLIR::getUniqueLabel();
  string jmpLabel = ( logOrEndLabel != "" ? logOrEndLabel : label0 );
  if ( isNestedLogOR ) {
    if ( logOrEndLabel == "" )
      logOrEndLabel = label0;
  } else
    logOrEndLabel = "";

  // Generate LHS operand.
  auto p1 = $action[2]();

  // Small hack: The tree pattern matcher will match the rightmost operator
  //             first if confronted with a chain like
  //             a || b || c || d
  //             so the first match will be
  //             ( a || b || c ) || ( d )
  // Therefore, we can reuse the result register that is delivered by the LHS
  // as our target register, if the LHS is produced by a logical operator.
  // TODO: In the future, this could be replaced by a more involved rule
  // framework that uses special nonterminals to handle the situation. Then also
  // the global variable communication above could be eliminated.
  LLIR_Register *reg = nullptr;
  if ( isNestedLogOR )
    reg = p1;
  else {
    reg = ARMINSTRUCTIONS.CreateRegister( "" );
    ARMINSTRUCTIONS.insertMOV( reg, p1, $1->getExp() );
  }

  // Compare reg with 0.
  ARMINSTRUCTIONS.insertCMP( OPER_AL, reg, 0, $1->getExp() );
  ARMINSTRUCTIONS.insertB( OPER_NE, jmpLabel, $1->getExp() );

  // LLIR
  // Insert new basic block for RHS operand.
  beginNewLLIRBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );

  // Generate RHS operand.
  auto p2 = $action[3]();

  // LLIR
  ARMINSTRUCTIONS.insertMOV( reg, p2, $1->getExp() );

  // Insert new basic block after the OR operation.
  beginNewLLIRBasicBlock(
    label0.c_str(), *$1->getExp()->getStmt().getBasicBlock() );

  return( reg );
};


dreg: tpm_BinaryExpLOGOR( ereg, ereg )
{
  $cost[0] = $cost[2] + $cost[3] + 2 * CT( INS_MOV_32 );

  if ( $2->getExp()->getType().isIntegralType() &&
       ( $2->getExp()->getType().bitSize() > 32 ) )
    $cost[0] += 4 * CT( INS_CMP_32 );
  else

  if ( ( $2->getExp()->getType().getType() == IR_Type::DOUBLE ) ||
       ( $2->getExp()->getType().getType() == IR_Type::LONG_DOUBLE ) )
    $cost[0] +=
      2 * CT( INS_BL_32 ) + 12 * CT( INS_MOV_32 ) + 4 * CT( INS_CMP_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpLOGOR( ereg, ereg )", $1 );

  // Evaluate the arguments.
  auto *rhsEReg = $action[3]();
  auto *lhsEReg = $action[2]();

  // Create a result register to represent an integer value of 0 or 1.
  auto *result = ARMINSTRUCTIONS.CreateRegister( "" );

  // Generate the operation.
  if ( $2->getExp()->getType().isIntegralType() &&
       ( $2->getExp()->getType().bitSize() > 32 ) ) {
    // Compare LS child of the lhs against zero.
    ARMINSTRUCTIONS.insertCMP(
      OPER_AL, lhsEReg->GetFirstChild(), 0, $1->getExp() );

    // Compare MS child of the lhs against zero, iff the LS child compared equal
    // to zero.
    ARMINSTRUCTIONS.insertCMP(
      OPER_EQ, lhsEReg->GetNextChild( lhsEReg->GetFirstChild() ), 0,
      $1->getExp() );

    // Compare LS child of the rhs against zero iff the lhs compared equal to
    // zero.
    ARMINSTRUCTIONS.insertCMP(
      OPER_EQ, rhsEReg->GetFirstChild(), 0, $1->getExp() );

    // Compare MS child of the rhs against zero, iff the LS child compared equal
    // to zero.
    ARMINSTRUCTIONS.insertCMP(
      OPER_EQ, rhsEReg->GetNextChild( rhsEReg->GetFirstChild() ), 0,
      $1->getExp() );
  } else {
    // Create temporary extended register to represent zero.
    auto *eregTmp = ARMINSTRUCTIONS.CreateERegister( "" );

    ARMINSTRUCTIONS.insertMOV( eregTmp->GetFirstChild(), 0, $1->getExp() );
    ARMINSTRUCTIONS.insertMOV(
      eregTmp->GetNextChild( eregTmp->GetFirstChild() ), 0, $1->getExp() );

    // Compare input double value of lhs against zero.
    ARMINSTRUCTIONS.insertEQ_D( OPER_AL, result, lhsEReg, eregTmp, $1->getExp() );

    // Compare result of softfloat routine against zero.
    ARMINSTRUCTIONS.insertCMP( OPER_AL, result, 0, $1->getExp() );

    // Iff the lhs compared equal to zero, compare rhs against zero.
    ARMINSTRUCTIONS.insertEQ_D( OPER_EQ, result, rhsEReg, eregTmp, $1->getExp() );

    // Compare result of softfloat routine against zero.
    ARMINSTRUCTIONS.insertCMP( OPER_AL, result, 0, $1->getExp() );
  }

  // If both input values compared equal to zero, the result is 0, else it is 1.
  ARMINSTRUCTIONS.insertMOV( OPER_AL, result, 0, $1->getExp() );
  ARMINSTRUCTIONS.insertMOV( OPER_NE, result, 1, $1->getExp() );

  return( result );
};


dreg: tpm_UnaryExpLOGNOT( dreg )
{
  $cost[0] = $cost[2];

  if ( $2->getExp()->getType().isIntegralType() ||
       $2->getExp()->getType().isPointerType() )
    $cost[0] += CT( INS_CMP_32 ) + 2 * CT( INS_MOV_32 );
  else

  if ( $2->getExp()->getType().getType() == IR_Type::FLOAT )
    $cost[0] += 3 * CT( INS_MOV_32 ) + CT( INS_BL_32 ) + CT( INS_CMP_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_UnaryExpLOGNOT( dreg )", $1 );

  auto p = $action[2]();

  // LLIR
  auto *reg = ARMINSTRUCTIONS.CreateRegister( "" );

  if ( $2->getExp()->getType().isIntegralType() ||
       $2->getExp()->getType().isPointerType() ) {
    ARMINSTRUCTIONS.insertCMP( OPER_AL, p, 0, $1->getExp() );
    ARMINSTRUCTIONS.insertMOV( OPER_EQ, reg, 1, $1->getExp() );
    ARMINSTRUCTIONS.insertMOV( OPER_NE, reg, 0, $1->getExp() );
  } else {
    auto *regTmp = ARMINSTRUCTIONS.CreateRegister( "" );
    ARMINSTRUCTIONS.insertMOV( regTmp, 0, $1->getExp() );
    ARMINSTRUCTIONS.insertEQ_F( OPER_AL, regTmp, p, regTmp, $1->getExp() );

    ARMINSTRUCTIONS.insertCMP( OPER_AL, regTmp, 0, $1->getExp() );
    ARMINSTRUCTIONS.insertMOV( OPER_EQ, reg, 1, $1->getExp() );
    ARMINSTRUCTIONS.insertMOV( OPER_NE, reg, 0, $1->getExp() );
  }

  return( reg );
};


dreg: tpm_UnaryExpLOGNOT( ereg )
{
  $cost[0] = $cost[2];

  if ( ( $2->getExp()->getType().isIntegralType() ) &&
       ( $2->getExp()->getType().bitSize() > 32 ) )
    $cost[0] += 2 * CT( INS_CMP_32 ) + 2 * CT( INS_MOV_32 );
  else

  if ( ( $2->getExp()->getType().getType() == IR_Type::DOUBLE ) ||
       ( $2->getExp()->getType().getType() == IR_Type::LONG_DOUBLE ) )
    $cost[0] += 9 * CT( INS_MOV_32 ) + CT( INS_BL_32 ) + CT( INS_CMP_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_UnaryExpLOGNOT( ereg )", $1 );

  // Evaluate the argument.
  auto *ereg = $action[2]();

  // Create extended LLIR_Register to represent an integer value of 0 or 1.
  auto *result = ARMINSTRUCTIONS.CreateRegister( "" );

  if ( $2->getExp()->getType().isIntegralType() &&
       ( $2->getExp()->getType().bitSize() > 32 ) ) {
    ARMINSTRUCTIONS.insertCMP( OPER_AL, ereg->GetFirstChild(), 0, $1->getExp() );

    // Check higher child iff lower child compared equal to zero.
    ARMINSTRUCTIONS.insertCMP(
      OPER_EQ, ereg->GetNextChild( ereg->GetFirstChild() ), 0, $1->getExp() );

    // Iff the input value compared equal to zero, the result is 1, else it is
    // 0.
    ARMINSTRUCTIONS.insertMOV( OPER_EQ, result, 1, $1->getExp() );
    ARMINSTRUCTIONS.insertMOV( OPER_NE, result, 0, $1->getExp() );
  }
  else {
    // Create temporary extended register to represent 0.
    auto *eregTmp = ARMINSTRUCTIONS.CreateERegister( "" );

    ARMINSTRUCTIONS.insertMOV( eregTmp->GetFirstChild(), 0, $1->getExp() );
    ARMINSTRUCTIONS.insertMOV(
      eregTmp->GetNextChild( eregTmp->GetFirstChild() ), 0, $1->getExp() );

    // Compare input double value of ereg against zero.
    ARMINSTRUCTIONS.insertEQ_D( OPER_AL, result, ereg, eregTmp, $1->getExp() );

    // Compare result of softfloat routine against zero.
    ARMINSTRUCTIONS.insertCMP( OPER_AL, result, 0, $1->getExp() );

    // Iff the input value compared equal to zero, the result is 1, else it is
    // 0.
    ARMINSTRUCTIONS.insertMOV( OPER_EQ, result, 1, $1->getExp() );
    ARMINSTRUCTIONS.insertMOV( OPER_NE, result, 0, $1->getExp() );
  }

  return( result );
};


ereg: tpm_UnaryExpBITNOT( ereg )
{
  if ( $2->getExp()->getType().isIntegralType() &&
       ( $2->getExp()->getType().bitSize() > 32 ) )
    $cost[0] = $cost[2] + 2 * CT( INS_MVN_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "ereg: tpm_UnaryExpBITNOT( ereg )", $1 );

  // Evaluate the argument.
  auto *ereg = $action[2]();

  // Create extended result LLIR_Register.
  auto *eresult = ARMINSTRUCTIONS.CreateERegister( "" );

  // Flip bits of both child registers via eor 0xffffffff.
  auto *reg = ARMINSTRUCTIONS.CreateRegister( "" );
  ARMINSTRUCTIONS.insertMVN( OPER_AL, reg, 0, $1->getExp() );
  ARMINSTRUCTIONS.insertEOR(
    eresult->GetFirstChild(), ereg->GetFirstChild(), reg, $1->getExp() );
  ARMINSTRUCTIONS.insertEOR(
    eresult->GetNextChild( eresult->GetFirstChild() ),
    ereg->GetNextChild( ereg->GetFirstChild() ), reg, $1->getExp() );

  return( eresult );
};
