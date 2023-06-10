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
# Statement rules
#
#
###############################################################################

stmt: tpm_LabelStmt
{
  $cost[0] = 0;
}
=
{
  RV32::DEBUG_RULE_ACTION( "stmt: tpm_LabelStmt", $1 );

  return;
};


stmt: tpm_ExpStmt( reg )
{
  $cost[0] = $cost[2];
}
=
{
  DSTART( "stmt: tpm_ExpStmt( reg )");

  $action[2]();
  return;
};


###############################################################################
#
#
# Integer constants of various bit widths
#
#
###############################################################################

uconst5: tpm_IntConstExp
{
  if ( RV32::isUConst5( *$1->getExp() ) )
    $cost[0] = 0;
  else
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION( "uconst5: tpm_IntConstExp", $1 );

  return( dynamic_cast<IR_IntConstExp *>( $1->getExp() )->getValue() );
};


const6: tpm_IntConstExp
{
  if ( RV32::isConst6( *$1->getExp() ) )
    $cost[0] = 100;
  else
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION( "const6: tpm_IntConstExp", $1 );

  return( dynamic_cast<IR_IntConstExp *>( $1->getExp() )->getValue() );
};


uconst6: tpm_IntConstExp
{
  if ( RV32::isUConst6( *$1->getExp() ) )
    $cost[0] = 100;
  else
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION( "uconst6: tpm_IntConstExp", $1 );

  return( dynamic_cast<IR_IntConstExp *>( $1->getExp() )->getValue() );
};


uconst8: tpm_IntConstExp
{
  if ( RV32::isUConst8( *$1->getExp() ) )
    $cost[0] = 1000;
  else
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION( "uconst8: tpm_IntConstExp", $1 );

  return( dynamic_cast<IR_IntConstExp *>( $1->getExp() )->getValue() );
};


const12: tpm_IntConstExp
{
  if ( RV32::isConst12( *$1->getExp() ) )
    $cost[0] = 10000;
  else
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION( "const12: tpm_IntConstExp", $1 );

  return( dynamic_cast<IR_IntConstExp *>( $1->getExp() )->getValue() );
};


uconst20: tpm_IntConstExp
{
  if ( RV32::isUConst20( *$1->getExp() ) )
    $cost[0] = 100000;
  else
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION( "uconst20: tpm_IntConstExp", $1 );

  return( dynamic_cast<IR_IntConstExp *>( $1->getExp() )->getValue() );
};


reg: tpm_IntConstExp
{
  if ( $1->getExp()->getType().isIntegralType() &&
       ( $1->getExp()->getType().bitSize() <= 32 ) ) {
    auto constValue =
      dynamic_cast<IR_IntConstExp *>( $1->getExp() )->getValue().getIntValue();

    $cost[0] = 1000000 + RVINSTRUCTIONS.getMOVConstantCost( constValue );
  } else
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION( "reg: tpm_IntConstExp", $1 );

  /*
    Loading of an integer constant may not be achieved directly with one machine
    operation. If the integer is larger than 12 bits, we have to generate LUI
    and ADDI operations to perform the move.
  */
  auto constValue =
    dynamic_cast<IR_IntConstExp *>( $1->getExp() )->getValue().getIntValue();

  auto &r = RVINSTRUCTIONS.createReg();
  RVINSTRUCTIONS.insertMOVConstant( r, constValue, $1->getExp() );

  return( r );
};


###############################################################################
#
#
# Simple expressions
#
#
###############################################################################

reg: tpm_SymbolExp
{
  // Handles register 'reg' symbols (non-stack, non-global).
  auto &symExp = dynamic_cast<IR_SymbolExp &>( *$1->getExp() );
  IR_Symbol &sym = symExp.getSymbol();

  if ( RV32::isDRegType( sym.getType() ) && ( RVCODESEL.getStack().getSymbolOffset( sym ) < 0 )
         && !sym.isGlobal() )
    $cost[0] = RV32::loadRegisterSymbolCost( symExp );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION( "reg: tpm_SymbolExp", $1 );

  auto *symExp = dynamic_cast<IR_SymbolExp *>( $1->getExp() );
  auto &r = dynamic_cast<RV_RegV &>( RV32::loadRegSym( *symExp ) );

  return( r );
};


reg: tpm_SymbolExp
{
  // Handles local stack 'reg' symbols.
  auto &symExp = dynamic_cast<IR_SymbolExp &>( *$1->getExp() );
  IR_Symbol &sym = symExp.getSymbol();

  if ( RV32::isDRegType( sym.getType() ) && ( RVCODESEL.getStack().getSymbolOffset( sym ) >= 0 )
         && !sym.isGlobal() )
    $cost[0] = RV32::loadStackSymbolCost( symExp );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION( "reg: tpm_SymbolExp", $1 );

  auto *symExp = dynamic_cast<IR_SymbolExp *>( $1->getExp() );

  auto lvalue = RV32::loadStackSymbol( *symExp, true );
  return( dynamic_cast<RV_RegV &>( *(lvalue.getResultReg()) ) );
};


reg: tpm_SymbolExp
{
  // Handles global 'reg' symbols (except enums).
  auto &symExp = dynamic_cast<IR_SymbolExp &>( *$1->getExp() );
  IR_Symbol &sym = symExp.getSymbol();

  if ( RV32::isDRegType( sym.getType() ) && sym.isGlobal() )
    $cost[0] = RV32::loadGlobalSymbolCost( symExp );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION( "reg: tpm_SymbolExp", $1 );

  auto *symExp = dynamic_cast<IR_SymbolExp *>( $1->getExp() );

  auto lvalue = RV32::loadGlobalSymbol( *symExp, true );
  return( dynamic_cast<RV_RegV &>( *(lvalue.getResultReg()) ) );
};


deref_reg: tpm_SymbolExp
{
  // Handles local stack 'reg' symbols.
  auto &symExp = dynamic_cast<IR_SymbolExp &>( *$1->getExp() );
  IR_Symbol &sym = symExp.getSymbol();

  if ( RV32::isDRegType( $1->getExp()->getType() ) && !sym.isGlobal() && !sym.getEnumType() &&
       ( RVCODESEL.getStack().getSymbolOffset( sym ) >= 0 ) )
    $cost[0] = RV32::loadStackSymbolCost( symExp );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION( "deref_reg1: tpm_SymbolExp", $1 );
  IR_SymbolExp *symExp = dynamic_cast<IR_SymbolExp *>( $1->getExp() );

  return( RV32::loadStackSymbol( *symExp, loadResult ) );
};


deref_reg: tpm_SymbolExp
{
  // Handles global 'reg' symbols (except enums).
  auto &symExp = dynamic_cast<IR_SymbolExp &>( *$1->getExp() );
  IR_Symbol &sym = symExp.getSymbol();

  if ( RV32::isDRegType( $1->getExp()->getType() ) && sym.isGlobal() && !sym.getEnumType() )
    $cost[0] = RV32::loadGlobalSymbolCost( symExp );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION( "deref_reg: tpm_SymbolExp", $1 );

  auto *symExp = dynamic_cast<IR_SymbolExp *>( $1->getExp() );
  return( RV32::loadGlobalSymbol( *symExp, loadResult ) );
};


reg: modified_reg
{
  // Chain rule that actually performs pointer arithmetics by transforming a
  // modified_reg into a plain reg.
  $cost[0] = $cost[1] + RV32::RV32_AddressModification::applyModificationCost();
}
=
{
  RV32::DEBUG_RULE_ACTION( "reg: modified_reg", $1 );

  auto *p = $action[1]().applyModification( $1->getExp() );
  return( *p );
};


reg: deref_reg
{
  // This chain rule enables all deref/array-index/component-access rules to
  // produce only deref_reg. If a parent rule requires a reg nonterminal
  // instead, then this chain rule will extract it from the deref_reg, which is
  // just a reg + memory access information.

  // We may only allow this conversion if the expression is not used in a
  // context that requires a deref_reg, because the value must be written back
  // to memory.
  if ( !RV32::isMemoryWriteLocation( *$1->getExp() ) )
    $cost[0] = $cost[1];
  else
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION( "reg: deref_reg", $1 );
  auto lvalue = $action[1]( true );
  
  ufAssertT( lvalue.getResultReg() != nullptr, "Missing lvalue.getResultReg()!" );
  
  return(
      ref( dynamic_cast<RV_RegV &>( *(lvalue.getResultReg()) ) ) );
};


#modified_reg: tpm_AssignExpPLUS( deref_reg, tpm_IntConstExp )
#{
#  $cost[0] = $cost[2];
#}
#=
#{
#  RV32::DEBUG_RULE_ACTION(
#    "modified_reg: tpm_AssignExpPLUS( deref_reg, tpm_IntConstExp )", $1 );

#  auto lvalue = $action[2]( true );

#  // Also retrieve the integer constant.
#  auto *intConstExp = dynamic_cast<const IR_IntConstExp *>( $3->getExp() );
#  int offset = intConstExp->getValue().getIntValue();

#  return(
#    RV32::RV32_AddressModification{
#      &lvalue, offset, &$2->getExp()->getType(), false,
#      AddressModification::ModTime::PRE, AddressModification::ModOper::ADD } );
#};


###############################################################################
#
#
# Arithmetic operations
#
#
###############################################################################

reg: tpm_BinaryExpPLUS( reg, reg )
{
  $cost[0] = $cost[2] + $cost[3] + RV32I::OperationFormat::RRR_1.getSize();
}
=
{
  RV32::DEBUG_RULE_ACTION( "reg: tpm_BinaryExpPLUS( reg, reg )", $1 );

  auto &reg2 = $action[2]();
  auto &reg3 = $action[3]();

  auto &r = RVINSTRUCTIONS.createReg();
  RVINSTRUCTIONS.insertADD( r, reg2, reg3, $1->getExp() );

  return( r );
};


reg: tpm_BinaryExpPLUS( reg, const12 )
{
  if ( $0->getExp()->getType().isIntegralType() )
    $cost[0] = $cost[2] + $cost[3] + RV32I::OperationFormat::RRC12_1.getSize();
  else

  if ( $0->getExp()->getType().isRealType() )
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION( "reg: tpm_BinaryExpPLUS( reg, const12 )", $1 );

  auto &reg2 = $action[2]();
  auto v = $action[3]().getIntValue();

  auto &r = RVINSTRUCTIONS.createReg();
  RVINSTRUCTIONS.insertADDI( r, reg2, v, $1->getExp() );

  return( r );
};


reg: tpm_BinaryExpMINUS( reg, reg )
{
  $cost[0] = $cost[2] + $cost[3] + RV32I::OperationFormat::RRR_1.getSize();
}
=
{
  RV32::DEBUG_RULE_ACTION( "reg: tpm_BinaryExpMINUS( reg, reg )", $1 );

  auto &reg2 = $action[2]();
  auto &reg3 = $action[3]();

  auto &r = RVINSTRUCTIONS.createReg();
  RVINSTRUCTIONS.insertSUB( r, reg2, reg3, $1->getExp() );

  return( r );
};


reg: tpm_BinaryExpMULT( reg, reg )
{
  $cost[0] = $cost[2] + $cost[3] + RV32I::OperationFormat::RRR_1.getSize();
}
=
{
  RV32::DEBUG_RULE_ACTION( "reg: tpm_BinaryExpMULT( reg, reg )", $1 );

  auto &reg2 = $action[2]();
  auto &reg3 = $action[3]();

  auto &r = RVINSTRUCTIONS.createReg();
  RVINSTRUCTIONS.insertMUL( r, reg2, reg3, $1->getExp() );

  return( r );
};


reg: tpm_BinaryExpDIV( reg, reg )
{
  $cost[0] = $cost[2] + $cost[3] + RV32I::OperationFormat::RRR_1.getSize();
}
=
{
  RV32::DEBUG_RULE_ACTION( "reg: tpm_BinaryExpDIV( reg, reg )", $1 );

  bool isUnsigned = $0->getExp()->getType().isUnsignedType();

  auto &reg2 = $action[2]();
  auto &reg3 = $action[3]();

  auto &r = RVINSTRUCTIONS.createReg();

  if ( isUnsigned )
      RVINSTRUCTIONS.insertDIVU( r, reg2, reg3, $1->getExp() );
  else
      RVINSTRUCTIONS.insertDIV( r, reg2, reg3, $1->getExp() );

  return( r );
};


reg: tpm_BinaryExpMOD( reg, reg )
{
  $cost[0] = $cost[2] + $cost[3] + RV32I::OperationFormat::RRR_1.getSize();
}
=
{
  RV32::DEBUG_RULE_ACTION( "reg: tpm_BinaryExpMOD( reg, reg )", $1 );

  auto &reg2 = $action[2]();
  auto &reg3 = $action[3]();

  bool isUnsigned = $0->getExp()->getType().isUnsignedType();

  auto &r = RVINSTRUCTIONS.createReg();

  if ( isUnsigned )
    RVINSTRUCTIONS.insertREMU( r, reg2, reg3, $1->getExp() );
  else
    RVINSTRUCTIONS.insertREM( r, reg2, reg3, $1->getExp() );

  return( r );
};


###############################################################################
#
#
# Logical AND, OR and NOT operators
#
#
###############################################################################

reg: tpm_BinaryExpLOGAND( reg, reg )
{
  $cost[0] =
    $cost[2] + $cost[3] + 2 * RVINSTRUCTIONS.getCMVCost() +
    RV32I::OperationFormat::RRL_1.getSize();
}
=
{
  RV32::DEBUG_RULE_ACTION( "reg: tpm_BinaryExpLOGAND( reg, reg )", $1 );

  /*
    Involves splitting of basic blocks. ANSI-C standards have been followed. If
    the LHS expression is false, the entire AND expression is treated as false
    and the RHS expression is never evaluated.
  */

  auto *bexp = dynamic_cast<IR_BinaryExp *>( $2->getExp() );
  bool isNestedLogAND = bexp && ( bexp->getOperator() == IR_BinaryExp::LOGAND );

  // Create new basic block immediately after this '&&' operator.
  auto *currentBB = RV32::RV32_wirBB;
  auto &b =
    ( RV32::logAndEndBlock == nullptr ) ?
      RVCODESEL.startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() ) :
      *RV32::logAndEndBlock;
  bool newEndBlock = ( RV32::logAndEndBlock == nullptr );
  if ( isNestedLogAND ) {
    if ( RV32::logAndEndBlock == nullptr )
      RV32::logAndEndBlock = &b;
  } else
    RV32::logAndEndBlock = nullptr;
  RV32::RV32_wirBB = currentBB;

  // Generate LHS operand.
  auto &reg2 = $action[2]();

  // Generate MOV and conditional JMP still in the current basic block.
  // Small hack: The tree pattern matcher will match the rightmost operator
  //             first if confronted with a chain like
  //             a && b && c && d
  //             so the first match will be
  //             ( a && b && c ) && ( d )
  // Therefore, we can reuse the result register that is delivered by the LHS
  // as our target register, if the LHS is produced by a logical operator.
  auto &r = isNestedLogAND ? reg2 : RVINSTRUCTIONS.createReg();
  if ( !isNestedLogAND )
    RVINSTRUCTIONS.insertCMV( r, reg2, $1->getExp() );
  auto &x0 = RVINSTRUCTIONS.createReg();
  RV32::bindToPHREG( x0, 0 );
  RVINSTRUCTIONS.insertBEQ( r, x0, b, $1->getExp() );

  // Create new basic block for RHS operand.
  RVCODESEL.startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );

  // Generate RHS operand.
  auto &reg3 = $action[3]();

  RVINSTRUCTIONS.insertCMV( r, reg3, $1->getExp() );
  if ( newEndBlock ) {
    RV32::RV32_wirBB = &b;
    RV32::logAndEndBlock = nullptr;
  }

  return( r );
};


reg: tpm_BinaryExpLOGOR( reg, reg )
{
  $cost[0] =
    $cost[2] + $cost[3] + 2 * RVINSTRUCTIONS.getCMVCost() +
    RV32I::OperationFormat::RRL_1.getSize();
}
=
{
  RV32::DEBUG_RULE_ACTION( "reg: tpm_BinaryExpLOGOR( reg, reg )", $1 );

  /*
    Involves splitting of basic blocks. ANSI-C standards have been followed. If
    the LHS expression is true, the entire OR expression is treated as true and
    the RHS expression is never evaluated.
  */

  auto *bexp = dynamic_cast<IR_BinaryExp *>( $2->getExp() );
  bool isNestedLogOR = bexp && ( bexp->getOperator() == IR_BinaryExp::LOGOR );

  // Create new basic block immediately after this '||' operator.
  auto *currentBB = RV32::RV32_wirBB;
  auto &b =
    ( RV32::logOrEndBlock == nullptr ) ?
      RVCODESEL.startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() ) :
      *RV32::logOrEndBlock;
  bool newEndBlock = ( RV32::logOrEndBlock == nullptr );
  if ( isNestedLogOR ) {
    if ( RV32::logOrEndBlock == nullptr )
      RV32::logOrEndBlock = &b;
  } else
    RV32::logOrEndBlock = nullptr;
  RV32::RV32_wirBB = currentBB;

  // Generate LHS operand.
  auto &reg2 = $action[2]();

  // Generate MOV and conditional JMP still in the current basic block.
  // Small hack: The tree pattern matcher will match the rightmost operator
  //             first if confronted with a chain like
  //             a || b || c || d
  //             so the first match will be
  //             ( a || b || c ) || ( d )
  // Therefore, we can reuse the result register that is delivered by the LHS
  // as our target register, if the LHS is produced by a logical operator.
  auto &r = isNestedLogOR ? reg2 : RVINSTRUCTIONS.createReg();
  if ( !isNestedLogOR )
    RVINSTRUCTIONS.insertCMV( r, reg2, $1->getExp() );
  auto &x0 = RVINSTRUCTIONS.createReg();
  RV32::bindToPHREG( x0, 0 );
  RVINSTRUCTIONS.insertBNE( r, x0, b, $1->getExp() );

  // Create new basic block for RHS operand.
  RVCODESEL.startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );

  // Generate RHS operand.
  auto &reg3 = $action[3]();

  RVINSTRUCTIONS.insertCMV( r, reg3, $1->getExp() );
  if ( newEndBlock ) {
    RV32::RV32_wirBB = &b;
    RV32::logOrEndBlock = nullptr;
  }

  return( r );
};


reg: tpm_UnaryExpLOGNOT( reg )
{
  $cost[0] = $cost[2] + RV32I::OperationFormat::RRC12_1.getSize();
}
=
{
  RV32::DEBUG_RULE_ACTION( "reg: tpm_UnaryExpLOGNOT( reg )", $1 );

  auto &reg2 = $action[2]();

  auto &r = RVINSTRUCTIONS.createReg();
  RVINSTRUCTIONS.insertSLTIU( r, reg2, 1, $1->getExp() );

  return( r );
};


###############################################################################
#
#
# Bitwise logical operators
#
#
###############################################################################

reg: tpm_BinaryExpAND( reg, reg )
{
 $cost[0] = $cost[2] + $cost[3] + RV32I::OperationFormat::RRR_1.getSize();
}
=
{
 RV32::DEBUG_RULE_ACTION( "reg: tpm_BinaryExpAND( reg, reg )", $1 );

 auto &reg2 = $action[2]();
 auto &reg3 = $action[3]();

 auto &r = RVINSTRUCTIONS.createReg();
 RVINSTRUCTIONS.insertAND( r, reg2, reg3, $1->getExp() );

 return( r );
};


reg: tpm_BinaryExpAND( reg, const12 )
{
 $cost[0] = $cost[2] + $cost[3] + RV32I::OperationFormat::RRC12_1.getSize();
}
=
{
 RV32::DEBUG_RULE_ACTION( "reg: tpm_BinaryExpAND( reg, const12 )", $1 );

 auto &reg2 = $action[2]();
 auto v = $action[3]().getIntValue();

 auto &r = RVINSTRUCTIONS.createReg();
 RVINSTRUCTIONS.insertANDI( r, reg2, v, $1->getExp() );

 return( r );
};


reg: tpm_BinaryExpOR( reg, reg )
{
 $cost[0] = $cost[2] + $cost[3] + RV32I::OperationFormat::RRR_1.getSize();
}
=
{
 RV32::DEBUG_RULE_ACTION( "reg: tpm_BinaryExpOR( reg, reg )", $1 );

 auto &reg2 = $action[2]();
 auto &reg3 = $action[3]();

 auto &r = RVINSTRUCTIONS.createReg();
 RVINSTRUCTIONS.insertOR( r, reg2, reg3, $1->getExp() );

 return( r );
};


reg: tpm_BinaryExpOR( reg, const12 )
{
 $cost[0] = $cost[2] + $cost[3] + RV32I::OperationFormat::RRC12_1.getSize();
}
=
{
 RV32::DEBUG_RULE_ACTION( "reg: tpm_BinaryExpOR( reg, const12 )", $1 );

 auto &reg2 = $action[2]();
 auto v = $action[3]().getIntValue();

 auto &r = RVINSTRUCTIONS.createReg();
 RVINSTRUCTIONS.insertORI( r, reg2, v, $1->getExp() );

 return( r );
};


reg: tpm_BinaryExpXOR( reg, reg )
{
  $cost[0] = $cost[2] + $cost[3] + RV32I::OperationFormat::RRR_1.getSize();
}
=
{
  RV32::DEBUG_RULE_ACTION( "reg: tpm_BinaryExpXOR( reg, reg )", $1 );

  auto &reg2 = $action[2]();
  auto &reg3 = $action[3]();

  auto &r = RVINSTRUCTIONS.createReg();
  RVINSTRUCTIONS.insertXOR( r, reg2, reg3, $1->getExp() );

  return( r );
};


reg: tpm_BinaryExpXOR( reg, const12 )
{
  $cost[0] = $cost[2] + $cost[3] + RV32I::OperationFormat::RRC12_1.getSize();
}
=
{
  RV32::DEBUG_RULE_ACTION( "reg: tpm_BinaryExpXOR( reg, const12 )", $1 );

  auto &reg2 = $action[2]();
  auto v = $action[3]().getIntValue();

  auto &r = RVINSTRUCTIONS.createReg();
  RVINSTRUCTIONS.insertXORI( r, reg2 , v, $1->getExp() );

  return( r );
};


reg: tpm_UnaryExpBITNOT( reg )
{
  $cost[0] = $cost[2] + RV32I::OperationFormat::RRR_1.getSize();
}
=
{
  RV32::DEBUG_RULE_ACTION( "reg: tpm_UnaryExpBITNOT( reg )", $1 );

  auto &reg2 = $action[2]();

  auto &r = RVINSTRUCTIONS.createReg();
  RVINSTRUCTIONS.insertNOT( r, reg2, $1->getExp() );

  return( r );
};


###############################################################################
#
#
# Shift operators
#
#
###############################################################################

reg: tpm_BinaryExpSHL( reg, reg )
{
  $cost[0] = $cost[2] + $cost[3] + RV32I::OperationFormat::RRR_1.getSize();
}
=
{
  RV32::DEBUG_RULE_ACTION( "reg: tpm_BinaryExpSHL( reg, reg )", $1 );

  auto &reg2 = $action[2]();
  auto &reg3 = $action[3]();

  auto &r = RVINSTRUCTIONS.createReg();
  RVINSTRUCTIONS.insertSLL( r, reg2, reg3, $1->getExp() );

  return( r );
};


reg: tpm_BinaryExpSHL( reg, uconst5 )
{
  $cost[0] = $cost[2] + $cost[3] + RV32I::OperationFormat::RRC5_1.getSize();
}
=
{
  RV32::DEBUG_RULE_ACTION( "reg: tpm_BinaryExpSHL( reg, uconst5 )", $1 );

  auto &reg2 = $action[2]();
  auto v = $action[3]().getIntValue();

  auto &r = RVINSTRUCTIONS.createReg();
  RVINSTRUCTIONS.insertSLLI( r, reg2, v, $1->getExp() );

  return( r );
};


reg: tpm_BinaryExpSHR( reg, reg )
{
  $cost[0] = $cost[2] + $cost[3] + RV32I::OperationFormat::RRR_1.getSize();
}
=
{
  RV32::DEBUG_RULE_ACTION( "reg: tpm_BinaryExpSHR( reg, reg )", $1 );

  auto t = $2->getExp()->getType().getType();
  auto &reg2 = $action[2]();
  auto &reg3 = $action[3]();

  auto &r = RVINSTRUCTIONS.createReg();

  if ( RVIR_CONFIGURATION->arithmeticSHR )
    if ( t == IR_Type::UNSIGNED_INT )
      RVINSTRUCTIONS.insertSRL( r, reg2, reg3, $1->getExp() );
    else
      RVINSTRUCTIONS.insertSRA( r, reg2, reg3, $1->getExp() );
  else
    RVINSTRUCTIONS.insertSRL( r, reg2, reg3, $1->getExp() );

  return( r );
};


reg: tpm_BinaryExpSHR( reg, uconst5 )
{
  $cost[0] = $cost[2] + $cost[3] + RV32I::OperationFormat::RRC5_1.getSize();
}
=
{
  RV32::DEBUG_RULE_ACTION( "reg: tpm_BinaryExpSHR( reg, uconst5 )", $1 );

  auto t = $2->getExp()->getType().getType();
  auto &reg2 = $action[2]();
  auto v = $action[3]().getIntValue();

  auto &r = RVINSTRUCTIONS.createReg();
  if ( RVIR_CONFIGURATION->arithmeticSHR )
    if ( t == IR_Type::UNSIGNED_INT )
      RVINSTRUCTIONS.insertSRLI( r, reg2, v, $1->getExp() );
    else
      RVINSTRUCTIONS.insertSRAI( r, reg2, v, $1->getExp() );
  else
    RVINSTRUCTIONS.insertSRLI( r, reg2, v, $1->getExp() );

  return( r );
};


###############################################################################
#
#
# Unary expressions
#
#
###############################################################################

const6: tpm_UnaryExpMINUS( const6 )
{
  $cost[0] = $cost[2];
}
=
{
  RV32::DEBUG_RULE_ACTION( "const6: tpm_UnaryExpMINUS( const6 )", $1 );

  return( -$action[2]() );
};


const12: tpm_UnaryExpMINUS( const12 )
{
  $cost[0] = $cost[2];
}
=
{
  RV32::DEBUG_RULE_ACTION( "const12: tpm_UnaryExpMINUS( const12 )", $1 );

  return( -$action[2]() );
};


reg: tpm_UnaryExpMINUS( reg )
{
  $cost[0] = $cost[2] + RV32I::OperationFormat::RRC12_1.getSize();
}
=
{
  RV32::DEBUG_RULE_ACTION( "reg: tpm_UnaryExpMINUS( reg )", $1 );

  auto &reg2 = $action[2]();

  auto &r = RVINSTRUCTIONS.createReg();
  auto &x0 = RVINSTRUCTIONS.createReg();
  RV32::bindToPHREG( x0, 0 );

  RVINSTRUCTIONS.insertSUB( r, x0, reg2, $1->getExp() );

  return( r );
};


const6: tpm_UnaryExpPLUS( const6 )
{
  $cost[0] = $cost[2];
}
=
{
  RV32::DEBUG_RULE_ACTION( "const6: tpm_UnaryExpPLUS( const6 )", $1 );

  return( $action[2]() );
};


const12: tpm_UnaryExpPLUS( const12 )
{
  $cost[0] = $cost[2];
}
=
{
  RV32::DEBUG_RULE_ACTION( "const12: tpm_UnaryExpPLUS( const12 )", $1 );

  return( $action[2]() );
};


reg: tpm_UnaryExpPLUS( reg )
{
  $cost[0] = $cost[2];
}
=
{
  RV32::DEBUG_RULE_ACTION( "reg: tpm_UnaryExpPLUS( reg )", $1 );

  return( $action[2]() );
};


reg: tpm_UnaryExpPOSTDEC( reg )
{
  $cost[0] =
    $cost[2] + RVINSTRUCTIONS.getCMVCost() +
    RV32I::OperationFormat::RRC12_1.getSize();
}
=
{
  RV32::DEBUG_RULE_ACTION( "reg: tpm_UnaryExpPOSTDEC( reg )", $1 );

  auto &reg2 = $action[2]();

  auto &r = RVINSTRUCTIONS.createReg();
  RVINSTRUCTIONS.insertCMV( r, reg2, $1->getExp() );

  RVINSTRUCTIONS.insertADDI( reg2, reg2, -1, $1->getExp() );

  return( r );
};


reg: tpm_UnaryExpPOSTINC( reg )
{
  $cost[0] =
    $cost[2] + RVINSTRUCTIONS.getCMVCost() +
    RV32I::OperationFormat::RRC12_1.getSize();
}
=
{
  RV32::DEBUG_RULE_ACTION( "reg: tpm_UnaryExpPOSTINC( reg )", $1 );

  auto &reg2 = $action[2]();

  auto &r = RVINSTRUCTIONS.createReg();
  RVINSTRUCTIONS.insertCMV( r, reg2, $1->getExp() );

  RVINSTRUCTIONS.insertADDI( reg2, reg2, 1, $1->getExp() );

  return( r );
};


reg: tpm_UnaryExpPREDEC( reg )
{
  $cost[0] = $cost[2] + RV32I::OperationFormat::RRC12_1.getSize();
}
=
{
  RV32::DEBUG_RULE_ACTION( "reg: tpm_UnaryExpPREDEC( reg )", $1 );

  auto &reg2 = $action[2]();

  RVINSTRUCTIONS.insertADDI( reg2, reg2, -1, $1->getExp() );

  return( reg2 );
};


reg: tpm_UnaryExpPREINC( reg )
{
  $cost[0] = $cost[2] + RV32I::OperationFormat::RRC12_1.getSize();
}
=
{
  RV32::DEBUG_RULE_ACTION( "reg: tpm_UnaryExpPREINC( reg )", $1 );

  auto &reg2 = $action[2]();

  RVINSTRUCTIONS.insertADDI( reg2, reg2, 1, $1->getExp() );

  return( reg2 );
};


###############################################################################
#
#
# Sizeof expressions
#
#
###############################################################################

uconst5: tpm_SizeOfExp
{
  int byteSize = RV32::computeSizeOf( $1 );

  if ( ( byteSize >= 0 ) &&
       ( byteSize <= (int) RV_Const5_Unsigned::getMaxValue( 5 ) ) )
    $cost[0] = 100;
  else
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION( "uconst5: tpm_SizeOfExp", $1 );

  return( RV32::computeSizeOf( $1 ) );
};


const6: tpm_SizeOfExp
{
  int byteSize = RV32::computeSizeOf( $1 );

  if ( ( byteSize >= RV_Const6_Signed::getMinValue( 6 ) ) &&
       ( byteSize <= RV_Const6_Signed::getMaxValue( 6 ) ) )
    $cost[0] = 100;
  else
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION( "const6: tpm_SizeOfExp", $1 );

  return( RV32::computeSizeOf( $1 ) );
};


uconst6: tpm_SizeOfExp
{
  int byteSize = RV32::computeSizeOf( $1 );

  if ( ( byteSize >= 0 ) &&
       ( byteSize <= (int) RV_Const6_Unsigned::getMaxValue( 6 ) ) )
    $cost[0] = 100;
  else
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION( "uconst6: tpm_SizeOfExp", $1 );

  return( RV32::computeSizeOf( $1 ) );
};


uconst8: tpm_SizeOfExp
{
  int byteSize = RV32::computeSizeOf( $1 );

  if ( ( byteSize >= 0 ) &&
       ( byteSize <= (int) RV_Const8_Unsigned::getMaxValue( 8 ) ) )
    $cost[0] = 100;
  else
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION( "uconst8: tpm_SizeOfExp", $1 );

  return( RV32::computeSizeOf( $1 ) );
};


const12: tpm_SizeOfExp
{
  int byteSize = RV32::computeSizeOf( $1 );

  if ( ( byteSize >= RV_Const12_Signed::getMinValue( 12 ) ) &&
       ( byteSize <= RV_Const12_Signed::getMaxValue( 12 ) ) )
    $cost[0] = 100;
  else
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION( "const12: tpm_SizeOfExp", $1 );

  return( RV32::computeSizeOf( $1 ) );
};


uconst20: tpm_SizeOfExp
{
  int byteSize = RV32::computeSizeOf( $1 );

  if ( ( byteSize >= 0 ) &&
       ( byteSize <= (int) RV_Const20_Unsigned::getMaxValue( 20 ) ) )
    $cost[0] = 100;
  else
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION( "uconst20: tpm_SizeOfExp", $1 );

  return( RV32::computeSizeOf( $1 ) );
};


reg: tpm_SizeOfExp
{
  // TODO: This rule should be deleted and replaced by one that returns an
  //       integer constant (which would be the rule that produces 'const16')
  //       plus a universal conversion rule 'reg: const16'. The same scheme
  //       should be applied to the rule 'reg: tpm_IntConstExp' to improve the
  //       modularity of the code.

  long long constValue = RV32::computeSizeOf( $1 );

  $cost[0] = 10000 + RVINSTRUCTIONS.getMOVConstantCost( constValue );
}
=
{
  RV32::DEBUG_RULE_ACTION( "reg: tpm_SizeOfExp", $1 );

  long long constValue = RV32::computeSizeOf( $1 );

  auto &r = RVINSTRUCTIONS.createReg();
  RVINSTRUCTIONS.insertMOVConstant( r, constValue, $1->getExp() );

  return( r );
};


#####################################################################
#
#
# If-then- and if-else statements
#
#
#####################################################################

stmt: tpm_IfStmt( nrel )
{
  $cost[0] = $cost[2];
}
=
{
  RV32::DEBUG_RULE_ACTION( "stmt: tpm_IfStmt( nrel )", $1 );

  auto &b = RVCODESEL.getWIRBlock(
    dynamic_cast<IR_IfStmt *>( $1->getStmt() )->getContinueBasicBlock() );

  $action[2]( b, false );

  return;
};


stmt: tpm_IfStmt( reg )
{
  $cost[0] = $cost[2] + RV32I::OperationFormat::RRL_1.getSize();
}
=
{
  RV32::DEBUG_RULE_ACTION( "stmt: tpm_IfStmt( reg )", $1 );

  auto &reg2 = $action[2]();

  auto &x0 = RVINSTRUCTIONS.createReg();
  RV32::bindToPHREG( x0, 0 );

  RVINSTRUCTIONS.insertBEQ(
    reg2, x0,
    RVCODESEL.getWIRBlock(
      dynamic_cast<IR_IfStmt *>( $1->getStmt() )->getContinueBasicBlock() ),
    $1->getExp(), RV32::RV32_InstructionFactory::IF_STMT );
};


stmt: tpm_IfElseStmt( nrel )
{
  $cost[0] = $cost[2];
}
=
{
  RV32::DEBUG_RULE_ACTION( "stmt: tpm_IfElseStmt( nrel )", $1 );

  auto &b = RVCODESEL.getWIRBlock(
    dynamic_cast<IR_IfElseStmt *>( $1->getStmt() )->getFalseBasicBlock() );

  $action[2]( b, false );

  return;
};


stmt: tpm_IfElseStmt( reg )
{
  $cost[0] = $cost[2] + RV32I::OperationFormat::RRL_1.getSize();
}
=
{
  RV32::DEBUG_RULE_ACTION( "stmt: tpm_IfElseStmt( reg )", $1 );

  auto &reg2 = $action[2]();

  auto &x0 = RVINSTRUCTIONS.createReg();
  RV32::bindToPHREG( x0, 0 );

  RVINSTRUCTIONS.insertBEQ(
    reg2, x0,
    RVCODESEL.getWIRBlock(
      dynamic_cast<IR_IfElseStmt *>( $1->getStmt() )->getFalseBasicBlock() ),
    $1->getExp(), RV32::RV32_InstructionFactory::IFELSE_STMT );
};


#####################################################################
#
#
# For-loop
#
#
#####################################################################

stmt: tpm_ForStmt( nrel )
{
  $cost[0] = $cost[2];
}
=
{
  RV32::DEBUG_RULE_ACTION( "stmt: tpm_ForStmt( nrel )", $1 );

  auto *loop = dynamic_cast<IR_LoopStmt *>( $1->getStmt()->getParent() );
  auto &b = RVCODESEL.getWIRBlock( loop->getFalseBasicBlock() );

  $action[2]( b, true );

  return;
};


stmt: tpm_ForStmt( reg )
{
  $cost[0] = $cost[2] + RV32I::OperationFormat::RRL_1.getSize();
}
=
{
  RV32::DEBUG_RULE_ACTION( "stmt: tpm_ForStmt( reg )", $1 );

  auto &reg2 = $action[2]();

  auto &x0 = RVINSTRUCTIONS.createReg();
  RV32::bindToPHREG( x0, 0 );

  RVINSTRUCTIONS.insertBEQ(
    reg2, x0,
    RVCODESEL.getWIRBlock(
      dynamic_cast<IR_LoopStmt *>(
        $1->getStmt()->getParent() )->getFalseBasicBlock() ),
    $1->getExp(), RV32::RV32_InstructionFactory::FOR_STMT );

  WIR_Operation &jmp = RV32::RV32_wirBB->rbegin()->get().begin()->get();
  jmp.insertContainer( WIR_LoopExit( true ) );
};


#####################################################################
#
#
# While-do-loop
#
#
#####################################################################

stmt: tpm_WhileStmt( nrel )
{
  $cost[0] = $cost[2];
}
=
{
  RV32::DEBUG_RULE_ACTION( "stmt: tpm_WhileStmt( nrel )", $1 );

  auto *loop = dynamic_cast<IR_LoopStmt *>( $1->getStmt() );
  auto &b = RVCODESEL.getWIRBlock( loop->getFalseBasicBlock() );

  $action[2]( b, true );

  return;
};


stmt: tpm_WhileStmt( reg )
{
  $cost[0] = $cost[2] + RV32I::OperationFormat::RRL_1.getSize();
}
=
{
  RV32::DEBUG_RULE_ACTION( "stmt: tpm_WhileStmt( reg )", $1 );

  auto &reg2 = $action[2]();

  auto &x0 = RVINSTRUCTIONS.createReg();
  RV32::bindToPHREG( x0, 0 );

  RVINSTRUCTIONS.insertBEQ(
    reg2, x0,
    RVCODESEL.getWIRBlock(
      dynamic_cast<IR_LoopStmt *>( $1->getStmt() )->getFalseBasicBlock() ),
    $1->getExp(), RV32::RV32_InstructionFactory::WHILE_STMT );

  WIR_Operation &jmp = RV32::RV32_wirBB->rbegin()->get().begin()->get();
  jmp.insertContainer( WIR_LoopExit( true ) );
};


#####################################################################
#
#
# Do-while-loop
#
#
#####################################################################

stmt: tpm_DoWhileStmt( rel )
{
  $cost[0] = $cost[2];
}
=
{

  RV32::DEBUG_RULE_ACTION( "stmt: tpm_DoWhileStmt( rel )", $1 );

  auto *loop = dynamic_cast<IR_LoopStmt *>( $1->getStmt() );
  auto &b = RVCODESEL.getWIRBlock( loop->getTrueBasicBlock() );

  $action[2]( b, true );

  return;
};


stmt: tpm_DoWhileStmt( reg )
{
  $cost[0] = $cost[2] + RV32I::OperationFormat::RRL_1.getSize();
}
=
{
  RV32::DEBUG_RULE_ACTION( "stmt: tpm_DoWhileStmt( reg )", $1 );

  auto &reg2 = $action[2]();

  auto &x0 = RVINSTRUCTIONS.createReg();
  RV32::bindToPHREG( x0, 0 );

  RVINSTRUCTIONS.insertBNE(
    reg2, x0,
    RVCODESEL.getWIRBlock(
      dynamic_cast<IR_LoopStmt *>( $1->getStmt() )->getTrueBasicBlock() ),
    $1->getExp(), RV32::RV32_InstructionFactory::DOWHILE_STMT );

  WIR_Operation &jmp = RV32::RV32_wirBB->rbegin()->get().begin()->get();
  jmp.insertContainer( WIR_LoopExit( false ) );
};


#####################################################################
#
#
# Selection
#
#
#####################################################################

reg: tpm_CondExp( nrel, reg, reg )
{
  $cost[0] =
    $cost[2] + $cost[3] + $cost[4] + 2 * RVINSTRUCTIONS.getCMVCost() +
    RV32I::OperationFormat::RL_1.getSize();
}
=
{
  RV32::DEBUG_RULE_ACTION( "reg: tpm_CondExp( nrel, reg, reg )", $1 );

  auto *currentBB = RV32::RV32_wirBB;
  // Create basic block for LHS argument of ':' operator.
  auto &b1 =
    RVCODESEL.startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  // Create basic block for RHS argument of ':' operator.
  auto &b2 =
    RVCODESEL.startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  // Create basic block for code after the '?' operator.
  auto &b3 =
    RVCODESEL.startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  RV32::RV32_wirBB = currentBB;

  // Generate code for the condition in the current basic block.
  $action[2]( b2, false );

  RV32::RV32_wirBB = &b1;

  // Generate code for LHS argument of ':' operator.
  auto &reg3 = $action[3]();

  auto &r = RVINSTRUCTIONS.createReg();
  if ( $0->getExp()->getType().getType() != IR_Type::VOID )
    RVINSTRUCTIONS.insertCMV( r, reg3, $1->getExp() );
  RVINSTRUCTIONS.insertJ( b3, $1->getExp() );

  // Switch to basic block for RHS argument of ':' operator.
  RV32::RV32_wirBB = &b2;

  // Generate code for RHS argument of ':' operator.
  auto &reg4 = $action[4]();

  if ( $0->getExp()->getType().getType() != IR_Type::VOID )
    RVINSTRUCTIONS.insertCMV( r, reg4, $1->getExp() );

  // Switch to basic block after the '?' operator.
  RV32::RV32_wirBB = &b3;

  return( r );
};


reg: tpm_CondExp( reg, reg, reg )
{
  $cost[0] =
    $cost[2] + $cost[3] + $cost[4] + RV32I::OperationFormat::RRL_1.getSize()
    + 2 * RVINSTRUCTIONS.getCMVCost() + RV32I::OperationFormat::RL_1.getSize();
}
=
{
  RV32::DEBUG_RULE_ACTION( "reg: tpm_CondExp( reg, reg, reg )", $1 );

  // Generate code for the condition in the current basic block.
  auto &reg2 = $action[2]();

  auto &x0 = RVINSTRUCTIONS.createReg();
  RV32::bindToPHREG( x0, 0 );

  // Create basic block for LHS argument of ':' operator.
  auto *currentBB = RV32::RV32_wirBB;
  auto &b1 =
    RVCODESEL.startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  // Create basic block for RHS argument of ':' operator.
  auto &b2 =
    RVCODESEL.startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  // Create basic block for code after the '?' operator.
  auto &b3 =
    RVCODESEL.startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  RV32::RV32_wirBB = currentBB;

  // Generate conditional branch in the current basic block.
  RVINSTRUCTIONS.insertBEQ( reg2, x0, b2, $1->getExp() );

  // Switch to basic block for LHS argument of ':' operator.
  RV32::RV32_wirBB = &b1;

  // Generate code for LHS argument of ':' operator.
  auto &reg3 = $action[3]();

  auto &r = RVINSTRUCTIONS.createReg();

  if ( $0->getExp()->getType().getType() != IR_Type::VOID )
    RVINSTRUCTIONS.insertCMV( r, reg3, $1->getExp() );
  RVINSTRUCTIONS.insertJ(
    b3, $1->getExp(), RV32::RV32_InstructionFactory::JUMP_STMT );

  // Switch to basic block for RHS argument of ':' operator.
  RV32::RV32_wirBB = &b2;

  // Generate code for RHS argument of ':' operator.
  auto &reg4 = $action[4]();

  if ( $0->getExp()->getType().getType() != IR_Type::VOID )
    RVINSTRUCTIONS.insertCMV( r, reg4, $1->getExp() );

  // Switch to basic block after the '?' operator.
  RV32::RV32_wirBB = &b3;

  return( r );
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
  $cost[0] = RV32I::OperationFormat::RL_1.getSize();
}
=
{
  RV32::DEBUG_RULE_ACTION( "stmt: tpm_ImplicitJump", $1 );

  // Generate new internal basic block for this implicit jump.
  RVCODESEL.startNewBasicBlock( *$1->getBasicBlock() );
  RVINSTRUCTIONS.insertJ(
    RVCODESEL.getWIRBlock( $1->getBasicBlock()->getImplicitSucc() ),
    $1->getExp(), RV32::RV32_InstructionFactory::JUMP_STMT );

  return;
};


stmt: tpm_JumpStmt
{
  $cost[0] = RV32I::OperationFormat::RL_1.getSize();
}
=
{
  RV32::DEBUG_RULE_ACTION( "stmt: tpm_JumpStmt", $1 );

  RVINSTRUCTIONS.insertJ(
    RVCODESEL.getWIRBlock( $1->getStmt()->getBasicBlock()->getImplicitSucc() ),
    $1->getExp(), RV32::RV32_InstructionFactory::JUMP_STMT );

  if ( dynamic_cast<IR_BreakStmt *>( $1->getStmt() ) ) {
    // From the current break statement upwards, check whether the break jumps
    // out of a loop or out of a switch statement.
    bool breakOutOfLoop = false;
    auto *currentStmt = $1->getStmt();

    while ( currentStmt ) {
      if ( dynamic_cast<IR_LoopStmt *>( currentStmt ) ) {
        breakOutOfLoop = true;
        break;
      } else

      if ( dynamic_cast<IR_SwitchStmt *>( currentStmt ) )
        break;
      else
        currentStmt = currentStmt->getParent();
    }

    if ( breakOutOfLoop ) {
      WIR_Operation &jmp = RV32::RV32_wirBB->rbegin()->get().begin()->get();
      jmp.insertContainer( WIR_Break() );
    }
  }

  return;
};


###############################################################################
#
#
# Switch, case and default statements
#
#
###############################################################################

stmt: tpm_SwitchStmt( reg )
{
  auto *stmt = dynamic_cast<IR_SwitchStmt *>( $1->getStmt() );
  $cost[0] = $cost[2] + RV32I::OperationFormat::RL_1.getSize();

  for ( auto &c : stmt->getCases() ) {
    int caseLabelValue = c.first.getIntValue();

    $cost[0] +=
      RVINSTRUCTIONS.getMOVConstantCost( caseLabelValue ) +
      RV32I::OperationFormat::RRL_1.getSize();
  }
}
=
{
  RV32::DEBUG_RULE_ACTION( "stmt: tpm_SwitchStmt( reg )", $1 );

  auto *stmt = dynamic_cast<IR_SwitchStmt *>( $1->getStmt() );

  auto &reg2 = $action[2]();

  // Traverse through all case statements and generate jump (and possibly move)
  // instructions.
  for ( auto &c : stmt->getCases() ) {
    int caseLabelValue = c.first.getIntValue();

    auto &r = RVINSTRUCTIONS.createReg();
    RVINSTRUCTIONS.insertMOVConstant( r, caseLabelValue, $1->getExp() );

    RVINSTRUCTIONS.insertBEQ(
      reg2, r, RVCODESEL.getWIRBlock( c.second->getBasicBlock() ),
      $1->getExp(), RV32::RV32_InstructionFactory::SWITCH_STMT );
    RVCODESEL.startNewBasicBlock( *$1->getStmt()->getBasicBlock() );
  }

  // Finally, emit an unconditional jump to either the basic block of the
  // default label or the succeeding basic block of the switch-statement (see
  // IR_SwitchStmt class description).
  RVINSTRUCTIONS.insertJ(
    RVCODESEL.getWIRBlock(
      stmt->getDefault() ?
        stmt->getDefault()->getBasicBlock() : stmt->getContinueBasicBlock() ),
    $1->getExp(), RV32::RV32_InstructionFactory::SWITCH_STMT );
};


stmt: tpm_CaseStmt
{
  // There is nothing to do here, since a case label starts a new basic block
  // whose label is already automatically emitted.
  $cost[0] = 0;
}
=
{
  RV32::DEBUG_RULE_ACTION( "stmt: tpm_CaseStmt", $1 );

  return;
};


stmt: tpm_DefaultStmt
{
  // There is nothing to do here, since a case label starts a new basic block
  // whose label is already automatically emitted.
  $cost[0] = 0;
}
=
{
  RV32::DEBUG_RULE_ACTION( "stmt: tpm_DefaultStmt", $1 );

  return;
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
  // This rule generates code for direct function calls.
  if ( RV32::isFunctionType( *$1->getExp() ) )
    $cost[0] =
      $cost[1] + RV32I::OperationFormat::RL_1.getSize() +
      RV32I::OperationFormat::RR_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION( "called_function: tpm_SymbolExp", $1 );

  auto &sym = dynamic_cast<IR_SymbolExp *>( $1->getExp() )->getSymbol();

  RVINSTRUCTIONS.insertCALL( dynamic_cast<IR_FunctionSymbol &>( sym ), args,
                             $1->getExp() );
  WIR_Operation &call = RV32::RV32_wirBB->rbegin()->get().begin()->get();

  // aiT expects that CRL2 basic blocks end with a CALL instruction, since it
  // is unknown if the called function will ever return. An example are the
  // paired C library functions setjmp/longjmp, where the control flow from the
  // longjmp returns to the point of the dynamically enclosing setjmp() call
  // (see Muchnick p.175). Thus, we start a new basic block after a call.
  RVCODESEL.startNewBasicBlock();

  if ( returnBehaviour == RV32::RegType::REGISTER ) {
    auto &r = RVINSTRUCTIONS.createReg();
    auto &x10 = RVINSTRUCTIONS.createReg();
    RV32::bindToPHREG( x10, 10 );

    RVINSTRUCTIONS.insertCMV( r, x10, $1->getExp() );
    call.pushBackParameter( WIR_RegisterParameter( x10, WIR_Usage::def,
                            true ) );
    return( &r );
  }

  return( (WIR_BaseRegister *) nullptr );
};


called_function: reg
{
  // This rule generates code for indirect function calls.
  if ( RV32::isFunctionPointer( *$1->getExp() ) ||
       RV32::isFunctionType( *$1->getExp() ) )
    $cost[0] =
      $cost[1] + RV32I::OperationFormat::RL_1.getSize() +
      RV32I::OperationFormat::RR_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION( "called_function: reg", $1 );

  auto &p = $action[1]();

  RVINSTRUCTIONS.insertCALLI( p, args, $1->getExp() );
  WIR_Operation &call = RV32::RV32_wirBB->rbegin()->get().begin()->get();

  // aiT expects that CRL2 basic blocks end with a CALL instruction, since it
  // is unknown if the called function will ever return. An example are the
  // paired C library functions setjmp/longjmp, where the control flow from the
  // longjmp returns to the point of the dynamically enclosing setjmp() call
  // (see Muchnick p.175). Thus, we start a new basic block after a call.
  RVCODESEL.startNewBasicBlock();

  if ( returnBehaviour == RV32::RegType::REGISTER ) {
    auto &r = RVINSTRUCTIONS.createReg();
    auto &x10 = RVINSTRUCTIONS.createReg();
    RV32::bindToPHREG( x10, 10 );

    RVINSTRUCTIONS.insertCMV( r, x10, $1->getExp() );
    call.pushBackParameter( WIR_RegisterParameter( x10, WIR_Usage::def,
                            true ) );
    return( &r );
  }

  return( (WIR_BaseRegister *) nullptr );
};


reg: tpm_CallExp( called_function, arg )
{
  $cost[0] = $cost[2] + $cost[3];
}
=
{
  RV32::DEBUG_RULE_ACTION( "reg: tpm_CallExp( called_function, arg )", $1 );

  auto *theCall = dynamic_cast<IR_CallExp *>( $1->getExp() );

  // For type conversions of complex types, certain library routines might need
  // to be invoked. This cannot be done while the argument vector is mapped into
  // regs or onto the stack, since those invocations would overwrite the already
  // assigned registers. Because of this, a two-pass approach is necessary.
  RV32::argList args;
  RV32::argList dryArgs;

  $action[3]( true, 0, 0, theCall, args, dryArgs );
  $action[3]( false, 0, 0, theCall, args, dryArgs );

  auto p =
    ( $0->getExp()->getType().getType() == IR_Type::VOID ) ?
    $action[2]( args, RV32::RegType::NO_REGISTER ) :
    $action[2]( args, RV32::RegType::REGISTER );

  if ( $0->getExp()->getType().getType() == IR_Type::VOID )
    return( RV32::dummyRegV );
  else
    return( ref( dynamic_cast<RV_RegV &>( *p ) ) );
};


arg: tpm_CallExpARG( reg, arg )
{
  // Functions in prototype form are not supported yet, because in that case,
  // the TriCore ABI forces us to derive a proper function type from the actual
  // parameters, which possibly means from the actual parameters in all places
  // where the function is called. This is not done yet.
  auto *cexp = dynamic_cast<IR_CallExp *>( $2->getExp()->getParent() );

  if ( cexp->getFunctionType().isPrototypeForm() )
    $cost[0] = $cost[2] + $cost[3] + RV32I::OperationFormat::RR_1.getSize()
               + RV32I::OperationFormat::RLR_2.getSize();
  else {
    throw ufFatalError( "Functions without prototype are not supported." );
    $cost[0] = COST_INFINITY;
  }
}
=
{
  RV32::DEBUG_RULE_ACTION( "arg: tpm_CallExpARG( reg, arg )", $1 );

  int incr = 0;
  if ( dryrun ) {

    // If the type is integral and smaller than 32 bits, we would need to
    // convert to the respective signed/unsigned 32-bit integer type before
    // actually passing the argument. Fortunately, our register representation
    // of such types (char / short) is always sign-extending those types to 32
    // bits so that we don't need to cast here.

    auto &p = $action[2]();
    dryArgs.push_back( p );
  } else {
    int regNo = RV32::RV32_Stack::isPassedThroughRegister(
                  theCall->getFunctionType(), index );
    auto &r = dynamic_cast<RV_RegV &>( dryArgs.front().get() );
    dryArgs.pop_front();

    if ( regNo != -1 ) {
      // Pass argument via registers.
      auto &phreg =
        dynamic_cast<RV_RegV &>( RV32::getFctArgReg( *theCall, index, regNo ) );
      RVINSTRUCTIONS.insertMOV( phreg, r, $1->getExp() );
      args.push_back( phreg );
    } else {
      // Pass argument via stack.
      incr = RV32::RV32_Stack::getStackSize(
               RV32::effectiveType( *$2->getExp() ) );
      auto &sp = RVINSTRUCTIONS.createReg();
      RV32::bindToPHREG( sp, 2 );

      // TODO fp version might not be used since sp version is cleaner
      // Calculate frame pointer relative offset using the stack frame size
      // and the offset given in relation to the stack pointer
      // auto *cexp = dynamic_cast<IR_CallExp *>( $2->getExp()->getParent() );
      // IR_Function *irFunc = cexp->getFunctionSymbol()->getFunction();
      // int stackFrameSize = RVCODESEL.getStack().getStackFrameSize( *irFunc );
      // int paramFrameSize = RVCODESEL.getStack().getParameterStackFrameSize(
      //                        irFunc->getSymbol().getType() );
      // int stackAdjOffset = stackFrameSize - paramFrameSize;
      // int fpOffset = offset - stackAdjOffset + 8;
      // auto &fp = RVINSTRUCTIONS.createReg();
      // RV32::RV32_wirFct->insertPrecolor( fp, RV32::RV32_wirProc->x8() );
      // RVINSTRUCTIONS.insertSW( r, fpOffset, fp, $1->getExp());

      RVINSTRUCTIONS.insertSW( r, offset, sp, $1->getExp());

      // Mark access to overflow region for register allocator.
      RV32::RV32_wirBB->rbegin()->get().begin()->
        get().getExplicitParameter(3).setDontOptimize();
    }
  }
  $action[3]( dryrun, index + 1, offset + incr, theCall, args, dryArgs );
  return;
};


arg: tpm_CallExpNOARG
{
  $cost[0] = 0;
}
=
{
  RV32::DEBUG_RULE_ACTION( "arg: tpm_CallExpNOARG", $1 );

  return;
};


stmt: tpm_ReturnStmtVOID
{
  list<int> calleeSavedRegs = RV32::RV32_Stack::getCalleeSavedRegs();
  int calleeSavedSize = calleeSavedRegs.size();

  $cost[0] = RVINSTRUCTIONS.getCMVCost() +
             calleeSavedSize * RV32I::OperationFormat::RC12R_1.getSize() +
             2 * RV32I::OperationFormat::RRC12_1.getSize();
}
=
{
  RV32::DEBUG_RULE_ACTION( "stmt: tpm_ReturnStmtVOID", $1 );

  auto &sp = RVINSTRUCTIONS.createReg();
  RV32::bindToPHREG( sp, 2 );
  auto &fp = RVINSTRUCTIONS.createReg();
  RV32::bindToPHREG( fp, 8 );

  //Number of callee saved registers and resulting sp offset
  list<int> calleeSavedRegs = RV32::RV32_Stack::getCalleeSavedRegs();
  int calleeSavedSize = calleeSavedRegs.size();
  ufAssertT( calleeSavedSize >= 2,
    "At least the ra and fp have to be saved by the callee!" );
  int offset = -(calleeSavedSize * 4);

  //Create temporary frame pointer since orginial one is overwritten next
  auto &tempFP = RVINSTRUCTIONS.createReg();
  RVINSTRUCTIONS.insertCMV( tempFP, fp, $1->getExp() );

  //Restore callee saved registers in the reverse order
  //they were put on the stack
  calleeSavedRegs.reverse();
  for ( int nSreg : calleeSavedRegs ) {
    auto &sreg = RVINSTRUCTIONS.createReg();
    RV32::bindToPHREG( sreg, nSreg );
    RVINSTRUCTIONS.insertLW( sreg, offset + 8, tempFP, $1->getExp() );
    offset += 4; // decrement before
  }
  ufAssertT( offset == 0,
    "Illegal stack pointer offset after pushing function arguments!" );

  //Set back sp
  RVINSTRUCTIONS.insertADDI( sp, tempFP, 8 , $1->getExp() );

  RVINSTRUCTIONS.insertRETURN( $1->getExp(),
    RV32::RV32_InstructionFactory::VOID_STMT );
  return;
};


stmt: tpm_ReturnStmt( reg )
{
  list<int> calleeSavedRegs = RV32::RV32_Stack::getCalleeSavedRegs();
  int calleeSavedSize = calleeSavedRegs.size();

  $cost[0] = $cost[2] + 2 * RVINSTRUCTIONS.getCMVCost() +
             calleeSavedSize * RV32I::OperationFormat::RC12R_1.getSize() +
             2 * RV32I::OperationFormat::RRC12_1.getSize();
}
=
{
  RV32::DEBUG_RULE_ACTION( "stmt: tpm_ReturnStmt( reg )", $1 );

  auto &reg = $action[2]();

  auto &sp = RVINSTRUCTIONS.createReg();
  RV32::bindToPHREG( sp, 2 );
  auto &fp = RVINSTRUCTIONS.createReg();
  RV32::bindToPHREG( fp, 8 );
  auto &x10 = RVINSTRUCTIONS.createReg();
  RV32::bindToPHREG( x10, 10 );

  // Move result into return value register
  RVINSTRUCTIONS.insertCMV( x10, reg, $1->getExp() );

  //Number of callee saved registers and resulting sp offset
  list<int> calleeSavedRegs = RV32::RV32_Stack::getCalleeSavedRegs();
  int calleeSavedSize = calleeSavedRegs.size();
  ufAssertT( calleeSavedSize >= 2,
    "At least the ra and fp have to be saved by the callee!" );
  int offset = -(calleeSavedSize * 4);

  //Create temporary frame pointer since orginial one is overwritten next
  auto &tempFP = RVINSTRUCTIONS.createReg();
  RVINSTRUCTIONS.insertCMV( tempFP, fp, $1->getExp() );

  //Restore callee saved registers in the reverse order
  //they were put on the stack
  calleeSavedRegs.reverse();
  for ( int nSreg : calleeSavedRegs ) {
    auto &sreg = RVINSTRUCTIONS.createReg();
    RV32::bindToPHREG( sreg, nSreg );
    RVINSTRUCTIONS.insertLW( sreg, offset + 8, tempFP, $1->getExp() );
    offset += 4; // decrement before
  }
  ufAssertT( offset == 0,
    "Illegal stack pointer offset after pushing function arguments!" );

  //Set back sp
  RVINSTRUCTIONS.insertADDI( sp, tempFP, 8 , $1->getExp() );

  RVINSTRUCTIONS.insertRET( x10, $1->getExp(),
    RV32::RV32_InstructionFactory::RETURN_STMT );
  return;
};



