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
# Statement rules
#
#
###############################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
stmt: tpm_ExpStmt( llereg )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "stmt: tpm_ExpStmt( llereg )", $1 );

  $action[2]();

  return;
};


###############################################################################
#
#
# Integer constants
#
#
###############################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
llconst: tpm_IntConstExp
{
  if ( isLongLongConstant( *dynamic_cast<IR_IntConstExp *>( $1->getExp() ) ) )
    $cost[0] = 0;
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "llconst: tpm_IntConstExp", $1 );

  return( dynamic_cast<IR_IntConstExp *>( $1->getExp() )->getValue() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
llereg: llconst
{
  long long lo = getLowerLongLongWord( $1->getExp() );
  long long hi = getUpperLongLongWord( $1->getExp() );

  $cost[0] = $cost[1];

  if ( ( lo >= TC_Const4_Signed::getMinValue( 4 ) ) &&
       ( lo <= TC_Const4_Signed::getMaxValue( 4 ) ) )
    $cost[0] += TC13::OperationFormat::SDC4_1.getSize();
  else

  if ( ( lo >= TC_Const16_Signed::getMinValue( 16 ) ) &&
       ( lo <= TC_Const16_Signed::getMaxValue( 16 ) ) )
    $cost[0] += TC13::OperationFormat::DC16_1.getSize();
  else

  if ( ( lo >= 0 ) && ( lo <= (int) TC_Const16_Unsigned::getMaxValue( 16 ) ) )
    $cost[0] += TC13::OperationFormat::DC16_2.getSize();
  else
    $cost[0] +=
      TC13::OperationFormat::DC16_2.getSize() +
      TC13::OperationFormat::DDC16_1.getSize();

  if ( ( hi >= TC_Const4_Signed::getMinValue( 4 ) ) &&
       ( hi <= TC_Const4_Signed::getMaxValue( 4 ) ) )
    $cost[0] += TC13::OperationFormat::SDC4_1.getSize();
  else

  if ( ( hi >= TC_Const16_Signed::getMinValue( 16 ) ) &&
       ( hi <= TC_Const16_Signed::getMaxValue( 16 ) ) )
    $cost[0] += TC13::OperationFormat::DC16_1.getSize();
  else

  if ( ( hi >= 0 ) && ( hi <= (int) TC_Const16_Unsigned::getMaxValue( 16 ) ) )
    $cost[0] += TC13::OperationFormat::DC16_2.getSize();
  else
    $cost[0] +=
      TC13::OperationFormat::DC16_2.getSize() +
      TC13::OperationFormat::DDC16_1.getSize();
}
=
{
  DEBUG_RULE_ACTION( "llereg: llconst", $1 );

  // Extract the two words as integer values.
  auto v = $action[1]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateERegister( "" );
  TCINSTRUCTIONS.insertMOVConstantLL( reg, &v, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createEReg();
  TCINSTRUCTIONS.insertMOVConstant( r, v, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


###############################################################################
#
#
# Simple expressions
#
#
###############################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
llereg: tpm_SymbolExp
{
  // Handles register 'llereg' symbols (non-stack, non-global).
  auto &symExp = dynamic_cast<IR_SymbolExp &>( *$1->getExp() );
  IR_Symbol &sym = symExp.getSymbol();

  if ( isLongLongType( sym ) && !sym.isGlobal() &&
       ( TCCODESEL->getStack()->getSymbolOffset( &sym ) < 0 ) )
    $cost[0] = loadRegisterSymbolCost( symExp );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "llereg: tpm_SymbolExp", $1 );

  auto *symExp = dynamic_cast<IR_SymbolExp *>( $1->getExp() );

  // LLIR
  LLIR_Register *reg = loadRegisterSymbol( *symExp );

  // WIR
  auto &r = dynamic_cast<TC_ERegV &>( loadRegSym( *symExp ) );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
deref_llereg: tpm_SymbolExp
{
  // Handles local stack 'llereg' symbols.
  IR_Symbol &sym = dynamic_cast<IR_SymbolExp *>( $1->getExp() )->getSymbol();

  if ( isLongLongType( sym ) && !sym.isGlobal() &&
       ( TCCODESEL->getStack()->getSymbolOffset( &sym ) >= 0 ) )
    $cost[0] = loadStackSymbolCost( static_cast<IR_SymbolExp>( sym ) );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "deref_llereg: tpm_SymbolExp", $1 );

  auto *symExp = dynamic_cast<IR_SymbolExp *>( $1->getExp() );
  return( loadStackSymbol( *symExp, loadResult ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
deref_llereg: tpm_SymbolExp
{
  // Handles global 'llereg' symbols.
  IR_Symbol &sym = dynamic_cast<IR_SymbolExp *>( $1->getExp() )->getSymbol();

  if ( isLongLongType( sym ) && sym.isGlobal() )
    $cost[0] = loadGlobalSymbolCost( static_cast<IR_SymbolExp>( sym ) );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "deref_llereg: tpm_SymbolExp", $1 );

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

# @author Heiko Falk <Heiko.Falk@tuhh.de>
llereg: tpm_BinaryExpPLUS( llereg, llereg )
{
  $cost[0] = $cost[2] + $cost[3] + 2 * TC13::OperationFormat::DDD_1.getSize();
}
=
{
  DEBUG_RULE_ACTION( "llereg: tpm_BinaryExpPLUS( llereg, llereg )", $1 );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateERegister( "" );
  TCINSTRUCTIONS.insertADDX(
    getLVLLChild( reg ), getLVLLChild( p1.first ), getLVLLChild( p2.first ),
    $1->getExp() );
  TCINSTRUCTIONS.insertADDC(
    getHVLLChild( reg ), getHVLLChild( p1.first ), getHVLLChild( p2.first ),
    $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createEReg();
  TCINSTRUCTIONS.insertADDX(
    dynamic_cast<TC_DRegV &>( r.begin()->get() ),
    dynamic_cast<TC_DRegV &>( p1.second.get().begin()->get() ),
    dynamic_cast<TC_DRegV &>( p2.second.get().begin()->get() ), $1->getExp() );
  TCINSTRUCTIONS.insertADDC(
    dynamic_cast<TC_DRegV &>( r.rbegin()->get() ),
    dynamic_cast<TC_DRegV &>( p1.second.get().rbegin()->get() ),
    dynamic_cast<TC_DRegV &>( p2.second.get().rbegin()->get() ), $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
llereg: tpm_BinaryExpMINUS( llereg, llereg )
{
  $cost[0] = $cost[2] + $cost[3] + 2 * TC13::OperationFormat::DDD_1.getSize();
}
=
{
  DEBUG_RULE_ACTION( "llereg: tpm_BinaryExpMINUS( llereg, llereg )", $1 );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateERegister( "" );
  TCINSTRUCTIONS.insertSUBX(
    getLVLLChild( reg ), getLVLLChild( p1.first ), getLVLLChild( p2.first ),
    $1->getExp() );
  TCINSTRUCTIONS.insertSUBC(
    getHVLLChild( reg ), getHVLLChild( p1.first ), getHVLLChild( p2.first ),
    $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createEReg();
  TCINSTRUCTIONS.insertSUBX(
    dynamic_cast<TC_DRegV &>( r.begin()->get() ),
    dynamic_cast<TC_DRegV &>( p1.second.get().begin()->get() ),
    dynamic_cast<TC_DRegV &>( p2.second.get().begin()->get() ), $1->getExp() );
  TCINSTRUCTIONS.insertSUBC(
    dynamic_cast<TC_DRegV &>( r.rbegin()->get() ),
    dynamic_cast<TC_DRegV &>( p1.second.get().rbegin()->get() ),
    dynamic_cast<TC_DRegV &>( p2.second.get().rbegin()->get() ), $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
llereg: tpm_BinaryExpMULT( llereg, llereg )
{
  $cost[0] =
    $cost[2] + $cost[3] + TC13::OperationFormat::EDD.getSize() +
    2 * TC13::OperationFormat::DDDD.getSize();
}
=
{
  DEBUG_RULE_ACTION( "llereg: tpm_BinaryExpMULT( llereg, llereg )", $1 );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateERegister( "" );
  TCINSTRUCTIONS.insertMUL_U(
    reg, getLVLLChild( p1.first ), getLVLLChild( p2.first ), $1->getExp() );
  TCINSTRUCTIONS.insertMADD(
    getHVLLChild( reg ), getHVLLChild( reg ), getLVLLChild( p1.first ),
    getHVLLChild( p2.first ), $1->getExp() );
  TCINSTRUCTIONS.insertMADD(
    getHVLLChild( reg ), getHVLLChild( reg ), getHVLLChild( p1.first ),
    getLVLLChild( p2.first ), $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createEReg();
  TCINSTRUCTIONS.insertMUL_U(
    r, dynamic_cast<TC_DRegV &>( p1.second.get().begin()->get() ),
    dynamic_cast<TC_DRegV &>( p2.second.get().begin()->get() ), $1->getExp() );
  TCINSTRUCTIONS.insertMADD(
    dynamic_cast<TC_DRegV &>( r.rbegin()->get() ),
    dynamic_cast<TC_DRegV &>( r.rbegin()->get() ),
    dynamic_cast<TC_DRegV &>( p1.second.get().begin()->get() ),
    dynamic_cast<TC_DRegV &>( p2.second.get().rbegin()->get() ), $1->getExp() );
  TCINSTRUCTIONS.insertMADD(
    dynamic_cast<TC_DRegV &>( r.rbegin()->get() ),
    dynamic_cast<TC_DRegV &>( r.rbegin()->get() ),
    dynamic_cast<TC_DRegV &>( p1.second.get().rbegin()->get() ),
    dynamic_cast<TC_DRegV &>( p2.second.get().begin()->get() ), $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
llereg: tpm_BinaryExpDIV( llereg, llereg )
{
  $cost[0] =
    $cost[2] + $cost[3] + 6 * TC13::OperationFormat::SDD_1.getSize() +
    TC13::OperationFormat::L.getSize();
}
=
{
  DEBUG_RULE_ACTION( "llereg: tpm_BinaryExpDIV( llereg, llereg )", $1 );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateERegister( "" );
  if ( $2->getExp()->getType().isUnsignedType() ||
       $3->getExp()->getType().isUnsignedType() )
    TCINSTRUCTIONS.insertDIV_ULL( reg, p1.first, p2.first, $1->getExp() );
  else
    TCINSTRUCTIONS.insertDIV_LL( reg, p1.first, p2.first, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createEReg();
  if ( $2->getExp()->getType().isUnsignedType() ||
       $3->getExp()->getType().isUnsignedType() )
    TCINSTRUCTIONS.insertDIV_ULL( r, p1.second, p2.second, $1->getExp() );
  else
    TCINSTRUCTIONS.insertDIV_LL( r, p1.second, p2.second, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
llereg: tpm_BinaryExpMOD( llereg, llereg )
{
  $cost[0] =
    $cost[2] + $cost[3] + 6 * TC13::OperationFormat::SDD_1.getSize() +
    TC13::OperationFormat::L.getSize();
}
=
{
  DEBUG_RULE_ACTION( "llereg: tpm_BinaryExpMOD( llereg, llereg )", $1 );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateERegister( "" );
  if ( $2->getExp()->getType().isUnsignedType() ||
       $3->getExp()->getType().isUnsignedType() )
    TCINSTRUCTIONS.insertMOD_ULL( reg, p1.first, p2.first, $1->getExp() );
  else
    TCINSTRUCTIONS.insertMOD_LL( reg, p1.first, p2.first, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createEReg();
  if ( $2->getExp()->getType().isUnsignedType() ||
       $3->getExp()->getType().isUnsignedType() )
    TCINSTRUCTIONS.insertMOD_ULL( r, p1.second, p2.second, $1->getExp() );
  else
    TCINSTRUCTIONS.insertMOD_LL( r, p1.second, p2.second, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


###############################################################################
#
#
# Logical AND and OR operators
#
#
###############################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpLOGAND( llereg, llereg )
{
  $cost[0] =
    $cost[2] + $cost[3] + 2 * TC13::OperationFormat::DDC9_1.getSize() +
    2 * TC13::OperationFormat::DDC9_3.getSize() +
    TC13::OperationFormat::DC4L_1.getSize();
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpLOGAND( llereg, llereg )", $1 );

  /*
    Involves splitting of basic blocks. ANSI-C standards have been followed. If
    the LHS expression is false, the entire AND expression is treated as false
    and the RHS expression is never evaluated.
  */

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

  // WIR
  // Create new basic block immediately after this '&&' operator.
  auto *currentBB = TC179x_wirBB;
  auto &b =
    logAndEndBlock == nullptr ?
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() ) :
    *logAndEndBlock;
  bool newEndBlock = ( logAndEndBlock == nullptr );
  if ( isNestedLogAND ) {
    if ( logAndEndBlock == nullptr )
      logAndEndBlock = &b;
  } else
    logAndEndBlock = nullptr;
  TC179x_wirBB = currentBB;

  // Generate LHS operand.
  auto p1 = $action[2]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertNE( reg, getHVLLChild( p1.first ), 0, $1->getExp() );
  TCINSTRUCTIONS.insertOR_NE( reg, getLVLLChild( p1.first ), 0, $1->getExp() );
  TCINSTRUCTIONS.insertJEQ( reg, 0, jmpLabel, $1->getExp() );

  // WIR
  // Generate MOV and conditional JMP still in the current basic block.
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertNE(
    r, dynamic_cast<TC_DRegV &>( p1.second.get().rbegin()->get() ), 0,
    $1->getExp() );
  TCINSTRUCTIONS.insertOR_NE(
    r, dynamic_cast<TC_DRegV &>( p1.second.get().begin()->get() ), 0,
    $1->getExp() );
  TCINSTRUCTIONS.insertJEQ( r, 0, b, $1->getExp() );

  // LLIR
  // Insert new basic block for RHS operand.
  beginNewLLIRBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  // Create new basic block for RHS operand.
  TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );

  // Generate RHS operand.
  auto p2 = $action[3]();

  // LLIR
  TCINSTRUCTIONS.insertNE( reg, getHVLLChild( p2.first ), 0, $1->getExp() );
  TCINSTRUCTIONS.insertOR_NE( reg, getLVLLChild( p2.first ), 0, $1->getExp() );

  // Insert new basic block after the AND operation.
  beginNewLLIRBasicBlock(
    label0.c_str(), *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  TCINSTRUCTIONS.insertNE(
    r, dynamic_cast<TC_DRegV &>( p2.second.get().rbegin()->get() ), 0,
    $1->getExp() );
  TCINSTRUCTIONS.insertOR_NE(
    r, dynamic_cast<TC_DRegV &>( p2.second.get().begin()->get() ), 0,
    $1->getExp() );
  if ( newEndBlock ) {
    TC179x_wirBB = &b;
    logAndEndBlock = nullptr;
  }

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpLOGAND( llereg, dreg )
{
  $cost[0] =
    $cost[2] + $cost[3] + 2 * TC13::OperationFormat::DDC9_1.getSize() +
    TC13::OperationFormat::DDC9_3.getSize() +
    TC13::OperationFormat::DC4L_1.getSize();
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpLOGAND( llereg, dreg )", $1 );

  /*
    Involves splitting of basic blocks. ANSI-C standards have been followed. If
    the LHS expression is false, the entire AND expression is treated as false
    and the RHS expression is never evaluated.
  */

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

  // WIR
  // Create new basic block immediately after this '&&' operator.
  auto *currentBB = TC179x_wirBB;
  auto &b =
    logAndEndBlock == nullptr ?
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() ) :
    *logAndEndBlock;
  bool newEndBlock = ( logAndEndBlock == nullptr );
  if ( isNestedLogAND ) {
    if ( logAndEndBlock == nullptr )
      logAndEndBlock = &b;
  } else
    logAndEndBlock = nullptr;
  TC179x_wirBB = currentBB;

  // Generate LHS operand.
  auto p1 = $action[2]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertNE( reg, getHVLLChild( p1.first ), 0, $1->getExp() );
  TCINSTRUCTIONS.insertOR_NE( reg, getLVLLChild( p1.first ), 0, $1->getExp() );
  TCINSTRUCTIONS.insertJEQ( reg, 0, jmpLabel, $1->getExp() );

  // WIR
  // Generate MOV and conditional JMP still in the current basic block.
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertNE(
    r, dynamic_cast<TC_DRegV &>( p1.second.get().rbegin()->get() ), 0,
    $1->getExp() );
  TCINSTRUCTIONS.insertOR_NE(
    r, dynamic_cast<TC_DRegV &>( p1.second.get().begin()->get() ), 0,
    $1->getExp() );
  TCINSTRUCTIONS.insertJEQ( r, 0, b, $1->getExp() );

  // LLIR
  // Insert new basic block for RHS operand.
  beginNewLLIRBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  // Create new basic block for RHS operand.
  TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );

  // Generate RHS operand.
  auto p2 = $action[3]();

  // LLIR
  TCINSTRUCTIONS.insertNE( reg, p2.first, 0, $1->getExp() );

  // Insert new basic block after the AND operation.
  beginNewLLIRBasicBlock(
    label0.c_str(), *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  TCINSTRUCTIONS.insertNE( r, p2.second, 0, $1->getExp() );
  if ( newEndBlock ) {
    TC179x_wirBB = &b;
    logAndEndBlock = nullptr;
  }

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpLOGAND( dreg, llereg )
{
  $cost[0] =
    $cost[2] + $cost[3] + 2 * TC13::OperationFormat::DDC9_1.getSize() +
    TC13::OperationFormat::DDC9_3.getSize() +
    TC13::OperationFormat::DC4L_1.getSize();
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpLOGAND( dreg, llereg )", $1 );

  /*
    Involves splitting of basic blocks. ANSI-C standards have been followed. If
    the LHS expression is false, the entire AND expression is treated as false
    and the RHS expression is never evaluated.
  */

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

  // WIR
  // Create new basic block immediately after this '&&' operator.
  auto *currentBB = TC179x_wirBB;
  auto &b =
    logAndEndBlock == nullptr ?
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() ) :
    *logAndEndBlock;
  bool newEndBlock = ( logAndEndBlock == nullptr );
  if ( isNestedLogAND ) {
    if ( logAndEndBlock == nullptr )
      logAndEndBlock = &b;
  } else
    logAndEndBlock = nullptr;
  TC179x_wirBB = currentBB;

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
    reg = p1.first;
  else {
    reg = TCINSTRUCTIONS.CreateRegister( "" );
    TCINSTRUCTIONS.insertMOV( reg, p1.first, $1->getExp() );
  }

  TCINSTRUCTIONS.insertNE( reg, p1.first, 0, $1->getExp() );
  TCINSTRUCTIONS.insertJEQ( reg, 0, jmpLabel, $1->getExp() );

  // WIR
  // Generate MOV and conditional JMP still in the current basic block.
  // Small hack: The tree pattern matcher will match the rightmost operator
  //             first if confronted with a chain like
  //             a && b && c && d
  //             so the first match will be
  //             ( a && b && c ) && ( d )
  // Therefore, we can reuse the result register that is delivered by the LHS
  // as our target register, if the LHS is produced by a logical operator.
  auto &r = isNestedLogAND ? p1.second.get() : TCINSTRUCTIONS.createDReg();
  if ( !isNestedLogAND )
    TCINSTRUCTIONS.insertMOV( r, p1.second, $1->getExp() );
  TCINSTRUCTIONS.insertNE( r, p1.second, 0, $1->getExp() );
  TCINSTRUCTIONS.insertJEQ( r, 0, b, $1->getExp() );

  // LLIR
  // Insert new basic block for RHS operand.
  beginNewLLIRBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  // Create new basic block for RHS operand.
  TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );

  // Generate RHS operand.
  auto p2 = $action[3]();

  // LLIR
  TCINSTRUCTIONS.insertNE( reg, getHVLLChild( p2.first ), 0, $1->getExp() );
  TCINSTRUCTIONS.insertOR_NE( reg, getLVLLChild( p2.first ), 0, $1->getExp() );

  // Insert new basic block after the AND operation.
  beginNewLLIRBasicBlock(
    label0.c_str(), *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  TCINSTRUCTIONS.insertNE(
    r, dynamic_cast<TC_DRegV &>( p2.second.get().rbegin()->get() ), 0,
    $1->getExp() );
  TCINSTRUCTIONS.insertOR_NE(
    r, dynamic_cast<TC_DRegV &>( p2.second.get().begin()->get() ), 0,
    $1->getExp() );
  if ( newEndBlock ) {
    TC179x_wirBB = &b;
    logAndEndBlock = nullptr;
  }

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpLOGOR( llereg, llereg )
{
  $cost[0] =
    $cost[2] + $cost[3] + 2 * TC13::OperationFormat::DDC9_1.getSize() +
    2 * TC13::OperationFormat::DDC9_3.getSize() +
    TC13::OperationFormat::DC4L_1.getSize();
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpLOGOR( llereg, llereg )", $1 );

  /*
    Involves splitting of basic blocks. ANSI-C standards have been followed. If
    the LHS expression is true, the entire OR expression is treated as true and
    the RHS expression is never evaluated.
  */

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

  // WIR
  // Create new basic block immediately after this '||' operator.
  auto *currentBB = TC179x_wirBB;
  auto &b =
    logOrEndBlock == nullptr ?
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() ) :
    *logOrEndBlock;
  bool newEndBlock = ( logOrEndBlock == nullptr );
  if ( isNestedLogOR ) {
    if ( logOrEndBlock == nullptr )
      logOrEndBlock = &b;
  } else
    logOrEndBlock = nullptr;
  TC179x_wirBB = currentBB;

  // Generate LHS operand.
  auto p1 = $action[2]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertNE( reg, getHVLLChild( p1.first ), 0, $1->getExp() );
  TCINSTRUCTIONS.insertOR_NE( reg, getLVLLChild( p1.first ), 0, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( reg, 0, jmpLabel, $1->getExp() );

  // WIR
  // Generate conditional JMP still in the current basic block.
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertNE(
    r, dynamic_cast<TC_DRegV &>( p1.second.get().rbegin()->get() ), 0,
    $1->getExp() );
  TCINSTRUCTIONS.insertOR_NE(
    r, dynamic_cast<TC_DRegV &>( p1.second.get().begin()->get() ), 0,
    $1->getExp() );
  TCINSTRUCTIONS.insertJNE( r, 0, b, $1->getExp() );

  // LLIR
  // Insert new basic block for RHS operand.
  beginNewLLIRBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  // Create new basic block for RHS operand.
  TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );

  // Generate RHS operand.
  auto p2 = $action[3]();

  // LLIR
  TCINSTRUCTIONS.insertNE( reg, getHVLLChild( p2.first ), 0, $1->getExp() );
  TCINSTRUCTIONS.insertOR_NE( reg, getLVLLChild( p2.first ), 0, $1->getExp() );

  // Insert new basic block after the OR operation.
  beginNewLLIRBasicBlock(
    label0.c_str(), *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  TCINSTRUCTIONS.insertNE(
    r, dynamic_cast<TC_DRegV &>( p2.second.get().rbegin()->get() ), 0,
    $1->getExp() );
  TCINSTRUCTIONS.insertOR_NE(
    r, dynamic_cast<TC_DRegV &>( p2.second.get().begin()->get() ), 0,
    $1->getExp() );
  if ( newEndBlock ) {
    TC179x_wirBB = &b;
    logAndEndBlock = nullptr;
  }

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpLOGOR( llereg, dreg )
{
  $cost[0] =
    $cost[2] + $cost[3] + 2 * TC13::OperationFormat::DDC9_1.getSize() +
    TC13::OperationFormat::DDC9_3.getSize() +
    TC13::OperationFormat::DC4L_1.getSize();
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpLOGOR( llereg, dreg )", $1 );

  /*
    Involves splitting of basic blocks. ANSI-C standards have been followed. If
    the LHS expression is true, the entire OR expression is treated as true and
    the RHS expression is never evaluated.
  */

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

  // WIR
  // Create new basic block immediately after this '||' operator.
  auto *currentBB = TC179x_wirBB;
  auto &b =
    logOrEndBlock == nullptr ?
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() ) :
    *logOrEndBlock;
  bool newEndBlock = ( logOrEndBlock == nullptr );
  if ( isNestedLogOR ) {
    if ( logOrEndBlock == nullptr )
      logOrEndBlock = &b;
  } else
    logOrEndBlock = nullptr;
  TC179x_wirBB = currentBB;

  // Generate LHS operand.
  auto p1 = $action[2]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertNE( reg, getHVLLChild( p1.first ), 0, $1->getExp() );
  TCINSTRUCTIONS.insertOR_NE( reg, getLVLLChild( p1.first ), 0, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( reg, 0, jmpLabel, $1->getExp() );

  // WIR
  // Generate conditional JMP still in the current basic block.
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertNE(
    r, dynamic_cast<TC_DRegV &>( p1.second.get().rbegin()->get() ), 0,
    $1->getExp() );
  TCINSTRUCTIONS.insertOR_NE(
    r, dynamic_cast<TC_DRegV &>( p1.second.get().begin()->get() ), 0,
    $1->getExp() );
  TCINSTRUCTIONS.insertJNE( r, 0, b, $1->getExp() );

  // LLIR
  // Insert new basic block for RHS operand.
  beginNewLLIRBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  // Create new basic block for RHS operand.
  TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );

  // Generate RHS operand.
  auto p2 = $action[3]();

  // LLIR
  TCINSTRUCTIONS.insertNE( reg, p2.first, 0, $1->getExp() );

  // Insert new basic block after the OR operation.
  beginNewLLIRBasicBlock(
    label0.c_str(), *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  TCINSTRUCTIONS.insertNE( r, p2.second, 0, $1->getExp() );
  if ( newEndBlock ) {
    TC179x_wirBB = &b;
    logAndEndBlock = nullptr;
  }

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpLOGOR( dreg, llereg )
{
  $cost[0] =
    $cost[2] + $cost[3] + 2 * TC13::OperationFormat::DDC9_1.getSize() +
    TC13::OperationFormat::DDC9_3.getSize() +
    TC13::OperationFormat::DC4L_1.getSize();
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpLOGOR( dreg, llereg )", $1 );

  /*
    Involves splitting of basic blocks. ANSI-C standards have been followed. If
    the LHS expression is true, the entire OR expression is treated as true and
    the RHS expression is never evaluated.
  */

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

  // WIR
  // Create new basic block immediately after this '||' operator.
  auto *currentBB = TC179x_wirBB;
  auto &b =
    logOrEndBlock == nullptr ?
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() ) :
    *logOrEndBlock;
  bool newEndBlock = ( logOrEndBlock == nullptr );
  if ( isNestedLogOR ) {
    if ( logOrEndBlock == nullptr )
      logOrEndBlock = &b;
  } else
    logOrEndBlock = nullptr;
  TC179x_wirBB = currentBB;

  // Generate LHS operand.
  auto p1 = $action[2]();

  // Small hack: The tree pattern matcher will match the rightmost operator
  //             first if confronted with a chain like
  //             a || b || c || d
  //             so the first match will be
  //             ( a || b || c ) || ( d )
  // Therefore, we can reuse the result register that is delivered by the LHS
  // as our target register, if the LHS is produced by a logical operator.
  // TODO: In the future this could be replaced by a more involved rule
  // framework that uses special nonterminals to handle the situation. Then also
  // the global variable communication above could be eliminated.
  LLIR_Register *reg = nullptr;
  if ( isNestedLogOR )
    reg = p1.first;
  else {
    reg = TCINSTRUCTIONS.CreateRegister( "" );
    TCINSTRUCTIONS.insertMOV( reg, p1.first, $1->getExp() );
  }

  TCINSTRUCTIONS.insertNE( reg, p1.first, 0, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( reg, 0, jmpLabel, $1->getExp() );

  // WIR
  // Generate MOV and conditional JMP still in the current basic block.
  // Small hack: The tree pattern matcher will match the rightmost operator
  //             first if confronted with a chain like
  //             a || b || c || d
  //             so the first match will be
  //             ( a || b || c ) || ( d )
  // Therefore, we can reuse the result register that is delivered by the LHS
  // as our target register, if the LHS is produced by a logical operator.
  auto &r = isNestedLogOR ? p1.second.get() : TCINSTRUCTIONS.createDReg();
  if ( !isNestedLogOR )
    TCINSTRUCTIONS.insertMOV( r, p1.second, $1->getExp() );
  TCINSTRUCTIONS.insertNE( r, p1.second, 0, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( r, 0, b, $1->getExp() );

  // LLIR
  // Insert new basic block for RHS operand.
  beginNewLLIRBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  // Create new basic block for RHS operand.
  TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );

  // Generate RHS operand.
  auto p2 = $action[3]();

  // LLIR
  TCINSTRUCTIONS.insertNE( reg, getHVLLChild( p2.first ), 0, $1->getExp() );
  TCINSTRUCTIONS.insertOR_NE( reg, getLVLLChild( p2.first ), 0, $1->getExp() );

  // Insert new basic block after the OR operation.
  beginNewLLIRBasicBlock(
    label0.c_str(), *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  TCINSTRUCTIONS.insertNE(
    r, dynamic_cast<TC_DRegV &>( p2.second.get().rbegin()->get() ), 0,
    $1->getExp() );
  TCINSTRUCTIONS.insertOR_NE(
    r, dynamic_cast<TC_DRegV &>( p2.second.get().begin()->get() ), 0,
    $1->getExp() );
  if ( newEndBlock ) {
    TC179x_wirBB = &b;
    logAndEndBlock = nullptr;
  }

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_UnaryExpLOGNOT( llereg )
{
  $cost[0] =
    $cost[2] + TC13::OperationFormat::DDD_1.getSize() +
    TC13::OperationFormat::DDC9_1.getSize();
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_UnaryExpLOGNOT( llereg )", $1 );

  auto p = $action[2]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertOR(
    reg, getLVLLChild( p.first ), getHVLLChild( p.first ), $1->getExp() );
  TCINSTRUCTIONS.insertEQ( reg, reg, 0, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertOR(
    r, dynamic_cast<TC_DRegV &>( p.second.get().begin()->get() ),
    dynamic_cast<TC_DRegV &>( p.second.get().rbegin()->get() ), $1->getExp() );
  TCINSTRUCTIONS.insertEQ( r, r, 0, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


###############################################################################
#
#
# Bitwise logical operators
#
#
###############################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
llereg: tpm_BinaryExpAND( llereg, llereg )
{
  $cost[0] = $cost[2] + $cost[3] + 2 * TC13::OperationFormat::DDD_1.getSize();
}
=
{
  DEBUG_RULE_ACTION( "llereg: tpm_BinaryExpAND( llereg, llereg )", $1 );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateERegister( "" );
  TCINSTRUCTIONS.insertAND(
    getLVLLChild( reg ), getLVLLChild( p1.first ), getLVLLChild( p2.first ),
    $1->getExp() );
  TCINSTRUCTIONS.insertAND(
    getHVLLChild( reg ), getHVLLChild( p1.first ), getHVLLChild( p2.first ),
    $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createEReg();
  TCINSTRUCTIONS.insertAND(
    dynamic_cast<TC_DRegV &>( r.begin()->get() ),
    dynamic_cast<TC_DRegV &>( p1.second.get().begin()->get() ),
    dynamic_cast<TC_DRegV &>( p2.second.get().begin()->get() ), $1->getExp() );
  TCINSTRUCTIONS.insertAND(
    dynamic_cast<TC_DRegV &>( r.rbegin()->get() ),
    dynamic_cast<TC_DRegV &>( p1.second.get().rbegin()->get() ),
    dynamic_cast<TC_DRegV &>( p2.second.get().rbegin()->get() ), $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
llereg: tpm_BinaryExpOR( llereg, llereg )
{
  $cost[0] = $cost[2] + $cost[3] + 2 * TC13::OperationFormat::DDD_1.getSize();
}
=
{
  DEBUG_RULE_ACTION( "llereg: tpm_BinaryExpOR( llereg, llereg )", $1 );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateERegister( "" );
  TCINSTRUCTIONS.insertOR(
    getLVLLChild( reg ), getLVLLChild( p1.first ), getLVLLChild( p2.first ),
    $1->getExp() );
  TCINSTRUCTIONS.insertOR(
    getHVLLChild( reg ), getHVLLChild( p1.first ), getHVLLChild( p2.first ),
    $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createEReg();
  TCINSTRUCTIONS.insertOR(
    dynamic_cast<TC_DRegV &>( r.begin()->get() ),
    dynamic_cast<TC_DRegV &>( p1.second.get().begin()->get() ),
    dynamic_cast<TC_DRegV &>( p2.second.get().begin()->get() ), $1->getExp() );
  TCINSTRUCTIONS.insertOR(
    dynamic_cast<TC_DRegV &>( r.rbegin()->get() ),
    dynamic_cast<TC_DRegV &>( p1.second.get().rbegin()->get() ),
    dynamic_cast<TC_DRegV &>( p2.second.get().rbegin()->get() ), $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
llereg: tpm_BinaryExpXOR( llereg, llereg )
{
  $cost[0] = $cost[2] + $cost[3] + 2 * TC13::OperationFormat::DDD_1.getSize();
}
=
{
  DEBUG_RULE_ACTION( "llereg: tpm_BinaryExpXOR( llereg, llereg )", $1 );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateERegister( "" );
  TCINSTRUCTIONS.insertXOR(
    getLVLLChild( reg ), getLVLLChild( p1.first ), getLVLLChild( p2.first ),
    $1->getExp() );
  TCINSTRUCTIONS.insertXOR(
    getHVLLChild( reg ), getHVLLChild( p1.first ), getHVLLChild( p2.first ),
    $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createEReg();
  TCINSTRUCTIONS.insertXOR(
    dynamic_cast<TC_DRegV &>( r.begin()->get() ),
    dynamic_cast<TC_DRegV &>( p1.second.get().begin()->get() ),
    dynamic_cast<TC_DRegV &>( p2.second.get().begin()->get() ), $1->getExp() );
  TCINSTRUCTIONS.insertXOR(
    dynamic_cast<TC_DRegV &>( r.rbegin()->get() ),
    dynamic_cast<TC_DRegV &>( p1.second.get().rbegin()->get() ),
    dynamic_cast<TC_DRegV &>( p2.second.get().rbegin()->get() ), $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
llereg: tpm_UnaryExpBITNOT( llereg )
{
  $cost[0] = $cost[2] + 2 * TC13::OperationFormat::DDC9_2.getSize();
}
=
{
  DEBUG_RULE_ACTION( "llereg: tpm_UnaryExpBITNOT( llereg )", $1 );

  auto p = $action[2]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateERegister( "" );
  TCINSTRUCTIONS.insertNOR(
    getLVLLChild( reg ), getLVLLChild( p.first ), 0, $1->getExp() );
  TCINSTRUCTIONS.insertNOR(
    getHVLLChild( reg ), getHVLLChild( p.first ), 0, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createEReg();
  TCINSTRUCTIONS.insertNOR(
    dynamic_cast<TC_DRegV &>( r.begin()->get() ),
    dynamic_cast<TC_DRegV &>( p.second.get().begin()->get() ), 0,
    $1->getExp() );
  TCINSTRUCTIONS.insertNOR(
    dynamic_cast<TC_DRegV &>( r.rbegin()->get() ),
    dynamic_cast<TC_DRegV &>( p.second.get().rbegin()->get() ), 0,
    $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


###############################################################################
#
#
# Shift operators
#
#
###############################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
llereg: tpm_BinaryExpSHL( llereg, llereg )
{
  $cost[0] =
    $cost[2] + $cost[3] + TC13::OperationFormat::DC5L.getSize() +
    TC13::OperationFormat::SDD_1.getSize() +
    TC13::OperationFormat::SDC4_1.getSize() +
    2 * TC13::OperationFormat::DDDD.getSize() +
    TC13::OperationFormat::L.getSize() + TC13::OperationFormat::DDD_1.getSize();
}
=
{
  DEBUG_RULE_ACTION( "llereg: tpm_BinaryExpSHL( llereg, llereg )", $1 );

  bool useSHA =
    TCIR_CONFIGURATION->arithmeticSHR &&
    !$2->getExp()->getType().isUnsignedType();

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  // LLIR
  const string lt32shamt = LLIR::getUniqueLabel();
  const string gt31shamt = LLIR::getUniqueLabel();
  const string endBlock = LLIR::getUniqueLabel();

  LLIR_Register *reg = TCINSTRUCTIONS.CreateERegister( "" );
  TCINSTRUCTIONS.insertJZ_T(
    getLVLLChild( p2.first ), 5, lt32shamt, $1->getExp() );

  // Create block for shifting by more than 31 bits.
  beginNewLLIRBasicBlock( gt31shamt.c_str() );
  TCINSTRUCTIONS.insertMOV(
    getHVLLChild( reg ), getLVLLChild( p1.first ), $1->getExp() );
  TCINSTRUCTIONS.insertMOV( getLVLLChild( reg ), 0, $1->getExp() );
  TCINSTRUCTIONS.insertDEXTR(
    getHVLLChild( reg ), getHVLLChild( reg ), getLVLLChild( reg ),
    getLVLLChild( p2.first ), $1->getExp() );
  TCINSTRUCTIONS.insertJ( endBlock.c_str(), $1->getExp() );

  // Create block for shifting by less than 32 bits.
  beginNewLLIRBasicBlock( lt32shamt.c_str() );
  TCINSTRUCTIONS.insertDEXTR(
    getHVLLChild( reg ), getHVLLChild( p1.first ), getLVLLChild( p1.first ),
    getLVLLChild( p2.first ), $1->getExp() );
  if ( useSHA )
    TCINSTRUCTIONS.insertSHA(
      getLVLLChild( reg ), getLVLLChild( p1.first ), getLVLLChild( p2.first ),
      $1->getExp() );
  else
    TCINSTRUCTIONS.insertSH(
      getLVLLChild( reg ), getLVLLChild( p1.first ), getLVLLChild( p2.first ),
      $1->getExp() );

  // Create end block.
  beginNewLLIRBasicBlock( endBlock.c_str() );

  // WIR
  // Create basic block for shifts by more than 31 bits.
  auto *currentBB = TC179x_wirBB;
  auto &b1 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  // Create basic block for shifts by less than 32 bits.
  auto &b2 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  // Create end basic block.
  auto &b3 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  TC179x_wirBB = currentBB;

  auto &r = TCINSTRUCTIONS.createEReg();
  TCINSTRUCTIONS.insertJZ_T(
    dynamic_cast<TC_DRegV &>( p2.second.get().begin()->get() ), 5, b2,
    $1->getExp() );

  // Generate code for shifts by more than 31 bits.
  TC179x_wirBB = &b1;
  TCINSTRUCTIONS.insertMOV(
    dynamic_cast<TC_DRegV &>( r.rbegin()->get() ),
    dynamic_cast<TC_DRegV &>( p1.second.get().begin()->get() ), $1->getExp() );
  TCINSTRUCTIONS.insertMOV(
    dynamic_cast<TC_DRegV &>( r.begin()->get() ), 0, $1->getExp() );
  TCINSTRUCTIONS.insertDEXTR(
    dynamic_cast<TC_DRegV &>( r.rbegin()->get() ),
    dynamic_cast<TC_DRegV &>( r.rbegin()->get() ),
    dynamic_cast<TC_DRegV &>( r.begin()->get() ),
    dynamic_cast<TC_DRegV &>( p2.second.get().begin()->get() ), $1->getExp() );
  TCINSTRUCTIONS.insertJ( b3, $1->getExp() );

  // Generate code for shifts by less than 32 bits.
  TC179x_wirBB = &b2;
  TCINSTRUCTIONS.insertDEXTR(
    dynamic_cast<TC_DRegV &>( r.rbegin()->get() ),
    dynamic_cast<TC_DRegV &>( p1.second.get().rbegin()->get() ),
    dynamic_cast<TC_DRegV &>( p1.second.get().begin()->get() ),
    dynamic_cast<TC_DRegV &>( p2.second.get().begin()->get() ), $1->getExp() );
  if ( useSHA )
    TCINSTRUCTIONS.insertSHA(
      dynamic_cast<TC_DRegV &>( r.begin()->get() ),
      dynamic_cast<TC_DRegV &>( p1.second.get().begin()->get() ),
      dynamic_cast<TC_DRegV &>( p2.second.get().begin()->get() ),
      $1->getExp() );
  else
    TCINSTRUCTIONS.insertSH(
      dynamic_cast<TC_DRegV &>( r.begin()->get() ),
      dynamic_cast<TC_DRegV &>( p1.second.get().begin()->get() ),
      dynamic_cast<TC_DRegV &>( p2.second.get().begin()->get() ),
      $1->getExp() );

  // Switch to end block.
  TC179x_wirBB = &b3;

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
llereg: tpm_BinaryExpSHL( llereg, dreg )
{
  $cost[0] =
    $cost[2] + $cost[3] + TC13::OperationFormat::DC5L.getSize() +
    TC13::OperationFormat::SDD_1.getSize() +
    TC13::OperationFormat::SDC4_1.getSize() +
    2 * TC13::OperationFormat::DDDD.getSize() +
    TC13::OperationFormat::L.getSize() + TC13::OperationFormat::DDD_1.getSize();
}
=
{
  DEBUG_RULE_ACTION( "llereg: tpm_BinaryExpSHL( llereg, dreg )", $1 );

  bool useSHA =
    TCIR_CONFIGURATION->arithmeticSHR &&
    !$2->getExp()->getType().isUnsignedType();

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  // LLIR
  const string lt32shamt = LLIR::getUniqueLabel();
  const string gt31shamt = LLIR::getUniqueLabel();
  const string endBlock = LLIR::getUniqueLabel();

  LLIR_Register *reg = TCINSTRUCTIONS.CreateERegister( "" );
  TCINSTRUCTIONS.insertJZ_T( p2.first, 5, lt32shamt, $1->getExp() );

  // Create block for shifting by more than 31 bits.
  beginNewLLIRBasicBlock( gt31shamt.c_str() );
  TCINSTRUCTIONS.insertMOV(
    getHVLLChild( reg ), getLVLLChild( p1.first ), $1->getExp() );
  TCINSTRUCTIONS.insertMOV( getLVLLChild( reg ), 0, $1->getExp() );
  TCINSTRUCTIONS.insertDEXTR(
    getHVLLChild( reg ), getHVLLChild( reg ), getLVLLChild( reg ), p2.first,
    $1->getExp() );
  TCINSTRUCTIONS.insertJ( endBlock.c_str(), $1->getExp() );

  // Create block for shifting by less than 32 bits.
  beginNewLLIRBasicBlock( lt32shamt.c_str() );
  TCINSTRUCTIONS.insertDEXTR(
    getHVLLChild( reg ), getHVLLChild( p1.first ), getLVLLChild( p1.first ),
    p2.first, $1->getExp() );
  if ( useSHA )
    TCINSTRUCTIONS.insertSHA(
      getLVLLChild( reg ), getLVLLChild( p1.first ), p2.first, $1->getExp() );
  else
    TCINSTRUCTIONS.insertSH(
      getLVLLChild( reg ), getLVLLChild( p1.first ), p2.first, $1->getExp() );

  // Create end block.
  beginNewLLIRBasicBlock( endBlock.c_str() );

  // WIR
  // Create basic block for shifts by more than 31 bits.
  auto *currentBB = TC179x_wirBB;
  auto &b1 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  // Create basic block for shifts by less than 32 bits.
  auto &b2 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  // Create end basic block.
  auto &b3 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  TC179x_wirBB = currentBB;

  auto &r = TCINSTRUCTIONS.createEReg();
  TCINSTRUCTIONS.insertJZ_T( p2.second, 5, b2, $1->getExp() );

  // Generate code for shifts by more than 31 bits.
  TC179x_wirBB = &b1;
  TCINSTRUCTIONS.insertMOV(
    dynamic_cast<TC_DRegV &>( r.rbegin()->get() ),
    dynamic_cast<TC_DRegV &>( p1.second.get().begin()->get() ), $1->getExp() );
  TCINSTRUCTIONS.insertMOV(
    dynamic_cast<TC_DRegV &>( r.begin()->get() ), 0, $1->getExp() );
  TCINSTRUCTIONS.insertDEXTR(
    dynamic_cast<TC_DRegV &>( r.rbegin()->get() ),
    dynamic_cast<TC_DRegV &>( r.rbegin()->get() ),
    dynamic_cast<TC_DRegV &>( r.begin()->get() ), p2.second, $1->getExp() );
  TCINSTRUCTIONS.insertJ( b3, $1->getExp() );

  // Generate code for shifts by less than 32 bits.
  TC179x_wirBB = &b2;
  TCINSTRUCTIONS.insertDEXTR(
    dynamic_cast<TC_DRegV &>( r.rbegin()->get() ),
    dynamic_cast<TC_DRegV &>( p1.second.get().rbegin()->get() ),
    dynamic_cast<TC_DRegV &>( p1.second.get().begin()->get() ), p2.second,
    $1->getExp() );
  if ( useSHA )
    TCINSTRUCTIONS.insertSHA(
      dynamic_cast<TC_DRegV &>( r.begin()->get() ),
      dynamic_cast<TC_DRegV &>( p1.second.get().begin()->get() ), p2.second,
      $1->getExp() );
  else
    TCINSTRUCTIONS.insertSH(
      dynamic_cast<TC_DRegV &>( r.begin()->get() ),
      dynamic_cast<TC_DRegV &>( p1.second.get().begin()->get() ), p2.second,
      $1->getExp() );

  // Switch to end block.
  TC179x_wirBB = &b3;

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
llereg: tpm_BinaryExpSHL( llereg, const9 )
{
  auto *c = dynamic_cast<IR_IntConstExp *>( $3->getExp() );

  if ( c ) {
    $cost[0] = $cost[2] + $cost[3];

    if ( c->getValue().getIntValue() >= 64 )
      // We don't use this rule for shift amounts leading to undefined behavior
      // according to the C99 standard.
      $cost[0] = COST_INFINITY;
    else

    if ( c->getValue().getIntValue() > 31 )
      $cost[0] +=
        TC13::OperationFormat::SDD_1.getSize() +
        TC13::OperationFormat::SDC4_1.getSize() +
        TC13::OperationFormat::DDDC5.getSize();
    else {
      if ( c->getValue().getIntValue() != 0 )
        $cost[0] +=
          TC13::OperationFormat::DDDC5.getSize() +
          TC13::OperationFormat::DDC9_1.getSize();
      else
        $cost[0] += TC13::OperationFormat::SDD_1.getSize();
    }
  } else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "llereg: tpm_BinaryExpSHL( llereg, const9 )", $1 );

  bool useSHA =
    TCIR_CONFIGURATION->arithmeticSHR &&
    !$2->getExp()->getType().isUnsignedType();

  auto p = $action[2]();
  auto v = $action[3]().getIntValue();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateERegister( "" );

  if ( v > 31 ) {
    TCINSTRUCTIONS.insertMOV(
      getHVLLChild( reg ), getLVLLChild( p.first ), $1->getExp() );
    TCINSTRUCTIONS.insertMOV( getLVLLChild( reg ), 0, $1->getExp() );
    TCINSTRUCTIONS.insertDEXTR(
      getHVLLChild( reg ), getHVLLChild( reg ), getLVLLChild( reg ), v & 0x1F,
      $1->getExp() );
  } else {
    if ( v != 0 ) {
      TCINSTRUCTIONS.insertDEXTR(
        getHVLLChild( reg ), getHVLLChild( p.first ), getLVLLChild( p.first ),
        v, $1->getExp() );
      if ( useSHA )
        TCINSTRUCTIONS.insertSHA(
          getLVLLChild( reg ), getLVLLChild( p.first ), v, $1->getExp() );
      else
        TCINSTRUCTIONS.insertSH(
          getLVLLChild( reg ), getLVLLChild( p.first ), v, $1->getExp() );
    } else
      TCINSTRUCTIONS.insertMOV( reg, p.first, $1->getExp() );
  }

  // WIR
  auto &r = TCINSTRUCTIONS.createEReg();

  if ( v > 31 ) {
    TCINSTRUCTIONS.insertMOV(
      dynamic_cast<TC_DRegV &>( r.rbegin()->get() ),
      dynamic_cast<TC_DRegV &>( p.second.get().begin()->get() ), $1->getExp() );
    TCINSTRUCTIONS.insertMOV(
      dynamic_cast<TC_DRegV &>( r.begin()->get() ), 0, $1->getExp() );
    TCINSTRUCTIONS.insertDEXTR(
      dynamic_cast<TC_DRegV &>( r.rbegin()->get() ),
      dynamic_cast<TC_DRegV &>( r.rbegin()->get() ),
      dynamic_cast<TC_DRegV &>( r.begin()->get() ), v & 0x1F, $1->getExp() );
  } else {
    if ( v != 0 ) {
      TCINSTRUCTIONS.insertDEXTR(
        dynamic_cast<TC_DRegV &>( r.rbegin()->get() ),
        dynamic_cast<TC_DRegV &>( p.second.get().rbegin()->get() ),
        dynamic_cast<TC_DRegV &>( p.second.get().begin()->get() ), v,
        $1->getExp() );
      if ( useSHA )
        TCINSTRUCTIONS.insertSHA(
          dynamic_cast<TC_DRegV &>( r.begin()->get() ),
          dynamic_cast<TC_DRegV &>( p.second.get().begin()->get() ), v,
          $1->getExp() );
      else
        TCINSTRUCTIONS.insertSH(
          dynamic_cast<TC_DRegV &>( r.begin()->get() ),
          dynamic_cast<TC_DRegV &>( p.second.get().begin()->get() ), v,
          $1->getExp() );
    } else
      TCINSTRUCTIONS.insertMOV( r, p.second, $1->getExp() );
  }

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
llereg: tpm_BinaryExpSHR( llereg, llereg )
{
  $cost[0] =
    $cost[2] + $cost[3] + TC13::OperationFormat::DC4L_1.getSize() +
    2 * TC13::OperationFormat::DDC9_1.getSize() +
    TC13::OperationFormat::DC5L.getSize() +
    2 * TC13::OperationFormat::DDD_1.getSize() +
    2 * TC13::OperationFormat::L.getSize() +
    TC13::OperationFormat::DDDD.getSize() +
    TC13::OperationFormat::SDD_1.getSize();

  if ( $2->getExp()->getType().isSignedType() )
    $cost[0] += TC13::OperationFormat::DDC5C5.getSize();
  else
    $cost[0] += TC13::OperationFormat::SDC4_1.getSize();
}
=
{
  DEBUG_RULE_ACTION( "llereg: tpm_BinaryExpSHR( llereg, llereg )", $1 );

  bool useSHA =
    TCIR_CONFIGURATION->arithmeticSHR &&
    !$2->getExp()->getType().isUnsignedType();

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  // LLIR
  const string initBlock = LLIR::getUniqueLabel();
  const string lt32shamt = LLIR::getUniqueLabel();
  const string gt31shamt = LLIR::getUniqueLabel();
  const string moveBlock = LLIR::getUniqueLabel();
  const string remBlock = LLIR::getUniqueLabel();
  const string endBlock = LLIR::getUniqueLabel();

  LLIR_Register *reg = TCINSTRUCTIONS.CreateERegister( "" );
  LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );

  TCINSTRUCTIONS.insertJEQ(
    getLVLLChild( p2.first ), 0, moveBlock, $1->getExp() );

  // Create block for computing the shift amount.
  beginNewLLIRBasicBlock( initBlock.c_str() );
  TCINSTRUCTIONS.insertRSUB(
    regTmp, getLVLLChild( p2.first ), 32, $1->getExp() );
  TCINSTRUCTIONS.insertJZ_T(
    getLVLLChild( p2.first ), 5, lt32shamt, $1->getExp() );

  // Create block for shifting by more than 31 bits.
  beginNewLLIRBasicBlock( gt31shamt.c_str() );
  if ( useSHA )
    TCINSTRUCTIONS.insertSHA(
      getLVLLChild( reg ), getHVLLChild( p1.first ), regTmp, $1->getExp() );
  else
    TCINSTRUCTIONS.insertSH(
      getLVLLChild( reg ), getHVLLChild( p1.first ), regTmp, $1->getExp() );
  if ( $2->getExp()->getType().isSignedType() )
    TCINSTRUCTIONS.insertEXTR(
      getHVLLChild( reg ), getLVLLChild( reg ), 31, 1, $1->getExp() );
  else
    TCINSTRUCTIONS.insertMOV( getHVLLChild( reg ), 0, $1->getExp() );
  TCINSTRUCTIONS.insertJ( endBlock.c_str(), $1->getExp() );

  // Create block for shifting by less than 32 bits.
  beginNewLLIRBasicBlock( lt32shamt.c_str() );
  TCINSTRUCTIONS.insertDEXTR(
    getLVLLChild( reg ), getHVLLChild( p1.first ), getLVLLChild( p1.first ),
    regTmp, $1->getExp() );
  TCINSTRUCTIONS.insertJ( remBlock.c_str(), $1->getExp() );

  // Create block for moving if shift amount is zero.
  beginNewLLIRBasicBlock( moveBlock.c_str() );
  TCINSTRUCTIONS.insertMOV(
    getLVLLChild( reg ), getLVLLChild( p1.first ), $1->getExp() );

  // Create block for computing the high-value result word.
  beginNewLLIRBasicBlock( remBlock.c_str() );
  TCINSTRUCTIONS.insertRSUB( regTmp, getLVLLChild( p2.first ), 0, $1->getExp() );
  if ( useSHA )
    TCINSTRUCTIONS.insertSHA(
      getHVLLChild( reg ), getHVLLChild( p1.first ), regTmp, $1->getExp() );
  else
    TCINSTRUCTIONS.insertSH(
      getHVLLChild( reg ), getHVLLChild( p1.first ), regTmp, $1->getExp() );

  // Create end block.
  beginNewLLIRBasicBlock( endBlock.c_str() );

  // WIR
  // Create basic block to compute the shift amount.
  auto *currentBB = TC179x_wirBB;
  auto &b1 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  // Create basic block for shifts by more than 31 bits.
  auto &b2 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  // Create basic block for shifts by less than 32 bits.
  auto &b3 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  // Create basic block for moves if shift amount is zero.
  auto &b4 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  // Create basic block to compute the high-value result word.
  auto &b5 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  // Create end basic block.
  auto &b6 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  TC179x_wirBB = currentBB;

  auto &r = TCINSTRUCTIONS.createEReg();
  auto &tmpReg = TCINSTRUCTIONS.createDReg();

  TCINSTRUCTIONS.insertJEQ(
    dynamic_cast<TC_DRegV &>( p2.second.get().begin()->get() ), 0, b4,
    $1->getExp() );

  // Create block to compute the shift amount.
  TC179x_wirBB = &b1;
  TCINSTRUCTIONS.insertRSUB(
    tmpReg, dynamic_cast<TC_DRegV &>( p2.second.get().begin()->get() ), 32,
    $1->getExp() );
  TCINSTRUCTIONS.insertJZ_T(
    dynamic_cast<TC_DRegV &>( p2.second.get().begin()->get() ), 5, b3,
    $1->getExp() );

  // Generate code for shifts by more than 31 bits.
  TC179x_wirBB = &b2;
  if ( useSHA )
    TCINSTRUCTIONS.insertSHA(
      dynamic_cast<TC_DRegV &>( r.begin()->get() ),
      dynamic_cast<TC_DRegV &>( p1.second.get().rbegin()->get() ), tmpReg,
      $1->getExp() );
  else
    TCINSTRUCTIONS.insertSH(
      dynamic_cast<TC_DRegV &>( r.begin()->get() ),
      dynamic_cast<TC_DRegV &>( p1.second.get().rbegin()->get() ), tmpReg,
      $1->getExp() );
  if ( $2->getExp()->getType().isSignedType() )
    TCINSTRUCTIONS.insertEXTR(
      dynamic_cast<TC_DRegV &>( r.rbegin()->get() ),
      dynamic_cast<TC_DRegV &>( r.begin()->get() ), 31, 1, $1->getExp() );
  else
    TCINSTRUCTIONS.insertMOV(
      dynamic_cast<TC_DRegV &>( r.rbegin()->get() ), 0, $1->getExp() );
  TCINSTRUCTIONS.insertJ( b6, $1->getExp() );

  // Generate code for shifts by less than 32 bits.
  TC179x_wirBB = &b3;
  TCINSTRUCTIONS.insertDEXTR(
    dynamic_cast<TC_DRegV &>( r.begin()->get() ),
    dynamic_cast<TC_DRegV &>( p1.second.get().rbegin()->get() ),
    dynamic_cast<TC_DRegV &>( p1.second.get().begin()->get() ), tmpReg,
    $1->getExp() );
  TCINSTRUCTIONS.insertJ( b5, $1->getExp() );

  // Generate move if shift amount is zero.
  TC179x_wirBB = &b4;
  TCINSTRUCTIONS.insertMOV(
    dynamic_cast<TC_DRegV &>( r.rbegin()->get() ),
    dynamic_cast<TC_DRegV &>( p1.second.get().begin()->get() ), $1->getExp() );

  // Generate code to compute the high-value result word.
  TC179x_wirBB = &b5;
  TCINSTRUCTIONS.insertRSUB(
    tmpReg, dynamic_cast<TC_DRegV &>( p2.second.get().begin()->get() ), 0,
    $1->getExp() );
  if ( useSHA )
    TCINSTRUCTIONS.insertSHA(
      dynamic_cast<TC_DRegV &>( r.rbegin()->get() ),
      dynamic_cast<TC_DRegV &>( p1.second.get().rbegin()->get() ), tmpReg,
      $1->getExp() );
  else
    TCINSTRUCTIONS.insertSH(
      dynamic_cast<TC_DRegV &>( r.rbegin()->get() ),
      dynamic_cast<TC_DRegV &>( p1.second.get().rbegin()->get() ), tmpReg,
      $1->getExp() );

  // Switch to end block.
  TC179x_wirBB = &b6;

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
llereg: tpm_BinaryExpSHR( llereg, dreg )
{
  $cost[0] =
    $cost[2] + $cost[3] + TC13::OperationFormat::DC4L_1.getSize() +
    2 * TC13::OperationFormat::DDC9_1.getSize() +
    TC13::OperationFormat::DC5L.getSize() +
    2 * TC13::OperationFormat::DDD_1.getSize() +
    2 * TC13::OperationFormat::L.getSize() +
    TC13::OperationFormat::DDDD.getSize() +
    TC13::OperationFormat::SDD_1.getSize();

  if ( $2->getExp()->getType().isSignedType() )
    $cost[0] += TC13::OperationFormat::DDC5C5.getSize();
  else
    $cost[0] += TC13::OperationFormat::SDC4_1.getSize();
}
=
{
  DEBUG_RULE_ACTION( "llereg: tpm_BinaryExpSHR( llereg, dreg )", $1 );

  bool useSHA =
    TCIR_CONFIGURATION->arithmeticSHR &&
    !$2->getExp()->getType().isUnsignedType();

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  // LLIR
  const string initBlock = LLIR::getUniqueLabel();
  const string lt32shamt = LLIR::getUniqueLabel();
  const string gt31shamt = LLIR::getUniqueLabel();
  const string moveBlock = LLIR::getUniqueLabel();
  const string remBlock = LLIR::getUniqueLabel();
  const string endBlock = LLIR::getUniqueLabel();

  LLIR_Register *reg = TCINSTRUCTIONS.CreateERegister( "" );
  LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );

  TCINSTRUCTIONS.insertJEQ( p2.first, 0, moveBlock, $1->getExp() );

  // Create block for computing the shift amount.
  beginNewLLIRBasicBlock( initBlock.c_str() );
  TCINSTRUCTIONS.insertRSUB( regTmp, p2.first, 32, $1->getExp() );
  TCINSTRUCTIONS.insertJZ_T( p2.first, 5, lt32shamt, $1->getExp() );

  // Create block for shifting by more than 31 bits.
  beginNewLLIRBasicBlock( gt31shamt.c_str() );
  if ( useSHA )
    TCINSTRUCTIONS.insertSHA(
      getLVLLChild( reg ), getHVLLChild( p1.first ), regTmp, $1->getExp() );
  else
    TCINSTRUCTIONS.insertSH(
      getLVLLChild( reg ), getHVLLChild( p1.first ), regTmp, $1->getExp() );
  if ( $2->getExp()->getType().isSignedType() )
    TCINSTRUCTIONS.insertEXTR(
      getHVLLChild( reg ), getLVLLChild( reg ), 31, 1, $1->getExp() );
  else
    TCINSTRUCTIONS.insertMOV( getHVLLChild( reg ), 0, $1->getExp() );
  TCINSTRUCTIONS.insertJ( endBlock.c_str(), $1->getExp() );

  // Create block for shifting by less than 32 bits.
  beginNewLLIRBasicBlock( lt32shamt.c_str() );
  TCINSTRUCTIONS.insertDEXTR(
    getLVLLChild( reg ), getHVLLChild( p1.first ), getLVLLChild( p1.first ),
    regTmp, $1->getExp() );
  TCINSTRUCTIONS.insertJ( remBlock.c_str(), $1->getExp() );

  // Create block for moving if shift amount is zero.
  beginNewLLIRBasicBlock( moveBlock.c_str() );
  TCINSTRUCTIONS.insertMOV(
    getLVLLChild( reg ), getLVLLChild( p1.first ), $1->getExp() );

  // Create block for computing the high-value result word.
  beginNewLLIRBasicBlock( remBlock.c_str() );
  TCINSTRUCTIONS.insertRSUB( regTmp, p2.first, 0, $1->getExp() );
  if ( useSHA )
    TCINSTRUCTIONS.insertSHA(
      getHVLLChild( reg ), getHVLLChild( p1.first ), regTmp, $1->getExp() );
  else
    TCINSTRUCTIONS.insertSH(
      getHVLLChild( reg ), getHVLLChild( p1.first ), regTmp, $1->getExp() );

  // Create end block.
  beginNewLLIRBasicBlock( endBlock.c_str() );

  // WIR
  // Create basic block to compute the shift amount.
  auto *currentBB = TC179x_wirBB;
  auto &b1 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  // Create basic block for shifts by more than 31 bits.
  auto &b2 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  // Create basic block for shifts by less than 32 bits.
  auto &b3 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  // Create basic block for moves if shift amount is zero.
  auto &b4 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  // Create basic block to compute the high-value result word.
  auto &b5 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  // Create end basic block.
  auto &b6 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  TC179x_wirBB = currentBB;

  auto &r = TCINSTRUCTIONS.createEReg();
  auto &tmpReg = TCINSTRUCTIONS.createDReg();

  TCINSTRUCTIONS.insertJEQ( p2.second, 0, b4, $1->getExp() );

  // Create block to compute the shift amount.
  TC179x_wirBB = &b1;
  TCINSTRUCTIONS.insertRSUB( tmpReg, p2.second, 32, $1->getExp() );
  TCINSTRUCTIONS.insertJZ_T( p2.second, 5, b3, $1->getExp() );

  // Generate code for shifts by more than 31 bits.
  TC179x_wirBB = &b2;
  if ( useSHA )
    TCINSTRUCTIONS.insertSHA(
      dynamic_cast<TC_DRegV &>( r.begin()->get() ),
      dynamic_cast<TC_DRegV &>( p1.second.get().rbegin()->get() ), tmpReg,
      $1->getExp() );
  else
    TCINSTRUCTIONS.insertSH(
      dynamic_cast<TC_DRegV &>( r.begin()->get() ),
      dynamic_cast<TC_DRegV &>( p1.second.get().rbegin()->get() ), tmpReg,
      $1->getExp() );
  if ( $2->getExp()->getType().isSignedType() )
    TCINSTRUCTIONS.insertEXTR(
      dynamic_cast<TC_DRegV &>( r.rbegin()->get() ),
      dynamic_cast<TC_DRegV &>( r.begin()->get() ), 31, 1, $1->getExp() );
  else
    TCINSTRUCTIONS.insertMOV(
      dynamic_cast<TC_DRegV &>( r.rbegin()->get() ), 0, $1->getExp() );
  TCINSTRUCTIONS.insertJ( b6, $1->getExp() );

  // Generate code for shifts by less than 32 bits.
  TC179x_wirBB = &b3;
  TCINSTRUCTIONS.insertDEXTR(
    dynamic_cast<TC_DRegV &>( r.begin()->get() ),
    dynamic_cast<TC_DRegV &>( p1.second.get().rbegin()->get() ),
    dynamic_cast<TC_DRegV &>( p1.second.get().begin()->get() ), tmpReg,
    $1->getExp() );
  TCINSTRUCTIONS.insertJ( b5, $1->getExp() );

  // Generate move if shift amount is zero.
  TC179x_wirBB = &b4;
  TCINSTRUCTIONS.insertMOV(
    dynamic_cast<TC_DRegV &>( r.begin()->get() ),
    dynamic_cast<TC_DRegV &>( p1.second.get().begin()->get() ), $1->getExp() );

  // Generate code to compute the high-value result word.
  TC179x_wirBB = &b5;
  TCINSTRUCTIONS.insertRSUB( tmpReg, p2.second, 0, $1->getExp() );
  if ( useSHA )
    TCINSTRUCTIONS.insertSHA(
      dynamic_cast<TC_DRegV &>( r.rbegin()->get() ),
      dynamic_cast<TC_DRegV &>( p1.second.get().rbegin()->get() ), tmpReg,
      $1->getExp() );
  else
    TCINSTRUCTIONS.insertSH(
      dynamic_cast<TC_DRegV &>( r.rbegin()->get() ),
      dynamic_cast<TC_DRegV &>( p1.second.get().rbegin()->get() ), tmpReg,
      $1->getExp() );

  // Switch to end block.
  TC179x_wirBB = &b6;

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
llereg: tpm_BinaryExpSHR( llereg, const9 )
{
  auto *c = dynamic_cast<IR_IntConstExp *>( $3->getExp() );

  if ( c ) {
    $cost[0] = $cost[2] + $cost[3];

    if ( c->getValue().getIntValue() >= 64 )
      // We don't use this rule for shift amounts leading to undefined behavior
      // according to the C99 standard.
      $cost[0] = COST_INFINITY;
    else

    if ( c->getValue().getIntValue() > 31 ) {
      $cost[0] += TC13::OperationFormat::DDC9_1.getSize();

      if ( $2->getExp()->getType().isSignedType() )
        $cost[0] += TC13::OperationFormat::DDC5C5.getSize();
      else
        $cost[0] += TC13::OperationFormat::SDC4_1.getSize();
    } else {
      if ( c->getValue().getIntValue() != 0 )
        $cost[0] +=
          TC13::OperationFormat::DDDC5.getSize() +
          TC13::OperationFormat::DDC9_1.getSize();
      else
        $cost[0] += TC13::OperationFormat::SDD_1.getSize();
    }
  } else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "llereg: tpm_BinaryExpSHR( llereg, const9 )", $1 );

  bool useSHA =
    TCIR_CONFIGURATION->arithmeticSHR &&
    !$2->getExp()->getType().isUnsignedType();

  auto p = $action[2]();
  auto v = $action[3]().getIntValue();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateERegister( "" );

  if ( v > 31 ) {
    if ( useSHA )
      TCINSTRUCTIONS.insertSHA(
        getLVLLChild( reg ), getHVLLChild( p.first ), -( v & 0x1F ),
        $1->getExp() );
    else
      TCINSTRUCTIONS.insertSH(
        getLVLLChild( reg ), getHVLLChild( p.first ), -( v & 0x1F ),
        $1->getExp() );
    if ( $2->getExp()->getType().isSignedType() )
      TCINSTRUCTIONS.insertEXTR(
        getHVLLChild( reg ), getLVLLChild( reg ), 31, 1, $1->getExp() );
    else
      TCINSTRUCTIONS.insertMOV( getHVLLChild( reg ), 0, $1->getExp() );
  } else {
    if ( v != 0 ) {
      TCINSTRUCTIONS.insertDEXTR(
        getLVLLChild( reg ), getHVLLChild( p.first ), getLVLLChild( p.first ),
        32 - v, $1->getExp() );
      if ( useSHA )
        TCINSTRUCTIONS.insertSHA(
          getHVLLChild( reg ), getHVLLChild( p.first ), -v, $1->getExp() );
      else
        TCINSTRUCTIONS.insertSH(
          getHVLLChild( reg ), getHVLLChild( p.first ), -v, $1->getExp() );
    } else
      TCINSTRUCTIONS.insertMOV( reg, p.first, $1->getExp() );
  }

  // WIR
  auto &r = TCINSTRUCTIONS.createEReg();

  if ( v > 31 ) {
    if ( useSHA )
      TCINSTRUCTIONS.insertSHA(
        dynamic_cast<TC_DRegV &>( r.begin()->get() ),
        dynamic_cast<TC_DRegV &>( p.second.get().rbegin()->get() ),
        -( v & 0x1F ), $1->getExp() );
    else
      TCINSTRUCTIONS.insertSH(
        dynamic_cast<TC_DRegV &>( r.begin()->get() ),
        dynamic_cast<TC_DRegV &>( p.second.get().rbegin()->get() ),
        -( v & 0x1F ), $1->getExp() );
    if ( $2->getExp()->getType().isSignedType() )
      TCINSTRUCTIONS.insertEXTR(
        dynamic_cast<TC_DRegV &>( r.rbegin()->get() ),
        dynamic_cast<TC_DRegV &>( r.begin()->get() ), 31, 1, $1->getExp() );
    else
      TCINSTRUCTIONS.insertMOV(
        dynamic_cast<TC_DRegV &>( r.rbegin()->get() ), 0, $1->getExp() );
  } else {
    if ( v != 0 ) {
      TCINSTRUCTIONS.insertDEXTR(
        dynamic_cast<TC_DRegV &>( r.begin()->get() ),
        dynamic_cast<TC_DRegV &>( p.second.get().rbegin()->get() ),
        dynamic_cast<TC_DRegV &>( p.second.get().begin()->get() ), 32 - v,
        $1->getExp() );
      if ( useSHA )
        TCINSTRUCTIONS.insertSHA(
          dynamic_cast<TC_DRegV &>( r.rbegin()->get() ),
          dynamic_cast<TC_DRegV &>( p.second.get().rbegin()->get() ), -v,
          $1->getExp() );
      else
        TCINSTRUCTIONS.insertSH(
          dynamic_cast<TC_DRegV &>( r.rbegin()->get() ),
          dynamic_cast<TC_DRegV &>( p.second.get().rbegin()->get() ), -v,
          $1->getExp() );
    } else
      TCINSTRUCTIONS.insertMOV( r, p.second, $1->getExp() );
  }

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: llereg
{
  // This rule converts an llereg to a dreg in the special case that an
  // operation like:
  //
  // int i, j; long long l; j = i << l;
  //
  // is performed. In the case of the shifting operators, only the integer
  // promotions are performed, which means that both operands are promoted to
  // integer if they have smaller rank. In this case, the RHS has higher rank,
  // but as all shift values larger than 32 are illegal, this high rank is never
  // required. Therefore, we just return the low-order register from the ereg in
  // this rule, to serve as the shift count register in the standard shifting
  // rules.
  IR_Exp *parent = $1->getExp()->getParent();
  auto *bexp = dynamic_cast<IR_BinaryExp *>( parent );
  auto *aexp = dynamic_cast<IR_AssignExp *>( parent );

  if ( ( bexp && ( &bexp->getOp2() == $1->getExp() ) &&
         ( ( bexp->getOperator() == IR_BinaryExp::SHL ) ||
           ( bexp->getOperator() == IR_BinaryExp::SHR ) ) ) ||
       ( aexp && ( &aexp->getRHS() == $1->getExp() ) &&
         ( ( aexp->getOperator() == IR_AssignExp::SHL ) ||
           ( aexp->getOperator() == IR_AssignExp::SHR ) ) ) )
    $cost[0] = 0;
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: llereg", $1 );

  auto p = $action[1]();

  return(
    make_pair(
      getLVLLChild( p.first ),
      ref( dynamic_cast<TC_DRegV &>( p.second.get().begin()->get() ) ) ) );
};


###############################################################################
#
#
# Unary expressions
#
#
###############################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
llconst: tpm_UnaryExpMINUS( llconst )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "llconst: tpm_UnaryExpMINUS( llconst )", $1 );

  return( -$action[2]() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
llereg: tpm_UnaryExpMINUS( llereg )
{
  $cost[0] =
    $cost[2] + TC13::OperationFormat::SDC4_1.getSize() +
    2 * TC13::OperationFormat::DDD_1.getSize();
}
=
{
  DEBUG_RULE_ACTION( "llereg: tpm_UnaryExpMINUS( llereg )", $1 );

  auto p = $action[2]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateERegister( "" );
  LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );

  TCINSTRUCTIONS.insertMOV( regTmp, 0, $1->getExp() );
  TCINSTRUCTIONS.insertSUBX(
    getLVLLChild( reg ), regTmp, getLVLLChild( p.first ), $1->getExp() );
  TCINSTRUCTIONS.insertSUBC(
    getHVLLChild( reg ), regTmp, getHVLLChild( p.first ), $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createEReg();
  auto &tmpReg = TCINSTRUCTIONS.createDReg();

  TCINSTRUCTIONS.insertMOV( tmpReg, 0, $1->getExp() );
  TCINSTRUCTIONS.insertSUBX(
    dynamic_cast<TC_DRegV &>( r.begin()->get() ), tmpReg,
    dynamic_cast<TC_DRegV &>( p.second.get().begin()->get() ), $1->getExp() );
  TCINSTRUCTIONS.insertSUBC(
    dynamic_cast<TC_DRegV &>( r.rbegin()->get() ), tmpReg,
    dynamic_cast<TC_DRegV &>( p.second.get().rbegin()->get() ), $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
llconst: tpm_UnaryExpPLUS( llconst )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "llconst: tpm_UnaryExpPLUS( llconst )", $1 );

  return( $action[2]() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
llereg: tpm_UnaryExpPLUS( llereg )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "llereg: tpm_UnaryExpPLUS( llereg )", $1 );

  return( $action[2]() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
llereg: tpm_UnaryExpPOSTDEC( llereg )
{
  $cost[0] =
    $cost[2] + 2 * TC13::OperationFormat::SDD_1.getSize() +
    2 * TC13::OperationFormat::DDC9_1.getSize();
}
=
{
  DEBUG_RULE_ACTION( "llereg: tpm_UnaryExpPOSTDEC( llereg )", $1 );

  auto p = $action[2]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateERegister( "" );

  TCINSTRUCTIONS.insertMOV( reg, p.first, $1->getExp() );
  TCINSTRUCTIONS.insertADDX(
    getLVLLChild( p.first ), getLVLLChild( p.first ), -1,  $1->getExp() );
  TCINSTRUCTIONS.insertADDC(
    getHVLLChild( p.first ), getHVLLChild( p.first ), -1, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createEReg();

  TCINSTRUCTIONS.insertMOV( r, p.second, $1->getExp() );
  TCINSTRUCTIONS.insertADDX(
    dynamic_cast<TC_DRegV &>( p.second.get().begin()->get() ),
    dynamic_cast<TC_DRegV &>( p.second.get().begin()->get() ), -1,
    $1->getExp() );
  TCINSTRUCTIONS.insertADDC(
    dynamic_cast<TC_DRegV &>( p.second.get().rbegin()->get() ),
    dynamic_cast<TC_DRegV &>( p.second.get().rbegin()->get() ), -1,
    $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
llereg: tpm_UnaryExpPOSTDEC( deref_llereg )
{
  $cost[0] =
    $cost[2] + 2 * TC13::OperationFormat::SDD_1.getSize() +
    2 * TC13::OperationFormat::DDC9_1.getSize() +
    TC_AddressModification::createStoreCost( $2->getExp() );
}
=
{
  DEBUG_RULE_ACTION( "llereg: tpm_UnaryExpPOSTDEC( deref_llereg )", $1 );

  auto lvalue = $action[2]( true );

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateERegister( "" );

  TCINSTRUCTIONS.insertMOV( reg, lvalue.getResultRegister(), $1->getExp() );
  TCINSTRUCTIONS.insertADDX(
    getLVLLChild( lvalue.getResultRegister() ),
    getLVLLChild( lvalue.getResultRegister() ), -1,  $1->getExp() );
  TCINSTRUCTIONS.insertADDC(
    getHVLLChild( lvalue.getResultRegister() ),
    getHVLLChild( lvalue.getResultRegister() ), -1, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createEReg();
  auto &tmpReg = dynamic_cast<TC_ERegV &>( *(lvalue.getResultReg()) );

  TCINSTRUCTIONS.insertMOV( r, tmpReg, $1->getExp() );
  TCINSTRUCTIONS.insertADDX(
    dynamic_cast<TC_DRegV &>( tmpReg.begin()->get() ),
    dynamic_cast<TC_DRegV &>( tmpReg.begin()->get() ), -1,  $1->getExp() );
  TCINSTRUCTIONS.insertADDC(
    dynamic_cast<TC_DRegV &>( tmpReg.rbegin()->get() ),
    dynamic_cast<TC_DRegV &>( tmpReg.rbegin()->get() ), -1,  $1->getExp() );

  lvalue.storeBack( $2->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
llereg: tpm_UnaryExpPOSTINC( llereg )
{
  $cost[0] =
    $cost[2] + 2 * TC13::OperationFormat::SDD_1.getSize() +
    2 * TC13::OperationFormat::DDC9_1.getSize();
}
=
{
  DEBUG_RULE_ACTION( "llereg: tpm_UnaryExpPOSTINC( llereg )", $1 );

  auto p = $action[2]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateERegister( "" );

  TCINSTRUCTIONS.insertMOV( reg, p.first, $1->getExp() );
  TCINSTRUCTIONS.insertADDX(
    getLVLLChild( p.first ), getLVLLChild( p.first ), 1,  $1->getExp() );
  TCINSTRUCTIONS.insertADDC(
    getHVLLChild( p.first ), getHVLLChild( p.first ), 0, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createEReg();

  TCINSTRUCTIONS.insertMOV( r, p.second, $1->getExp() );
  TCINSTRUCTIONS.insertADDX(
    dynamic_cast<TC_DRegV &>( p.second.get().begin()->get() ),
    dynamic_cast<TC_DRegV &>( p.second.get().begin()->get() ), 1,
    $1->getExp() );
  TCINSTRUCTIONS.insertADDC(
    dynamic_cast<TC_DRegV &>( p.second.get().rbegin()->get() ),
    dynamic_cast<TC_DRegV &>( p.second.get().rbegin()->get() ), 0,
    $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
llereg: tpm_UnaryExpPOSTINC( deref_llereg )
{
  $cost[0] =
    $cost[2] + 2 * TC13::OperationFormat::SDD_1.getSize() +
    2 * TC13::OperationFormat::DDC9_1.getSize() +
    TC_AddressModification::createStoreCost( $2->getExp() );
}
=
{
  DEBUG_RULE_ACTION( "llereg: tpm_UnaryExpPOSTINC( deref_llereg )", $1 );

  auto lvalue = $action[2]( true );

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateERegister( "" );

  TCINSTRUCTIONS.insertMOV( reg, lvalue.getResultRegister(), $1->getExp() );
  TCINSTRUCTIONS.insertADDX(
    getLVLLChild( lvalue.getResultRegister() ),
    getLVLLChild( lvalue.getResultRegister() ), 1,  $1->getExp() );
  TCINSTRUCTIONS.insertADDC(
    getHVLLChild( lvalue.getResultRegister() ),
    getHVLLChild( lvalue.getResultRegister() ), 0, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createEReg();
  auto &tmpReg = dynamic_cast<TC_ERegV &>( *(lvalue.getResultReg()) );

  TCINSTRUCTIONS.insertMOV( r, tmpReg, $1->getExp() );
  TCINSTRUCTIONS.insertADDX(
    dynamic_cast<TC_DRegV &>( tmpReg.begin()->get() ),
    dynamic_cast<TC_DRegV &>( tmpReg.begin()->get() ), 1,  $1->getExp() );
  TCINSTRUCTIONS.insertADDC(
    dynamic_cast<TC_DRegV &>( tmpReg.rbegin()->get() ),
    dynamic_cast<TC_DRegV &>( tmpReg.rbegin()->get() ), 0,  $1->getExp() );

  lvalue.storeBack( $2->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
llereg: tpm_UnaryExpPREDEC( llereg )
{
  $cost[0] =
    $cost[2] + 2 * TC13::OperationFormat::DDC9_1.getSize();
}
=
{
  DEBUG_RULE_ACTION( "llereg: tpm_UnaryExpPREDEC( llereg )", $1 );

  auto p = $action[2]();

  // LLIR
  TCINSTRUCTIONS.insertADDX(
    getLVLLChild( p.first ), getLVLLChild( p.first ), -1,  $1->getExp() );
  TCINSTRUCTIONS.insertADDC(
    getHVLLChild( p.first ), getHVLLChild( p.first ), -1, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertADDX(
    dynamic_cast<TC_DRegV &>( p.second.get().begin()->get() ),
    dynamic_cast<TC_DRegV &>( p.second.get().begin()->get() ), -1,
    $1->getExp() );
  TCINSTRUCTIONS.insertADDC(
    dynamic_cast<TC_DRegV &>( p.second.get().rbegin()->get() ),
    dynamic_cast<TC_DRegV &>( p.second.get().rbegin()->get() ), -1,
    $1->getExp() );

  return( p );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
llereg: tpm_UnaryExpPREDEC( deref_llereg )
{
  $cost[0] =
    $cost[2] + 2 * TC13::OperationFormat::DDC9_1.getSize() +
    TC_AddressModification::createStoreCost( $2->getExp() );
}
=
{
  DEBUG_RULE_ACTION( "llereg: tpm_UnaryExpPREDEC( deref_llereg )", $1 );

  auto lvalue = $action[2]( true );

  // LLIR
  TCINSTRUCTIONS.insertADDX(
    getLVLLChild( lvalue.getResultRegister() ),
    getLVLLChild( lvalue.getResultRegister() ), -1, $1->getExp() );
  TCINSTRUCTIONS.insertADDC(
    getHVLLChild( lvalue.getResultRegister() ),
    getHVLLChild( lvalue.getResultRegister() ), -1, $1->getExp() );

  // WIR
  auto &tmpReg = dynamic_cast<TC_ERegV &>( *(lvalue.getResultReg()) );

  TCINSTRUCTIONS.insertADDX(
    dynamic_cast<TC_DRegV &>( tmpReg.begin()->get() ),
    dynamic_cast<TC_DRegV &>( tmpReg.begin()->get() ), -1,  $1->getExp() );
  TCINSTRUCTIONS.insertADDC(
    dynamic_cast<TC_DRegV &>( tmpReg.rbegin()->get() ),
    dynamic_cast<TC_DRegV &>( tmpReg.rbegin()->get() ), -1,  $1->getExp() );

  lvalue.storeBack( $2->getExp() );

  return( make_pair( lvalue.getResultRegister(), ref( tmpReg ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
llereg: tpm_UnaryExpPREINC( llereg )
{
  $cost[0] = $cost[2] + 2 * TC13::OperationFormat::DDC9_1.getSize();
}
=
{
  DEBUG_RULE_ACTION( "llereg: tpm_UnaryExpPREINC( llereg )", $1 );

  auto p = $action[2]();

  // LLIR
  TCINSTRUCTIONS.insertADDX(
    getLVLLChild( p.first ), getLVLLChild( p.first ), 1, $1->getExp() );
  TCINSTRUCTIONS.insertADDC(
    getHVLLChild( p.first ), getHVLLChild( p.first ), 0, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertADDX(
    dynamic_cast<TC_DRegV &>( p.second.get().begin()->get() ),
    dynamic_cast<TC_DRegV &>( p.second.get().begin()->get() ), 1,
    $1->getExp() );
  TCINSTRUCTIONS.insertADDC(
    dynamic_cast<TC_DRegV &>( p.second.get().rbegin()->get() ),
    dynamic_cast<TC_DRegV &>( p.second.get().rbegin()->get() ), 0,
    $1->getExp() );

  return( p );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
llereg: tpm_UnaryExpPREINC( deref_llereg )
{
  $cost[0] =
    $cost[2] + 2 * TC13::OperationFormat::DDC9_1.getSize() +
    TC_AddressModification::createStoreCost( $2->getExp() );
}
=
{
  DEBUG_RULE_ACTION( "llereg: tpm_UnaryExpPREINC( deref_llereg )", $1 );

  auto lvalue = $action[2]( true );

  // LLIR
  TCINSTRUCTIONS.insertADDX(
    getLVLLChild( lvalue.getResultRegister() ),
    getLVLLChild( lvalue.getResultRegister() ), 1,  $1->getExp() );
  TCINSTRUCTIONS.insertADDC(
    getHVLLChild( lvalue.getResultRegister() ),
    getHVLLChild( lvalue.getResultRegister() ), 0, $1->getExp() );

  // WIR
  auto &tmpReg = dynamic_cast<TC_ERegV &>( *(lvalue.getResultReg()) );

  TCINSTRUCTIONS.insertADDX(
    dynamic_cast<TC_DRegV &>( tmpReg.begin()->get() ),
    dynamic_cast<TC_DRegV &>( tmpReg.begin()->get() ), 1,  $1->getExp() );
  TCINSTRUCTIONS.insertADDC(
    dynamic_cast<TC_DRegV &>( tmpReg.rbegin()->get() ),
    dynamic_cast<TC_DRegV &>( tmpReg.rbegin()->get() ), 0,  $1->getExp() );

  lvalue.storeBack( $2->getExp() );

  return( make_pair( lvalue.getResultRegister(), ref( tmpReg ) ) );
};


###############################################################################
#
#
# Sizeof expressions
#
#
###############################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
const9: tpm_UnaryExpSIZEOF( llereg )
{
  const int byteSize = computeSizeOf( $2 );

  if ( ( byteSize >= TC_Const9_Signed::getMinValue( 9 ) ) &&
       ( byteSize <= TC_Const9_Signed::getMaxValue( 9 ) ) )
    $cost[0] = 100;
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "const9: tpm_UnaryExpSIZEOF( llereg )", $1 );

  return( (long) computeSizeOf( $2 ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
uconst9: tpm_UnaryExpSIZEOF( llereg )
{
  const int byteSize = computeSizeOf( $2 );

  if ( ( byteSize >= 0 ) &&
       ( byteSize <= (int) TC_Const9_Unsigned::getMaxValue( 9 ) ) )
    $cost[0] = 100;
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "uconst9: tpm_UnaryExpSIZEOF( llereg )", $1 );

  return( (long) computeSizeOf( $2 ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
const16: tpm_UnaryExpSIZEOF( llereg )
{
  const int byteSize = computeSizeOf( $2 );

  if ( ( byteSize >= TC_Const16_Signed::getMinValue( 16 ) ) &&
       ( byteSize <= TC_Const16_Signed::getMaxValue( 16 ) ) )
    $cost[0] = 1000;
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "const16: tpm_UnaryExpSIZEOF( llereg )", $1 );

  return( (long) computeSizeOf( $2 ) );
};


###############################################################################
#
#
#  Relations
#
#
###############################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpLT( llereg, llereg )
{
  $cost[0] =
    $cost[2] + $cost[3] + TC13::OperationFormat::DDD_1.getSize() +
    3 * TC13::OperationFormat::DDD_2.getSize();
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpLT( llereg, llereg )", $1 );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertEQ(
    reg, getHVLLChild( p1.first ), getHVLLChild( p2.first ), $1->getExp() );
  TCINSTRUCTIONS.insertAND_LT_U(
    reg, getLVLLChild( p1.first ), getLVLLChild( p2.first ), $1->getExp() );
  if ( $2->getExp()->getType().isUnsignedType() ||
       $3->getExp()->getType().isUnsignedType() )
    TCINSTRUCTIONS.insertOR_LT_U(
      reg, getHVLLChild( p1.first ), getHVLLChild( p2.first ), $1->getExp() );
  else
    TCINSTRUCTIONS.insertOR_LT(
      reg, getHVLLChild( p1.first ), getHVLLChild( p2.first ), $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertEQ(
    r, dynamic_cast<TC_DRegV &>( p1.second.get().rbegin()->get() ),
    dynamic_cast<TC_DRegV &>( p2.second.get().rbegin()->get() ), $1->getExp() );
  TCINSTRUCTIONS.insertAND_LT_U(
    r, dynamic_cast<TC_DRegV &>( p1.second.get().begin()->get() ),
    dynamic_cast<TC_DRegV &>( p2.second.get().begin()->get() ), $1->getExp() );
  if ( $2->getExp()->getType().isUnsignedType() ||
       $3->getExp()->getType().isUnsignedType() )
    TCINSTRUCTIONS.insertOR_LT_U(
      r, dynamic_cast<TC_DRegV &>( p1.second.get().rbegin()->get() ),
      dynamic_cast<TC_DRegV &>( p2.second.get().rbegin()->get() ),
      $1->getExp() );
  else
    TCINSTRUCTIONS.insertOR_LT(
      r, dynamic_cast<TC_DRegV &>( p1.second.get().rbegin()->get() ),
      dynamic_cast<TC_DRegV &>( p2.second.get().rbegin()->get() ),
      $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpGT( llereg, llereg )
{
  $cost[0] =
    $cost[2] + $cost[3] + TC13::OperationFormat::DDD_1.getSize() +
    3 * TC13::OperationFormat::DDD_2.getSize();
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpGT( llereg, llereg )", $1 );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertEQ(
    reg, getHVLLChild( p2.first ), getHVLLChild( p1.first ), $1->getExp() );
  TCINSTRUCTIONS.insertAND_LT_U(
    reg, getLVLLChild( p2.first ), getLVLLChild( p1.first ), $1->getExp() );
  if ( $2->getExp()->getType().isUnsignedType() ||
       $3->getExp()->getType().isUnsignedType() )
    TCINSTRUCTIONS.insertOR_LT_U(
      reg, getHVLLChild( p2.first ), getHVLLChild( p1.first ), $1->getExp() );
  else
    TCINSTRUCTIONS.insertOR_LT(
      reg, getHVLLChild( p2.first ), getHVLLChild( p1.first ), $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertEQ(
    r, dynamic_cast<TC_DRegV &>( p2.second.get().rbegin()->get() ),
    dynamic_cast<TC_DRegV &>( p1.second.get().rbegin()->get() ), $1->getExp() );
  TCINSTRUCTIONS.insertAND_LT_U(
    r, dynamic_cast<TC_DRegV &>( p2.second.get().begin()->get() ),
    dynamic_cast<TC_DRegV &>( p1.second.get().begin()->get() ), $1->getExp() );
  if ( $2->getExp()->getType().isUnsignedType() ||
       $3->getExp()->getType().isUnsignedType() )
    TCINSTRUCTIONS.insertOR_LT_U(
      r, dynamic_cast<TC_DRegV &>( p2.second.get().rbegin()->get() ),
      dynamic_cast<TC_DRegV &>( p1.second.get().rbegin()->get() ),
      $1->getExp() );
  else
    TCINSTRUCTIONS.insertOR_LT(
      r, dynamic_cast<TC_DRegV &>( p2.second.get().rbegin()->get() ),
      dynamic_cast<TC_DRegV &>( p1.second.get().rbegin()->get() ),
      $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpLEQ( llereg, llereg )
{
  $cost[0] =
    $cost[2] + $cost[3] + TC13::OperationFormat::DDD_1.getSize() +
    3 * TC13::OperationFormat::DDD_2.getSize();
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpLEQ( llereg, llereg )", $1 );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertEQ(
    reg, getHVLLChild( p1.first ), getHVLLChild( p2.first ), $1->getExp() );
  TCINSTRUCTIONS.insertAND_GE_U(
    reg, getLVLLChild( p2.first ), getLVLLChild( p1.first ), $1->getExp() );
  if ( $2->getExp()->getType().isUnsignedType() ||
       $3->getExp()->getType().isUnsignedType() )
    TCINSTRUCTIONS.insertOR_LT_U(
      reg, getHVLLChild( p1.first ), getHVLLChild( p2.first ), $1->getExp() );
  else
    TCINSTRUCTIONS.insertOR_LT(
      reg, getHVLLChild( p1.first ), getHVLLChild( p2.first ), $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertEQ(
    r, dynamic_cast<TC_DRegV &>( p1.second.get().rbegin()->get() ),
    dynamic_cast<TC_DRegV &>( p2.second.get().rbegin()->get() ), $1->getExp() );
  TCINSTRUCTIONS.insertAND_GE_U(
    r, dynamic_cast<TC_DRegV &>( p2.second.get().begin()->get() ),
    dynamic_cast<TC_DRegV &>( p1.second.get().begin()->get() ), $1->getExp() );
  if ( $2->getExp()->getType().isUnsignedType() ||
       $3->getExp()->getType().isUnsignedType() )
    TCINSTRUCTIONS.insertOR_LT_U(
      r, dynamic_cast<TC_DRegV &>( p1.second.get().rbegin()->get() ),
      dynamic_cast<TC_DRegV &>( p2.second.get().rbegin()->get() ),
      $1->getExp() );
  else
    TCINSTRUCTIONS.insertOR_LT(
      r, dynamic_cast<TC_DRegV &>( p1.second.get().rbegin()->get() ),
      dynamic_cast<TC_DRegV &>( p2.second.get().rbegin()->get() ),
      $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpGEQ( llereg, llereg )
{
  $cost[0] =
    $cost[2] + $cost[3] + TC13::OperationFormat::DDD_1.getSize() +
    3 * TC13::OperationFormat::DDD_2.getSize();
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpGEQ( llereg, llereg )", $1 );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertEQ(
    reg, getHVLLChild( p1.first ), getHVLLChild( p2.first ), $1->getExp() );
  TCINSTRUCTIONS.insertAND_GE_U(
    reg, getLVLLChild( p1.first ), getLVLLChild( p2.first ), $1->getExp() );
  if ( $2->getExp()->getType().isUnsignedType() ||
       $3->getExp()->getType().isUnsignedType() )
    TCINSTRUCTIONS.insertOR_LT_U(
      reg, getHVLLChild( p2.first ), getHVLLChild( p1.first ), $1->getExp() );
  else
    TCINSTRUCTIONS.insertOR_LT(
      reg, getHVLLChild( p2.first ), getHVLLChild( p1.first ), $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertEQ(
    r, dynamic_cast<TC_DRegV &>( p1.second.get().rbegin()->get() ),
    dynamic_cast<TC_DRegV &>( p2.second.get().rbegin()->get() ), $1->getExp() );
  TCINSTRUCTIONS.insertAND_GE_U(
    r, dynamic_cast<TC_DRegV &>( p1.second.get().begin()->get() ),
    dynamic_cast<TC_DRegV &>( p2.second.get().begin()->get() ), $1->getExp() );
  if ( $2->getExp()->getType().isUnsignedType() ||
       $3->getExp()->getType().isUnsignedType() )
    TCINSTRUCTIONS.insertOR_LT_U(
      r, dynamic_cast<TC_DRegV &>( p2.second.get().rbegin()->get() ),
      dynamic_cast<TC_DRegV &>( p1.second.get().rbegin()->get() ),
      $1->getExp() );
  else
    TCINSTRUCTIONS.insertOR_LT(
      r, dynamic_cast<TC_DRegV &>( p2.second.get().rbegin()->get() ),
      dynamic_cast<TC_DRegV &>( p1.second.get().rbegin()->get() ),
      $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpEQ( llereg, llereg )
{
  $cost[0] =
    $cost[2] + $cost[3] + TC13::OperationFormat::DDD_1.getSize() +
    TC13::OperationFormat::DDD_2.getSize();
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpEQ( llereg, llereg )", $1 );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertEQ(
    reg, getHVLLChild( p1.first ), getHVLLChild( p2.first ), $1->getExp() );
  TCINSTRUCTIONS.insertAND_EQ(
    reg, getLVLLChild( p1.first ), getLVLLChild( p2.first ), $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertEQ(
    r, dynamic_cast<TC_DRegV &>( p1.second.get().rbegin()->get() ),
    dynamic_cast<TC_DRegV &>( p2.second.get().rbegin()->get() ), $1->getExp() );
  TCINSTRUCTIONS.insertAND_EQ(
    r, dynamic_cast<TC_DRegV &>( p1.second.get().begin()->get() ),
    dynamic_cast<TC_DRegV &>( p2.second.get().begin()->get() ), $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_BinaryExpNEQ( llereg, llereg )
{
  $cost[0] =
    $cost[2] + $cost[3] + TC13::OperationFormat::DDD_1.getSize() +
    TC13::OperationFormat::DDD_2.getSize();
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpNEQ( llereg, llereg )", $1 );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertNE(
    reg, getHVLLChild( p1.first ), getHVLLChild( p2.first ), $1->getExp() );
  TCINSTRUCTIONS.insertOR_NE(
    reg, getLVLLChild( p1.first ), getLVLLChild( p2.first ), $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertNE(
    r, dynamic_cast<TC_DRegV &>( p1.second.get().rbegin()->get() ),
    dynamic_cast<TC_DRegV &>( p2.second.get().rbegin()->get() ), $1->getExp() );
  TCINSTRUCTIONS.insertOR_NE(
    r, dynamic_cast<TC_DRegV &>( p1.second.get().begin()->get() ),
    dynamic_cast<TC_DRegV &>( p2.second.get().begin()->get() ), $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


#####################################################################
#
#
# If-then-else statements
#
#
#####################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
stmt: tpm_IfStmt( llereg )
{
  $cost[0] =
    $cost[2] + TC13::OperationFormat::DDC9_1.getSize() +
    TC13::OperationFormat::DDC9_3.getSize() +
    TC13::OperationFormat::DC4L_1.getSize();
}
=
{
  DEBUG_RULE_ACTION( "stmt: tpm_IfStmt( llereg )", $1 );

  auto p = $action[2]();

  const InstructionFactory::StmtType t = InstructionFactory::IF_STMT;

  // LLIR
  stringstream ss;
  ss << dynamic_cast<IR_IfStmt *>( $1->getStmt() )->getContinueBasicBlock();
  string bb = TCCODESEL->getBlockLabel( ss.str() );

  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );

  TCINSTRUCTIONS.insertNE( reg, getHVLLChild( p.first ), 0, $1->getExp(), t );
  TCINSTRUCTIONS.insertOR_NE( reg, getLVLLChild( p.first ), 0, $1->getExp(), t );
  TCINSTRUCTIONS.insertJEQ( reg, 0, bb, $1->getExp(), t );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertNE(
    r, dynamic_cast<TC_DRegV &>( p.second.get().rbegin()->get() ), 0,
    $1->getExp(), t );
  TCINSTRUCTIONS.insertOR_NE(
    r, dynamic_cast<TC_DRegV &>( p.second.get().begin()->get() ), 0,
    $1->getExp(), t );
  TCINSTRUCTIONS.insertJEQ(
    r, 0,
    TCCODESEL->getWIRBlock(
      dynamic_cast<IR_IfStmt *>( $1->getStmt() )->getContinueBasicBlock() ),
    $1->getExp(), t );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
stmt: tpm_IfElseStmt( llereg )
{
  $cost[0] =
    $cost[2] + TC13::OperationFormat::DDC9_1.getSize() +
    TC13::OperationFormat::DDC9_3.getSize() +
    TC13::OperationFormat::DC4L_1.getSize();
}
=
{
  DEBUG_RULE_ACTION( "stmt: tpm_IfElseStmt( llereg )", $1 );

  auto p = $action[2]();

  const InstructionFactory::StmtType t = InstructionFactory::IFELSE_STMT;

  // LLIR
  stringstream ss;
  ss << dynamic_cast<IR_IfElseStmt *>( $1->getStmt() )->getFalseBasicBlock();
  string bb = TCCODESEL->getBlockLabel( ss.str() );

  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );

  TCINSTRUCTIONS.insertNE( reg, getHVLLChild( p.first ), 0, 0, t );
  TCINSTRUCTIONS.insertOR_NE( reg, getLVLLChild( p.first ), 0, 0, t );
  TCINSTRUCTIONS.insertJEQ( reg, 0, bb, 0, t );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertNE(
    r, dynamic_cast<TC_DRegV &>( p.second.get().rbegin()->get() ), 0,
    $1->getExp(), t );
  TCINSTRUCTIONS.insertOR_NE(
    r, dynamic_cast<TC_DRegV &>( p.second.get().begin()->get() ), 0,
    $1->getExp(), t );
  TCINSTRUCTIONS.insertJEQ(
    r, 0,
    TCCODESEL->getWIRBlock(
      dynamic_cast<IR_IfElseStmt *>( $1->getStmt() )->getFalseBasicBlock() ),
    $1->getExp(), t );
};


#####################################################################
#
#
# For-loop
#
#
#####################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
stmt: tpm_ForStmt( llereg )
{
  $cost[0] =
    $cost[2] + TC13::OperationFormat::DDC9_1.getSize() +
    TC13::OperationFormat::DDC9_3.getSize() +
    TC13::OperationFormat::DC4L_1.getSize();
}
=
{
  DEBUG_RULE_ACTION( "stmt: tpm_ForStmt( llereg )", $1 );

  const InstructionFactory::StmtType t = InstructionFactory::FOR_STMT;

  // LLIR
  stringstream tmpStr, ss;
  tmpStr << $1->getStmt()->getBasicBlock();
  string bb = TCCODESEL->getBlockLabel( tmpStr.str() );
  if ( !TCCODESEL->containsBB( bb ) )
    beginNewLLIRBasicBlock( bb.c_str(), *$1->getStmt()->getBasicBlock() );

  auto p = $action[2]();

  auto *loop = dynamic_cast<IR_LoopStmt *>( $1->getStmt()->getParent() );
  ss << loop->getFalseBasicBlock();
  bb = TCCODESEL->getBlockLabel( ss.str() );

  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );

  TCINSTRUCTIONS.insertNE( reg, getHVLLChild( p.first ), 0, 0, t );
  TCINSTRUCTIONS.insertOR_NE( reg, getLVLLChild( p.first ), 0, 0, t );
  TCINSTRUCTIONS.insertJEQ( reg, 0, bb, 0, t );
  TCCODESEL->getLastLLIRBB()->AddPragma(
    new LLIR_Pragma( "Loop condition: FOR", true ) );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertNE(
    r, dynamic_cast<TC_DRegV &>( p.second.get().rbegin()->get() ), 0,
    $1->getExp(), t );
  TCINSTRUCTIONS.insertOR_NE(
    r, dynamic_cast<TC_DRegV &>( p.second.get().begin()->get() ), 0,
    $1->getExp(), t );
  TCINSTRUCTIONS.insertJEQ(
    r, 0,
    TCCODESEL->getWIRBlock(
      dynamic_cast<IR_LoopStmt *>(
        $1->getStmt()->getParent() )->getFalseBasicBlock() ),
    $1->getExp(), t );

  WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
  jmp.insertContainer( WIR_LoopExit( true ) );
};


#####################################################################
#
#
# While-loop
#
#
#####################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
stmt: tpm_WhileStmt( llereg )
{
  $cost[0] =
    $cost[2] + TC13::OperationFormat::DDC9_1.getSize() +
    TC13::OperationFormat::DDC9_3.getSize() +
    TC13::OperationFormat::DC4L_1.getSize();
}
=
{
  DEBUG_RULE_ACTION( "stmt: tpm_WhileStmt( llereg )", $1 );

  auto p = $action[2]();

  const InstructionFactory::StmtType t = InstructionFactory::WHILE_STMT;

  // LLIR
  stringstream ss;
  ss << dynamic_cast<IR_LoopStmt *>( $1->getStmt() )->getFalseBasicBlock();
  string bb = TCCODESEL->getBlockLabel( ss.str() );

  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );

  TCINSTRUCTIONS.insertNE( reg, getHVLLChild( p.first ), 0, 0, t );
  TCINSTRUCTIONS.insertOR_NE( reg, getLVLLChild( p.first ), 0, 0, t );
  TCINSTRUCTIONS.insertJEQ( reg, 0, bb, 0, t );
  TCCODESEL->getLastLLIRBB()->AddPragma(
    new LLIR_Pragma( "Loop condition: WHILE", true ) );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertNE(
    r, dynamic_cast<TC_DRegV &>( p.second.get().rbegin()->get() ), 0,
    $1->getExp(), t );
  TCINSTRUCTIONS.insertOR_NE(
    r, dynamic_cast<TC_DRegV &>( p.second.get().begin()->get() ), 0,
    $1->getExp(), t );
  TCINSTRUCTIONS.insertJEQ(
    r, 0,
    TCCODESEL->getWIRBlock(
      dynamic_cast<IR_LoopStmt *>( $1->getStmt() )->getFalseBasicBlock() ),
    $1->getExp(), t );

  WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
  jmp.insertContainer( WIR_LoopExit( true ) );
};


#####################################################################
#
#
# doWhile-loop
#
#
#####################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
stmt: tpm_DoWhileStmt( llereg )
{
  auto *doWhileStmt = dynamic_cast<IR_DoWhileStmt *>( $1->getStmt() );

  if ( TCCODESEL->getConfig()->getEnableLOOPInstruction() &&
       TCCODESEL->hasApplicableLoop( doWhileStmt ) )
    $cost[0] = transformToLOOPLoopCost( *doWhileStmt );
  else
    $cost[0] =
      $cost[2] + TC13::OperationFormat::DDC9_1.getSize() +
      TC13::OperationFormat::DDC9_3.getSize() +
      TC13::OperationFormat::DC4L_1.getSize();
}
=
{
  DEBUG_RULE_ACTION( "stmt: tpm_DoWhileStmt( llereg )", $1 );

  auto *doWhileStmt = dynamic_cast<IR_DoWhileStmt *>( $1->getStmt() );

  if ( TCCODESEL->getConfig()->getEnableLOOPInstruction() &&
       TCCODESEL->hasApplicableLoop( doWhileStmt ) )
    transformToLOOPLoop( *doWhileStmt );
  else {
    auto p = $action[2]();

    const InstructionFactory::StmtType t = InstructionFactory::DOWHILE_STMT;

    // LLIR
    stringstream ss;
    ss << dynamic_cast<IR_LoopStmt *>( $1->getStmt() )->getTrueBasicBlock();
    string bb = TCCODESEL->getBlockLabel( ss.str() );

    LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );

    TCINSTRUCTIONS.insertNE( reg, getHVLLChild( p.first ), 0, 0, t );
    TCINSTRUCTIONS.insertOR_NE( reg, getLVLLChild( p.first ), 0, 0, t );
    TCINSTRUCTIONS.insertJNE( reg, 0, bb, 0, t );
    TCCODESEL->getLastLLIRBB()->AddPragma(
      new LLIR_Pragma( "Loop condition: DOWHILE", true ) );

    // WIR
    auto &r = TCINSTRUCTIONS.createDReg();
    TCINSTRUCTIONS.insertNE(
      r, dynamic_cast<TC_DRegV &>( p.second.get().rbegin()->get() ), 0,
      $1->getExp(), t );
    TCINSTRUCTIONS.insertOR_NE(
      r, dynamic_cast<TC_DRegV &>( p.second.get().begin()->get() ), 0,
      $1->getExp(), t );
    TCINSTRUCTIONS.insertJNE(
      r, 0,
      TCCODESEL->getWIRBlock(
        dynamic_cast<IR_LoopStmt *>( $1->getStmt() )->getTrueBasicBlock() ),
      $1->getExp(), t );

    WIR_Operation &jmp = TC179x_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( false ) );
  }
};


#####################################################################
#
#
# Selection
#
#
#####################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
areg: tpm_CondExp( llereg, areg, areg )
{
  IR_Type *t3 = $3->getExp()->getType().usualUnaryConversion();
  IR_Type *t4 = $4->getExp()->getType().usualUnaryConversion();
  IR_Type *t0 = t3->usualBinaryConversion( *t4 );

  if ( isARegType( *t0 ) )
    $cost[0] =
      $cost[2] + $cost[3] + $cost[4] + TC13::OperationFormat::DDC9_1.getSize() +
      TC13::OperationFormat::DDC9_3.getSize() +
      TC13::OperationFormat::DC4L_1.getSize() +
      TC13::OperationFormat::L.getSize() +
      2 * TC13::OperationFormat::SAA_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "areg: tpm_CondExp( llereg, areg, areg )", $1 );

  string label0 = LLIR::getUniqueLabel();
  string label1 = LLIR::getUniqueLabel();
  string label2 = LLIR::getUniqueLabel();

  // Generate code for the condition in the current basic block.
  auto p1 = $action[2]();

  // LLIR
  // Generate conditional branch in the current basic block.
  LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertEQ( regTmp, getHVLLChild( p1.first ), 0, $1->getExp() );
  TCINSTRUCTIONS.insertAND_EQ(
    regTmp, getLVLLChild( p1.first ), 0, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( regTmp, 0, label1, $1->getExp() );

  // WIR
  // Create basic block for LHS argument of ':' operator.
  auto *currentBB = TC179x_wirBB;
  auto &b1 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  // Create basic block for RHS argument of ':' operator.
  auto &b2 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  // Create basic block for code after the '?' operator.
  auto &b3 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  TC179x_wirBB = currentBB;

  // Generate conditional branch in the current basic block.
  auto &tmpReg = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertEQ(
    tmpReg, dynamic_cast<TC_DRegV &>( p1.second.get().rbegin()->get() ), 0,
    $1->getExp() );
  TCINSTRUCTIONS.insertAND_EQ(
    tmpReg, dynamic_cast<TC_DRegV &>( p1.second.get().begin()->get() ), 0,
    $1->getExp() );
  TCINSTRUCTIONS.insertJNE( tmpReg, 0, b2, $1->getExp() );

  // LLIR
  // Create basic block for LHS argument of ':' operator.
  beginNewLLIRBasicBlock(
    label0.c_str(), *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  // Switch to basic block for LHS argument of ':' operator.
  TC179x_wirBB = &b1;

  // Generate code for LHS argument of ':' operator.
  auto p2 = $action[3]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "", true );
  TCINSTRUCTIONS.insertMOV_AA( reg, p2.first, $1->getExp() );
  TCINSTRUCTIONS.insertJ( label2, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createAReg();
  TCINSTRUCTIONS.insertMOV_AA( r, p2.second, $1->getExp() );
  TCINSTRUCTIONS.insertJ( b3, $1->getExp() );

  // LLIR
  // Create basic block for RHS argument of ':' operator.
  beginNewLLIRBasicBlock(
    label1.c_str(), *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  // Switch to basic block for RHS argument of ':' operator.
  TC179x_wirBB = &b2;

  // Generate code for RHS argument of ':' operator.
  auto p3 = $action[4]();

  // LLIR
  TCINSTRUCTIONS.insertMOV_AA( reg, p3.first, $1->getExp() );

  // Create new basic block containing future parent instructions.
  beginNewLLIRBasicBlock(
    label2.c_str(), *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  TCINSTRUCTIONS.insertMOV_AA( r, p3.second, $1->getExp() );
  // Switch to basic block after the '?' operator.
  TC179x_wirBB = &b3;

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: tpm_CondExp( llereg, dreg, dreg )
{
  IR_Type *t3 = $3->getExp()->getType().usualUnaryConversion();
  IR_Type *t4 = $4->getExp()->getType().usualUnaryConversion();
  IR_Type *t0 = t3->usualBinaryConversion( *t4 );

  if ( isDRegType( *t0 ) )
    $cost[0] =
      $cost[2] + $cost[3] + $cost[4] + TC13::OperationFormat::DDC9_1.getSize() +
      TC13::OperationFormat::DDC9_3.getSize() +
      TC13::OperationFormat::DC4L_1.getSize() +
      2 * TC13::OperationFormat::SDD_1.getSize() +
      TC13::OperationFormat::L.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_CondExp( llereg, dreg, dreg )", $1 );

  string label0 = LLIR::getUniqueLabel();
  string label1 = LLIR::getUniqueLabel();
  string label2 = LLIR::getUniqueLabel();

  // Generate code for the condition in the current basic block.
  auto p1 = $action[2]();

  // LLIR
  // Generate conditional branch in the current basic block.
  LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );                      \
  TCINSTRUCTIONS.insertEQ( regTmp, getHVLLChild( p1.first ), 0, $1->getExp() );
  TCINSTRUCTIONS.insertAND_EQ(
    regTmp, getLVLLChild( p1.first ), 0, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( regTmp, 0, label1, $1->getExp() );

  // WIR
  // Create basic block for LHS argument of ':' operator.
  auto *currentBB = TC179x_wirBB;
  auto &b1 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  // Create basic block for RHS argument of ':' operator.
  auto &b2 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  // Create basic block for code after the '?' operator.
  auto &b3 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  TC179x_wirBB = currentBB;

  // Generate conditional branch in the current basic block.
  auto &tmpReg = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertEQ(
    tmpReg, dynamic_cast<TC_DRegV &>( p1.second.get().rbegin()->get() ), 0,
    $1->getExp() );
  TCINSTRUCTIONS.insertAND_EQ(
    tmpReg, dynamic_cast<TC_DRegV &>( p1.second.get().begin()->get() ), 0,
    $1->getExp() );
  TCINSTRUCTIONS.insertJNE( tmpReg, 0, b2, $1->getExp() );

  // LLIR
  // Create basic block for LHS argument of ':' operator.
  beginNewLLIRBasicBlock(
    label0.c_str(), *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  // Switch to basic block for LHS argument of ':' operator.
  TC179x_wirBB = &b1;

  // Generate code for LHS argument of ':' operator.
  auto p2 = $action[3]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );

  TCINSTRUCTIONS.insertMOV( reg, p2.first, $1->getExp() );
  TCINSTRUCTIONS.insertJ( label2, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();

  TCINSTRUCTIONS.insertMOV( r, p2.second, $1->getExp() );
  TCINSTRUCTIONS.insertJ( b3, $1->getExp() );

  // LLIR
  // Create basic block for RHS argument of ':' operator.
  beginNewLLIRBasicBlock(
    label1.c_str(), *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  // Switch to basic block for RHS argument of ':' operator.
  TC179x_wirBB = &b2;

  // Generate code for RHS argument of ':' operator.
  auto p3 = $action[4]();

  // LLIR
  TCINSTRUCTIONS.insertMOV( reg, p3.first, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertMOV( r, p3.second, $1->getExp() );

  // LLIR
  // Create basic block for code after the '?' operator.
  beginNewLLIRBasicBlock(
    label2.c_str(), *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  // Switch to basic block after the '?' operator.
  TC179x_wirBB = &b3;

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
llereg: tpm_CondExp( nrel, llereg, llereg )
{
  $cost[0] =
    $cost[2] + $cost[3] + $cost[4] +
    4 * TC13::OperationFormat::SDD_1.getSize() +
    TC13::OperationFormat::L.getSize();
}
=
{
  DEBUG_RULE_ACTION( "llereg: tpm_CondExp( nrel, llereg, llereg )", $1 );

  string label0 = LLIR::getUniqueLabel();
  string label1 = LLIR::getUniqueLabel();
  string label2 = LLIR::getUniqueLabel();

  // WIR
  auto *currentBB = TC179x_wirBB;
  // Create basic block for LHS argument of ':' operator.
  auto &b1 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  // Create basic block for RHS argument of ':' operator.
  auto &b2 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  // Create basic block for code after the '?' operator.
  auto &b3 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  TC179x_wirBB = currentBB;

  // Generate code for the condition in the current basic block.
  $action[2]( label1, b2, false );

  // LLIR
  // Create basic block for LHS argument of ':' operator.
  beginNewLLIRBasicBlock(
    label0.c_str(), *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  TC179x_wirBB = &b1;

  // Generate code for LHS argument of ':' operator.
  auto p2 = $action[3]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateERegister( "" );

  TCINSTRUCTIONS.insertMOV( reg, p2.first, $1->getExp() );
  TCINSTRUCTIONS.insertJ( label2, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createEReg();

  TCINSTRUCTIONS.insertMOV( r, p2.second, $1->getExp() );
  TCINSTRUCTIONS.insertJ( b3, $1->getExp() );

  // LLIR
  // Create basic block for RHS argument of ':' operator.
  beginNewLLIRBasicBlock(
    label1.c_str(), *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  // Switch to basic block for RHS argument of ':' operator.
  TC179x_wirBB = &b2;

  // Generate code for RHS argument of ':' operator.
  auto p3 = $action[4]();

  // LLIR
  TCINSTRUCTIONS.insertMOV( reg, p3.first, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertMOV( r, p3.second, $1->getExp() );

  // LLIR
  // Create basic block for code after the '?' operator.
  beginNewLLIRBasicBlock(
    label2.c_str(), *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  // Switch to basic block after the '?' operator.
  TC179x_wirBB = &b3;

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
llereg: tpm_CondExp( dreg, llereg, llereg )
{
  $cost[0] =
    $cost[2] + $cost[3] + $cost[4] + TC13::OperationFormat::SDL.getSize() +
    4 * TC13::OperationFormat::SDD_1.getSize() +
    TC13::OperationFormat::L.getSize();
}
=
{
  DEBUG_RULE_ACTION( "llereg: tpm_CondExp( dreg, llereg, llereg )", $1 );

  string label0 = LLIR::getUniqueLabel();
  string label1 = LLIR::getUniqueLabel();
  string label2 = LLIR::getUniqueLabel();

  // Generate code for the condition in the current basic block.
  auto p1 = $action[2]();

  // LLIR
  // Generate conditional branch in the current basic block.
  TCINSTRUCTIONS.insertJZ( p1.first, label1, $1->getExp() );

  // WIR
  // Create basic block for LHS argument of ':' operator.
  auto *currentBB = TC179x_wirBB;
  auto &b1 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  // Create basic block for RHS argument of ':' operator.
  auto &b2 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  // Create basic block for code after the '?' operator.
  auto &b3 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  TC179x_wirBB = currentBB;

  // Generate conditional branch in the current basic block.
  TCINSTRUCTIONS.insertJZ( p1.second, b2, $1->getExp() );

  // LLIR
  // Create basic block for LHS argument of ':' operator.
  beginNewLLIRBasicBlock(
    label0.c_str(), *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  // Switch to basic block for LHS argument of ':' operator.
  TC179x_wirBB = &b1;

  // Generate code for LHS argument of ':' operator.
  auto p2 = $action[3]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateERegister( "" );

  TCINSTRUCTIONS.insertMOV( reg, p2.first, $1->getExp() );
  TCINSTRUCTIONS.insertJ( label2, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createEReg();

  TCINSTRUCTIONS.insertMOV( r, p2.second, $1->getExp() );
  TCINSTRUCTIONS.insertJ( b3, $1->getExp() );

  // LLIR
  // Create basic block for RHS argument of ':' operator.
  beginNewLLIRBasicBlock(
    label1.c_str(), *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  // Switch to basic block for RHS argument of ':' operator.
  TC179x_wirBB = &b2;

  // Generate code for RHS argument of ':' operator.
  auto p3 = $action[4]();

  // LLIR
  TCINSTRUCTIONS.insertMOV( reg, p3.first, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertMOV( r, p3.second, $1->getExp() );

  // LLIR
  // Create basic block for code after the '?' operator.
  beginNewLLIRBasicBlock(
    label2.c_str(), *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  // Switch to basic block after the '?' operator.
  TC179x_wirBB = &b3;

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
llereg: tpm_CondExp( llereg, llereg, llereg )
{
  $cost[0] =
    $cost[2] + $cost[3] + $cost[4] + TC13::OperationFormat::DDC9_1.getSize() +
    TC13::OperationFormat::DDC9_3.getSize() +
    TC13::OperationFormat::DC4L_1.getSize() +
    4 * TC13::OperationFormat::SDD_1.getSize() +
    TC13::OperationFormat::L.getSize();
}
=
{
  DEBUG_RULE_ACTION( "llereg: tpm_CondExp( llereg, llereg, llereg )", $1 );

  string label0 = LLIR::getUniqueLabel();
  string label1 = LLIR::getUniqueLabel();
  string label2 = LLIR::getUniqueLabel();

  // Generate code for the condition in the current basic block.
  auto p1 = $action[2]();

  // LLIR
  // Generate conditional branch in the current basic block.
  LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertEQ( regTmp, getHVLLChild( p1.first ), 0, $1->getExp() );
  TCINSTRUCTIONS.insertAND_EQ(
    regTmp, getLVLLChild( p1.first ), 0, $1->getExp() );
  TCINSTRUCTIONS.insertJNE( regTmp, 0, label1, $1->getExp() );

  // WIR
  // Create basic block for LHS argument of ':' operator.
  auto *currentBB = TC179x_wirBB;
  auto &b1 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  // Create basic block for RHS argument of ':' operator.
  auto &b2 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  // Create basic block for code after the '?' operator.
  auto &b3 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  TC179x_wirBB = currentBB;

  // Generate conditional branch in the current basic block.
  auto &tmpReg = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertEQ(
    tmpReg, dynamic_cast<TC_DRegV &>( p1.second.get().rbegin()->get() ), 0,
    $1->getExp() );
  TCINSTRUCTIONS.insertAND_EQ(
    tmpReg, dynamic_cast<TC_DRegV &>( p1.second.get().begin()->get() ), 0,
    $1->getExp() );
  TCINSTRUCTIONS.insertJNE( tmpReg, 0, b2, $1->getExp() );

  // LLIR
  // Create basic block for LHS argument of ':' operator.
  beginNewLLIRBasicBlock(
    label0.c_str(), *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  // Switch to basic block for LHS argument of ':' operator.
  TC179x_wirBB = &b1;

  // Generate code for LHS argument of ':' operator.
  auto p2 = $action[3]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateERegister( "" );

  TCINSTRUCTIONS.insertMOV( reg, p2.first, $1->getExp() );
  TCINSTRUCTIONS.insertJ( label2, $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createEReg();

  TCINSTRUCTIONS.insertMOV( r, p2.second, $1->getExp() );
  TCINSTRUCTIONS.insertJ( b3, $1->getExp() );

  // LLIR
  // Create basic block for RHS argument of ':' operator.
  beginNewLLIRBasicBlock(
    label1.c_str(), *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  // Switch to basic block for RHS argument of ':' operator.
  TC179x_wirBB = &b2;

  // Generate code for RHS argument of ':' operator.
  auto p3 = $action[4]();

  // LLIR
  TCINSTRUCTIONS.insertMOV( reg, p3.first, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertMOV( r, p3.second, $1->getExp() );

  // LLIR
  // Create basic block for code after the '?' operator.
  beginNewLLIRBasicBlock(
    label2.c_str(), *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  // Switch to basic block after the '?' operator.
  TC179x_wirBB = &b3;

  return( make_pair( reg, ref( r ) ) );
};


###############################################################################
#
#
# Switch, case and default statements
#
#
###############################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
stmt: tpm_SwitchStmt( llereg )
{
  auto *stmt = dynamic_cast<IR_SwitchStmt *>( $1->getStmt() );
  $cost[0] = $cost[2] + TC13::OperationFormat::L.getSize();

  for ( auto c : stmt->getCases() ) {
    const Integer constHi = getUpperLongLongWord( c.first );
    const Integer constLo = getLowerLongLongWord( c.first );

    // If the constant values have more than 9 bits, we must store them in
    // registers to perform the comparison.
    if ( getBitWidth( constHi ) > 9 ) {
      if ( ( constHi >= TC_Const16_Signed::getMinValue( 16 ) ) &&
           ( constHi <= TC_Const16_Signed::getMaxValue( 16 ) ) )
        $cost[0] += TC13::OperationFormat::DC16_1.getSize();
      else

      if ( ( constHi >= 0 ) &&
           ( constHi <= (int) TC_Const16_Unsigned::getMaxValue( 16 ) ) )
        $cost[0] += TC13::OperationFormat::DC16_2.getSize();
      else
        $cost[0] +=
          TC13::OperationFormat::DC16_2.getSize() +
          TC13::OperationFormat::DDC16_1.getSize();

      $cost[0] += TC13::OperationFormat::DDD_1.getSize();
    } else
      $cost[0] += TC13::OperationFormat::DDC9_1.getSize();

    if ( getBitWidth( constLo ) > 9 ) {
      if ( ( constLo >= TC_Const16_Signed::getMinValue( 16 ) ) &&
           ( constLo <= TC_Const16_Signed::getMaxValue( 16 ) ) )
        $cost[0] += TC13::OperationFormat::DC16_1.getSize();
      else

      if ( ( constLo >= 0 ) &&
           ( constLo <= (int) TC_Const16_Unsigned::getMaxValue( 16 ) ) )
        $cost[0] += TC13::OperationFormat::DC16_2.getSize();
      else
        $cost[0] +=
          TC13::OperationFormat::DC16_2.getSize() +
          TC13::OperationFormat::DDC16_1.getSize();

      $cost[0] += TC13::OperationFormat::DDD_1.getSize();
    } else
      $cost[0] += TC13::OperationFormat::DDC9_1.getSize();

    $cost[0] += TC13::OperationFormat::DC4L_1.getSize();
  }
}
=
{
  DEBUG_RULE_ACTION( "stmt: tpm_SwitchStmt( llereg )", $1 );

  auto p = $action[2]();

  auto *stmt = dynamic_cast<IR_SwitchStmt *>( $1->getStmt() );
  const InstructionFactory::StmtType t = InstructionFactory::SWITCH_STMT;

  // LLIR
  // Traverse through all case statements and generate jump (and possibly move)
  // instructions.
  for ( auto it = stmt->getCases().begin(); it != stmt->getCases().end();
        ++it ) {
    const Integer constHi = getUpperLongLongWord( it->first );
    const Integer constLo = getLowerLongLongWord( it->first );

    // Generate label for case statement used with the jump instruction below.
    // To get a unique id, read the basic block's hex address.
    stringstream caseAddress;
    caseAddress << it->second->getBasicBlock();
    string caseStmtLabel = TCCODESEL->getBlockLabel( caseAddress.str() );

    // If the constant values have more than 9 bits, we must store them in
    // registers to perform the comparison.
    LLIR_Register *regConstHi = nullptr;
    if ( getBitWidth( constHi ) > 9 ) {
      regConstHi = TCINSTRUCTIONS.CreateRegister( "" );
      TCINSTRUCTIONS.insertMOVConstant( regConstHi, constHi, $1->getExp() );
    }

    LLIR_Register *regConstLo = nullptr;
    if ( getBitWidth( constLo ) > 9 ) {
      regConstLo = TCINSTRUCTIONS.CreateRegister( "" );
      TCINSTRUCTIONS.insertMOVConstant( regConstLo, constLo, $1->getExp() );
    }

    // Check for equality with the case number.
    LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );
    if ( regConstHi )
      TCINSTRUCTIONS.insertEQ(
        regTmp, getHVLLChild( p.first ), regConstHi, $1->getExp() );
    else
      TCINSTRUCTIONS.insertEQ(
        regTmp, getHVLLChild( p.first ), constHi, $1->getExp() );

    if ( regConstLo )
      TCINSTRUCTIONS.insertAND_EQ(
        regTmp, getLVLLChild( p.first ), regConstLo, $1->getExp() );
    else
      TCINSTRUCTIONS.insertAND_EQ(
        regTmp, getLVLLChild( p.first ), constLo, $1->getExp() );

    TCINSTRUCTIONS.insertJNE(
      regTmp, 0, caseStmtLabel, $1->getExp(), t );

    // Begin new basic block after each jump instruction.
    beginNewLLIRBasicBlock();
  }

  // Finally, emit an unconditional jump to either the basic block of the
  // default-label or the succeeding basic block of the switch-statement.
  stringstream ss;
  if ( stmt->getDefault() )
    ss << stmt->getDefault()->getBasicBlock();
  else
    ss << stmt->getContinueBasicBlock();
  TCINSTRUCTIONS.insertJ( TCCODESEL->getBlockLabel( ss.str() ), $1->getExp(), t );

  // WIR
  // Traverse through all case statements and generate jump (and possibly move)
  // instructions.
  for ( auto c : stmt->getCases() ) {
    const Integer constHi = getUpperLongLongWord( c.first );
    const Integer constLo = getLowerLongLongWord( c.first );

    // If the constant values have more than 9 bits, we must store them in
    // registers to perform the comparison.
    TC_DRegV *regConstHi = nullptr ;
    if ( getBitWidth( constHi ) > 9 ) {
      regConstHi = &(TCINSTRUCTIONS.createDReg());
      TCINSTRUCTIONS.insertMOVConstant( *regConstHi, constHi, $1->getExp() );
    }

    TC_DRegV *regConstLo = nullptr;
    if ( getBitWidth( constLo ) > 9 ) {
      regConstLo = &(TCINSTRUCTIONS.createDReg());
      TCINSTRUCTIONS.insertMOVConstant( *regConstLo, constLo, $1->getExp() );
    }

    // Check for equality with the case number.
    auto &tmpReg = TCINSTRUCTIONS.createDReg();
    if ( regConstHi != nullptr )
      TCINSTRUCTIONS.insertEQ(
        tmpReg, dynamic_cast<TC_DRegV &>( p.second.get().rbegin()->get() ),
        *regConstHi, $1->getExp() );
    else
      TCINSTRUCTIONS.insertEQ(
        tmpReg, dynamic_cast<TC_DRegV &>( p.second.get().rbegin()->get() ),
        constHi, $1->getExp() );

    if ( regConstLo != nullptr )
      TCINSTRUCTIONS.insertAND_EQ(
        tmpReg, dynamic_cast<TC_DRegV &>( p.second.get().begin()->get() ),
        *regConstLo, $1->getExp() );
    else
      TCINSTRUCTIONS.insertAND_EQ(
        tmpReg, dynamic_cast<TC_DRegV &>( p.second.get().begin()->get() ),
        constLo, $1->getExp() );

    TCINSTRUCTIONS.insertJNE(
      tmpReg, 0, TCCODESEL->getWIRBlock( c.second->getBasicBlock() ),
      $1->getExp(), t );

    // Begin new basic block after each jump instruction.
    TCCODESEL->startNewBasicBlock();
  }

  // Finally, emit an unconditional jump to either the basic block of the
  // default-label or the succeeding basic block of the switch-statement.
  TCINSTRUCTIONS.insertJ(
    TCCODESEL->getWIRBlock(
      stmt->getDefault() ?
        stmt->getDefault()->getBasicBlock() : stmt->getContinueBasicBlock() ),
    $1->getExp(), t );
};


##############################################################################
#
#
# Function calls, argument passing and returns
#
#
###############################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
llereg: tpm_CallExp( called_function, arg )
{
  if ( isLongLongType( *$0->getExp() ) )
    $cost[0] = $cost[2] + $cost[3];
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "llereg: tpm_CallExp( called_function, arg )", $1 );

  auto *theCall = dynamic_cast<IR_CallExp *>( $1->getExp() );

  // For type conversions of complex types, certain library routines might need
  // to be invoked. This cannot be done while the argument vector is mapped into
  // regs or onto stack, since those invocations would overwrite the already
  // assigned registers. Because of this, a two-pass approach is necessary.
  regptrList rhsRegs;
  regptrList lhsRegs;

  argList args;
  argList dryArgs;

  $action[3]( true, 0, 0, theCall, &lhsRegs, &rhsRegs, args, dryArgs );
  $action[3]( false, 0, 0, theCall, &lhsRegs, &rhsRegs, args, dryArgs );

  auto p = $action[2]( lhsRegs, args, RegType::EXTENDED_REGISTER );

  return(
    make_pair( p.first, ref( dynamic_cast<TC_ERegV &>( *(p.second )) ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
arg: tpm_CallExpARG( llereg, arg )
{
  // Functions in prototype form are not supported yet, because in that case,
  // the TriCore ABI forces us to derive a proper function type from the actual
  // parameters, which possibly means from the actual parameters in all places
  // where the function is called. This is not done yet.
  auto *cexp = dynamic_cast<IR_CallExp *>( $2->getExp()->getParent() );

  if ( cexp->getFunctionType().isPrototypeForm() )
    $cost[0] = $cost[2] + $cost[3] + 2 * TC13::OperationFormat::SDD_1.getSize();
  else {
    throw ufFatalError( "Functions without prototype are not supported." );
    $cost[0] = COST_INFINITY;
  }
}
=
{
  DEBUG_RULE_ACTION( "arg: tpm_CallExpARG( llereg, arg )", $1 );

  int incr = 0;

  if ( dryrun ) {

    auto p = $action[2]();

    rhsRegs->push_back( p.first );
    dryArgs.push_back( p.second.get() );

  } else {

    int regNo =
      Stack::isPassedThroughRegister( theCall->getFunctionType(), index );

    // LLIR
    LLIR_Register *rhsreg = rhsRegs->front();
    rhsRegs->pop_front();

    if ( regNo ) {
      LLIR_Register *lhsreg =
        &getFunctionArgumentRegister( *theCall, index, regNo );
      TCINSTRUCTIONS.insertMOV( lhsreg, rhsreg, $1->getExp() );

      if ( lhsRegs != nullptr )
        lhsRegs->push_back( lhsreg );
    } else {
      // Pass argument via the stack.
      incr = Stack::getStackSize( &effectiveType( *$2->getExp() ) );

      LLIR_Register *sp = TCINSTRUCTIONS.CreateRegister( PHREG_SP );
      TCINSTRUCTIONS.insertST_D( OPER_BASE, sp, offset, rhsreg, $1->getExp() );
      TCCODESEL->getLastLLIRBB()->GetLastIns()->AddPragma(
        new LLIR_Pragma( "Passing overflow function parameter", true ) );
    }

    // WIR
    auto &r = dynamic_cast<TC_ERegV &>( dryArgs.front().get() );
    dryArgs.pop_front();

    if ( regNo ) {
      // Pass argument via registers.
      auto &phreg =
        dynamic_cast<TC_ERegV &>( getFctArgReg( *theCall, index, regNo ) );
      TCINSTRUCTIONS.insertMOV( phreg, r, $1->getExp() );

      args.push_back( phreg );
    } else {
      // Pass argument via stack.
      incr = Stack::getStackSize( &effectiveType( *$2->getExp() ) );

      auto &sp = TCINSTRUCTIONS.createAReg();
      TC179x_wirFct->insertPrecolor( sp, TC179x_wirProc->SP() );
      TCINSTRUCTIONS.insertST_D( sp, offset, r, $1->getExp() );

      // Mark access to overflow region for register allocator.
      TC179x_wirBB->rbegin()->get().begin()->get().begin()->
        get().setDontOptimize();
    }
  }

  $action[3](
    dryrun, index + 1, offset + incr, theCall, lhsRegs, rhsRegs, args,
    dryArgs );

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
stmt: tpm_ReturnStmt( llereg )
{
  $cost[0] =
    $cost[2] + 2 * TC13::OperationFormat::SDD_1.getSize() +
    TC13::OperationFormat::S.getSize();
}
=
{
  DEBUG_RULE_ACTION( "stmt: tpm_ReturnStmt( llereg )", $1 );

  auto p = $action[2]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateERegister( "" );
  bindToPHREG( *reg, 2 );

  // Copy result to E2.
  TCINSTRUCTIONS.insertMOV( reg, p.first, $1->getExp() );
  TCINSTRUCTIONS.insertRET( reg, $1->getExp(), InstructionFactory::RETURN_STMT );

  // WIR
  auto &e2 = TCINSTRUCTIONS.createEReg();
  TC179x_wirFct->insertPrecolor( e2, TC179x_wirProc->E2() );
  TCINSTRUCTIONS.insertMOV( e2, p.second, $1->getExp() );
  TCINSTRUCTIONS.insertRET( e2, $1->getExp(), InstructionFactory::RETURN_STMT );
};


##############################################################################
#
#
# Expressions for Pointer Handling
#
#
###############################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
areg: tpm_UnaryExpADDR( deref_llereg )
{
  if ( !isZeroOpADDR( *$1->getExp() ) )
    $cost[0] = $cost[2] + TC13::OperationFormat::AAC16BOA.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "areg: tpm_UnaryExpADDR( deref_llereg )", $1 );

  auto lvalue = $action[2]( false );
  lvalue.convertToBaseOffsetForm( $1->getExp() );

  // LLIR
  auto *reg =
    &loadAccessLocationToAReg(
      lvalue.getAddress().getARegister(), lvalue.getOffset(), "",
      $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createAReg();
  auto &aReg = dynamic_cast<TC_ARegV &>( lvalue.getAddress().getAReg() );

  if ( lvalue.getOffset() == 0 )
    TCINSTRUCTIONS.insertMOV_AA( r, aReg, $1->getExp() );
  else
    TCINSTRUCTIONS.insertLEA( r, aReg, lvalue.getOffset(), $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
deref_llereg: tpm_UnaryExpDEREF( areg )
{
  if ( isLongLongPointer( *$2->getExp() ) )
    $cost[0] = $cost[2] + TC13::OperationFormat::EAC10BOA.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "deref_llereg: tpm_UnaryExpDEREF( areg )", $1 );

  auto p = $action[2]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateERegister( "" );
  if ( loadResult )
    TCINSTRUCTIONS.insertLD_D( reg, OPER_BASE, p.first, 0, $1->getExp() );

  // WIR
  TC_ERegV *r = nullptr;

  if ( loadResult ) {
    r = &(TCINSTRUCTIONS.createEReg());

    TCINSTRUCTIONS.insertLD_D( *r, p.second, 0, $1->getExp() );
  }

  return(
    TC_LValue {
      loadResult ? reg : nullptr, r,
      TC_AddressModification {
        p.first, p.second, 0, getBaseType( *$2->getExp() ), true } } );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
deref_llereg: tpm_UnaryExpDEREF( modified_areg )
{
  // This rule implements a load instruction combined with an address
  // increment / decrement.
  if ( isLongLongPointer( *$2->getExp() ) )
    $cost[0] = $cost[2] + TC13::OperationFormat::EAC10BOA.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "deref_llereg: tpm_UnaryExpDEREF( modified_areg )", $1 );

  auto amod = $action[2]();

  LLIR_Register *reg = nullptr;
  TC_ERegV *r = nullptr;

  if ( loadResult ) {
    reg = TCINSTRUCTIONS.CreateERegister( "" );
    r = &(TCINSTRUCTIONS.createEReg());

    amod.createLoad( reg, r, $1->getExp() );
  }

  return( TC_LValue { reg, r, amod } );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
addrOffset: llconst
{
  // All operations on aregs are already implemented in tc_rules.m4, but these
  // rules like e.g., pointer addition and subtraction, only take "dreg" and
  // "addrOffset" operands to generate the operations. For long long values that
  // are added/subtracted to/from a pointer, we need to provide this conversion
  // rule that allows to convert "llconst"s to "addressOffset"s.
  if ( isUsedForPointerArithmetic( *$1->getExp() ) )
    $cost[0] = 0;
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "addrOffset: llconst", $1 );

  return( tolong( $action[1]() ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: llereg
{
  // All operations on aregs are already implemented in tc_rules.m4, but these
  // rules like, e.g., pointer addition and subtraction, only take "dreg" and
  // "addrOffset" operands to generate the operations. For long long values that
  // are added/subtracted to/from a pointer, we need to provide this conversion
  // rule that allows to convert "llereg"s to "dreg"s.
  if ( isUsedForPointerArithmetic( *$1->getExp() ) )
    $cost[0] =
      Cast::castingCost( getIntSymbol()->getType(), $1->getExp()->getType() );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: llereg", $1 );

  auto p = $action[1]();

  // LLIR
  LLIR_Register *reg =
    Cast::doCasting(
      getIntSymbol()->getType(), $1->getExp()->getType(), p.first );
  ufAssertT( reg, "longlong to int cast must have nonzero result." );

  // WIR
  auto &r =
    dynamic_cast<TC_DRegV &>(
      Cast::doCasting(
        getIntSymbol()->getType(), $1->getExp()->getType(), p.second ) );

  return( make_pair( reg, ref( r ) ) );
};


##############################################################################
#
#
# Expressions for Array Handling
#
#
###############################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
deref_llereg: tpm_IndexExp( areg, dreg )
{
  if ( isLongLongType( *getBaseType( *$2->getExp() ) ) )
    $cost[0] =
      $cost[2] + $cost[3] + TC13::OperationFormat::EAC10BOA.getSize() +
      loadRegisterRelativeAddressCost( longLongBytes );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "deref_llereg: tpm_IndexExp( areg, dreg )", $1 );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateERegister( "" );
  LLIR_Register *regTmp =
    loadRegisterRelativeAddress(
      p1.first, p2.first, longLongBytes, $1->getExp() );

  if ( loadResult )
    TCINSTRUCTIONS.insertLD_D( reg, OPER_BASE, regTmp, 0, $1->getExp() );

  // WIR
  TC_ERegV *r = nullptr;
  auto &tmpReg =
    loadRegRelativeAddr( p1.second, p2.second, longLongBytes, $1->getExp() );

  if ( loadResult ) {
    r = &(TCINSTRUCTIONS.createEReg());

    TCINSTRUCTIONS.insertLD_D( *r, tmpReg, 0, $1->getExp() );
  }

  return(
    TC_LValue {
      loadResult ? reg : nullptr, r,
      TC_AddressModification {
        regTmp, tmpReg, 0, getBaseType( *$2->getExp() ), true } } );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
deref_llereg: tpm_IndexExp( areg, addrOffset )
{
  if ( isLongLongType( *getBaseType( *$2->getExp() ) ) )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::EAC10BOA.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "deref_llereg: tpm_IndexExp( areg, addrOffset )", $1 );

  auto p = $action[2]();
  int off = $action[3]().getIntValue() * longLongBytes;

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateERegister( "" );
  if ( loadResult )
    TCINSTRUCTIONS.insertLD_D( reg, OPER_BASE, p.first, off, $1->getExp() );

  // WIR
  TC_ERegV *r = nullptr;

  if ( loadResult ) {
    r = &(TCINSTRUCTIONS.createEReg());

    TCINSTRUCTIONS.insertLD_D( *r, p.second, off, $1->getExp() );
  }

  return(
    TC_LValue {
      loadResult ? reg : nullptr, r,
      TC_AddressModification {
        p.first, p.second, off, getBaseType( *$2->getExp() ), true } } );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
addrOffset: llconst
{
  // All index expression rules take constants ("addrOffset") or registers
  // ("dreg") as their index. If an "llconst" or an "llereg" is used instead of
  // an "addrOffset" or "dreg", then we need mapping rules that allow us to
  // convert an "llconst" or an "llereg" into an "addrOffset" or a "dreg". This
  // is done in this rule.
  if ( isUsedAsIndex( *$1->getExp() ) )
    $cost[0] = 0;
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "addrOffset: llconst", $1 );

  return( tolong( $action[1]() ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: llereg
{
  // All index expression rules take constants ("addrOffset") or registers
  // ("dreg") as their index. If an "llconst" or an "llereg" is used instead of
  // an "addrOffset" or "dreg", then we need mapping rules that allow us to
  // convert an "llconst" or an "llereg" into an "addrOffset" or a "dreg". This
  // is done in this rule.
  if ( isUsedAsIndex( *$1->getExp() ) )
    $cost[0] =
      Cast::castingCost( getIntSymbol()->getType(), $1->getExp()->getType() );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: llereg", $1 );

  auto p = $action[1]();

  // LLIR
  LLIR_Register *reg =
    Cast::doCasting(
      getIntSymbol()->getType(), $1->getExp()->getType(), p.first );
  ufAssertT( reg, "longlong to int cast must have nonzero result." );

  // WIR
  auto &r =
    dynamic_cast<TC_DRegV &>(
      Cast::doCasting(
        getIntSymbol()->getType(), $1->getExp()->getType(), p.second ) );

  return( make_pair( reg, ref( r ) ) );
};
