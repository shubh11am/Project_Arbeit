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
# pointers


###############################################################################
#
#
# Statement rules
#
#
###############################################################################


stmt: tpm_ExpStmt( areg )
{
  $cost[0] = $cost[2];
}
=
{
  RV32::DEBUG_RULE_ACTION( "stmt: tpm_ExpStmt( areg )", $1 );

  $action[2]();
  return;
};


###############################################################################
#
#
# Simple expressions
#
#
###############################################################################


areg: tpm_SymbolExp
{
  // Acquire the expression and the symbol itself.
  auto &symExp = dynamic_cast<IR_SymbolExp &>( *$1->getExp() );
  auto &sym = symExp.getSymbol();

  // Match this rule iff the symbol is of pointer type, is not global and does
  // not reside in the stack.
  if ( ( RV32::isPointerType( sym ) ||
       ( RV32::isArrayType( $1->getExp()->getType() ) && 
       RV32::isFunctionArgument( *$1->getExp() ) ) ) &&
       !sym.isGlobal() && ( RVCODESEL.getStack().getSymbolOffset( sym ) < 0 ) )
    $cost[0] = RV32::loadRegisterSymbolCost( symExp );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION( "areg: tpm_SymbolExp", $1 );
   
  // Acquire the expression.
  auto &symExp = dynamic_cast<IR_SymbolExp &>( *$1->getExp() );

  // And load the symbol.
  return( dynamic_cast<RV_RegV &>( RV32::loadRegSym( symExp ) ) );
};


areg: tpm_SymbolExp
{
  // Handles function symbols - always load (global) symbols address
  auto &symExp = dynamic_cast<IR_SymbolExp &>( *$1->getExp() );
  auto &sym = symExp.getSymbol();

  if ( RV32::isFunctionType( sym ) )
    $cost[0] = RV32::loadGlobalSymbolCost( symExp );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION( "areg: tpm_SymbolExp", $1 );

  auto &symExp = dynamic_cast<IR_SymbolExp &>( *$1->getExp() );
  auto lvalue = RV32::loadGlobalSymbol( symExp, true );

  return( dynamic_cast<RV_RegV &>( *(lvalue.getResultReg()) ) );
};


areg: tpm_SymbolExp
{
  // This rule handles "real" arrays (not function arguments).
  // Only their address must be loaded into a register.
  auto &symExp = dynamic_cast<IR_SymbolExp &>( *$1->getExp() );
  auto &sym = symExp.getSymbol();

  if ( RV32::isArrayType( sym.getType() ) && 
     !RV32::isFunctionArgument( symExp ) ) {
    $cost[0] = 0;

    if ( sym.isGlobal() )
      $cost[0] += RV32::loadGlobalSymbolCost( symExp );
    else

    if ( RVCODESEL.getStack().getSymbolOffset( sym ) >= 0 )
      $cost[0] += RV32::loadStackSymbolCost( symExp );
    else
      $cost[0] += RV32::loadRegisterSymbolCost( symExp );
  } else
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION( "areg: tpm_SymbolExp", $1 );

  auto &symExp = dynamic_cast<IR_SymbolExp &>( *$1->getExp() );
  auto &sym = symExp.getSymbol();

  if ( sym.isGlobal() ) {
    auto lvalue = RV32::loadGlobalSymbol( symExp, true );
    return( dynamic_cast<RV_RegV &>( *(lvalue.getResultReg()) ) );
  } else
  if ( RVCODESEL.getStack().getSymbolOffset( sym ) >= 0 ) {
    auto lvalue = RV32::loadStackSymbol( symExp, true );
    return( dynamic_cast<RV_RegV &>( *(lvalue.getResultReg()) ) );
  } else {
    return( dynamic_cast<RV_RegV &>( RV32::loadRegSym( symExp ) ) );
  }
};


deref_areg: tpm_SymbolExp
{
  // Acquire the expression and the symbol itself.
  auto &symExp = dynamic_cast<IR_SymbolExp &>( *$1->getExp() );
  auto &sym = symExp.getSymbol();

  // Match this rule iff the symbol is of pointer type, is not global and does
  // reside in the stack.
  
  if ( ( RV32::isPointerType( sym ) ||
       ( RV32::isArrayType( symExp.getType() ) && 
       RV32::isFunctionArgument( symExp ) ) ) &&
       !sym.isGlobal() &&
       ( RVCODESEL.getStack().getSymbolOffset( sym ) >= 0 ) )
    $cost[0] = RV32::loadStackSymbolCost(symExp);
  else
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION( "deref_areg: tpm_SymbolExp", $1 );

  // Acquire the expression.
  auto &symExp = dynamic_cast<IR_SymbolExp &>( *$1->getExp() );

  // Pass through the info on whether the result should actually be loaded.
  return( RV32::loadStackSymbol( symExp, loadResult ) );
};


deref_areg: tpm_SymbolExp
{
  // Handles global 'areg' symbols.
  auto &symExp = dynamic_cast<IR_SymbolExp &>( *$1->getExp() );
  auto &sym = symExp.getSymbol();

  if ( RV32::isPointerType( sym ) && sym.isGlobal() )
    $cost[0] = RV32::loadGlobalSymbolCost( symExp );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION( "deref_areg: tpm_SymbolExp", $1 );

  auto &symExp = dynamic_cast<IR_SymbolExp &>( *$1->getExp() );
  return( RV32::loadGlobalSymbol( symExp, loadResult ) );
};


areg: deref_areg
{
  // Chain rule that transforms a deref_areg to a simple areg.
  // If the memory location of the deref_areg is not important, i.e., the
  // memory location of the deref_areg is not written to, it can be converted.
  if ( !(RV32::isMemoryWriteLocation( *$1->getExp() )) )
    $cost[0] = $cost[1];
  else
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION( "areg: deref_areg", $1 );

  auto di = $action[1]( true );

  return( dynamic_cast<RV_RegV &>( *(di.getResultReg()) ) );
};


###############################################################################
#
#
# Arithmetic operations
#
#
###############################################################################


areg: modified_areg
{
  // Chain rule that actually performs pointer arithmetics by transforming a
  // modified_areg into a plain areg.
  $cost[0] = $cost[1] + RV32::RV32_AddressModification::applyModificationCost();
}
=
{
  RV32::DEBUG_RULE_ACTION( "areg: modified_areg", $1 );

  // Get the RV32_AddressModification.
  auto amod = $action[1]();

  // And perform the pointer arithmetics.
  return( *amod.applyModification( $1->getExp() ) );
};


areg: tpm_BinaryExpPLUS( areg, addrOffset )
{
  $cost[0] = $cost[2] + $cost[3] + RV32I::OperationFormat::RRC12_1.getSize();
}
=
{
  RV32::DEBUG_RULE_ACTION( "areg: tpm_BinaryExpPLUS( areg, addrOffset )", $1 );

  auto *t = RV32::getBaseType( $2->getExp()->getType() );
  const int off = $action[3]().getIntValue() * RV32::computeSizeOf( t );

  auto &p = $action[2]();

  auto &r = RVINSTRUCTIONS.createReg();
  RVINSTRUCTIONS.insertADDI( r, p, off, $1->getExp() );

  return( r );
};


modified_areg: tpm_BinaryExpPLUS( areg, reg )
{
  $cost[0] = $cost[2] + $cost[3];
}
=
{
  RV32::DEBUG_RULE_ACTION( "modified_areg: tpm_BinaryExpPLUS( areg, reg )", $1 );

  // Retrieve the base type.
  auto &ptrType = dynamic_cast<IR_PointerType &>( $2->getExp()->getType() );
  auto &t = ptrType.getBaseType();

  // Evaluate both sides of the expression, left-hand side first.
  auto &lhsReg = $action[2]();
  auto &rhsReg = $action[3]();

  // Assemble the RV32_AddressModification.
  return(
    RV32::RV32_AddressModification {
      lhsReg, rhsReg, &t, AddressModification::ModTime::NONE,
      AddressModification::ModOper::ADD } );
};


areg: tpm_BinaryExpMINUS( areg, addrOffset )
{
  $cost[0] = $cost[2] + $cost[3] + RV32I::OperationFormat::RRC12_1.getSize();
}
=
{
  RV32::DEBUG_RULE_ACTION( "areg: tpm_BinaryExpMINUS( areg, addrOffset )", $1 );

  auto *t = RV32::getBaseType( $2->getExp()->getType() );
  const int off = -( $action[3]().getIntValue() * RV32::computeSizeOf( t ) );

  auto &p = $action[2]();

  auto &r = RVINSTRUCTIONS.createReg();
  RVINSTRUCTIONS.insertADDI( r, p, off, $1->getExp() );

  return( r );
};


modified_areg: tpm_BinaryExpMINUS( areg, reg )
{
  $cost[0] = $cost[2] + $cost[3];
}
=
{
  RV32::DEBUG_RULE_ACTION( "modified_areg: tpm_BinaryExpMINUS( areg, reg )", $1 );

  // Retrieve the base type.
  auto &ptrType = dynamic_cast<IR_PointerType &>( $2->getExp()->getType() );
  auto &t = ptrType.getBaseType();

  // Evaluate both sides of the expression, left-hand side first.
  auto &lhsReg = $action[2]();
  auto &rhsReg = $action[3]();

  // Assemble the RV32_AddressModification.
  return(
    RV32::RV32_AddressModification {
      lhsReg, rhsReg, &t, AddressModification::ModTime::NONE,
      AddressModification::ModOper::SUB } );
};


reg: tpm_BinaryExpMINUS( areg, areg )
{
  //IR_Type *baseType = getBaseType( *$2->getExp() );
  auto *t = RV32::getBaseType( $2->getExp()->getType() );
  const int byteSize = RV32::computeSizeOf( t );

  // Determine wether size of a pointer type is a power of two.
  bool isPowerOfTwo = false;
  long long powerVal = 2;

  for ( int i = 1; i < 32; ++i, powerVal *= 2 )
    if ( byteSize == powerVal ) {
      isPowerOfTwo = true;
      break;
    }

  $cost[0] =
    $cost[2] + $cost[3] + RV32IMC::OperationFormat::RRR_1.getSize();

  if ( isPowerOfTwo )
    $cost[0] += RV32I::OperationFormat::RRC5_1.getSize();
  else {
    $cost[0] += RVINSTRUCTIONS.getMOVConstantCost( byteSize ) + 
    RV32IM::OperationFormat::RRR_1.getSize();
  }
}
=
{
  RV32::DEBUG_RULE_ACTION( "reg: tpm_BinaryExpMINUS( areg, areg )", $1 );

  auto *t = RV32::getBaseType( $2->getExp()->getType() );
  const int byteSize = RV32::computeSizeOf( t );

  // Determine whether size of pointer type is a power of two.
  bool isPowerOfTwo = false;
  long long powerVal = 2;
  long power = 0;

  for ( int i = 1; i < 32; ++i, powerVal *= 2 )
    if ( byteSize == powerVal ) {
      isPowerOfTwo = true;
      power = i;
      break;
    }

  auto &p1 = $action[2]();
  auto &p2 = $action[3]();

  //auto &tmpAReg = RVINSTRUCTIONS.createReg();
  auto &r = RVINSTRUCTIONS.createReg();

  // Generate the operation.
  RVINSTRUCTIONS.insertSUB( r, p1, p2, $1->getExp() );

  if ( isPowerOfTwo )
    RVINSTRUCTIONS.insertSRAI( r, r, power, $1->getExp() );
    //TCINSTRUCTIONS.insertSHA( r, r, -power, $1->getExp() );
  else {
    auto &tmpReg = RVINSTRUCTIONS.createReg();
    //TCINSTRUCTIONS.insertMOVConstant( tmpReg, byteSize, $1->getExp() );
    //TCINSTRUCTIONS.insertDIV_W( r, r, tmpReg, $1->getExp() );
    RVINSTRUCTIONS.insertMOVConstant( tmpReg, byteSize, $1->getExp() );
    RVINSTRUCTIONS.insertDIV( r, r, tmpReg, $1->getExp() );
  }

  return( r );
};


###############################################################################
#
#
#  Relations
#
#
###############################################################################


nrel: tpm_BinaryExpEQ( areg, areg )
{
  $cost[0] = $cost[2] + $cost[3] + RV32I::OperationFormat::RRL_1.getSize();
}
=
{
  RV32::DEBUG_RULE_ACTION( "nrel: tpm_BinaryExpEQ( areg, areg )", $1 );

  auto &reg2 = $action[2]();
  auto &reg3 = $action[3]();

  RVINSTRUCTIONS.insertBNE( reg2, reg3, b, $1->getExp());

  if ( markLoopExit ) {
    WIR_Operation &jmp = RV32::RV32_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( true ) );
  }

  return;
};


nrel: tpm_BinaryExpNEQ( areg, areg )
{
  $cost[0] = $cost[2] + $cost[3] + RV32I::OperationFormat::RRL_1.getSize();
}
=
{
  RV32::DEBUG_RULE_ACTION( "nrel: tpm_BinaryExpNEQ( areg, areg )", $1 );

  auto &reg2 = $action[2]();
  auto &reg3 = $action[3]();

  RVINSTRUCTIONS.insertBEQ( reg2, reg3, b, $1->getExp());

  if ( markLoopExit ) {
    WIR_Operation &jmp = RV32::RV32_wirBB->rbegin()->get().begin()->get();
    jmp.insertContainer( WIR_LoopExit( true ) );
  }

  return;
};


reg: tpm_BinaryExpLT( areg, areg )
{
  $cost[0] = $cost[2] + $cost[3] + RV32I::OperationFormat::RRR_1.getSize();
}
=
{
  RV32::DEBUG_RULE_ACTION( "reg: tpm_BinaryExpLT( areg, areg )", $1 );

  auto &p1 = $action[2]();
  auto &p2 = $action[3]();

  auto &r = RVINSTRUCTIONS.createReg();
  // use SLT
  // TCINSTRUCTIONS.insertLT_A( r, p1.second, p2.second, $1->getExp() );
  RVINSTRUCTIONS.insertSLT( r, p1, p2, $1->getExp() );

  return( r );
};


reg: tpm_BinaryExpGT( areg, areg )
{
  $cost[0] = $cost[2] + $cost[3] + RV32I::OperationFormat::RRR_1.getSize();
}
=
{
  RV32::DEBUG_RULE_ACTION( "reg: tpm_BinaryExpGT( areg, areg )", $1 );

  auto &p1 = $action[2]();
  auto &p2 = $action[3]();

  auto &r = RVINSTRUCTIONS.createReg();
  // use SLT
  // TCINSTRUCTIONS.insertLT_A( r, p2.second, p1.second, $1->getExp() );
  RVINSTRUCTIONS.insertSLT( r, p2, p1, $1->getExp() );

  return( r );
};


reg: tpm_BinaryExpLEQ( areg, areg )
{
  $cost[0] = $cost[2] + $cost[3] + RV32I::OperationFormat::RRR_1.getSize() + 
  RV32I::OperationFormat::RRC12_1.getSize();
}
=
{
  RV32::DEBUG_RULE_ACTION( "reg: tpm_BinaryExpLEQ( areg, areg )", $1 );

  auto &p1 = $action[2]();
  auto &p2 = $action[3]();

  auto &r = RVINSTRUCTIONS.createReg();

  RVINSTRUCTIONS.insertADDI( r, p2, 1, $1->getExp() );
  RVINSTRUCTIONS.insertSLT( r, p1, r, $1->getExp() );

  return( r );
};


reg: tpm_BinaryExpGEQ( areg, areg )
{
  $cost[0] += $cost[2] + $cost[3] + RV32I::OperationFormat::RRR_1.getSize() + 
  RV32IMC::OperationFormat::RRC12_1.getSize();
}
=
{
  RV32::DEBUG_RULE_ACTION( "reg: tpm_BinaryExpGEQ( areg, areg )", $1 );

  auto &p1 = $action[2]();
  auto &p2 = $action[3]();

  auto &r = RVINSTRUCTIONS.createReg();

  IR_Type &operandType = RV32::effectiveType( *$2->getExp() );
  if ( operandType.isUnsignedType() )
    RVINSTRUCTIONS.insertSLTU( r, p1, p2, $1->getExp() );
  else
    RVINSTRUCTIONS.insertSLT( r, p1, p2, $1->getExp() );

  RVINSTRUCTIONS.insertXORI( r, r, 1, $1->getExp() );

  return( r );
};


###############################################################################
#
#
# comma rules
#
#
###############################################################################


reg: tpm_BinaryExpCOMMA( reg, reg )
{
  // This rule does not do anything by itself.
  $cost[0] = $cost[2] + $cost[3];
}
=
{
  RV32::DEBUG_RULE_ACTION( "reg: tpm_BinaryExpCOMMA( reg, reg )", $1 );

  // Evaluate both expressions, always starting with the left side.
  $action[2]();
  return( $action[3]() );
};

reg: tpm_BinaryExpCOMMA( areg, reg )
{
  // This rule does not do anything by itself.
  $cost[0] = $cost[2] + $cost[3];
}
=
{
  RV32::DEBUG_RULE_ACTION( "reg: tpm_BinaryExpCOMMA( areg, reg )", $1 );

  // Evaluate both expressions, always starting with the left side.
  $action[2]();
  return( $action[3]() );
};

areg: tpm_BinaryExpCOMMA( reg, areg )
{
  // This rule does not do anything by itself.
  $cost[0] = $cost[2] + $cost[3];
}
=
{
  RV32::DEBUG_RULE_ACTION( "areg: tpm_BinaryExpCOMMA( reg, areg )", $1 );

  // Evaluate both expressions, always starting with the left side.
  $action[2]();
  return( $action[3]() );
};

areg: tpm_BinaryExpCOMMA( areg, areg )
{
  // This rule does not do anything by itself.
  $cost[0] = $cost[2] + $cost[3];
}
=
{
  RV32::DEBUG_RULE_ACTION( "areg: tpm_BinaryExpCOMMA( areg, areg )", $1 );

  // Evaluate both expressions, always starting with the left side.
  $action[2]();
  return( $action[3]() );
};


###############################################################################
#
#
# Logical AND, OR and NOT operators
#
#
###############################################################################


reg: tpm_UnaryExpLOGNOT( areg )
{
  $cost[0] = $cost[2] + RV32I::OperationFormat::RRC12_1.getSize();
}
=
{
  RV32::DEBUG_RULE_ACTION( "reg: tpm_UnaryExpLOGNOT( areg )", $1 );

  auto &p = $action[2]();

  auto &r = RVINSTRUCTIONS.createReg();
  //from RiscV isa: sltiu rd, rs, 1
  //TCINSTRUCTIONS.insertEQZ_A( r, p.second, $1->getExp() );
  RVINSTRUCTIONS.insertSLTIU( r, p, 1, $1->getExp() );

  return( r );
};


###############################################################################
#
#
# Unary expressions
#
#
###############################################################################


addrOffset: tpm_UnaryExpMINUS( addrOffset )
{
  $cost[0] = $cost[2];
}
=
{
  RV32::DEBUG_RULE_ACTION( "addrOffset: tpm_UnaryExpMINUS( addrOffset )", $1 );

  return( -$action[2]() );
};


areg: tpm_UnaryExpADDR( deref_reg )
{
  if ( !RV32::isZeroOpADDR( *$1->getExp() ) )
    $cost[0] = $cost[2];
  else
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION( "areg: tpm_UnaryExpADDR( deref_reg )", $1 );

  auto lvalue = $action[2]( false );
  lvalue.convertToBaseOffsetForm( $1->getExp() );


  auto &r = RVINSTRUCTIONS.createReg();

   if ( lvalue.getOffset() == 0 ){
   RVINSTRUCTIONS.insertMOV(
      r, dynamic_cast<RV_RegV &>( lvalue.getAddress().getAReg() ),
      $1->getExp() );
   }
   else {
   RVINSTRUCTIONS.insertMOV(
     r, dynamic_cast<RV_RegV &>( lvalue.getAddress().getAReg() ),
     $1->getExp() );
   RVINSTRUCTIONS.insertADDI(
     r, r, lvalue.getOffset(),
     $1->getExp() );
   }
  return( r );
};



areg: tpm_UnaryExpADDR( deref_areg )
{
  if ( !RV32::isZeroOpADDR( *$1->getExp() ) )
    $cost[0] = $cost[2];
  else
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION( "areg: tpm_UnaryExpADDR( deref_areg )", $1 );

  auto lvalue = $action[2]( false );
  lvalue.convertToBaseOffsetForm( $1->getExp() );

  
  auto &r = RVINSTRUCTIONS.createReg();

  if ( lvalue.getOffset() == 0 )
    RVINSTRUCTIONS.insertCMV(
      r, dynamic_cast<RV_RegV &>( lvalue.getAddress().getAReg() ),
      $1->getExp() );
  else
    RVINSTRUCTIONS.insertADDI(
      r, dynamic_cast<RV_RegV &>( lvalue.getAddress().getAReg() ),
      lvalue.getOffset(), $1->getExp() );

  return( r );
};


areg: tpm_UnaryExpADDR( zero_op_areg )
{
  $cost[0] = $cost[2];
}
=
{
  RV32::DEBUG_RULE_ACTION( "areg: tpm_UnaryExpADDR( zero_op_areg )", $1 );

  return( $action[2]() );
};


deref_reg: tpm_UnaryExpDEREF( areg )
{
  // Acquire the type of this expression.
  auto &t = $1->getExp()->getType();

  // Only use this rule if the expression is not of pointer, array or composed
  // type and is a 32 bit type.
  if ( RV32::isPointerType( t ) || RV32::isArrayType( t ) || RV32::isComposedType( t ) ||
       RV32::isERegType( t ) )
    $cost[0] = COST_INFINITY;
  else {
    // for a more accurate cost, a createLoadCost Funktion might be implemented
    $cost[0] = $cost[2] + RV32I::OperationFormat::RC12R_1.getSize();  
  }
}
=
{
  RV32::DEBUG_RULE_ACTION( "deref_reg: tpm_UnaryExpDEREF( areg )", $1 );

  // Acquire the type of this expression.
  auto &t = RV32::effectiveType( *$1->getExp() );

  // Evaluate the given expression, a.k.a. the address.
  auto &addrReg = $action[2]();

  // Assemble an RV32_AddressModification with no offset.
  RV32::RV32_AddressModification amod = RV32::RV32_AddressModification( addrReg, 0, &t, false );

  // Load the register, if neccessary.
  WIR::WIR_VirtualRegister* resultReg = &RVINSTRUCTIONS.createReg();;
  if ( loadResult ) {
    //resultReg = &RVINSTRUCTIONS.createReg();
    amod.createLoad( resultReg, $1->getExp() );
  }

  // Then, assemble and return the lvalue with this AddressModification.
  return( RV32::RV32_LValue { resultReg, amod } );
};


deref_areg: tpm_UnaryExpDEREF( areg )
{
  auto &t = $1->getExp()->getType();
  if ( RV32::isPointerType( t ) &&
       !RV32::isFunctionPointer( *$2->getExp() ) )
    $cost[0] = $cost[2] + RV32I::OperationFormat::RC12R_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION( "deref_areg: tpm_UnaryExpDEREF( areg )", $1 );

  auto &t = $1->getExp()->getType();
  auto &p = $action[2]();

  RV_RegV &r = RVINSTRUCTIONS.createReg();

  if ( loadResult ) {

    RVINSTRUCTIONS.insertLW( r, 0, p, $1->getExp() );
  }

  return(
    RV32::RV32_LValue {
      &r, RV32::RV32_AddressModification {
        p, 0, &t, true } } );
};


zero_op_areg: tpm_UnaryExpDEREF( areg )
{
  $cost[0] = $cost[2];
}
=
{
  RV32::DEBUG_RULE_ACTION( "zero_op_areg: tpm_UnaryExpDEREF( areg )", $1 );

  return( $action[2]() );
};


zero_op_areg: tpm_IndexExp( areg, addrOffset )
{
  // This rule is only applicable if the parent operation is an address operator
  // (it's the only one that can take a 'zero_op_areg'). Therefore, this rule
  // just computes the access location and the address operator can then return
  // it without any memory access being performed (see ANSI C 6.5.3.2).
  $cost[0] = $cost[2] + RV32I::OperationFormat::RRC12_1.getSize();
}
=
{
  RV32::DEBUG_RULE_ACTION( "zero_op_areg: tpm_IndexExp( areg, addrOffset )", $1 );

  auto &p = $action[2]();
  auto intConst = $action[3]().getIntValue();

  auto *t = RV32::getBaseType( $2->getExp()->getType() );
  int off = intConst * RV32::computeSizeOf( t );

  auto &r = RVINSTRUCTIONS.createReg();

  RVINSTRUCTIONS.insertADDI( r, p, off, $1->getExp() );

  return( r );
};


###############################################################################
#
#
# Unary expressions INC DEC
#
#
###############################################################################


reg: tpm_UnaryExpPREINC( deref_reg )
{
  auto &t = RV32::effectiveType( *$2->getExp() );
  $cost[0] = $cost[2];

  if ( t.isIntegralType() ) {
    $cost[0] += RV32I::OperationFormat::RRC12_1.getSize();
    // if ( t.bitSize() != 32 )
    //   see truncateCosts:
    //   $cost[0] += 3*RV32I::OperationFormat::RR_1.getSize();
  } else
    $cost[0] += RV32I::OperationFormat::RRR_1.getSize() + 
    RV32I::OperationFormat::RRC12_1.getSize(); //see getMOVConstantCost

  //used as createStore is called
  $cost[0] += RV32::RV32_AddressModification::createStoreCost($2->getExp());
}
=
{
  RV32::DEBUG_RULE_ACTION( "reg: tpm_UnaryExpPREINC( deref_reg )", $1 );

  auto lvalue = $action[2]( true );
  auto &t = RV32::effectiveType( *$2->getExp() );

  auto &rTmp = dynamic_cast<RV_RegV &>( *lvalue.getResultReg() );

  // Increase it.
  if ( t.isIntegralType() ) {
    RVINSTRUCTIONS.insertADDI( rTmp, rTmp, 1, $1->getExp() );

    // Truncate if needed. (not implemented properly yet)
    // if ( t.bitSize() != 32 ) {
    //   auto &tmp = RVINSTRUCTIONS.createReg();
    //   RV32::Cast::doTruncation( rTmp, tmp, t, $2->getExp() );
    //   RVINSTRUCTIONS.insertMOV( rTmp, tmp, $1->getExp() );
    // }
  } else

  if ( t.getType() == IR_Type::FLOAT ) {
    auto &tmp = RVINSTRUCTIONS.createReg();
    ICD_Float::Float f( 1.0f );

    // Move f to a register.
    RVINSTRUCTIONS.insertMOVConstant(
      tmp, f.getValue().getComposed(), $1->getExp() );

    // Perform add.
    RVINSTRUCTIONS.insertADD( rTmp, rTmp, tmp, $1->getExp() );
  }

  // Store back the incremented value.
  lvalue.storeBack( $2->getExp(), &rTmp );

  return( rTmp );
};


reg: tpm_UnaryExpPREDEC( deref_reg )
{
  auto &t = RV32::effectiveType( *$2->getExp() );
  $cost[0] = $cost[2];

  if ( t.isIntegralType() ) {
    $cost[0] += RV32I::OperationFormat::RRC12_1.getSize();
    //if ( t.bitSize() != 32 )
    //  $cost[0] += Cast::truncateCosts( $0->getExp()->getType() ); todo
  } else
    $cost[0] += RV32I::OperationFormat::RRR_1.getSize() +
    RV32I::OperationFormat::RRC12_1.getSize(); //see getMOVConstantCost

  //used as createStore is called
  $cost[0] += RV32::RV32_AddressModification::createStoreCost($2->getExp());
}
=
{
  RV32::DEBUG_RULE_ACTION( "reg: tpm_UnaryExpPREDEC( deref_reg )", $1 );

  auto lvalue = $action[2]( true );
  auto &t = RV32::effectiveType( *$2->getExp() );

  auto &rTmp = dynamic_cast<RV_RegV &>( *(lvalue.getResultReg()) );

  // Decrement it.
  if ( t.isIntegralType() ) {
    RVINSTRUCTIONS.insertADDI( rTmp, rTmp, -1, $1->getExp() );

    // Truncate if needed. (not implemented yet TODO)
    //if ( t.bitSize() != 32 ) {
    //  auto *tmp = RVINSTRUCTIONS.CreateReg();
    //  Cast::truncate( rTmp, tmp, t, $2->getExp() );
    //  RVINSTRUCTIONS.insertMOV( rTmp, tmp, $1->getExp() );
    //}
  } else

  if ( t.getType() == IR_Type::FLOAT ) {
    auto &tmp = RVINSTRUCTIONS.createReg();
    ICD_Float::Float f( 1.0f );

    // Move f to a register.
    RVINSTRUCTIONS.insertMOVConstant(
      tmp, f.getValue().getComposed(), $1->getExp() );

    // Perform sub.
    RVINSTRUCTIONS.insertSUB( rTmp, rTmp, tmp, $1->getExp() );
  }

  // Store back the incremented value.
  lvalue.storeBack( $2->getExp(), &rTmp );

  return( rTmp );
};


areg: tpm_UnaryExpPOSTINC( areg )
{
  $cost[0] =
    $cost[2] + RV32I::OperationFormat::RRC12_1.getSize();
}
=
{
  RV32::DEBUG_RULE_ACTION( "areg: tpm_UnaryExpPOSTINC( areg )", $1 );

  auto *t = RV32::getBaseType( $2->getExp()->getType() );
  int off = RV32::computeSizeOf( t );

  auto &p = $action[2]();

  auto &r = RVINSTRUCTIONS.createReg();
  RVINSTRUCTIONS.insertMOV( r, p, $1->getExp() );
  RVINSTRUCTIONS.insertADDI( p, p, off, $1->getExp() );

  return( r );
};


areg: tpm_UnaryExpPOSTDEC( areg )
{
  $cost[0] =
    $cost[2] + RV32I::OperationFormat::RRC12_1.getSize();
}
=
{
  RV32::DEBUG_RULE_ACTION( "areg: tpm_UnaryExpPOSTINC( areg )", $1 );

  auto *t = RV32::getBaseType( $2->getExp()->getType() );
  int off = -RV32::computeSizeOf( t );

  auto &p = $action[2]();

  auto &r = RVINSTRUCTIONS.createReg();
  RVINSTRUCTIONS.insertMOV( r, p, $1->getExp() );
  RVINSTRUCTIONS.insertADDI( p, p, off, $1->getExp() );

  return( r );
};


areg: tpm_UnaryExpPREINC( areg )
{
  $cost[0] =
    $cost[2] + RV32I::OperationFormat::RRC12_1.getSize();
}
=
{
  RV32::DEBUG_RULE_ACTION( "areg: tpm_UnaryExpPREINC( areg )", $1 );

  auto *t = RV32::getBaseType( $2->getExp()->getType() );
  int off = RV32::computeSizeOf( t );

  auto &p = $action[2]();

  RVINSTRUCTIONS.insertADDI( p, p, off, $1->getExp() );

  return( p );
};


areg: tpm_UnaryExpPREDEC( areg )
{
  $cost[0] =
    $cost[2] + RV32I::OperationFormat::RRC12_1.getSize();
}
=
{
  RV32::DEBUG_RULE_ACTION( "areg: tpm_UnaryExpPREINC( areg )", $1 );

  auto *t = RV32::getBaseType( $2->getExp()->getType() );
  int off = -RV32::computeSizeOf( t );

  auto &p = $action[2]();

  RVINSTRUCTIONS.insertADDI( p, p, off, $1->getExp() );

  return( p );
};


areg: tpm_UnaryExpPOSTINC( deref_areg )
{
  $cost[0] =
    $cost[2] + RV32I::OperationFormat::RR_1.getSize() +
    RV32I::OperationFormat::RRC12_1.getSize() +
    RV32::RV32_AddressModification::createStoreCost( $2->getExp() );
}
=
{
  RV32::DEBUG_RULE_ACTION( "areg: tpm_UnaryExpPOSTINC( deref_areg )", $1 );

  auto *t = RV32::getBaseType( $2->getExp()->getType() );
  const int off = RV32::computeSizeOf( t );

  auto lvalue = $action[2]( true );

  auto &r = RVINSTRUCTIONS.createReg();
  auto &tmpReg = dynamic_cast<RV_RegV &>( *(lvalue.getResultReg()) );
  RVINSTRUCTIONS.insertMOV( r, tmpReg, $1->getExp() );
  RVINSTRUCTIONS.insertADDI( tmpReg, tmpReg, off, $1->getExp() );

  lvalue.storeBack( $2->getExp() );

  return( r );
};


areg: tpm_UnaryExpPOSTDEC( deref_areg )
{
  $cost[0] =
    $cost[2] + RV32I::OperationFormat::RR_1.getSize() +
    RV32I::OperationFormat::RRC12_1.getSize() +
    RV32::RV32_AddressModification::createStoreCost( $2->getExp() );
}
=
{
  RV32::DEBUG_RULE_ACTION( "areg: tpm_UnaryExpPOSTDEC( deref_areg )", $1 );

  auto *t = RV32::getBaseType( $2->getExp()->getType() );
  const int off = -RV32::computeSizeOf( t );

  auto lvalue = $action[2]( true );

  auto &r = RVINSTRUCTIONS.createReg();
  auto &tmpReg = dynamic_cast<RV_RegV &>( *(lvalue.getResultReg()) );
  RVINSTRUCTIONS.insertMOV( r, tmpReg, $1->getExp() );
  RVINSTRUCTIONS.insertADDI( tmpReg, tmpReg, off, $1->getExp() );

  lvalue.storeBack( $2->getExp() );

  return( r );
};


areg: tpm_UnaryExpPREINC( deref_areg )
{
  $cost[0] =
    $cost[2] + RV32I::OperationFormat::RRC12_1.getSize() +
    RV32::RV32_AddressModification::createStoreCost( $2->getExp() );
}
=
{
  RV32::DEBUG_RULE_ACTION( "areg: tpm_UnaryExpPREINC( deref_areg )", $1 );

  auto *t = RV32::getBaseType( $2->getExp()->getType() );
  const int off = RV32::computeSizeOf( t );

  auto lvalue = $action[2]( true );

  auto &r = dynamic_cast<RV_RegV &>( *(lvalue.getResultReg()) );
  RVINSTRUCTIONS.insertADDI( r, r, off, $1->getExp() );

  lvalue.storeBack( $2->getExp() );

  return( r );
};


areg: tpm_UnaryExpPREDEC( deref_areg )
{
  $cost[0] =
    $cost[2] + RV32I::OperationFormat::RRC12_1.getSize() +
    RV32::RV32_AddressModification::createStoreCost( $2->getExp() );
}
=
{
  RV32::DEBUG_RULE_ACTION( "areg: tpm_UnaryExpPREDEC( deref_areg )", $1 );

  auto *t = RV32::getBaseType( $2->getExp()->getType() );
  const int off = -RV32::computeSizeOf( t );

  auto lvalue = $action[2]( true );

  auto &r = dynamic_cast<RV_RegV &>( *(lvalue.getResultReg()) );
  RVINSTRUCTIONS.insertADDI( r, r, off, $1->getExp() );

  lvalue.storeBack( $2->getExp() );

  return( r );
};


###############################################################################
#
#
# Function calls, argument passing and returns
#
#
###############################################################################


areg: tpm_CallExp( called_function, arg )
{
  if ( RV32::isARegType( $0->getExp()->getType() ) )
    $cost[0] = $cost[2] + $cost[3];
  else
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION( "areg: tpm_CallExp( called_function, arg )", $1 );

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


arg: tpm_CallExpARG( areg, arg )
{
  // The argument promotions must be present in the form of implicit casts, so
  // that there is no need to treat them here.
  // Functions in prototype form are not supported yet.
  auto *cexp = dynamic_cast<IR_CallExp *>( $2->getExp()->getParent() );

  if ( cexp->getFunctionType().isPrototypeForm() )
    $cost[0] = $cost[2] + $cost[3] + RVINSTRUCTIONS.getCMVCost();
  else {
    throw ufFatalError( "Functions without prototype are not supported." );
    $cost[0] = COST_INFINITY;
  }
}
=
{
  RV32::DEBUG_RULE_ACTION( "arg: tpm_CallExpARG( areg, arg )", $1 );

  int incr = 0;

  if ( dryrun ) {

    auto &p = $action[2]();
    dryArgs.push_back( p );

  } else {

    int regNo =
      RVCODESEL.getStack().isPassedThroughRegister( theCall->getFunctionType(), index );

    auto &r = dynamic_cast<RV_RegV &>( dryArgs.front().get() );
    dryArgs.pop_front();

    if ( regNo ) {
      // Pass argument via registers.
      auto &phreg =
        dynamic_cast<RV_RegV &>( RV32::getFctArgReg( *theCall, index, regNo ) );
      RVINSTRUCTIONS.insertCMV( phreg, r, $1->getExp() );

      args.push_back( phreg );
    } else {
      // Pass argument via stack.
      incr = RVCODESEL.getStack().getStackSize( RV32::effectiveType( *$2->getExp() ) );

      auto &sp = RVINSTRUCTIONS.createReg();
      RV32::RV32_wirFct->insertPrecolor( sp, RV32::RV32_wirProc->SP() );
      RVINSTRUCTIONS.insertSW( sp, offset, r, $1->getExp() );

      // Mark access to overflow region for register allocator.
      RV32::RV32_wirBB->rbegin()->get().begin()->get().begin()->
        get().setDontOptimize();
    }
  }

  $action[3]( dryrun, index + 1, offset + incr, theCall, args, dryArgs );

  return;
};


stmt: tpm_ReturnStmt( areg )
{
  list<int> calleeSavedRegs = RV32::RV32_Stack::getCalleeSavedRegs();
  int calleeSavedSize = calleeSavedRegs.size();

  $cost[0] = $cost[2] + 2 * RVINSTRUCTIONS.getCMVCost() +
             calleeSavedSize * RV32I::OperationFormat::RC12R_1.getSize() +
             2 * RV32I::OperationFormat::RRC12_1.getSize();
}
=
{
  RV32::DEBUG_RULE_ACTION( "stmt: tpm_ReturnStmt( areg )", $1 );

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


###############################################################################
#
#
# Constants
#
#
###############################################################################


addrOffset: tpm_IntConstExp
{
  if ( RV32::isAddrOffset( $1->getExp()->getType() ) )
    $cost[0] = 0;
  else
    $cost[0] = COST_INFINITY;
}
=
{
  RV32::DEBUG_RULE_ACTION( "addrOffset: tpm_IntConstExp", $1 );

  return( dynamic_cast<IR_IntConstExp*>( $1->getExp() )->getValue() );
};