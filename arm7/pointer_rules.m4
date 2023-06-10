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

# Loading pointer symbols (a.k.a. variables).

# Local Pointer Symbols.
# These symbols do not have a corresponding memory location associated with
# them, their values are only held in registers.

areg: tpm_SymbolExp
{
  // Acquire the expression and the symbol itself.
  auto &symExp = dynamic_cast<IR_SymbolExp &>( *$1->getExp() );
  auto &sym = symExp.getSymbol();

  // Match this rule iff the symbol is of pointer type, is not global and does
  // not reside in the stack.
  if ( ( isPointerType( sym ) ||
         ( isArrayType( sym ) && isFunctionArgument( symExp ) ) ) &&
       !sym.isGlobal() && ( ARMCODESEL->getStack()->getSymbolOffset( &sym ) < 0 ) )
    $cost[0] = loadRegisterSymbolCost( symExp );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "areg: tpm_SymbolExp", $1 );

  // Acquire the expression.
  auto &symExp = dynamic_cast<IR_SymbolExp &>( *$1->getExp() );

  // And load the symbol.
  return( loadRegisterSymbol( symExp ) );
};


areg: tpm_SymbolExp
{
  // Acquire the expression and the symbol itself.
  auto &symExp = dynamic_cast<IR_SymbolExp &>( *$1->getExp() );
  auto &sym = symExp.getSymbol();
  auto &t = sym.getType();

  // This rule loads the address of a function to use as a function pointer.
  if ( isFunctionType( t ) )
    $cost[0] = CT( INS_LDR_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "areg: tpm_SymbolExp", $1 );

  // Acquire the expression and the symbol itself.
  auto &symExp = dynamic_cast<IR_SymbolExp &>( *$1->getExp() );
  auto &sym = symExp.getSymbol();

  // Check if there is already a literal pool present.
  auto *func = ARMCODESEL->getLastLLIRFunction();
  string poolLabel = func->getDataSectionAddressLabel( sym.getName() );

  // If not, create one.
  if ( poolLabel.empty() ) {
    auto *addrBB = createLiteralPool( sym.getName(), $1->getExp() );
    poolLabel = addrBB->GetLabel();
  }

  auto *reg = ARMINSTRUCTIONS.CreateRegister( "" );

  // Generate the operation.
  ARMINSTRUCTIONS.insertLDR( OPER_AL, reg, false, poolLabel, $1->getExp() );

  return( reg );
};


# Stack Pointer Symbols
# These symbols have a corresponding memory location associated with them.

deref_areg: tpm_SymbolExp
{
  // Acquire the expression and the symbol itself.
  auto &symExp = dynamic_cast<IR_SymbolExp &>( *$1->getExp() );
  auto &sym = symExp.getSymbol();

  // Match this rule iff the symbol is of pointer type, is not global and does
  // reside in the stack.
  if ( ( isPointerType( sym ) ||
         ( isArrayType( sym ) && isFunctionArgument( symExp ) ) ) &&
       !sym.isGlobal() &&
       ( ARMCODESEL->getStack()->getSymbolOffset( &sym ) >= 0 ) )
    $cost[0] = loadStackSymbolCost( symExp );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "deref_areg: tpm_SymbolExp", $1 );

  // Acquire the expression.
  auto &symExp = dynamic_cast<IR_SymbolExp &>( *$1->getExp() );

  if ( dryRun )
    // In case of a dry run, do not actually load the symbol, but only its
    // offset.
    return( loadStackSymbolDryRun( symExp ) );
  else
    // Pass through the info on whether the result should actually be loaded.
    return( loadStackSymbol( symExp, loadResult ) );
};


deref_areg: tpm_SymbolExp
{
  // Handles global 'areg' symbols.
  auto &symExp = dynamic_cast<IR_SymbolExp &>( *$1->getExp() );
  auto &sym = symExp.getSymbol();

  if ( isPointerType( sym ) && sym.isGlobal() )
    $cost[0] = loadGlobalSymbolCost( symExp );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "deref_areg: tpm_SymbolExp", $1 );

  auto *symExp = dynamic_cast<IR_SymbolExp *>( $1->getExp() );
  return( loadGlobalSymbol( *symExp, loadResult ) );
};


areg: deref_areg
{
  // Chain rule that transforms a deref_areg to a simple areg.
  // If the memory location of the deref_areg is not important, i.e., the
  // memory location of the deref_areg is not written to, it can be converted.
  if ( !isMemoryWriteLocation( *$1->getExp() ) )
    $cost[0] = $cost[1];
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "areg: deref_areg", $1 );

  auto di = $action[1]( true, false );

  return( di.getResultRegister() );
};


# Simple assignments to deref_dregs, aregs and deref_aregs.

# Dereferencing.
# Depending on the base-type of the pointer either a deref_dreg or deref_areg is
# returned. One version implements a simple load by taking an areg, whereas the
# other version will insert an advanced load by taking an
# ARM_AddressModification.

deref_dreg: tpm_UnaryExpDEREF( areg )
{
  // Acquire the type of this expression.
  auto &t = $1->getExp()->getType();

  // Only use this rule if the expression is not of pointer, array or composed
  // type and is a 32 bit type.
  if ( isPointerType( t ) || isArrayType( t ) || isComposedType( t ) ||
       isERegType( t ) )
    $cost[0] = COST_INFINITY;
  else {
    // We always have the cost of evaluating the operand, and we assume that we
    // are always loading, which is not neccessarily true.

    // Construct a dryRun instance to calculate the appropriate costs.
    ARM_AddressModification dryRunAmod { nullptr, &t, true };
    $cost[0] = $cost[2] + dryRunAmod.createLoadCost();
  }
}
=
{
  DEBUG_RULE_ACTION( "deref_dreg: tpm_UnaryExpDEREF( areg )", $1 );

  // Acquire the type of this expression.
  auto &t = effectiveType( *$1->getExp() );

  // In case of a dry run, return an appropriate lvalue.
  if ( dryRun ) {
    ARM_AddressModification dryRunAmod { nullptr, &t, true };

    // Perform a "pseudo-load" if neccessary, to update the AddresModification.
    if ( loadResult )
      dryRunAmod.createLoadCost();

    // Finally, construct an appropriate lvalue to return.
    return( ARM_LValue { nullptr, dryRunAmod, true } );
  }

  // Evaluate the given expression, a.k.a. the address.
  auto *addrReg = $action[2]();

  // Assemble an ARM_AddressModification with no offset.
  ARM_AddressModification amod { addrReg, &t };

  // Load the register, if neccessary.
  LLIR_Register *resultReg = nullptr;
  if ( loadResult ) {
    resultReg = ARMINSTRUCTIONS.CreateRegister( "" );
    amod.createLoad( resultReg, $1->getExp() );
  }

  // Then, assemble and return the lvalue with this AddressModification.
  return( ARM_LValue { resultReg, amod } );
};


deref_ereg: tpm_UnaryExpDEREF( areg )
{
  // Acquire the type of this expression.
  auto &t = $1->getExp()->getType();

  // Only use this rule if the expression is not of pointer, array or composed
  // type and a 64 bit type.
  if ( isPointerType( t ) || isArrayType( t ) || isComposedType( t ) ||
       !isERegType( t ) )
    $cost[0] = COST_INFINITY;
  else {
    // We always have the cost of evaluating the operand, and we assume that we
    // are always loading, which is not neccessarily true.

    // Construct a dryRun instance to calculate the appropriate costs.
    ARM_AddressModification dryRunAmod { nullptr, &t, true };
    $cost[0] = $cost[2] + dryRunAmod.createLoadCost();
  }
}
=
{
  DEBUG_RULE_ACTION( "deref_ereg: tpm_UnaryExpDEREF( areg )", $1 );

  // Acquire the type of this expression.
  auto &t = effectiveType( *$1->getExp() );

  // In case of a dry run, return an appropriate lvalue.
  if ( dryRun ) {
    ARM_AddressModification dryRunAmod { nullptr, &t, true };

    // Perform a "pseudo-load" if neccessary, to update the AddresModification.
    if ( loadResult )
      dryRunAmod.createLoadCost();

    // Finally, construct an appropriate lvalue to return.
    return( ARM_LValue { nullptr, dryRunAmod, true } );
  }

  // Evaluate the given expression, a.k.a. the address.
  auto *addrReg = $action[2]();

  // Assemble an ARM_AddressModification with no offset.
  ARM_AddressModification amod { addrReg, &t };

  // Load the register, if neccessary.
  LLIR_Register *resultReg = nullptr;
  if ( loadResult ) {
    resultReg = ARMINSTRUCTIONS.CreateERegister( "" );
    amod.createLoad( resultReg, $1->getExp() );
  }

  // Then, assemble and return the lvalue with this AddressModification.
  return( ARM_LValue { resultReg, amod } );
};


deref_areg: tpm_UnaryExpDEREF( areg )
{
  // Acquire the type of this expression.
  auto &t = $1->getExp()->getType();

  // Only use this rule if the expression is of pointer type.
  if ( isPointerType( t ) ) {
    // We always have the cost of evaluating the operand, and we assume that we
    // are always loading, which is not neccessarily true.

    // Construct a dryRun instance to calculate the appropriate costs.
    ARM_AddressModification dryRunAmod { nullptr, &t, true };
    $cost[0] = $cost[2] + dryRunAmod.createLoadCost();
  } else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "deref_areg: tpm_UnaryExpDEREF( areg )", $1 );

  // Acquire the type of this expression.
  auto &t = effectiveType( *$1->getExp() );

  // In case of a dry run, return an appropriate lvalue.
  if ( dryRun ) {
    ARM_AddressModification dryRunAmod { nullptr, &t, true };

    // Perform a "pseudo-load" if neccessary, to update the AddresModification.
    if ( loadResult )
      dryRunAmod.createLoadCost();

    // Finally, construct an appropriate lvalue to return.
    return( ARM_LValue { nullptr, dryRunAmod, true } );
  }

  // Evaluate the given expression, a.k.a. the address.
  auto *addrReg = $action[2]();

  // Assemble an ARM_AddressModification with no offset.
  ARM_AddressModification amod { addrReg, &t };

  // Load the register, if neccessary.
  LLIR_Register *resultReg = nullptr;
  if ( loadResult ) {
    resultReg = ARMINSTRUCTIONS.CreateRegister( "" );
    amod.createLoad( resultReg, $1->getExp() );
  }

  // Then, assemble and return the lvalue with this AddressModification.
  return( ARM_LValue { resultReg, amod } );
};


composed_type_object: tpm_UnaryExpDEREF( areg )
{
  // Pointers to structs and unions are not actually loading anything. In this
  // case, we simply repackage the pointer and let the composed rules take care
  // of this.

  // Acquire the type of this expression.
  auto &t = $1->getExp()->getType();

  // Only use this rule if the expression is of composed type.
  if ( isComposedType( t ) )
    // There only is the cost of the operand, since no actual loads are
    // performed at this point.
    $cost[0] = $cost[2];
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "composed_type_object: tpm_UnaryExpDEREF( areg )", $1 );

  // In case of a dry run, return only the offset.
  if ( dryRun )
    // Since we only pass through an address register, the offset is always 0.
    return( AddressWithOffset { nullptr, 0 } );

  // Acuire the operand.
  auto *reg = $action[2]();

  // And return as a composed_type_object with no offset.
  return( AddressWithOffset { reg, 0 } );
};


composed_type_object: areg
{
  // This is a special case for the '->' operator. The left hand side of such an
  // operator is not of composed type, but of pointer type. Nonetheless, since
  // composed types are handled as pointers internally, we can simply pass
  // through the address register in that case.

  // Only do this if this areg-nonterminal is part of a component access
  // expression with '->' operator.

  // Acquire the parent expression and attempt a cast.
  auto *parent =
    dynamic_cast<IR_ComponentAccessExp *>( $1->getExp()->getParent() );

  // Check that the cast was successful, that the component access expression
  // uses the arrow operator and that this expression is the left-hand side of
  // the arrow operator.
  if ( parent &&
       ( parent->getOperator() == IR_ComponentAccessExp::Operator::ARROW ) &&
       ( &parent->getBaseExp() == $1->getExp() ) )
    $cost[0] = $cost[1];
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "composed_type_object: areg", $1 );

  // In case of a dry run, only return the offset, which is always 0.
  if ( dryRun )
    return( AddressWithOffset { nullptr, 0 } );

  // Simply pass through the address register with offset 0.
  return( AddressWithOffset { $action[1](), 0 } );
};


deref_dreg: tpm_UnaryExpDEREF( modified_areg )
{
  // This rule implements a load instruction combined with an address
  // increment / decrement.
  switch ( getBaseType( *$2->getExp() )->getType() ) {
    case IR_Type::CHAR:
    case IR_Type::UNSIGNED_CHAR:
    case IR_Type::BOOL:
    case IR_Type::SHORT:
    case IR_Type::UNSIGNED_SHORT:
    case IR_Type::INT:
    case IR_Type::UNSIGNED_INT:
    case IR_Type::LONG:
    case IR_Type::UNSIGNED_LONG:
    case IR_Type::FLOAT: {
      $cost[0] = $cost[2] + ARM_AddressModification::createLoadWorstCost();
      break;
    }

    default: {
      $cost[0] = COST_INFINITY;
      break;
    }
  }
}
=
{
  DEBUG_RULE_ACTION( "deref_dreg: tpm_UnaryExpDEREF( modified_areg )", $1 );

  // In case of a dry run, return an appropriate lvalue.
  if ( dryRun ) {
    auto dryRunAmod = $action[2]( true );

    // Perform a "pseudo-load" if neccessary, to update the
    // ARM_AddressModification.
    if ( loadResult )
      dryRunAmod.createLoadCost();

    // Finally, construct an appropriate lvalue to return.
    return( ARM_LValue { nullptr, dryRunAmod, true } );
  }

  // Retrieve the given ARM_AddressModification.
  auto amod = $action[2]( false );

  // Load the register, if neccessary.
  LLIR_Register *resultReg = nullptr;
  if ( loadResult ) {
    resultReg = ARMINSTRUCTIONS.CreateRegister( "" );
    amod.createLoad( resultReg, $1->getExp() );
  }

  // Then, assemble and return the lvalue with this AddressModification.
  return( ARM_LValue { resultReg, amod } );
};


deref_ereg: tpm_UnaryExpDEREF( modified_areg )
{
  // Acquire the type of this expression.
  auto &t = $1->getExp()->getType();

  // Only use this rule if the expression is not of pointer, array or composed
  // type.
  if ( isPointerType( t ) || isArrayType( t ) || isComposedType( t ) ||
       !isERegType( t ) )
    $cost[0] = COST_INFINITY;
  else
    // We always have the cost of evaluating the operand, and we assume that we
    // we are always loading, which is not neccessarily true.
    $cost[0] = $cost[2] + ARM_AddressModification::createLoadWorstCost();
}
=
{
  DEBUG_RULE_ACTION( "deref_ereg: tpm_UnaryExpDEREF( modified_areg )", $1 );

  // In case of a dry run, return an appropriate lvalue.
  if ( dryRun ) {
    auto dryRunAmod = $action[2]( true );

    // Perform a "pseudo-load" if neccessary, to update the
    // ARM_AddressModification.
    if ( loadResult )
      dryRunAmod.createLoadCost();

    // Finally, construct an appropriate lvalue to return.
    return( ARM_LValue { nullptr, dryRunAmod, true } );
  }

  // Retrieve the given ARM_AddressModification.
  auto amod = $action[2]( false );

  // Load the register, if neccessary.
  LLIR_Register *resultReg = nullptr;
  if ( loadResult ) {
    resultReg = ARMINSTRUCTIONS.CreateERegister( "" );
    amod.createLoad( resultReg, $1->getExp() );
  }

  // Then, assemble and return the lvalue with this AddressModification.
  return( ARM_LValue { resultReg, amod } );
};


deref_areg: tpm_UnaryExpDEREF( modified_areg )
{
  // Acquire the type of this expression.
  auto &t = $1->getExp()->getType();

  // Only use this rule if the expression is of pointer type.
  if ( isPointerType( *getBaseType( *$2->getExp() ) ) &&
       !isFunctionPointer( *$2->getExp() ) )
    // We always have the cost of evaluating the operand, and we assume that we
    // we are always loading, which is not neccessarily true.
    $cost[0] = $cost[2] + ARM_AddressModification::createLoadWorstCost();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "deref_areg: tpm_UnaryExpDEREF( modified_areg )", $1 );

  // In case of a dry run, return an appropriate lvalue.
  if ( dryRun ) {
    auto dryRunAmod = $action[2]( true );

    // Perform a "pseudo-load" if neccessary, to update the
    // ARM_AddressModification.
    if ( loadResult )
      dryRunAmod.createLoadCost();

    // Finally, construct an appropriate lvalue to return.
    return( ARM_LValue { nullptr, dryRunAmod, true } );
  }

  // Retrieve the given ARM_AddressModification.
  auto amod = $action[2]( false );

  // Load the register, if neccessary.
  LLIR_Register *resultReg = nullptr;
  if ( loadResult ) {
    resultReg = ARMINSTRUCTIONS.CreateRegister( "" );
    amod.createLoad( resultReg, $1->getExp() );
  }

  // Then, assemble and return the lvalue with this AddressModification.
  return( ARM_LValue { resultReg, amod } );
};


areg: tpm_UnaryExpDEREF( areg )
{
  // Special rule for dereferencing a function pointer

  // Acquire the type of this expression.
  auto &t = $1->getExp()->getType();

  // Only use this rule for function pointers. For expressions (*fnptr)(), the
  // dereferencing does nothing.
  if ( isFunctionType( t ) )
    $cost[0] = $cost[2];
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "areg: tpm_UnaryExpDEREF( areg )", $1 );

  return( $action[2]() );
};

# @author Heiko Falk <Heiko.Falk@tuhh.de>
areg: tpm_UnaryExpDEREF( areg )
{
  // This rule only handles base type arrays, because in that case, we only
  // compute the new address and return it in an areg.
  if ( isArrayType( *getBaseType( *$2->getExp() ) ) )
    $cost[0] = $cost[2];
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "areg: tpm_UnaryExpDEREF( areg )", $1 );

  return( $action[2]() );
};


# Address operator

areg: tpm_UnaryExpADDR( deref_dreg )
{
  $cost[0] = $cost[2] + ARM_LValue::calculateAddressWorstCost();
}
=
{
  DEBUG_RULE_ACTION( "areg: tpm_UnaryExpADDR( deref_dreg )", $1 );

  // Acquire the lvalue.
  auto di = $action[2]( false, false );

  // Let the ARM_LValue / ARM_AddressModification handle the arithmetics.
  return( di.calculateAddress( $1->getExp() ) );
};


areg: tpm_UnaryExpADDR( deref_ereg )
{
  $cost[0] = $cost[2] + ARM_LValue::calculateAddressWorstCost();
}
=
{
  DEBUG_RULE_ACTION( "areg: tpm_UnaryExpADDR( deref_ereg )", $1 );

  // Acquire the lvalue.
  auto di = $action[2]( false, false );

  // Let the ARM_LValue / ARM_AddressModification handle the arithmetics.
  return( di.calculateAddress( $1->getExp() ) );
};


areg: tpm_UnaryExpADDR( deref_areg )
{
  $cost[0] = $cost[2] + ARM_LValue::calculateAddressWorstCost();
}
=
{
  DEBUG_RULE_ACTION( "areg: tpm_UnaryExpADDR( deref_areg )", $1 );

  // Acquire the lvalue.
  auto di = $action[2]( false, false );

  // Let the ARM_LValue / ARM_AddressModification handle the arithmetics.
  return( di.calculateAddress( $1->getExp() ) );
};


modified_areg: tpm_UnaryExpADDR( composed_type_object )
{
  // In this case, there's only the cost of the operand, since no pointer
  // arithmetics are performed at this point.
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION(
    "modified_areg: tpm_UnaryExpADDR( composed_type_object )", $1 );

  // Acquire the type of the operand, the base type of the pointer that we are
  // about to construct.
  auto &ptrType = dynamic_cast<IR_PointerType &>( $1->getExp()->getType() );
  auto &t = ptrType.getBaseType();

  // Get the operand.
  auto awo = $action[2]( false );

  // Construct the ARM_AddressModification with the info.
  return(
    ARM_AddressModification {
      awo.getARegister(), awo.getOffset(), &t,
      AddressModification::ModTime::NONE, AddressModification::ModOper::ADD,
      true, false } );
};


areg: tpm_UnaryExpADDR( areg )
{
  // Special rule for loading the address of a function. The symbol expression
  // has already inserted the load instruction.
  auto *symExp = dynamic_cast<IR_SymbolExp *>( $2->getExp() );
  if ( symExp ) {
    auto &sym = symExp->getSymbol();
    auto &t = sym.getType();

    if ( isFunctionType( t ) )
      $cost[0] = $cost[2];
    else
      $cost[0] = COST_INFINITY;
  } else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "areg: tpm_UnaryExpADDR( areg )", $1 );

  // Just pass through the function address.
  return( $action[2]() );
};


dreg: tpm_AssignExpASSIGN( deref_dreg, dreg )
{
  // Assignment to a deref_dreg.
  $cost[0] = $cost[2] + $cost[3] + ARM_LValue::storeBackWorstCost();

  // In case of a bitfield, we need to consider additional costs.
  if ( isBitfieldType( $2->getExp()->getType() ) )
    $cost[0] +=
      insertValueIntoBitfieldCost() + ARM_LValue::signExtendBitfieldWorstCost();
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_AssignExpASSIGN( deref_dreg, dreg )", $1 );

  // Evaluate both sides of the expression, with the right-hand side first.
  auto *rhsReg = $action[3]();
  auto lhsReg = $action[2]( false, false );

  // Insert the store.
  lhsReg.storeBack( rhsReg, $1->getExp() );

  // And return the right-hand side.
  return( rhsReg );
};


ereg: tpm_AssignExpASSIGN( deref_ereg, ereg )
{
  // Assignment to a 64bit deref_ereg.
  $cost[0] = $cost[2] + $cost[3] + ARM_LValue::storeBackWorstCost();
}
=
{
  DEBUG_RULE_ACTION( "ereg: tpm_AssignExpASSIGN( deref_ereg, ereg )", $1 );

  // Evaluate both sides of the expression, with the right-hand side first.
  auto *rhsReg = $action[3]();
  auto lhsReg = $action[2]( false, false );

  // Insert the store.
  lhsReg.storeBack( rhsReg, $1->getExp() );

  if ( lhsReg.isBitfieldAccess() )
    // For a signed 7-bit field, the value 0x7f represents -1 and not 127. So we
    // extend the sign before returning the value.
    return( lhsReg.signExtendBitfield( rhsReg ) );
  else
    // And return the right-hand side.
    return( rhsReg );
};


areg: tpm_AssignExpASSIGN( areg, areg )
{
  // Assignments to aregs or deref_aregs. Check that RHS is not a string and LHS
  // an array. Otherwise, this rule matches string initalization:
  // char str[3] = "ab";
  auto *stringConstExp = dynamic_cast<IR_StringConstExp *>( $3->getExp() );

  if ( !stringConstExp || !isArrayType( *$2->getExp() ) )
    $cost[0] = $cost[2] + $cost[3] + CT( INS_MOV_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "areg: tpm_AssignExpASSIGN( areg, areg )", $1 );

  // Evaluate the arguments.
  auto *rhsReg = $action[3]();
  auto *lhsReg = $action[2]();

  // LHS is an areg, a local register not stored in memory. Therefore, a MOV
  // instruction (instead of a ST) is inserted.
  ARMINSTRUCTIONS.insertMOV( lhsReg, rhsReg, $1->getExp() );

  return( lhsReg );
};


areg: tpm_AssignExpASSIGN( deref_areg, areg )
{
  $cost[0] = $cost[2] + $cost[3] + ARM_LValue::storeBackWorstCost();
}
=
{
  DEBUG_RULE_ACTION( "areg: tpm_AssignExpASSIGN( deref_areg, areg )", $1 );

  // Evaluate the arguments.
  auto *rhsReg = $action[3]();
  auto lhsReg = $action[2]( false, false );

  // Let the lvalue handle the store.
  lhsReg.storeBack( rhsReg, $1->getExp() );

  // An assignment always returns an rvalue, so the lvalue is not needed
  // anymore.
  return( rhsReg );
};


# pointer arithmetics

areg: modified_areg
{
  // Chain rule that actually performs pointer arithmetics by transforming a
  // modified_areg into a plain areg.
  $cost[0] = $cost[1] + ARM_AddressModification::applyModificationWorstCost();
}
=
{
  DEBUG_RULE_ACTION( "areg: modified_areg", $1 );

  // Get the ARM_AddressModification.
  auto amod = $action[1]( false );

  // And perform the pointer arithmetics.
  return( amod.applyModification( $1->getExp() ) );
};


# all the unary in- and decrements

modified_areg: tpm_UnaryExpPREINC( areg )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "modified_areg: tpm_UnaryExpPREINC( areg )", $1 );

  // Acquire the base type of the pointer.
  auto &ptrType = dynamic_cast<IR_PointerType &>( $2->getExp()->getType() );
  auto &t = ptrType.getBaseType();

  // In case of a dry run, return an appropriate instance.
  if ( dryRun )
    return(
      ARM_AddressModification {
        static_cast<LLIR_Register *>( nullptr ), 1, &t,
        AddressModification::ModTime::PRE, AddressModification::ModOper::ADD,
        false, true } );

  // Evaluate the argument.
  auto *reg = $action[2]();

  // Assemble the ARM_AddressModification.
  return(
    ARM_AddressModification {
      reg, 1, &t, AddressModification::ModTime::PRE,
      AddressModification::ModOper::ADD } );
};


modified_areg: tpm_UnaryExpPREINC( deref_areg )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "modified_areg: tpm_UnaryExpPREINC( deref_areg )", $1 );

  // Acquire the base type of the pointer.
  auto &ptrType = dynamic_cast<IR_PointerType &>( $2->getExp()->getType() );
  auto &t = ptrType.getBaseType();

  // In case of a dry run, return an appropriate instance.
  if ( dryRun ) {
    auto dryRunDI = $action[2]( true, true );
    return(
      ARM_AddressModification {
        &dryRunDI, 1, &t, AddressModification::ModTime::PRE,
        AddressModification::ModOper::ADD, false, true } );
  }

  // Evaluate the argument.
  auto di = $action[2]( true, false );

  // Assemble the ARM_AddressModification.
  return(
    ARM_AddressModification {
      &di, 1, &t, AddressModification::ModTime::PRE,
      AddressModification::ModOper::ADD } );
};


modified_areg: tpm_UnaryExpPREDEC( areg )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "modified_areg: tpm_UnaryExpPREDEC( areg )", $1 );

  // Acquire the base type of the pointer.
  auto &ptrType = dynamic_cast<IR_PointerType &>( $2->getExp()->getType() );
  auto &t = ptrType.getBaseType();

  // In case of a dry run, return an appropriate instance.
  if ( dryRun )
    return(
      ARM_AddressModification {
        static_cast<LLIR_Register *>( nullptr ), 1, &t,
        AddressModification::ModTime::PRE, AddressModification::ModOper::SUB,
        false, true } );

  // Evaluate the argument.
  auto *reg = $action[2]();

  // Assemble the ARM_AddressModification.
  return(
    ARM_AddressModification {
      reg, 1, &t, AddressModification::ModTime::PRE,
      AddressModification::ModOper::SUB } );
};


modified_areg: tpm_UnaryExpPREDEC( deref_areg )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "modified_areg: tpm_UnaryExpPREDEC( deref_areg )", $1 );

  // Acquire the base type of the pointer.
  auto &ptrType = dynamic_cast<IR_PointerType &>( $2->getExp()->getType() );
  auto &t = ptrType.getBaseType();

  // In case of a dry run, return an appropriate instance.
  if ( dryRun ) {
    auto dryRunDI = $action[2]( true, true );
    return(
      ARM_AddressModification {
        &dryRunDI, 1, &t, AddressModification::ModTime::PRE,
        AddressModification::ModOper::SUB, false, true } );
  }

  // Evaluate the argument.
  auto di = $action[2]( true, false );

  // Assemble the ARM_AddressModification.
  return(
    ARM_AddressModification {
      &di, 1, &t, AddressModification::ModTime::PRE,
      AddressModification::ModOper::SUB } );
};


modified_areg: tpm_UnaryExpPOSTINC( areg )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "modified_areg: tpm_UnaryExpPOSTINC( areg )", $1 );

  // Acquire the base type of the pointer.
  auto &ptrType = dynamic_cast<IR_PointerType &>( $2->getExp()->getType() );
  auto &t = ptrType.getBaseType();

  // In case of a dry run, return an appropriate instance.
  if ( dryRun )
    return(
      ARM_AddressModification {
        static_cast<LLIR_Register *>( nullptr ), 1, &t,
        AddressModification::ModTime::POST, AddressModification::ModOper::ADD,
        false, true } );

  // Evaluate the argument.
  auto *reg = $action[2]();

  // Assemble the ARM_AddressModification.
  return(
    ARM_AddressModification {
      reg, 1, &t, AddressModification::ModTime::POST,
      AddressModification::ModOper::ADD } );
};


modified_areg: tpm_UnaryExpPOSTINC( deref_areg )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "modified_areg: tpm_UnaryExpPOSTINC( deref_areg )", $1 );

  // Acquire the base type of the pointer.
  auto &ptrType = dynamic_cast<IR_PointerType &>( $2->getExp()->getType() );
  auto &t = ptrType.getBaseType();

  // In case of a dry run, return an appropriate instance.
  if ( dryRun ) {
    auto dryRunDI = $action[2]( true, true );
    return(
      ARM_AddressModification {
        &dryRunDI, 1, &t, AddressModification::ModTime::POST,
        AddressModification::ModOper::ADD, false, true } );
  }

  // Evaluate the argument.
  auto di = $action[2]( true, false );

  // Assemble the ARM_AddressModification.
  return(
    ARM_AddressModification {
      &di, 1, &t, AddressModification::ModTime::POST,
      AddressModification::ModOper::ADD } );
};


modified_areg: tpm_UnaryExpPOSTDEC( areg )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "modified_areg: tpm_UnaryExpPOSTDEC( areg )", $1 );

  // Acquire the base type of the pointer.
  auto &ptrType = dynamic_cast<IR_PointerType &>( $2->getExp()->getType() );
  auto &t = ptrType.getBaseType();

  // In case of a dry run, return an appropriate instance.
  if ( dryRun )
    return(
      ARM_AddressModification {
        static_cast<LLIR_Register *>( nullptr ), 1, &t,
        AddressModification::ModTime::POST, AddressModification::ModOper::SUB,
        false, true } );

  // Evaluate the argument.
  auto *reg = $action[2]();

  // Assemble the ARM_AddressModification.
  return(
    ARM_AddressModification {
      reg, 1, &t, AddressModification::ModTime::POST,
      AddressModification::ModOper::SUB } );
};


modified_areg: tpm_UnaryExpPOSTDEC( deref_areg )
{
  // There is no cost associated with assembling the modified_areg.
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "modified_areg: tpm_UnaryExpPOSTDEC( deref_areg )", $1 );

  // Acquire the base type of the pointer.
  auto &ptrType = dynamic_cast<IR_PointerType &>( $2->getExp()->getType() );
  auto &t = ptrType.getBaseType();

  // In case of a dry run, return an appropriate instance.
  if ( dryRun ) {
    auto dryRunDI = $action[2]( true, true );
    return(
      ARM_AddressModification {
        &dryRunDI, 1, &t, AddressModification::ModTime::POST,
        AddressModification::ModOper::SUB, false, true } );
  }

  // Evaluate the argument.
  auto di = $action[2]( true, false );

  // Assemble the ARM_AddressModification.
  return(
    ARM_AddressModification {
      &di, 1, &t, AddressModification::ModTime::POST,
      AddressModification::ModOper::SUB } );
};


# quick assignments to pointers (+=, -=)

modified_areg: tpm_AssignExpPLUS( areg, dreg )
{
  $cost[0] = $cost[2] + $cost[3];
}
=
{
  DEBUG_RULE_ACTION( "modified_areg: tpm_AssignExpPLUS( areg, dreg )", $1 );

  // Retrieve the base type.
  auto &ptrType = dynamic_cast<IR_PointerType &>( $2->getExp()->getType() );
  auto &t = ptrType.getBaseType();

  // In case of a dry run, create and return an appropriate instance.
  if ( dryRun )
    return(
      ARM_AddressModification {
        static_cast<LLIR_Register *>( nullptr ), nullptr, &t,
        AddressModification::ModTime::PRE, AddressModification::ModOper::ADD,
        false, true } );

  // Evaluate both expressions.
  auto *rhsReg = $action[3]();
  auto *lhsReg = $action[2]();

  // Assemble the ARM_AddressModification.
  return(
    ARM_AddressModification {
      lhsReg, rhsReg, &t, AddressModification::ModTime::PRE,
      AddressModification::ModOper::ADD } );
};


modified_areg: tpm_AssignExpPLUS( deref_areg, dreg )
{
  $cost[0] = $cost[2] + $cost[3];
}
=
{
  DEBUG_RULE_ACTION(
    "modified_areg: tpm_AssignExpPLUS( deref_areg, dreg )", $1 );

  // Retrieve the base type.
  auto &ptrType = dynamic_cast<IR_PointerType &>( $2->getExp()->getType() );
  auto &t = ptrType.getBaseType();

  // In case of a dry run, create and return an appropriate instance.
  if ( dryRun ) {
    auto dryRunDI = $action[2]( true, true );
    return(
      ARM_AddressModification {
        &dryRunDI, nullptr, &t, AddressModification::ModTime::PRE,
        AddressModification::ModOper::ADD, false, true } );
  }

  // Evaluate both expressions.
  auto *rhsReg = $action[3]();
  auto lhsReg = $action[2]( true, false );

  // Assemble the ARM_AddressModification.
  return(
    ARM_AddressModification {
      &lhsReg, rhsReg, &t, AddressModification::ModTime::PRE,
      AddressModification::ModOper::ADD } );
};


modified_areg: tpm_AssignExpPLUS( areg, tpm_IntConstExp )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION(
    "modified_areg: tpm_AssignExpPLUS( areg, tpm_IntConstExp )", $1 );

  // Retrieve the base type.
  auto &ptrType = dynamic_cast<IR_PointerType &>( $2->getExp()->getType() );
  auto &t = ptrType.getBaseType();

  // Also retrieve the integer constant.
  auto *intConstExp = dynamic_cast<const IR_IntConstExp *>( $3->getExp() );
  int offset = intConstExp->getValue().getIntValue();

  // In case of a dry run, create and return an appropriate instance.
  if ( dryRun )
    return(
      ARM_AddressModification {
        static_cast<LLIR_Register *>( nullptr ), offset, &t,
        AddressModification::ModTime::PRE, AddressModification::ModOper::ADD,
        false, true } );

  // Evaluate the left-hand-side.
  auto *lhsReg = $action[2]();

  // Assemble the ARM_AddressModification.
  return(
    ARM_AddressModification {
      lhsReg, offset, &t, AddressModification::ModTime::PRE,
      AddressModification::ModOper::ADD } );
};


modified_areg: tpm_AssignExpPLUS( deref_areg, tpm_IntConstExp )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION(
    "modified_areg: tpm_AssignExpPLUS( deref_areg, tpm_IntConstExp )", $1 );

  // Retrieve the base type.
  auto &ptrType = dynamic_cast<IR_PointerType &>( $2->getExp()->getType() );
  auto &t = ptrType.getBaseType();

  // Also retrieve the integer constant.
  auto *intConstExp = dynamic_cast<const IR_IntConstExp *>( $3->getExp() );
  int offset = intConstExp->getValue().getIntValue();

  // In case of a dry run, create and return an appropriate instance.
  if ( dryRun ) {
    auto dryRunDI = $action[2]( true, true );
    return(
      ARM_AddressModification {
        &dryRunDI, offset, &t, AddressModification::ModTime::PRE,
        AddressModification::ModOper::ADD, false, true } );
  }

  // Evaluate the left-hand-side.
  auto lhsReg = $action[2]( true, false );

  // Assemble the ARM_AddressModification.
  return(
    ARM_AddressModification {
      &lhsReg, offset, &t, AddressModification::ModTime::PRE,
      AddressModification::ModOper::ADD } );
};


# -=

modified_areg: tpm_AssignExpMINUS( areg, dreg )
{
  $cost[0] = $cost[2] + $cost[3];
}
=
{
  DEBUG_RULE_ACTION( "modified_areg: tpm_AssignExpMINUS( areg, dreg )", $1 );

  // Retrieve the base type.
  auto &ptrType = dynamic_cast<IR_PointerType &>( $2->getExp()->getType() );
  auto &t = ptrType.getBaseType();

  // In case of a dry run, create and return an appropriate instance.
  if ( dryRun )
    return(
      ARM_AddressModification {
        static_cast<LLIR_Register *>( nullptr ), nullptr, &t,
        AddressModification::ModTime::PRE, AddressModification::ModOper::SUB,
        false, true } );

  // Evaluate both expressions.
  auto *rhsReg = $action[3]();
  auto *lhsReg = $action[2]();

  // Assemble the ARM_AddressModification.
  return(
    ARM_AddressModification {
      lhsReg, rhsReg, &t, AddressModification::ModTime::PRE,
      AddressModification::ModOper::SUB } );
};


modified_areg: tpm_AssignExpMINUS( deref_areg, dreg )
{
  $cost[0] = $cost[2] + $cost[3];
}
=
{
  DEBUG_RULE_ACTION(
    "modified_areg: tpm_AssignExpMINUS( deref_areg, dreg )", $1 );

  // Retrieve the base type.
  auto &ptrType = dynamic_cast<IR_PointerType &>( $2->getExp()->getType() );
  auto &t = ptrType.getBaseType();

  // In case of a dry run, create and return an appropriate instance.
  if ( dryRun ) {
    auto dryRunDI = $action[2]( true, true );
    return(
      ARM_AddressModification {
        &dryRunDI, nullptr, &t, AddressModification::ModTime::PRE,
        AddressModification::ModOper::SUB, false, true } );
  }

  // Evaluate both expressions.
  auto *rhsReg = $action[3]();
  auto lhsReg = $action[2]( true, false );

  // Assemble the ARM_AddressModification.
  return(
    ARM_AddressModification {
      &lhsReg, rhsReg, &t, AddressModification::ModTime::PRE,
      AddressModification::ModOper::SUB } );
};


modified_areg: tpm_AssignExpMINUS( areg, tpm_IntConstExp )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION(
    "modified_areg: tpm_AssignExpMINUS( areg, tpm_IntConstExp )", $1 );

  // Retrieve the base type.
  auto &ptrType = dynamic_cast<IR_PointerType &>( $2->getExp()->getType() );
  auto &t = ptrType.getBaseType();

  // Also retrieve the integer constant.
  auto *intConstExp = dynamic_cast<const IR_IntConstExp *>( $3->getExp() );
  int offset = intConstExp->getValue().getIntValue();

  // In case of a dry run, create and return an appropriate instance.
  if ( dryRun )
    return(
      ARM_AddressModification {
        static_cast<LLIR_Register *>( nullptr ), offset, &t,
        AddressModification::ModTime::PRE, AddressModification::ModOper::SUB,
        false, true } );

  // Evaluate the left-hand-side.
  auto *lhsReg = $action[2]();

  // Assemble the ARM_AddressModification.
  return(
    ARM_AddressModification {
      lhsReg, offset, &t, AddressModification::ModTime::PRE,
      AddressModification::ModOper::SUB } );
};


modified_areg: tpm_AssignExpMINUS( deref_areg, tpm_IntConstExp )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION(
    "modified_areg: tpm_AssignExpMINUS( deref_areg, tpm_IntConstExp )", $1 );

  // Retrieve the base type.
  auto &ptrType = dynamic_cast<IR_PointerType &>( $2->getExp()->getType() );
  auto &t = ptrType.getBaseType();

  // Also retrieve the integer constant.
  auto *intConstExp = dynamic_cast<const IR_IntConstExp *>( $3->getExp() );
  int offset = intConstExp->getValue().getIntValue();

  // In case of a dry run, create and return an appropriate instance.
  if ( dryRun ) {
    auto dryRunDI = $action[2]( true, true );
    return(
      ARM_AddressModification {
        &dryRunDI, offset, &t, AddressModification::ModTime::PRE,
        AddressModification::ModOper::SUB, false, true } );
  }

  // Evaluate the left-hand-side.
  auto lhsReg = $action[2]( true, false );

  // Assemble the ARM_AddressModification.
  return(
    ARM_AddressModification {
      &lhsReg, offset, &t, AddressModification::ModTime::PRE,
      AddressModification::ModOper::SUB } );
};


# binary pointer arithmetics operators (+, -)

modified_areg: tpm_BinaryExpPLUS( areg, dreg )
{
  $cost[0] = $cost[2] + $cost[3];
}
=
{
  DEBUG_RULE_ACTION( "modified_areg: tpm_BinaryExpPLUS( areg, dreg )", $1 );

  // Retrieve the base type.
  auto &ptrType = dynamic_cast<IR_PointerType &>( $2->getExp()->getType() );
  auto &t = ptrType.getBaseType();

  // In case of a dry run, create and return an appropriate instance.
  if ( dryRun )
    return(
      ARM_AddressModification {
        static_cast<LLIR_Register *>( nullptr ), nullptr, &t,
        AddressModification::ModTime::NONE, AddressModification::ModOper::ADD,
        false, true } );

  // Evaluate both sides of the expression, left-hand side first.
  auto *lhsReg = $action[2]();
  auto *rhsReg = $action[3]();

  // Assemble the ARM_AddressModification.
  return(
    ARM_AddressModification {
      lhsReg, rhsReg, &t, AddressModification::ModTime::NONE,
      AddressModification::ModOper::ADD } );
};


modified_areg: tpm_BinaryExpPLUS( areg, tpm_IntConstExp )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION(
    "modified_areg: tpm_BinaryExpPLUS( areg, tpm_IntConstExp )", $1 );

  // Retrieve the base type.
  auto &ptrType = dynamic_cast<IR_PointerType &>( $2->getExp()->getType() );
  auto &t = ptrType.getBaseType();

  // Also retrieve the integer constant.
  auto *intConstExp = dynamic_cast<const IR_IntConstExp *> ( $3->getExp() );
  int offset = intConstExp->getValue().getIntValue();

  // In case of a dry run, create and return an appropriate instance.
  if ( dryRun )
    return(
      ARM_AddressModification {
        static_cast<LLIR_Register *>( nullptr ), offset, &t,
        AddressModification::ModTime::NONE, AddressModification::ModOper::ADD,
        false, true } );

  // Evaluate the left-hand side.
  auto *lhsReg = $action[2]();

  // Assemble the ARM_AddressModification.
  return(
    ARM_AddressModification {
      lhsReg, offset, &t, AddressModification::ModTime::NONE,
      AddressModification::ModOper::ADD } );
};


modified_areg: tpm_BinaryExpMINUS( areg, dreg )
{
  $cost[0] = $cost[2] + $cost[3];
}
=
{
  DEBUG_RULE_ACTION( "modified_areg: tpm_BinaryExpMINUS( areg, dreg )", $1 );

  // Retrieve the base type.
  auto &ptrType = dynamic_cast<IR_PointerType &>( $2->getExp()->getType() );
  auto &t = ptrType.getBaseType();

  // In case of a dry run, create and return an appropriate instance.
  if ( dryRun )
    return(
      ARM_AddressModification {
        static_cast<LLIR_Register *>( nullptr ), nullptr, &t,
        AddressModification::ModTime::NONE, AddressModification::ModOper::SUB,
        false, true } );

  // Evaluate both sides of the expression, left-hand side first.
  auto *lhsReg = $action[2]();
  auto *rhsReg = $action[3]();

  // Assemble the ARM_AddressModification.
  return(
    ARM_AddressModification {
      lhsReg, rhsReg, &t, AddressModification::ModTime::NONE,
      AddressModification::ModOper::SUB } );
};


modified_areg: tpm_BinaryExpMINUS( areg, tpm_IntConstExp )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION(
    "modified_areg: tpm_BinaryExpMINUS( areg, tpm_IntConstExp )", $1 );

  // Retrieve the base type.
  auto &ptrType = dynamic_cast<IR_PointerType &>( $2->getExp()->getType() );
  auto &t = ptrType.getBaseType();

  // Also retrieve the integer constant.
  auto *intConstExp = dynamic_cast<const IR_IntConstExp *>( $3->getExp() );
  int offset = intConstExp->getValue().getIntValue();

  // In case of a dry run, create and return an appropriate instance.
  if ( dryRun )
    return(
      ARM_AddressModification {
        static_cast<LLIR_Register *>( nullptr ), offset, &t,
        AddressModification::ModTime::NONE, AddressModification::ModOper::SUB,
        false, true } );

  // Evaluate the left-hand side.
  auto *lhsReg = $action[2]();

  // Assemble the ARM_AddressModification.
  return(
    ARM_AddressModification {
      lhsReg, offset, &t, AddressModification::ModTime::NONE,
      AddressModification::ModOper::SUB } );
};


# Special pointer arithmetics.

dreg: tpm_BinaryExpMINUS( areg, areg )
{
  // Subtracting pointers yields the number of elements of the base type that
  // are/could be present in the memory interval.
  $cost[0] = $cost[2] + $cost[3] + CT( INS_SUB_32 );

  // Acquire the size of the pointer's base type.
  auto &ptrType = dynamic_cast<IR_PointerType &>( $2->getExp()->getType() );
  auto &t = ptrType.getBaseType();

  int baseTypeSize =
    ( t.getType() == IR_Type::VOID ) ? 1 : computeSizeOf( &t );

  // If the base type is a power of 2, this can be implemented rather
  // efficiently, because the division boils down to a shift.
  if ( ( baseTypeSize & (baseTypeSize - 1) ) == 0 )
    $cost[0] += CT( INS_MOV_32 );
  else {
    // TODO: Absurdly expensive, difficult to quantify, since an external
    // function is called.
  }
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_BinaryExpMINUS( areg, areg )", $1 );

  // The register that will store the result.
  auto *resultReg = ARMINSTRUCTIONS.CreateRegister( "" );

  // First, evaluate both aregs.
  auto *lhsReg = $action[2]();
  auto *rhsReg = $action[3]();

  // First, perform the subtraction into a new register.
  ARMINSTRUCTIONS.insertSUB( resultReg, lhsReg, rhsReg, $1->getExp() );

  // This register now contains the difference in bytes. The expression shall
  // return the differnce in multiples of the base type, however, so the result
  // now has to be divided.

  // Acquire the size of the pointer's base type.
  auto &ptrType = dynamic_cast<IR_PointerType &>( $2->getExp()->getType() );
  auto &t = ptrType.getBaseType();

  int baseTypeSize =
    ( t.getType() == IR_Type::VOID ) ? 1 : computeSizeOf( &t );

  // If the base type is a power of 2, this can be implemented rather
  // efficiently, because the division boils down to a shift.
  if ( ( baseTypeSize & (baseTypeSize - 1) ) == 0 ) {
    // Determine the exponent.
    int e = 0;
    for ( ; baseTypeSize != 1; baseTypeSize >>= 1, ++e ) ;

    // Shift the result by the correct amount of bits. Use an arithemtic shift,
    // since the subtraction's result could be negative, which is intended.
    ARMINSTRUCTIONS.insertMOV( resultReg, resultReg, OPER_ASR, e, $1->getExp() );
  } else {
    // TODO: The ugly case. Currently performing division at runtime, which is
    // *incredibly* expensive.

    // Load the typeSize into a register.
    auto *typeReg = ARMINSTRUCTIONS.CreateRegister( "" );
    ARMINSTRUCTIONS.insertMOV_ORR( typeReg, baseTypeSize, $1->getExp() );

    // Then, perform division at runtime.
    ARMINSTRUCTIONS.insertUDIVSI(
      OPER_AL, resultReg, resultReg, typeReg, $1->getExp() );
  }

  return( resultReg );
};


# The C standard mandates that expressions of the form &*e shall not emit any
# additional instructions compared to expression e by itself.
# These rules take care of that.

zero_op_modified_areg: tpm_UnaryExpDEREF( modified_areg )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION(
    "zero_op_modified_areg: tpm_UnaryExpDEREF( modified_areg )", $1 );

  // Pass down the info whether this specific modified_areg is a dry run
  // instance.
  return( $action[2]( dryRun ) );
};


zero_op_areg: tpm_UnaryExpDEREF( areg )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "zero_op_areg: tpm_UnaryExpDEREF( areg )", $1 );

  // Simply pass through the address register.
  return( $action[2]() );
};

# @author Heiko Falk <Heiko.Falk@tuhh.de>
zero_op_areg: tpm_IndexExp( areg, dreg )
{
  // This rule is only applicable if the parent operation is an address operator
  // (it's the only one that can take a 'zero_op_areg'). Therefore, this rule
  // just computes the access location and the address operator can then return
  // it without any memory access being performed (see ANSI C 6.5.3.2).

  $cost[0] = $cost[2] + $cost[3] + loadRegisterRelativeAddressCost();
}
=
{
  DEBUG_RULE_ACTION( "zero_op_areg: tpm_IndexExp( areg, dreg )", $1 );

  const int byteSize = computeSizeOf( getBaseType( *$2->getExp() ) );

  auto p1 = $action[2]();
  auto p2 = $action[3]();

  // LLIR
  LLIR_Register *reg =
    loadRegisterRelativeAddress( p1, p2, byteSize, $1->getExp() );

  return reg;
};

modified_areg: tpm_UnaryExpADDR( zero_op_modified_areg )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION(
    "modified_areg: tpm_UnaryExpADDR( zero_op_modified_areg )", $1 );

  // Simply pass through the ARM_AddressModification.
  return( $action[2]( dryRun ) );
};


areg: tpm_UnaryExpADDR( zero_op_areg )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "areg: tpm_UnaryExpADDR( zero_op_areg )", $1 );

  // Simply pass through the LLIR_Register*.
  return( $action[2]() );
};


dreg: areg
{
  // Chain rule allowing for an areg to be treated like a reg. This conversion
  // is valid if the pointer is part of a relation (==, <, ...) or is used as a
  // condition. (||, &&, if, for ...).
  bool conversionAllowed = false;

  // Check whether this areg appears as part of a relational expressions. These
  // are ==, !=, <, <=, >, >=.
  auto *bexp = dynamic_cast<IR_BinaryExp *>( $1->getExp()->getParent() );
  if ( bexp )
    switch ( bexp->getOperator() ) {
      case IR_BinaryExp::Operator::LT:
      case IR_BinaryExp::Operator::GT:
      case IR_BinaryExp::Operator::LEQ:
      case IR_BinaryExp::Operator::GEQ:
      case IR_BinaryExp::Operator::EQ:
      case IR_BinaryExp::Operator::NEQ: {
        conversionAllowed = true;
        break;
      }

      default:
        break;
    }

  // Check for a logical NOT.
  auto *uexp = dynamic_cast<IR_UnaryExp *>( $1->getExp()->getParent() );
  if ( uexp && ( uexp->getOperator() == IR_UnaryExp::Operator::LOGNOT ) )
    conversionAllowed = true;

  // Next, check for logical operators (||, &&) as well as the cases where the
  // expression *is* the conditional statement of an if, for, etc...
  conversionAllowed = conversionAllowed || $1->getExp()->isConditionExp();

  // Finally, decide.
  $cost[0] = conversionAllowed ? $cost[1] : COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: areg", $1 );

  // Simply pass through the register.
  return( $action[1]() );
};


# Rules for the any_reg nonterminal.

any_reg: dreg
{
  $cost[0] = $cost[1];
}
=
{
  DEBUG_RULE_ACTION( "any_reg: dreg", $1 );

  // Simply pass through the dreg.
  return( $action[1]() );
};


any_reg: areg
{
  $cost[0] = $cost[1];
}
=
{
  DEBUG_RULE_ACTION( "any_reg: areg", $1 );

  // Simply pass through the areg.
  return( $action[1]() );
};


any_reg: ereg
{
  $cost[0] = $cost[1];
}
=
{
  DEBUG_RULE_ACTION( "any_reg: ereg", $1 );

  // Simply pass through the ereg.
  return( $action[1]() );
};
