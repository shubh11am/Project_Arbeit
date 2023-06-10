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
#  Copyright 2009 - 2022
#
#  Hamburg University of Technology (TUHH)
#  Institute of Embedded Systems
#  21071 Hamburg
#  Germany
#
#  http://www.tuhh.de/es/esd/research/wcc
#
#


##############################################################################
#
#
# Initialization of composed types (structs/unions) and arrays
#
#
###############################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
initializable_object: tpm_SymbolExp
{
  // This rule converts arrays into the 'initializable_object' nonterminal to
  // make them initializable by using the following rules. We only need to care
  // about symbol expressions here, because they are the only ones that can be
  // initialized with an init list.
  if ( isArrayType( *$1->getExp() ) )
    $cost[0] = 0;
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "initializable_object: tpm_SymbolExp", $1 );

  // TODO: If arrays are modeled with areg + offset too in the future (as
  //       composed type objects are modeled), then this rule can be simplified
  //       in the same way as 'initializable_object: composed_type_object'
  auto *symexp = dynamic_cast<IR_SymbolExp *>( $1->getExp() );

  auto *areg = TCINSTRUCTIONS.CreateRegister( PHREG_SP, true );
  auto &sp = TCINSTRUCTIONS.createAReg();
  bindToPHREG( sp, 10 );

  return(
    TC_AddressWithOffset {
      areg, sp,
      TCCODESEL->getStack()->getSymbolOffset( &symexp->getSymbol() ) } );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
initializable_object: composed_type_object
{
  // This rule converts composed type nonterminals to the 'initializable_object'
  // nonterminal to make them initializable by using the following rules.
  if ( isComposedType( *$1->getExp() ) )
    $cost[0] = $cost[1];
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "initializable_object: composed_type_object", $1 );

  return( $action[1]() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
initialized_object: tpm_AssignExpASSIGN( initializable_object, init_list )
{
  // This rule does not have the same properties as the assignments in
  // "assignment.m4" (no casting of operands). Therefore, it must stay here.
  auto *initExp = dynamic_cast<IR_InitListExp *>( $3->getExp() );
  IR_Type *initialized_type = &( initExp->getType() );
  auto *lhs = dynamic_cast<IR_SymbolExp *>( $2->getExp() );

  $cost[0] = $cost[2] + $cost[3];

  if ( !lhs->getSymbol().isGlobal() &&
       !coversAllElements( *initExp ) )
    $cost[0] += initWithZeroCost( initialized_type->sizeOf(), *initExp );

  bool useBaseOffsetAddressing =
    ( initialized_type->sizeOf() <= TC_Const10_Signed::getMaxValue(10 ) );

  if ( !useBaseOffsetAddressing )
    $cost[0] += TC13::OperationFormat::AAC16BOA.getSize();
}
=
{
  DEBUG_RULE_ACTION(
    "initialized_object: tpm_AssignExpASSIGN( initializable_object, init_list )",
    $1 );

  auto *initExp = dynamic_cast<IR_InitListExp *>( $3->getExp() );
  IR_Type *initialized_type = &( initExp->getType() );
  auto *lhs = dynamic_cast<IR_SymbolExp *>( $2->getExp() );
  ufAssert( initExp );
  ufAssertT( lhs, "Unexpected LHS form!" );

  // Get base address of initialized component.
  auto addr = $action[2]();

  // Fill the complete initialized area with zeroes. This is only required if at
  // least one element is not initialized in a local ('auto' storage duration)
  // variable. Static variables are implicitly initialized, because the .bss
  // section is intialized with zeroes.
  if ( !lhs->getSymbol().isGlobal() &&
       !coversAllElements( *initExp ) )
    initWithZero( addr, initialized_type->sizeOf(), *initExp );

  // If the offset is not wider than 10 bits, we use base + offset addressing.
  // Otherwise, we use a copy of the address register and do post-increments
  // after each initialization step to update the access location (this needs an
  // inital LEA instruction, the first alternative does not.)
  const bool useBaseOffsetAddressing =
    ( addr.getOffset() + initialized_type->sizeOf() <=
      TC_Const10_Signed::getMaxValue(10 ) );

  if ( useBaseOffsetAddressing )
    $action[3](
      addr.getARegister(), addr.getAReg(), addr.getOffset(),
      useBaseOffsetAddressing );
  else {
    // Compute offset of first initialized element. It could be different from
    // zero, because C99 allows initialization in any arbitrary order.
    long additional_offset = 0;
    IR_InitListExp *searchedExp = initExp;
    IR_Exp *firstExp = nullptr;

    do {
      // Get first exp and add offset
      firstExp = *( searchedExp->getInitExps().begin() );
      additional_offset += searchedExp->getOffset( firstExp ).getIntValue();

      // Check if it is an InitListExp too. If so we have to descend the tree
      // and further accumulate the offsets.
      searchedExp = dynamic_cast<IR_InitListExp *>( firstExp );
    } while ( searchedExp != nullptr );

    // Allocate and load an address register with the effective address. Even if
    // there's no offset, we need a copy (areg_orig may be the SP, etc.).
    // LLIR
    LLIR_Register *areg = TCINSTRUCTIONS.CreateRegister( "", true );
    TCINSTRUCTIONS.insertLEA(
      areg, OPER_BASE, addr.getARegister(),
      addr.getOffset() + additional_offset, $1->getExp() );

    // WIR
    auto &aReg = TCINSTRUCTIONS.createAReg();
    TCINSTRUCTIONS.insertLEA(
      aReg, addr.getAReg(), addr.getOffset() + additional_offset,
      $1->getExp() );

    const long address_increment = initialized_type->sizeOf();
    $action[3]( areg, aReg, address_increment, useBaseOffsetAddressing );
  }

  return( addr );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
init_list: tpm_InitListExp( init_list )
{
  // This rule just consumes the 'tpm_InitListExp' terminal and starts the real
  // init list processing by calling its child rule.
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "init_list: tpm_InitListExp( init_list )", $1 );

  $action[2]( areg, aReg, address_increment, useBaseOffsetAddressing );

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
init_list: tpm_InitListExpELEM( init_list_element, init_list )
{
  $cost[0] = $cost[2] + $cost[3];
}
=
{
  DEBUG_RULE_ACTION(
    "init_list: tpm_InitListExpELEM( init_list_element, init_list )", $1 );

  IR_Exp * const initExp = $2->getExp();
  auto *initList = dynamic_cast<IR_InitListExp *>( initExp->getParent() );
  ufAssert( initList );

  if ( useBaseOffsetAddressing ) {
    $action[2](
      areg, aReg,
      address_increment + initList->getOffset( initExp ).getIntValue(),
      useBaseOffsetAddressing );
    $action[3]( areg, aReg, address_increment, useBaseOffsetAddressing );
  } else {
    long offset_current = initList->getOffset( initExp ).getIntValue();
    long offset_next;
    long component_size;

    auto it_next = initList->getInitExpIterator( initExp );
    ++it_next;
    if ( it_next != initList->getInitExps().end() ) {
      // Compute offset of next initialized element, because C99 allows
      // initialization in any arbitrary order.
      offset_next = initList->getOffset( *it_next ).getIntValue();
      auto *searchedExp = dynamic_cast<IR_InitListExp *>( *it_next );
      while ( searchedExp != nullptr ) {
        IR_Exp *firstExp = *( searchedExp->getInitExps().begin() );
        offset_next += searchedExp->getOffset( firstExp ).getIntValue();

        // Check if it is an InitListExp too. If so, then we have to descend the
        // tree and further accumulate the offsets.
        searchedExp = dynamic_cast<IR_InitListExp *>( firstExp );
      }

      component_size = offset_next - offset_current;
    } else
      component_size = address_increment;

    // Check increment.
    if ( ( component_size >= TC_Const10_Signed::getMinValue( 10 ) ) &&
         ( component_size <= TC_Const10_Signed::getMaxValue( 10 ) ) &&
         // Bitfield rules cannot increment the address register - must use LEA.
         !dynamic_cast<const IR_BitfieldType *>(
           getTypeInInitList( *initExp ) ) )
      // OK, can be done with normal POSTINC.
      $action[2]( areg, aReg, component_size, useBaseOffsetAddressing );
    else {
      // Offset too big or bitfield, we need a LEA.
      $action[2]( areg, aReg, 0, useBaseOffsetAddressing );

      TCINSTRUCTIONS.insertLEA(
        areg, OPER_BASE, areg, component_size, $1->getExp() );
      TCINSTRUCTIONS.insertLEA( aReg, aReg, component_size, $1->getExp() );
    }

    $action[3](
      areg, aReg, address_increment - component_size, useBaseOffsetAddressing );
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
init_list: tpm_InitListExpNOELEM
{
  $cost[0] = 0;
}
=
{
  DEBUG_RULE_ACTION( "init_list: tpm_InitListExpNOELEM", $1 );

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
init_list_element: dreg
{
  // This rule handles all integer types from '_Bool' to 'long' plus 'float'
  // (all those types that can be stored in a single data register), except for
  // bitfield types.
  const IR_Type *ilType = getTypeInInitList( *$1->getExp() );

  if ( ilType && !isBitfieldType( *ilType ) ) {
    switch ( ilType->getType() ) {
      case IR_Type::BOOL:
      case IR_Type::CHAR:
      case IR_Type::UNSIGNED_CHAR:
      case IR_Type::SHORT:
      case IR_Type::UNSIGNED_SHORT: {
        $cost[0] = $cost[1] + TC13::OperationFormat::AC10DBOA_1.getSize();
        break;
      }

      case IR_Type::INT:
      case IR_Type::UNSIGNED_INT:
      case IR_Type::LONG:
      case IR_Type::UNSIGNED_LONG:
      case IR_Type::FLOAT: {
        $cost[0] = $cost[1] + TC13::OperationFormat::AC16DBOA.getSize();
        break;
      }

      default: {
        $cost[0] = COST_INFINITY;
        break;
      }
    }
  } else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "init_list_element: dreg", $1 );

  const IR_Type *ilType = getTypeInInitList( *$1->getExp() );
  const bool useAutoIncrement =
    !useBaseOffsetAddressing && ( address_increment != 0 );

  auto p = $action[1]();

  const string mode = ( useAutoIncrement ? OPER_POSTINC : OPER_BASE );

  switch ( ilType->getType() ) {
    case IR_Type::BOOL:
    case IR_Type::CHAR:
    case IR_Type::UNSIGNED_CHAR: {
      TCINSTRUCTIONS.insertST_B(
        mode, areg, address_increment, p.first, $1->getExp() );

      if ( useAutoIncrement )
        TCINSTRUCTIONS.insertST_B(
          TC13::AddressingMode::post, aReg, address_increment, p.second,
          $1->getExp() );
      else
        TCINSTRUCTIONS.insertST_B(
          aReg, address_increment, p.second, $1->getExp() );
      break;
    }

    case IR_Type::SHORT:
    case IR_Type::UNSIGNED_SHORT: {
      TCINSTRUCTIONS.insertST_H(
        mode, areg, address_increment, p.first, $1->getExp() );

      if ( useAutoIncrement )
        TCINSTRUCTIONS.insertST_H(
          TC13::AddressingMode::post, aReg, address_increment, p.second,
          $1->getExp() );
      else
        TCINSTRUCTIONS.insertST_H(
          aReg, address_increment, p.second, $1->getExp() );
      break;
    }

    default: {
      TCINSTRUCTIONS.insertST_W(
        mode, areg, address_increment, p.first, $1->getExp() );

      if ( useAutoIncrement )
        TCINSTRUCTIONS.insertST_W(
          TC13::AddressingMode::post, aReg, address_increment, p.second,
          $1->getExp() );
      else
        TCINSTRUCTIONS.insertST_W(
          aReg, address_increment, p.second, $1->getExp() );
      break;
    }
  }

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
init_list_element: dreg
{
  // This rule handles bitfield assignments in init lists.
  const IR_Type *ilType = getTypeInInitList( *$1->getExp() );

  if ( ilType && isBitfieldType( *ilType ) )
    $cost[0] =
      $cost[1] + TC13::OperationFormat::SDA_1.getSize() +
      TC13::OperationFormat::DDC9_2.getSize() +
      TC13::OperationFormat::SAD_1.getSize() + writeValueToBitfieldCost();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "init_list_element: dreg", $1 );

  auto p = $action[1]();

  // Get some type information about the composed type and the current symbol.
  auto *initList = dynamic_cast<IR_InitListExp *>( $1->getExp()->getParent() );
  auto *compType = dynamic_cast<IR_ComposedType *>( &initList->getType() );
  auto *bitfieldSym = &initList->getInitializedComponent( $1->getExp() );
  auto *bitfieldType =
    dynamic_cast<IR_BitfieldType *>( &bitfieldSym->getType() );

  // Extract bitfield parameters.
  const int bitfieldOffset = getBitfieldBitOffset( *compType, *bitfieldSym );
  const int bitfieldLength = getBitfieldLength( *bitfieldSym );

  // LLIR
  LLIR_Register *iaddr = areg;
  if ( useBaseOffsetAddressing ) {
    LLIR_Register * const newAReg = TCINSTRUCTIONS.CreateRegister( "", true );
    TCINSTRUCTIONS.insertLEA(
      newAReg, OPER_BASE, areg, address_increment, $1->getExp() );
    iaddr = newAReg;
  }

  // WIR
  const TC_ARegV &iAddr =
    useBaseOffsetAddressing ?
      TCINSTRUCTIONS.createAReg() : aReg;
  if ( useBaseOffsetAddressing )
    TCINSTRUCTIONS.insertLEA( iAddr, aReg, address_increment, $1->getExp() );

  // TODO: Figure out whether we must cast the value to the bitfield type (there
  //       will be no implicit cast terminal!). If this is the case, it has to
  //       be done here, and in the assignment rules.

  // We need a 4-byte aligned base address to store the bitfield. 'iaddr' may
  // be unaligned, so we must enforce the alignment by nullifying the last 2
  // bits. Before we can do this, we must copy the address to a new register.

  // LLIR
  LLIR_Register *areg_truncated = TCINSTRUCTIONS.CreateRegister( "", true );
  LLIR_Register *dreg_truncated = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertMOV_D( dreg_truncated, iaddr, $1->getExp() );
  TCINSTRUCTIONS.insertANDN( dreg_truncated, dreg_truncated, 3, $1->getExp() );
  TCINSTRUCTIONS.insertMOV_A( areg_truncated, dreg_truncated, $1->getExp() );

  // WIR
  auto &aTrunc = TCINSTRUCTIONS.createAReg();
  auto &dTrunc = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertMOV_D( dTrunc, iAddr, $1->getExp() );
  TCINSTRUCTIONS.insertANDN( dTrunc, dTrunc, 3, $1->getExp() );
  TCINSTRUCTIONS.insertMOV_A( aTrunc, dTrunc, $1->getExp() );

  writeValueToBitfield(
    p.first, areg_truncated, AddressModification::ModTime::NONE, p.second,
    aTrunc, *bitfieldType, 0, bitfieldOffset, bitfieldLength, $1->getExp() );

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
init_list_element: llereg
{
  // This rule handles bitfield assignments in init lists.
  const IR_Type *ilType = getTypeInInitList( *$1->getExp() );

  if ( ilType && isBitfieldType( *ilType ) )
    $cost[0] =
      $cost[1] + TC13::OperationFormat::SDA_1.getSize() +
      TC13::OperationFormat::DDC9_2.getSize() +
      TC13::OperationFormat::SAD_1.getSize() + writeValueToBitfieldCost();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "init_list_element: llereg", $1 );

  auto p = $action[1]();

  // Get some type information about the composed type and the current symbol.
  auto *initList = dynamic_cast<IR_InitListExp *>( $1->getExp()->getParent() );
  auto *compType = dynamic_cast<IR_ComposedType *>( &initList->getType() );
  auto *bitfieldSym = &initList->getInitializedComponent( $1->getExp() );
  auto *bitfieldType =
    dynamic_cast<IR_BitfieldType *>( &bitfieldSym->getType() );

  // Extract bitfield parameters.
  const int bitfieldOffset = getBitfieldBitOffset( *compType, *bitfieldSym );
  const int bitfieldLength = getBitfieldLength( *bitfieldSym );

  // LLIR
  LLIR_Register *iaddr = areg;
  if ( useBaseOffsetAddressing ) {
    LLIR_Register * const newAReg = TCINSTRUCTIONS.CreateRegister( "", true );
    TCINSTRUCTIONS.insertLEA(
      newAReg, OPER_BASE, areg, address_increment, $1->getExp() );
    iaddr = newAReg;
  }

  // WIR
  const TC_ARegV &iAddr =
    useBaseOffsetAddressing ?
      TCINSTRUCTIONS.createAReg() : aReg;
  if ( useBaseOffsetAddressing )
    TCINSTRUCTIONS.insertLEA( iAddr, aReg, address_increment, $1->getExp() );

  // TODO: Figure out whether we must cast the value to the bitfield type (there
  //       will be no implicit cast terminal!). If this is the case, it has to
  //       be done here, and in the assignment rules.

  // We need a 4-byte aligned base address to store the bitfield. 'iaddr' may
  // be unaligned, so we must enforce the alignment by nullifying the last 2
  // bits. Before we can do this, we must copy the address to a new register.

  // LLIR
  LLIR_Register *areg_truncated = TCINSTRUCTIONS.CreateRegister( "", true );
  LLIR_Register *dreg_truncated = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertMOV_D( dreg_truncated, iaddr, $1->getExp() );
  TCINSTRUCTIONS.insertANDN( dreg_truncated, dreg_truncated, 3, $1->getExp() );
  TCINSTRUCTIONS.insertMOV_A( areg_truncated, dreg_truncated, $1->getExp() );

  // WIR
  auto &aTrunc = TCINSTRUCTIONS.createAReg();
  auto &dTrunc = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertMOV_D( dTrunc, iAddr, $1->getExp() );
  TCINSTRUCTIONS.insertANDN( dTrunc, dTrunc, 3, $1->getExp() );
  TCINSTRUCTIONS.insertMOV_A( aTrunc, dTrunc, $1->getExp() );

  writeValueToBitfield(
    p.first->GetFirstChild(), areg_truncated,
    AddressModification::ModTime::NONE, p.second.get().begin()->get(), aTrunc,
    *bitfieldType, 0, bitfieldOffset, bitfieldLength, $1->getExp() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
init_list_element: ereg
{
  auto *ilType = getTypeInInitList( *$1->getExp() );

  if ( ilType && !isBitfieldType( *ilType ) && isDoubleType( *ilType ) )
    $cost[0] = $cost[1] + TC13::OperationFormat::AC10EBOA.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "init_list_element: ereg", $1 );

  bool useAutoIncrement =
    !useBaseOffsetAddressing && ( address_increment != 0 );

  auto p = $action[1]();

  // LLIR
  string mode = ( useAutoIncrement ? OPER_POSTINC : OPER_BASE );
  TCINSTRUCTIONS.insertST_D(
    mode, areg, address_increment, p.first, $1->getExp() );

  // WIR
  if ( useAutoIncrement )
    TCINSTRUCTIONS.insertST_D(
      TC13::AddressingMode::post, aReg, address_increment, p.second,
      $1->getExp() );
  else
    TCINSTRUCTIONS.insertST_D(
      aReg, address_increment, p.second, $1->getExp() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
init_list_element: llereg
{
  auto *ilType = getTypeInInitList( *$1->getExp() );

  if ( ilType && !isBitfieldType( *ilType ) && isLongLongType( *ilType ) )
    $cost[0] = $cost[1] + TC13::OperationFormat::AC10EBOA.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "init_list_element: llereg", $1 );

  bool useAutoIncrement =
    !useBaseOffsetAddressing && ( address_increment != 0 );

  auto p = $action[1]();

  // LLIR
  string mode = ( useAutoIncrement ? OPER_POSTINC : OPER_BASE );
  TCINSTRUCTIONS.insertST_D(
    mode, areg, address_increment, p.first, $1->getExp() );

  // WIR
  if ( useAutoIncrement )
    TCINSTRUCTIONS.insertST_D(
      TC13::AddressingMode::post, aReg, address_increment, p.second,
      $1->getExp() );
  else
    TCINSTRUCTIONS.insertST_D(
      aReg, address_increment, p.second, $1->getExp() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
init_list_element: areg
{
  auto *ilType = getTypeInInitList( *$1->getExp() );

  if ( ilType && !isBitfieldType( *ilType ) && isARegType( *ilType ) )
    $cost[0] = $cost[1] + TC13::OperationFormat::AC10ABOA.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "init_list_element: areg", $1 );

  const bool useAutoIncrement =
    !useBaseOffsetAddressing && ( address_increment != 0 );

  auto p = $action[1]();

  // LLIR
  const string mode = ( useAutoIncrement ? OPER_POSTINC : OPER_BASE );
  TCINSTRUCTIONS.insertST_A(
    mode, areg, address_increment, p.first, $1->getExp() );

  // WIR
  if ( useAutoIncrement )
    TCINSTRUCTIONS.insertST_A(
      TC13::AddressingMode::post, aReg, address_increment, p.second,
      $1->getExp() );
  else
    TCINSTRUCTIONS.insertST_A(
      aReg, address_increment, p.second, $1->getExp() );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
init_list_element: init_list
{
  // This rule handles nested init lists. In that case, we must reset the
  // offsets that were already computed for the initialization of the current
  // object.
  $cost[0] = $cost[1];
}
=
{
  DEBUG_RULE_ACTION( "init_list_element: init_list", $1 );

  auto *initList = dynamic_cast<IR_InitListExp *>( $1->getExp() );

  if ( useBaseOffsetAddressing ) {
    //IR_InitListExp * const parentList = dynamic_cast<IR_InitListExp *>(
    //                                      initList->getParent() );
    //ufAssertT( parentList, "Invalid code structure!" );
    //address_increment += parentList->getOffset( initList ).getIntValue();
  } else {
    IR_Exp *firstExp = *( initList->getInitExps().begin() );
    address_increment -= initList->getOffset( firstExp ).getIntValue();
  }

  $action[1]( areg, aReg, address_increment, useBaseOffsetAddressing );

  return;
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
composed_type_object: initialized_object
{
  // This rule converts 'initialized_object's back to composed type
  // nonterminals. This enables us to have a single init list rule (see above)
  // that handles all cases.
  if ( isComposedType( *$1->getExp() ) )
    $cost[0] = $cost[1];
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "composed_type_object: initialized_object", $1 );

  $action[1]();

  IR_Symbol *sym = getLHSSymbol( *$1->getExp() );
  ufAssertT( sym, "Unknown initializer structure!" );

  // We have to reload the base address of the object here. "areg" holds the
  // incremented value (points to the location after the last initializer).
  // LLIR
  auto *areg = TCINSTRUCTIONS.CreateRegister( PHREG_SP, true );

  // WIR
  auto &sp = TCINSTRUCTIONS.createAReg();
  bindToPHREG( sp, 10 );

  return(
    TC_AddressWithOffset {
      areg, sp, TCCODESEL->getStack()->getSymbolOffset( sym ) } );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
areg: initialized_object
{
  // This rule converts 'initialized_object's back to array nonterminals. This
  // enables to have a single init list rule (see above) that handles all cases.
  if ( isArrayType( *$1->getExp() ) )
    $cost[0] = $cost[1];
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "areg: initialized_object", $1 );

  IR_Symbol *sym = getLHSSymbol( *$1->getExp() );
  auto off = TCCODESEL->getStack()->getSymbolOffset( sym );

  $action[1]();

  // LLIR
  LLIR_Register *stackp = TCINSTRUCTIONS.CreateRegister( PHREG_SP, true );
  auto *reg = &loadAccessLocationToAReg( stackp, off, "", $1->getExp() );

  // WIR
  auto &r = TCINSTRUCTIONS.createAReg();
  auto &sp = TCINSTRUCTIONS.createAReg();
  bindToPHREG( sp, 10 );

  if ( off == 0 )
    TCINSTRUCTIONS.insertMOV_AA( r, sp, $1->getExp() );
  else
    TCINSTRUCTIONS.insertLEA( r, sp, off, $1->getExp() );

  return( make_pair( reg, ref( r ) ) );
};
