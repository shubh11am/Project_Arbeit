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


###############################################################################
#
#
# initializer list rules for arrays and structs
#
#
###############################################################################

areg: initialized_object
{
  // This rule converts 'initialized_object's back to array nonterminals. This
  // enables us to have a single init list rule (see above) which handles all
  // cases.
  if ( isArrayType( *$1->getExp() ) )
    $cost[0] = $cost[1];
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "areg: initialized_object", $1 );

  // We have to reload the base address of the object here. "reg" holds the
  // incremented value (points to the location after the last initializer).

  auto *sp = ARMINSTRUCTIONS.CreateRegister( PHREG_SP, true );
  auto *sym = getLHSSymbol( *$1->getExp() );

  $action[1]();

  return(
    &loadAccessLocationToReg( sp, ARMCODESEL->getStack()->getSymbolOffset( sym ),
    "", $1->getExp() ) );
};


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

  // TODO: If arrays are modeled with reg + offset too in the future (like
  //       composed type objects are modeled), then this rule can be simplified
  //       in the same way as 'initializable_object: composed_type_object'.

  auto *symexp = dynamic_cast<IR_SymbolExp *>( $1->getExp() );

  return(
    AddressWithOffset {
      ARMINSTRUCTIONS.CreateRegister( PHREG_SP, true ),
      ARMCODESEL->getStack()->getSymbolOffset( &symexp->getSymbol() ) } );
};


composed_type_object: initialized_object
{
  // This rule converts 'initialized_object's back to composed type
  // nonterminals. This enables us to have a single init list rule (see above)
  // which handles all cases.
  if ( isComposedType( *$1->getExp() ) )
    $cost[0] = $cost[1];
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "composed_type_object: initialized_object", $1 );

  auto *sym = getLHSSymbol( *$1->getExp() );

  // In case of a dry run, only return the offset.
  if ( dryRun )
    return(
      AddressWithOffset {
        nullptr, ARMCODESEL->getStack()->getSymbolOffset( sym ) } );

  $action[1]();

  return(
    AddressWithOffset {
      ARMINSTRUCTIONS.CreateRegister( PHREG_SP, true ),
      ARMCODESEL->getStack()->getSymbolOffset( sym ) } );
};


initializable_object: composed_type_object
{
  // This rule converts composed type nonterminals into the
  // 'initializable_object' nonterminal to make them initializable.
  if ( isComposedType( *$1->getExp() ) )
    $cost[0] = $cost[1];
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "initializable_object: composed_type_object", $1 );

  return( $action[1]( false ) );
};


initialized_object: tpm_AssignExpASSIGN( initializable_object, init_list )
{
  // This rule does not have the same properties as the assignments in
  // "assignment.m4" (no casting of operands), therefore it must stay here.

  $cost[0] = $cost[2] + $cost[3];

  auto *initExp = dynamic_cast<IR_InitListExp *>( $3->getExp() );
  auto *lhs = dynamic_cast<IR_SymbolExp *>( $2->getExp() );

  if ( !coversAllElements( *initExp ) ) {
    auto &t = lhs->getType();
    unsigned int size = static_cast<unsigned int>( t.sizeOf() );

    UninitializedMemoryArea uninitMem { size };
    uninitMem.splitByInitExpressions( initExp );

    // This cost calculation is not completely accurate as the object offset is
    // needed. As initializable_object has no dry run capabilities, this value
    // is set to zero.
    $cost[0] += uninitMem.zeroMemoryCost( 0 );
  }
}
=
{
  DEBUG_RULE_ACTION(
    "initialized_object: tpm_AssignExpASSIGN( initializable_object, "
    "init_list )", $1 );

  auto *initExp = dynamic_cast<IR_InitListExp *>( $3->getExp() );
  auto *lhs = dynamic_cast<IR_SymbolExp *>( $2->getExp() );

  // Get addr of initializable_object component (sp + offset).
  auto addr = $action[2]();

  // If not all elements are covered, the remaining are zeroed.
  if ( !coversAllElements( *initExp ) ) {
    auto &t = lhs->getType();
    UninitializedMemoryArea uninitMem {
      static_cast<unsigned int>( t.sizeOf() ) };
    uninitMem.splitByInitExpressions( initExp );
    uninitMem.zeroMemory( addr, initExp );
  }

  // Do initialization with useBaseOffsetAddressing = true.
  $action[3]( addr.getARegister(), addr.getOffset(), true );

  return( addr );
};


initialized_object: tpm_AssignExpASSIGN( initializable_object, areg )
{
  // This rule should only map to string initializations.
  auto *stringConst = dynamic_cast<IR_StringConstExp *>( $3->getExp() );

  if ( stringConst && isArrayType( *$2->getExp() ) ) {
    $cost[0] = $cost[2] + $cost[3];

    auto &t = dynamic_cast<IR_ArrayType &>( $2->getExp()->getType() );
    unsigned int length = stringConst->getValue().length();
    int unused = t.sizeOf() - length;

    // Instructions for the loop.
    if ( length >= 32 ) {
      unsigned int loopIterations = length / 16;
      length -= loopIterations * 16;
      $cost[0] += ARMINSTRUCTIONS.insertMOV_ORR_Cost( loopIterations );
      $cost[0] +=
        CT( INS_STM_32 ) + CT( INS_LDM_32 ) + CT( INS_SUB_32 ) +
        CT( INS_CMP_32 ) + CT( INS_B_32 );
    }

    // Calculate costs for LDMIA/STMIA with 4/3/2/1 registers.
    unsigned int additionalLoadStorePairs = 0;
    additionalLoadStorePairs += length / 16;
    length %= 16;
    additionalLoadStorePairs += length / 12;
    length %= 12;
    additionalLoadStorePairs += length / 8;
    length %= 8;
    additionalLoadStorePairs += length / 4;
    length %= 4;

    // Halfword and byte operations.
    if ( length >= 2 )
      additionalLoadStorePairs++;
    if ( length % 2 == 1 )
      additionalLoadStorePairs++;

    $cost[0] +=
      ( CT( INS_LDM_32 ) + CT( INS_STM_32 ) ) * additionalLoadStorePairs;

    if ( unused > 0 )
      // This calculation is not accurate, as it assumes that the array
      // beginning is 4 bytes aligned.
      $cost[0] += initWithZeroCost( unused, stringConst->getValue().length() );
  } else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION(
    "initialized_object: tpm_AssignExpASSIGN( initializable_object, areg )",
    $1 );

  auto *stringConstExp = dynamic_cast<IR_StringConstExp *>( $3->getExp() );

  auto exp = $1->getExp();
  auto *src = $action[3]();

  // Get addr of initializable_object component (sp + offset).
  auto addr = $action[2]();

  auto *lhs = dynamic_cast<IR_SymbolExp *>( $2->getExp() );

  // This string already contains the terminating null.
  string stringValue = stringConstExp->getValue();

  auto &t = lhs->getType();

  // Check whether there is enough room in our array for the string.
  // length will be modified to contain the number of remaining bytes.
  unsigned int length = std::min( (int) stringValue.length(), t.sizeOf() );

  // Add the offset to the base register.
  auto *dest = ARMINSTRUCTIONS.CreateRegister( "" );
  ARMINSTRUCTIONS.insertADD( dest, addr.getARegister(), addr.getOffset(), exp );

  auto *temp = ARMINSTRUCTIONS.CreateRegister( "" );

  // How many registers do we use to copy the string? Capped at 4 registers,
  // because register pressure is not known.
  unsigned char registerCount = min( length / 4 , (unsigned int) 4 );
  unsigned char byteCount = registerCount * 4;

  std::deque<LLIR_Register *> registers = { temp };
  for( int i = 1; i < registerCount; ++i ) {
    auto *r = ARMINSTRUCTIONS.CreateRegister( "" );
    // Bind registers to force ascending order.
    bindToPHREG( *r, 4 + i );
    registers.push_back( r );
  }

  // Insert a loop for LDM/STM with four registers.
  if ( length >= 32 ) {
    unsigned int loopIterations = length / 16;
    length -= loopIterations * 16;

    auto *loopCounter = ARMINSTRUCTIONS.CreateRegister( "" );
    ARMINSTRUCTIONS.insertMOV_ORR( loopCounter, loopIterations, exp );

    string loopEntry = LLIR::getUniqueLabel();

    beginNewLLIRBasicBlock( loopEntry.c_str());

    auto *bb = ARMCODESEL->getLastLLIRBB();
    ARMCODESEL->addLoopboundToLLIR(
      bb, loopIterations, loopIterations, LLIR_Loopbound::TAIL_CONTROLLED );

    ARMINSTRUCTIONS.insertLDM(
      OPER_IA, OPER_AL, src, OPER_WRITEBACK, &registers, exp );
    ARMINSTRUCTIONS.insertSTM(
      OPER_IA, OPER_AL, dest, OPER_WRITEBACK, &registers, exp );

    ARMINSTRUCTIONS.insertSUB( loopCounter, loopCounter, 1, exp );
    ARMINSTRUCTIONS.insertCMP( OPER_AL, loopCounter, 0, exp );
    ARMINSTRUCTIONS.insertB( OPER_GT, loopEntry, exp );

    beginNewLLIRBasicBlock();
  }

  // Insert LDM/STM instructions without a loop (decreasing registerCount).
  for ( int i = 0; i < registerCount; ++i ) {
    if ( length >= byteCount ) {
      ARMINSTRUCTIONS.insertLDM(
        OPER_IA, OPER_AL, src, OPER_WRITEBACK, &registers, exp );
      ARMINSTRUCTIONS.insertSTM(
        OPER_IA, OPER_AL, dest, OPER_WRITEBACK, &registers, exp );
      length -= byteCount;
    }

    registers.pop_back();
    byteCount -= 4;
  }

  // Halfword copy?
  if ( length >= 2 ) {
    ARMINSTRUCTIONS.insertLDRH( OPER_IMMPOST, temp, src, 2, exp );
    ARMINSTRUCTIONS.insertSTRH( OPER_AL, OPER_IMMPOST, temp, dest, 2, exp );
  }

  // Odd number of bytes to copy?
  if ( length % 2 == 1 ) {
    ARMINSTRUCTIONS.insertLDRB( OPER_IMMPOST, temp, src, 1, exp );
    ARMINSTRUCTIONS.insertSTRB( OPER_AL, OPER_IMMPOST, temp, dest, 1, exp );
  }

  int unused = t.sizeOf() - stringValue.length();
  if ( unused > 0 )
    initWithZero( dest, unused, addr.getOffset() + stringValue.length(), exp );

  return( addr );
};


init_list: tpm_InitListExp( init_list )
{
  // This rule just consumes the 'tpm_InitListExp' terminal and starts the real
  // init list processing by calling its child rule.
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "init_list: tpm_InitListExp( init_list )", $1 );

  // Generate operand trees.
  $action[2]( reg, address_increment, useBaseOffsetAddressing );
};


init_list: tpm_InitListExpELEM( init_list_element, init_list )
{
  $cost[0] = $cost[2] + $cost[3];
}
=
{
  DEBUG_RULE_ACTION(
    "init_list: tpm_InitListExpELEM( init_list_element, init_list )", $1 );

  // Get current ELEM expression.
  auto *elemExp = $2->getExp();

  // Get initListExp to determine the offset of elemExp in the initList.
  auto *initList = dynamic_cast<IR_InitListExp *>( elemExp->getParent() );

  if ( useBaseOffsetAddressing ) {
    // Load the elements onto the stack
    $action[2](
      reg, address_increment + initList->getOffset( elemExp ).getIntValue(),
      useBaseOffsetAddressing );

    // and descend in init_list.
    $action[3]( reg, address_increment, useBaseOffsetAddressing );
  }
};


init_list: tpm_InitListExpNOELEM
{
  $cost[0] = 0;
}
=
{
  DEBUG_RULE_ACTION( "init_list: tpm_InitListExpNOELEM", $1 );
};


init_list_element: init_list
{
  // This rule handles nested initializer lists.
  $cost[0] = $cost[1];
}
=
{
  DEBUG_RULE_ACTION( "init_list_element: init_list", $1 );

  if ( useBaseOffsetAddressing )
    // Process nested init list.
    $action[1]( reg, address_increment, useBaseOffsetAddressing );
};


init_list_element: any_reg
{
  // Handles all integer types from '_Bool' to 'long' plus 'float' (all those
  // types that can be stored in a single data register), except for bitfield
  // types.
  auto *ilType = getTypeInInitList( *$1->getExp() );

  if ( ilType && !isBitfieldType( *ilType ) ) {
    switch ( ilType->getType() ) {
      case IR_Type::BOOL:
      case IR_Type::CHAR:
      case IR_Type::UNSIGNED_CHAR: {
        $cost[0] = $cost[1] + CT( INS_STRB_32 );
        break;
      }

      case IR_Type::SHORT:
      case IR_Type::UNSIGNED_SHORT: {
        $cost[0] = $cost[1] + CT( INS_STRH_32 );
        break;
      }

      case IR_Type::INT:
      case IR_Type::UNSIGNED_INT:
      case IR_Type::LONG:
      case IR_Type::UNSIGNED_LONG:
      case IR_Type::FLOAT:
      case IR_Type::POINTER: {
        $cost[0] = $cost[1] + CT( INS_STR_32 );
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
  DEBUG_RULE_ACTION( "init_list_element: any_reg", $1 );

  // Get type of value in reg.
  auto *ilType = getTypeInInitList( *$1->getExp() );

  // Get vreg which has the appropriate value moved to it.
  auto *data = $action[1]();

  // Distinguish between byte (8bit), halfword (16bit) and word (32 bit).
  switch ( ilType->getType() ) {
    case IR_Type::BOOL:
    case IR_Type::CHAR:
    case IR_Type::UNSIGNED_CHAR: {
      ARMINSTRUCTIONS.insertSTRB(
        OPER_IMMOFF, OPER_AL, data, reg, address_increment, $1->getExp() );
      break;
    }

    case IR_Type::SHORT:
    case IR_Type::UNSIGNED_SHORT: {
      ARMINSTRUCTIONS.insertSTRH(
        OPER_IMMOFF, OPER_AL, data, reg, address_increment, $1->getExp() );
      break;
    }

    case IR_Type::INT:
    case IR_Type::UNSIGNED_INT:
    case IR_Type::LONG:
    case IR_Type::UNSIGNED_LONG:
    case IR_Type::FLOAT:
    case IR_Type::POINTER: {
      ARMINSTRUCTIONS.insertSTR(
        OPER_IMMOFF, OPER_AL, data, reg, address_increment, $1->getExp() );
      break;
    }

    default:
      break;
  }
};


##############################################################################
#
#
# Array rules
#
#
###############################################################################

deref_dreg: tpm_IndexExp( areg, addrOffset )
{
  // Handles array access with constant indices.
  $cost[0] = $cost[2];

  auto *baseType = getBaseType( *$2->getExp() );
  switch ( baseType->getType() ) {
    case IR_Type::CHAR: {
      $cost[0] += CT( INS_LDRSB_32 );
      break;
    }

    case IR_Type::UNSIGNED_CHAR:
    case IR_Type::BOOL: {
      $cost[0] += CT( INS_LDRB_32 );
      break;
    }

    case IR_Type::SHORT: {
      $cost[0] += CT( INS_LDRSH_32 );
      break;
    }

    case IR_Type::UNSIGNED_SHORT: {
      $cost[0] += CT( INS_LDRH_32 );
      break;
    }

    case IR_Type::INT:
    case IR_Type::UNSIGNED_INT:
    case IR_Type::LONG:
    case IR_Type::UNSIGNED_LONG:
    case IR_Type::FLOAT: {
      $cost[0] += CT( INS_LDR_32 );
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
  DEBUG_RULE_ACTION( "deref_dreg: tpm_IndexExp( areg, addrOffset )", $1 );

  auto *baseType = getBaseType( *$2->getExp() );
  const int byteSize = computeSizeOf( baseType );
  int offset = $action[3]().getIntValue() * byteSize;

  // In case of a dry run, store the important info in a dry run instance and
  // return immediately, not inserting any instructions.
  if ( dryRun )
    return(
      ARM_LValue {
        nullptr,
        ARM_AddressModification {
          static_cast<LLIR_Register *>( nullptr ), offset, baseType,
          AddressModification::ModTime::NONE, AddressModification::ModOper::ADD,
          true, true },
        true } );

  auto *reg0 = ARMINSTRUCTIONS.CreateRegister( "", false );
  auto *iaddr = $action[2]();

  // Generate the operation.
  if ( loadResult ) {
    switch ( baseType->getType() ) {
      case IR_Type::CHAR: {
        ARMINSTRUCTIONS.insertLDRSB(
          OPER_IMMOFF, reg0, iaddr, offset, $1->getExp() );
        break;
      }

      case IR_Type::UNSIGNED_CHAR:
      case IR_Type::BOOL: {
        ARMINSTRUCTIONS.insertLDRB(
          OPER_IMMOFF, reg0, iaddr, offset, $1->getExp() );
        break;
      }

      case IR_Type::SHORT: {
        ARMINSTRUCTIONS.insertLDRSH(
          OPER_IMMOFF, reg0, iaddr, offset, $1->getExp() );
        break;
      }

      case IR_Type::UNSIGNED_SHORT: {
        ARMINSTRUCTIONS.insertLDRH(
          OPER_IMMOFF, reg0, iaddr, offset, $1->getExp() );
        break;
      }

      case IR_Type::INT:
      case IR_Type::UNSIGNED_INT:
      case IR_Type::LONG:
      case IR_Type::UNSIGNED_LONG:
      case IR_Type::FLOAT: {
        ARMINSTRUCTIONS.insertLDR(
          OPER_IMMOFF, reg0, iaddr, offset, $1->getExp() );
        break;
      }

      default:
        break;
    }
  }

  // Generate result.
  return(
    ARM_LValue {
      loadResult ? reg0 : nullptr,
      ARM_AddressModification {
        iaddr, offset, baseType, AddressModification::ModTime::NONE,
        AddressModification::ModOper::ADD, true } } );
};


deref_ereg: tpm_IndexExp( areg, addrOffset )
{
  // Handles array access with constant indices.
  $cost[0] = $cost[2];

  auto *baseType = getBaseType( *$2->getExp() );
  switch ( baseType->getType() ) {
    case IR_Type::DOUBLE:
    case IR_Type::LONG_DOUBLE:
    case IR_Type::UNSIGNED_LONG_LONG:
    case IR_Type::LONG_LONG: {
      $cost[0] += 2 * CT( INS_LDR_32 );
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
  DEBUG_RULE_ACTION( "deref_ereg: tpm_IndexExp( areg, addrOffset )", $1 );

  auto *baseType = getBaseType( *$2->getExp() );
  const int byteSize = computeSizeOf( baseType );
  int offset = $action[3]().getIntValue() * byteSize;

  // In case of a dry run, store the important info in a dry run instance and
  // return immediately, not inserting any instructions.
  if ( dryRun )
    return(
      ARM_LValue {
        nullptr,
        ARM_AddressModification {
          static_cast<LLIR_Register *>( nullptr ), offset, baseType,
          AddressModification::ModTime::NONE, AddressModification::ModOper::ADD,
          true, true },
        true } );

  auto *reg0 = ARMINSTRUCTIONS.CreateERegister( "" );
  auto *iaddr  = $action[2]();

  ARM_AddressModification amod {
    iaddr, offset, baseType, AddressModification::ModTime::NONE,
    AddressModification::ModOper::ADD, true };

  // Generate the operation.
  if ( loadResult )
    switch ( baseType->getType() ) {
      case IR_Type::DOUBLE:
      case IR_Type::LONG_DOUBLE:
      case IR_Type::UNSIGNED_LONG_LONG:
      case IR_Type::LONG_LONG: {
        amod.createLoad( reg0, $1->getExp() );
        break;
      }

      default:
        break;
    }

  // Generate result.
  return( ARM_LValue { loadResult ? reg0 : nullptr, amod } );
};


deref_areg: tpm_IndexExp( areg, addrOffset )
{
  // Handles array access with constant indices.
  $cost[0] = $cost[2];

  auto *baseType = getBaseType( *$2->getExp() );
  switch ( baseType->getType() ) {
    case IR_Type::POINTER: {
      $cost[0] += CT( INS_LDR_32 );
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
  DEBUG_RULE_ACTION( "deref_areg: tpm_IndexExp( areg, addrOffset )", $1 );

  auto *baseType = getBaseType( *$2->getExp() );
  const int byteSize = computeSizeOf( baseType );
  int offset = $action[3]().getIntValue() * byteSize;

  // In case of a dry run, store the important info in a dry run instance and
  // return immediately, not inserting any instructions.
  if ( dryRun )
    return(
      ARM_LValue {
        nullptr,
        ARM_AddressModification {
          static_cast<LLIR_Register *>( nullptr ), offset, baseType,
          AddressModification::ModTime::NONE, AddressModification::ModOper::ADD,
          true, true },
        true } );

  auto *reg0 = ARMINSTRUCTIONS.CreateRegister( "", false );
  auto *iaddr = $action[2]();

  // Generate the operation.
  if ( loadResult ) {
    switch ( baseType->getType() ) {
      case IR_Type::POINTER: {
        ARMINSTRUCTIONS.insertLDR(
          OPER_IMMOFF, reg0, iaddr, offset, $1->getExp() );
        break;
      }

      default:
        break;
    }
  }

  return(
    ARM_LValue {
      loadResult ? reg0 : nullptr,
      ARM_AddressModification {
        iaddr, offset, baseType, AddressModification::ModTime::NONE,
        AddressModification::ModOper::ADD, true } } );
};


areg: tpm_IndexExp( areg, addrOffset )
{
  // Handles array access with constant indices.
  // Chain rule for multi dimensional array access.
  auto *baseType = getBaseType( *$2->getExp() );

  if ( isArrayType( *baseType ) )
    $cost[0] = $cost[2];
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "areg: tpm_IndexExp( areg, addrOffset )", $1 );

  // Compute offset.
  const int byteSize = computeSizeOf( getBaseType( *$2->getExp() ) );
  const int offset = $action[3]().getIntValue() * byteSize;

  // Get symbol address register.
  auto *iaddr = $action[2]();

  // Add offset to address if necessary.
  if ( offset ) {
    // Check if the offset is too large.
    if ( isValidARMImmediate( offset ) ) {
      ARMINSTRUCTIONS.insertADD( iaddr, iaddr, offset, $2->getExp() );
    } else {
      LLIR_Register* r = ARMINSTRUCTIONS.CreateRegister( "" );
      ARMINSTRUCTIONS.insertMOV_ORR( r, offset, $2->getExp() );
      ARMINSTRUCTIONS.insertADD( iaddr, iaddr, r, $2->getExp() );
    }
  }

  return( iaddr );
};


deref_dreg: tpm_IndexExp( areg, dreg )
{
  // Handles array access with variable indices.
  auto *baseType = getBaseType( *$2->getExp() );

  $cost[0] = $cost[2] + $cost[3] + loadRegisterRelativeAddressCost();

  switch ( baseType->getType() ) {
    case IR_Type::CHAR: {
      $cost[0] += CT( INS_LDRSB_32 );
      break;
    }

    case IR_Type::UNSIGNED_CHAR:
    case IR_Type::BOOL: {
      $cost[0] += CT( INS_LDRB_32 );
      break;
    }

    case IR_Type::SHORT: {
      $cost[0] += CT( INS_LDRSH_32 );
      break;
    }

    case IR_Type::UNSIGNED_SHORT: {
      $cost[0] += CT( INS_LDRH_32 );
      break;
    }

    case IR_Type::INT:
    case IR_Type::UNSIGNED_INT:
    case IR_Type::LONG:
    case IR_Type::UNSIGNED_LONG:
    case IR_Type::FLOAT: {
      $cost[0] += CT( INS_LDR_32 );
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
  DEBUG_RULE_ACTION( "deref_dreg: tpm_IndexExp( areg, dreg )", $1 );

  auto *baseType = getBaseType( *$2->getExp() );
  const int byteSize = computeSizeOf( baseType );

  // In case of a dry run, store the important info in a dry run instance and
  // return immediately, not inserting any instructions.
  if ( dryRun )
    return(
      ARM_LValue {
        nullptr,
        ARM_AddressModification {
          static_cast<LLIR_Register *>( nullptr ), static_cast<long>( 0 ),
          baseType, AddressModification::ModTime::NONE,
          AddressModification::ModOper::ADD, true, true },
        true } );

  // Generate target register holding the result of the current operation.
  auto *reg0 = ARMINSTRUCTIONS.CreateRegister( "", false );

  // Generate operand trees.
  auto *baseReg( $action[2]() );
  auto *offsetReg( $action[3]() );

  // Compute address pointer.
  auto *iaddr =
    loadRegisterRelativeAddress( baseReg, offsetReg, byteSize, $1->getExp() );

  // Generate the operation.
  if ( loadResult ) {
    switch ( baseType->getType() ) {
      // Generate the operation, depending on the type.
      case IR_Type::CHAR: {
          ARMINSTRUCTIONS.insertLDRSB(
            OPER_IMMOFF, reg0, iaddr, 0, $1->getExp() );
        break;
      }

      case IR_Type::UNSIGNED_CHAR:
      case IR_Type::BOOL: {
        ARMINSTRUCTIONS.insertLDRB( OPER_IMMOFF, reg0, iaddr, 0, $1->getExp() );
        break;
      }

      case IR_Type::SHORT: {
        ARMINSTRUCTIONS.insertLDRSH( OPER_IMMOFF, reg0, iaddr, 0, $1->getExp() );
        break;
      }

      case IR_Type::UNSIGNED_SHORT: {
        ARMINSTRUCTIONS.insertLDRH( OPER_IMMOFF, reg0, iaddr, 0, $1->getExp() );
        break;
      }

      case IR_Type::INT:
      case IR_Type::UNSIGNED_INT:
      case IR_Type::LONG:
      case IR_Type::UNSIGNED_LONG:
      case IR_Type::FLOAT: {
        ARMINSTRUCTIONS.insertLDR( OPER_IMMOFF, reg0, iaddr, 0, $1->getExp() );
        break;
      }

      default:
        break;
    }
  }

  return(
    ARM_LValue {
      loadResult ? reg0 : nullptr,
      ARM_AddressModification {
        iaddr, static_cast<long>( 0 ), baseType,
        AddressModification::ModTime::NONE, AddressModification::ModOper::ADD,
        true } } );
};


deref_ereg: tpm_IndexExp( areg, dreg )
{
  // Handles array access with variable indices.
  auto *baseType = getBaseType( *$2->getExp() );

  $cost[0] = $cost[2] + $cost[3] + loadRegisterRelativeAddressCost();

  switch ( baseType->getType() ) {
    case IR_Type::DOUBLE:
    case IR_Type::LONG_DOUBLE:
    case IR_Type::UNSIGNED_LONG_LONG:
    case IR_Type::LONG_LONG: {
      $cost[0] += 2 * CT( INS_LDR_32 );
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
  DEBUG_RULE_ACTION( "deref_ereg: tpm_IndexExp( areg, dreg )", $1 );

  auto *baseType = getBaseType( *$2->getExp() );
  const int byteSize = computeSizeOf( baseType );

  // In case of a dry run, store the important info in a dry run instance and
  // return immediately, not inserting any instructions.
  if ( dryRun )
    return(
      ARM_LValue {
        nullptr,
        ARM_AddressModification {
          static_cast<LLIR_Register *>( nullptr ), static_cast<long>( 0 ),
          baseType, AddressModification::ModTime::NONE,
          AddressModification::ModOper::ADD, true, true },
        true } );

  // Generate target register holding the result of the current operation.
  auto *reg0 = ARMINSTRUCTIONS.CreateERegister( "" );

  // Generate operand trees.
  auto *baseReg( $action[2]() );
  auto *offsetReg( $action[3]() );

  // Compute address pointer.
  auto *iaddr =
    loadRegisterRelativeAddress( baseReg, offsetReg, byteSize, $1->getExp() );

  ARM_AddressModification amod {
    iaddr, static_cast<long>( 0 ), baseType, AddressModification::ModTime::NONE,
    AddressModification::ModOper::ADD, true };

  // Generate the operation.
  if ( loadResult )
    amod.createLoad( reg0, $1->getExp() );

  return(
    ARM_LValue {
      loadResult ? reg0 : nullptr,
      ARM_AddressModification {
        iaddr, static_cast<long>( 0 ), baseType,
        AddressModification::ModTime::NONE, AddressModification::ModOper::ADD,
        true } } );
};


deref_areg: tpm_IndexExp( areg, dreg )
{
  // Handles array access with variable indices.
  auto *baseType = getBaseType( *$2->getExp() );

  $cost[0] = $cost[2] + $cost[3] + loadRegisterRelativeAddressCost();

  switch ( baseType->getType() ) {
    case IR_Type::POINTER: {
      $cost[0] += CT( INS_LDR_32 );
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
  DEBUG_RULE_ACTION( "deref_areg: tpm_IndexExp( areg, dreg )", $1 );

  auto *baseType = getBaseType( *$2->getExp() );
  const int byteSize = computeSizeOf( baseType );

  // In case of a dry run, store the important info in a dry-run instance and
  // return immediately, not inserting any instructions.
  if ( dryRun )
    return(
      ARM_LValue {
        nullptr,
        ARM_AddressModification {
          static_cast<LLIR_Register *>( nullptr ), static_cast<long>( 0 ),
          baseType, AddressModification::ModTime::NONE,
          AddressModification::ModOper::ADD, true, true },
        true } );

  // Generate target register holding the result of the current operation.
  auto *reg0 = ARMINSTRUCTIONS.CreateRegister( "", false );

  // Generate operand trees.
  auto *baseReg( $action[2]() );
  auto *offsetReg( $action[3]() );

  // Compute address pointer.
  auto *iaddr =
    loadRegisterRelativeAddress( baseReg, offsetReg, byteSize, $1->getExp() );

  // Generate the operation.
  if ( loadResult ) {
    switch ( baseType->getType() ) {
      // Generate the operation, depending on the type.
      case IR_Type::POINTER: {
        ARMINSTRUCTIONS.insertLDR( OPER_IMMOFF, reg0, iaddr, 0, $1->getExp() );
        break;
      }

      default:
        break;
    }
  }

  // Generate result.
  return(
    ARM_LValue {
      loadResult ? reg0 : nullptr,
      ARM_AddressModification {
        iaddr, static_cast<long>( 0 ), baseType,
        AddressModification::ModTime::NONE, AddressModification::ModOper::ADD,
        true } } );
};


areg: tpm_IndexExp( areg, dreg )
{
  // Handles array access with variable indices.
  // Chain rule for multi dimensional array access.
  auto *baseType = getBaseType( *$2->getExp() );

  if ( isArrayType( *baseType ) )
    $cost[0] = $cost[2] + $cost[3] + loadRegisterRelativeAddressCost();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "areg: tpm_IndexExp( areg, dreg )", $1 );

  auto *baseType = getBaseType( *$2->getExp() );
  const int byteSize = computeSizeOf( baseType );

  // Generate operand trees.
  auto *op1( $action[2]() );
  auto *op2( $action[3]() );

  // Compute address pointer.
  return( loadRegisterRelativeAddress( op1, op2, byteSize, $1->getExp() ) );
};


areg: tpm_SymbolExp
{
  // Handles "real" arrays (not function arguments). Those arrays do not need to
  // be loaded with LD_X, only their address must be loaded into a register.
  auto &symExp = dynamic_cast<IR_SymbolExp &>( *$1->getExp() );
  auto &sym = symExp.getSymbol();

  // TODO: check this if statement.
  if ( isArrayType( sym ) /*&& !isFunctionArgument( symExp )*/ ) {
    $cost[0] = 0;

    if ( sym.isGlobal() )
      $cost[0] += loadGlobalSymbolCost( symExp );
    else

    if ( ARMCODESEL->getStack()->getSymbolOffset( &sym ) >= 0 ) {
      $cost[0] += loadStackSymbolCost( symExp );
      // Load a dry-run version of the lvalue to properly calculate the cost
      // of transforming the address.
      auto dryRunDI = loadStackSymbolDryRun( symExp );
      $cost[0] += dryRunDI.calculateAddressCost();
    } else
      $cost[0] += loadRegisterSymbolCost( symExp );
  } else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "areg: tpm_SymbolExp", $1 );

  auto &symExp = dynamic_cast<IR_SymbolExp &>( *$1->getExp() );
  auto &sym = symExp.getSymbol();

  // LLIR
  LLIR_Register *reg = nullptr;

  if ( sym.isGlobal() )
    reg = loadGlobalSymbol( symExp, false ).calculateAddress( &symExp );
  else

  // TODO: Evaluate the consequences of the following part.
  if ( ARMCODESEL->getStack()->getSymbolOffset( &sym ) >= 0 )
    reg = loadStackSymbol( symExp, false ).calculateAddress( &symExp );
  else
    reg = loadRegisterSymbol( symExp );

  return( reg );
};


dreg: deref_dreg
{
  // This chain rule enables us to let all deref/array-index/component-access
  // rules produce only deref_dreg. If a parent rule requires a reg nonterminal
  // instead, then this chain rule will extract it from the deref_dreg, which is
  // just a reg + memory access information.

  // We may only allow this conversion if the expression is not used in a
  // context that requires a deref_dreg, because the value must be written back
  // to memory.
  if ( !isMemoryWriteLocation( *$1->getExp() ) )
    $cost[0] = $cost[1];
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: deref_dreg", $1 );

  return( $action[1]( true, false ).getResultRegister() );
};


ereg: deref_ereg
{
  // This chain rule enables us to let all deref/array-index/component-access
  // rules produce only deref_ereg. If a parent rule requires a reg nonterminal
  // instead, then this chain rule will extract it from the deref_ereg, which is
  // just a reg + memory access information.

  // We may only allow this conversion if the expression is not used in a
  // context that requires a deref_dreg, because the value must be written back
  // to memory.
  if ( !isMemoryWriteLocation( *$1->getExp() ) )
    $cost[0] = $cost[1];
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "ereg: deref_ereg", $1 );

  return( $action[1]( true, false ).getResultRegister() );
};


###############################################################################
#
#
# struct rules (without casting, just nonterminal conversion)
#
#
###############################################################################

stmt: tpm_ExpStmt( composed_type_object )
{
  $cost[0] = $cost[2];
}
=
{
  DEBUG_RULE_ACTION( "stmt: tpm_ExpStmt( composed_type_object )", $1 );

  // Generate operand trees.
  $action[2]( false );
};


deref_dreg: tpm_ComponentAccessExp( composed_type_object, component_offset )
{
  if ( !isArrayType( *$3->getExp() ) && !isPointerType( *$3->getExp() ) )
    // This is the worst case cost, if the value is requested and not just the
    // address.
    $cost[0] = $cost[2] + $cost[3] + CT( INS_LDR_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION(
    "deref_dreg: tpm_ComponentAccessExp( composed_type_object, "
    "component_offset )", $1 );

  // Get access location.
  auto compAddr = $action[2]( dryRun );
  long fieldOffset = $action[3]();

  // Compute the final offset.
  auto byteOffset = compAddr.getOffset() + fieldOffset;

  // Do not insert any instructions on a dry run, only return the lvalue with
  // the relevant information.
  if ( dryRun )
    return(
      ARM_LValue {
        nullptr,
        ARM_AddressModification {
          static_cast<LLIR_Register *>( nullptr ), byteOffset,
          &effectiveType( *$0->getExp() ), AddressModification::ModTime::NONE,
          AddressModification::ModOper::ADD, true, true },
        true } );

  // Generate parameters for the access.
  auto *reg0 = ARMINSTRUCTIONS.CreateRegister( "" );
  auto *reg_base = compAddr.getARegister();

  // Now finally do the access if requested.
  if ( loadResult ) {
    IR_Exp &compExp = *$3->getExp();

    // Use different load instructions depending on the type of the variable.
    switch ( compExp.getType().getType() ) {
      case IR_Type::CHAR:
      case IR_Type::UNSIGNED_CHAR:
      case IR_Type::BOOL: {
        ARMINSTRUCTIONS.insertLDRB(
          OPER_IMMOFF, reg0, reg_base, byteOffset, &compExp );
        break;
      }

      case IR_Type::SHORT:
      case IR_Type::UNSIGNED_SHORT: {
        ARMINSTRUCTIONS.insertLDRH(
          OPER_IMMOFF, reg0, reg_base, byteOffset, &compExp );
        break;
      }

      case IR_Type::INT:
      case IR_Type::UNSIGNED_INT:
      case IR_Type::LONG:
      case IR_Type::UNSIGNED_LONG:
      case IR_Type::POINTER:
      case IR_Type::FLOAT: {
        ARMINSTRUCTIONS.insertLDR(
          OPER_IMMOFF, reg0, reg_base, byteOffset, &compExp );
        break;
      }

      case IR_Type::LONG_LONG:
      case IR_Type::UNSIGNED_LONG_LONG:
      case IR_Type::DOUBLE:
      case IR_Type::LONG_DOUBLE: {
        ufAssertT( 0, "Uncovered case!" );
        break;
      }

      case IR_Type::ARRAY: {
        ARMINSTRUCTIONS.insertLDR(
          OPER_IMMOFF, reg0, reg_base, byteOffset, &compExp );
        break;
      }

      default:
        break;
    }
  }

  // Generate result.
  return(
    ARM_LValue {
      loadResult ? reg0 : nullptr,
      ARM_AddressModification {
        reg_base, byteOffset, &effectiveType( *$0->getExp() ),
        AddressModification::ModTime::NONE, AddressModification::ModOper::ADD,
        true } } );
};


deref_areg: tpm_ComponentAccessExp( composed_type_object, component_offset )
{
  if ( !isArrayType( *$3->getExp() ) && isPointerType( *$3->getExp() ) )
    // This is the worst case cost, if the value is requested and not just the
    // address.
    $cost[0] = $cost[2] + $cost[3] + CT( INS_LDR_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION(
    "deref_areg: tpm_ComponentAccessExp( composed_type_object, "
    "component_offset )", $1 );

  // Get access location.
  auto compAddr = $action[2]( dryRun );
  long fieldOffset = $action[3]();

  // Compute the final offset.
  auto byteOffset = compAddr.getOffset() + fieldOffset;

  // Do not insert any instructions on a dry run, only return the lvalue with
  // the relevant information.
  if ( dryRun )
    return(
      ARM_LValue {
        nullptr,
        ARM_AddressModification {
          static_cast<LLIR_Register *>( nullptr ), byteOffset,
          &effectiveType( *$0->getExp() ), AddressModification::ModTime::NONE,
          AddressModification::ModOper::ADD, true, true },
        true } );

  // Generate parameters for the access.
  auto *reg0 = ARMINSTRUCTIONS.CreateRegister( "" );
  auto *reg_base = compAddr.getARegister();

  // Now finally do the access if requested.
  if ( loadResult ) {
    IR_Exp &compExp = *$3->getExp();

    // Use different load instructions depending on the type of the variable.
    switch ( compExp.getType().getType() ) {
      case IR_Type::CHAR:
      case IR_Type::UNSIGNED_CHAR:
      case IR_Type::BOOL: {
        ARMINSTRUCTIONS.insertLDRB(
          OPER_IMMOFF, reg0, reg_base, byteOffset, &compExp );
        break;
      }

      case IR_Type::SHORT:
      case IR_Type::UNSIGNED_SHORT: {
        ARMINSTRUCTIONS.insertLDRH(
          OPER_IMMOFF, reg0, reg_base, byteOffset, &compExp );
        break;
      }

      case IR_Type::INT:
      case IR_Type::UNSIGNED_INT:
      case IR_Type::LONG:
      case IR_Type::UNSIGNED_LONG:
      case IR_Type::POINTER:
      case IR_Type::FLOAT: {
        ARMINSTRUCTIONS.insertLDR(
          OPER_IMMOFF, reg0, reg_base, byteOffset, &compExp );
        break;
      }

      case IR_Type::LONG_LONG:
      case IR_Type::UNSIGNED_LONG_LONG:
      case IR_Type::DOUBLE:
      case IR_Type::LONG_DOUBLE: {
        ufAssertT( 0, "Uncovered case!" );
        break;
      }

      case IR_Type::ARRAY: {
        ARMINSTRUCTIONS.insertLDR(
          OPER_IMMOFF, reg0, reg_base, byteOffset, &compExp );
        break;
      }

      default:
        break;
    }
  }

  // Generate result.
  return(
    ARM_LValue {
      loadResult ? reg0 : nullptr,
      ARM_AddressModification {
        reg_base, byteOffset, &effectiveType( *$0->getExp() ),
        AddressModification::ModTime::NONE, AddressModification::ModOper::ADD,
        true } } );
};


composed_type_object: tpm_ComponentAccessExp( composed_type_object,
                                              component_offset )
{
  // Handles structs inside structs.
  if ( isComposedType( *$3->getExp() ) )
    $cost[0] = $cost[2] + $cost[3];
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION(
    "composed_type_object: tpm_ComponentAccessExp( composed_type_object, "
    "component_offset )", $1 );

  auto compAddr = $action[2]( dryRun );
  long fieldOffset = $action[3]();

  return(
    AddressWithOffset {
      compAddr.getARegister(), compAddr.getOffset() + fieldOffset } );
};


areg: tpm_ComponentAccessExp( composed_type_object, component_offset )
{
  // This rule generates code for accesses to arrays in structs. Arrays are a
  // special case in that sense that no load instruction is generated, but only
  // the array address is computed and returned. In addition, their symbol
  // itself is not modifiable (fixed base address) so that we only generate a
  // 'reg' here, and no 'deref_dreg'.

  if ( isArrayType( *$3->getExp() ) )
    $cost[0] = $cost[2] + $cost[3] + CT( INS_ADD_32 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION(
    "areg: tpm_ComponentAccessExp( composed_type_object, component_offset )",
    $1 );

  // Get access location.
  auto compAddr = $action[2]( false );
  long fieldOffset = $action[3]();
  auto byteOffset = compAddr.getOffset() + fieldOffset;

  if ( byteOffset != 0 ) {
    auto *reg = ARMINSTRUCTIONS.CreateRegister( "", true );
    ARMINSTRUCTIONS.insertADD( reg, compAddr.getARegister(), byteOffset );
    return( reg );
  } else
    return( compAddr.getARegister() );
};


composed_type_object: tpm_AssignExpASSIGN( composed_type_object,
                                           composed_type_object )
{
  // This rule handles copy operations through assignments of structs. It does
  // not have the same properties as the assignments in "assignment.m4" (no
  // casting of operands), therefore it must stay here.
  auto *ctype = dynamic_cast<IR_ComposedType *>( &$2->getExp()->getType() );

  $cost[0] = $cost[2] + $cost[3] + copyComposedTypeCost( *ctype );
}
=
{
  DEBUG_RULE_ACTION(
    "composed_type_object: tpm_AssignExpASSIGN( composed_type_object, "
    "composed_type_object )", $1 );

  // In case of a dry-run, only evaluate the offset of the lhs.
  if ( dryRun )
    return( $action[2]( true ) );

  // Get operand addresses.
  auto lhs = $action[2]( false );
  auto rhs = $action[3]( false );

  // Copy the composed type.
  auto *ctype = dynamic_cast<IR_ComposedType *>( &$2->getExp()->getType() );

  copyComposedType(
    *ctype, rhs.getARegister(), rhs.getOffset(), lhs.getARegister(),
    lhs.getOffset(), *ARMCODESEL->getLastLLIRBB(),
    ARMCODESEL->getCurrentInstruction() );

  return( lhs );
};


composed_type_object: tpm_IndexExp( areg, addrOffset )
{
  // This rule handles access to structs within arrays with constant offset.
  if ( isComposedArray( *$2->getExp() ) || isComposedPointer( *$2->getExp() ) )
    $cost[0] = $cost[2] + $cost[3];
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION(
    "composed_type_object: tpm_IndexExp( areg, addrOffset )", $1 );

  const int byteSize = computeSizeOf( getBaseType( *$2->getExp() ) );
  const int offset = $action[3]().getIntValue() * byteSize;

  // In case of a dry run, only the offset is needed.
  if ( dryRun )
    return( AddressWithOffset { nullptr, offset } );

  return( AddressWithOffset { $action[2](), offset } );
};


composed_type_object: tpm_IndexExp( areg, dreg )
{
  // This rule handles access to structs within arrays with variable offset.
  if ( isComposedArray( *$2->getExp() ) || isComposedPointer( *$2->getExp() ) )
    $cost[0] = $cost[2] + $cost[3] + loadRegisterRelativeAddressCost();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "composed_type_object: tpm_IndexExp( areg, dreg )", $1 );

  // In case of a dry run, return only the offset, which is always 0.
  if ( dryRun )
    return( AddressWithOffset { nullptr, 0 } );

  auto byteSize = computeSizeOf( getBaseType( *$2->getExp() ) );

  auto *baseReg = $action[2]();
  auto *offsetReg = $action[3]();

  // LLIR
  auto *reg =
    loadRegisterRelativeAddress( baseReg, offsetReg, byteSize, $1->getExp() );

  return( AddressWithOffset { reg, 0 } );
};


composed_type_object: tpm_CallExp( called_function, arg )
{
  if ( isComposedType( *$0->getExp() ) )
    $cost[0] = $cost[2] + $cost[3];
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION(
    "composed_type_object: tpm_CallExp( called_function, arg )", $1 );

  // In case of a dry run, return only the offset, which is always 0.
  if ( dryRun )
    return( AddressWithOffset { nullptr, 0 } );

  // Do parameter passing.
  auto *theCall = dynamic_cast<IR_CallExp *>( $1->getExp() );

  // Create register which is used to pass the absolute address of the call
  // result buffer to the callee.
  auto *reg0 = ARMINSTRUCTIONS.CreateRegister( "", true );

  // Pass it in R0.
  bindToPHREG( *reg0, 0 );

  auto *currentFunction = &$1->getExp()->getStmt().getFunction();
  auto offset =
    ARMCODESEL->getStack()->getCallResultBufferOffset( currentFunction );

  auto *sp = ARMINSTRUCTIONS.CreateRegister( PHREG_SP, true );
  ARMINSTRUCTIONS.insertADD( reg0, sp, offset );

  regptr_list rhsRegs;
  regptr_list lhsRegs;

  // Put arguments in register or onto the stack.
  $action[3]( true, 0, 0, theCall, &lhsRegs, &rhsRegs );
  lhsRegs.clear();
  $action[3]( false, 0, 0, theCall, &lhsRegs, &rhsRegs );

  lhsRegs.push_front( reg0 );

  // Generate the call.
  return( AddressWithOffset { $action[2]( lhsRegs, DATA_REGISTER ), 0 } );
};


arg: tpm_CallExpARG( composed_type_object, arg )
{
  // This rule handles composed type function parameters.
  auto *cexp = dynamic_cast<IR_CallExp *>( $2->getExp()->getParent() );

  if ( cexp->getFunctionType().isPrototypeForm() )
    $cost[0] = $cost[2] + $cost[3] + CT( INS_LDR_32 ) + CT( INS_MOV_32 );
  else {
    $cost[0] = COST_INFINITY;
    throw ufFatalError( "Functions without prototype are not supported." );
  }
}
=
{
  DEBUG_RULE_ACTION( "arg: tpm_CallExpARG( composed_type_object, arg )", $1 );

  int incr = 0;

  if ( dryrun ) {
    AddressWithOffset rhs { $action[2]( false ) };
    LLIR_Register *reg_rhs;

    // Load an address register with the base address of the struct in memory.
    reg_rhs =
      &loadAccessLocationToReg(
        rhs.getARegister(), rhs.getOffset(), "", $2->getExp() );

    rhsRegs->push_back( reg_rhs );
  } else {
    auto *rhsreg = rhsRegs->front();
    rhsRegs->pop_front();

    int myRegister =
      Stack::isPassedThroughRegister( theCall->getFunctionType(), index );

    // RHS is an address register with the base address of the struct in memory
    // Are there still free address registers?
    if ( myRegister != -1 ) {
      // Pass argument via register. Precolors lhsreg with myRegister.
      auto *lhsreg =
        &getFunctionArgumentRegister( *theCall, index, myRegister );
      ARMINSTRUCTIONS.insertMOV( lhsreg, rhsreg, $1->getExp() );

      if ( ( lhsRegs != nullptr ) && ( lhsreg != nullptr ) )
        lhsRegs->push_back( lhsreg );
    } else {
      // Pass argument via the stack.
      auto *sp = ARMINSTRUCTIONS.CreateRegister( PHREG_SP );

      // TODO: We could save stack space here if we'd not use 4 bytes for each
      //       char/short too, but this would also require adapting the symbol
      //       load rules.
      incr = intBytes;

      // Generate the operation.
      ARMINSTRUCTIONS.insertSTR(
        OPER_IMMOFF, OPER_AL, rhsreg, sp, offset, $1->getExp() );
      ARMCODESEL->getLastLLIRBB()->GetLastIns()->AddPragma(
        new LLIR_Pragma( "Passing overflow function parameter", true ) );
    }
  }

  $action[3]( dryrun, index + 1, offset + incr, theCall, lhsRegs, rhsRegs );
};


stmt: tpm_ReturnStmt( composed_type_object )
{
  // composed_type_object cost.
  $cost[0] = $cost[2];

  // callresult buffer address calculation cost.
  $cost[0] += CT( INS_ADD_32 );

  // copyComposedType to callresult buffer cost.
  $cost[0] +=
    copyComposedTypeCost(
      dynamic_cast<IR_ComposedType &>( $2->getExp()->getType() ) );

  // stack reduction cost.
  $cost[0] += CT( INS_MOV_32 ) + CT( INS_SUB_32 ) + CT( INS_LDM_32 );
}
=
{
  DEBUG_RULE_ACTION( "stmt: tpm_ReturnStmt( composed_type_object )", $1 );

  // Generate operand trees.
  auto source = $action[2]( false );

  auto *fp = ARMINSTRUCTIONS.CreateRegister( PHREG_FP );
  auto *sp = ARMINSTRUCTIONS.CreateRegister( PHREG_SP );
  auto *pc = ARMINSTRUCTIONS.CreateRegister( PHREG_PC );

  auto *reg_target = ARMINSTRUCTIONS.CreateRegister( "", true );
  auto *reg0 = ARMINSTRUCTIONS.CreateRegister( "", true );
  bindToPHREG( *reg0, 0 );

  // Backup address of result struct from R0 at the function start.
  auto *insMov = insMOV( OPER_AL, "", reg_target, reg0 );
  ARMCODESEL->getLastLLIRFunction()->GetFirstBB()->InsertIns( insMov );

  // Insert copy code (in the current block).
  copyComposedType(
    dynamic_cast<IR_ComposedType &>( $2->getExp()->getType() ),
    source.getARegister(), source.getOffset(), reg_target, 0,
    *ARMCODESEL->getLastLLIRBB(), ARMCODESEL->getCurrentInstruction() );

  auto *final = ARMINSTRUCTIONS.CreateRegister( "" );

  // Save all registers in a queue.
  deque<LLIR_Register *> registers;
  registers.push_back( fp );
  registers.push_back( sp );
  registers.push_back( pc );

  // Return result in register R0.
  bindToPHREG( *final, 0 );

  // Copy result (pointer to result buffer) to R0.
  ARMINSTRUCTIONS.insertMOV( final, reg_target, $1->getExp() );

  // Set new position of SP.
  ARMINSTRUCTIONS.insertSUB( sp, fp, 12, $1->getExp() );
  ARMINSTRUCTIONS.insertLDM(
    OPER_FD, OPER_AL, sp, OPER_NOWRITEBACK, &registers, $1->getExp(),
    InstructionFactory::RETURN_STMT );
};


composed_type_object: tpm_SymbolExp
{
  // This rule handles address generation for local composed symbols.
  auto *exp = dynamic_cast<IR_SymbolExp *>( $1->getExp() );
  auto *baseSymbol = &exp->getSymbol();

  if ( isComposedType( *exp ) && !exp->getSymbol().isGlobal() ) {
    auto &symmap = *ARMCODESEL->getStack()->getSymbolMap();
    auto symbolIt = symmap.find( baseSymbol );
    auto symbolType =
      ( symbolIt != symmap.end() ) ?
        ( *symbolIt ).second.getSymbolType() : SymbolInfo::LOCAL_VAR;

    // TRICORE
    // Structs may be passed in an areg (base pointer), dreg (if they are
    // smaller than 64 bits) or via the stack (if the registers are full),
    // or it may be just a local (stack) variable
    // ARM7
    // TODO: simplify this, structs are always passed via pointer.
    if ( ( symbolIt == symmap.end() ) ||
         ( symbolType == SymbolInfo::LOCAL_STACK_VAR ) ||
         ( symbolType == SymbolInfo::D_ARGUMENT ) ||
         ( symbolType == SymbolInfo::A_ARGUMENT ) ||
         ( symbolType == SymbolInfo::LOCAL_COMPOSED_ARGUMENT ) ) {
      $cost[0] = 0;
      if ( !ARMCODESEL->getStack()->getComposedPushCostAdded( baseSymbol ) )
        $cost[0] += copyComposedOnStackCost( *baseSymbol );
    } else
     $cost[0] = COST_INFINITY;
  } else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "composed_type_object: tpm_SymbolExp", $1 );

  auto *composedSym = dynamic_cast<IR_SymbolExp *>( $1->getExp() );
  auto *baseSymbol = &composedSym->getSymbol();

  // Find out whether the struct is a function argument or a local struct. In
  // the first case, get the stack offset to the buffer saving space for a copy
  // of the struct. In the second case, get the stack offset to the local
  // struct.
  long offset =
    ( baseSymbol->getSymbolTable().getFunction() ) ?
      ARMCODESEL->getStack()->getComposedParameterBufferOffset( baseSymbol ) :
      ARMCODESEL->getStack()->getSymbolOffset( baseSymbol );

  // In case of a dry run, only return the offset.
  if ( dryRun )
    return( AddressWithOffset { nullptr, offset } );

  // This inserts code for copying a function argument struct into a local
  // location only if it is actually accessed.
  if ( baseSymbol->getSymbolTable().getFunction() &&
       !ARMCODESEL->getStack()->getComposedPushed( baseSymbol ) )
    copyComposedOnStack( *baseSymbol, *ARMCODESEL->getLastLLIRFunction() );

  return(
    AddressWithOffset {
      ARMINSTRUCTIONS.CreateRegister( PHREG_SP, true ), offset } );
};


composed_type_object: tpm_SymbolExp
{
  // This rule handles address generation for global composed symbols.
  auto *exp = dynamic_cast<IR_SymbolExp *>( $1->getExp() );

  if ( isComposedType( *exp ) && exp->getSymbol().isGlobal() )
    $cost[0] = loadGlobalSymbolCost( *exp );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "composed_type_object: tpm_SymbolExp", $1 );

  // In case of a dry run, only return the offset which is always 0.
  if ( dryRun )
    return( AddressWithOffset { nullptr, 0 } );

  auto *composedType = dynamic_cast<IR_SymbolExp *>( $1->getExp() );

  // loadGlobalSymbol and return it.
  return(
    AddressWithOffset {
      loadGlobalSymbol( *composedType, false ).calculateAddress(
        $1->getExp() ), 0 } );
};


component_offset: tpm_SymbolExp
{
  // This rule generates the offset from the base pointer for a given composed
  // type field (except for bitfields, these are handled separately).
  if ( isComponentExp( *$1->getExp() ) && !isBitfieldType( *$1->getExp() ) )
    $cost[0] = 0;
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "component_offset: tpm_SymbolExp", $1 );

  // Get the composed type that is accessed here.
  auto *symExp = dynamic_cast<IR_SymbolExp *>( $1->getExp() );
  auto *compType = getParentComposedType( *symExp );

  // Return offset.
  return( compType->getOffset( symExp->getSymbol() ) );
};


###############################################################################
#
#
# Array/Struct post/pre inc/dec
#
#
###############################################################################

dreg: tpm_UnaryExpPOSTINC( deref_dreg )
{
  auto &t = effectiveType( *$2->getExp() );
  $cost[0] = $cost[2] + CT( INS_MOV_32 );

  if ( t.isIntegralType() ) {
    $cost[0] += CT( INS_ADD_32 );
    if ( t.bitSize() != 32 )
      $cost[0] += Cast::truncateCosts( $0->getExp()->getType() );
  } else
    $cost[0] += CT( INS_BL_32 ) + 4 * CT( INS_MOV_32 );

  $cost[0] += ARM_LValue::storeBackWorstCost();
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_UnaryExpPOSTINC( deref_dreg )", $1 );

  auto lvalue = $action[2]( true, false );
  auto &t = effectiveType( *$2->getExp() );

  auto *rTmp = lvalue.getResultRegister();
  auto *r = ARMINSTRUCTIONS.CreateRegister( "" );

  // Move the original value to r.
  ARMINSTRUCTIONS.insertMOV( r, rTmp, $1->getExp() );

  // Increase it.
  if ( t.isIntegralType() ) {
    ARMINSTRUCTIONS.insertADD( OPER_AL, rTmp, rTmp, 1, $1->getExp() );

    // Truncate if needed.
    if ( t.bitSize() != 32 ) {
      auto *tmp = ARMINSTRUCTIONS.CreateRegister( "" );
      Cast::truncate( rTmp, tmp, t, $2->getExp() );
      ARMINSTRUCTIONS.insertMOV( rTmp, tmp, $1->getExp() );
    }
  } else

  if ( t.getType() == IR_Type::FLOAT ) {
    auto *tmp = ARMINSTRUCTIONS.CreateRegister( "" );
    Float f( 1.0f );

    // Move f to a register.
    ARMINSTRUCTIONS.insertMOV_ORR(
      tmp, f.getValue().getComposed(), $1->getExp() );
    // Perform add.
    ARMINSTRUCTIONS.insertADD_F( OPER_AL, rTmp, rTmp, tmp, $1->getExp() );
  }

  // Store back the incremented value.
  lvalue.storeBack( rTmp, $2->getExp() );

  return( r );
};


ereg: tpm_UnaryExpPOSTINC( deref_ereg )
{
  auto &t = effectiveType( *$2->getExp() );
  $cost[0] = $cost[2];

  if ( ( t.getType() == IR_Type::LONG_LONG ) ||
       ( t.getType() == IR_Type::UNSIGNED_LONG_LONG ) )
    $cost[0] += CT( INS_ADD_32 ) + CT( INS_ADC_32 ) + 2 * CT( INS_MOV_32 );
  else

  if ( ( t.getType() == IR_Type::DOUBLE ) ||
       ( t.getType() == IR_Type::LONG_DOUBLE ) )
    $cost[0] += CT( INS_BL_32 ) + 8 * CT( INS_MOV_32 );
  else
    $cost[0] = COST_INFINITY;

  $cost[0] += ARM_LValue::storeBackWorstCost();
}
=
{
  DEBUG_RULE_ACTION( "ereg: tpm_UnaryExpPOSTINC( deref_ereg )", $1 );

  auto lvalue = $action[2]( true, false );
  auto &t = effectiveType( *$2->getExp() );

  auto *rTmp = lvalue.getResultRegister();
  auto *r = ARMINSTRUCTIONS.CreateERegister( "" );
  auto *one = ARMINSTRUCTIONS.CreateERegister( "" );

  // Move the original value to r.
  ARMINSTRUCTIONS.insertMOV(
    r->GetFirstChild(), rTmp->GetFirstChild(), $1->getExp() );

  // Increase it.
  if ( t.isIntegralType() ) {
    // Move a long long int 1 to the new register.
    ARMINSTRUCTIONS.insertMOV(
      one->GetNextChild( one->GetFirstChild() ), 0, $1->getExp() );
    ARMINSTRUCTIONS.insertMOV( one->GetFirstChild(), 1, $1->getExp() );

    // Increment operand.
    ARMINSTRUCTIONS.insertADD_LL( OPER_AL, rTmp, rTmp, one, $1->getExp() );
  } else {
    // Move a double 1 to the new register. For double, the first child holds
    // high, the second low.
    unsigned long low = 0x00000000;
    unsigned long high = 0x3FF00000;

    ARMINSTRUCTIONS.insertMOV_ORR(
      one->GetNextChild( one->GetFirstChild() ), low, $1->getExp() );
    ARMINSTRUCTIONS.insertMOV_ORR( one->GetFirstChild(), high, $1->getExp() );

    // And add them.
    ARMINSTRUCTIONS.insertADD_D( OPER_AL, rTmp, rTmp, one, $1->getExp() );
  }

  // Store back the incremented value.
  lvalue.storeBack( rTmp, $2->getExp() );

  return( r );
};


dreg: tpm_UnaryExpPOSTDEC( deref_dreg )
{
  auto &t = effectiveType( *$2->getExp() );
  $cost[0] = $cost[2] + CT( INS_MOV_32 );

  if ( t.isIntegralType() ) {
    $cost[0] += CT( INS_SUB_32 );
    if ( t.bitSize() != 32 )
      $cost[0] += Cast::truncateCosts( $0->getExp()->getType() );
  } else
    $cost[0] += CT( INS_BL_32 ) + 4 * CT( INS_MOV_32 );

  $cost[0] += ARM_LValue::storeBackWorstCost();
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_UnaryExpPOSTDEC( deref_dreg )", $1 );

  auto lvalue = $action[2]( true, false );
  auto &t = effectiveType( *$2->getExp() );

  auto *rTmp = lvalue.getResultRegister();
  auto *r = ARMINSTRUCTIONS.CreateRegister( "" );

  // Move the original value to r.
  ARMINSTRUCTIONS.insertMOV( r, rTmp, $1->getExp() );

  // Decrease it.
  if ( t.isIntegralType() ) {
    ARMINSTRUCTIONS.insertSUB( OPER_AL, rTmp, rTmp, 1, $1->getExp() );

    // Truncate if needed.
    if ( t.bitSize() != 32 ) {
      auto *tmp = ARMINSTRUCTIONS.CreateRegister( "" );
      Cast::truncate( rTmp, tmp, t, $2->getExp() );
      ARMINSTRUCTIONS.insertMOV( rTmp, tmp, $1->getExp() );
    }
  } else {
    auto *tmp = ARMINSTRUCTIONS.CreateRegister( "" );
    Float f( 1.0f );

    // Move f to a register.
    ARMINSTRUCTIONS.insertMOV_ORR(
      tmp, f.getValue().getComposed(), $1->getExp() );

    // Perform add.
    ARMINSTRUCTIONS.insertSUB_F( OPER_AL, rTmp, rTmp, tmp, $1->getExp() );
  }

  // Store back the decremented value.
  lvalue.storeBack( rTmp, $2->getExp() );

  return( r );
};


ereg: tpm_UnaryExpPOSTDEC( deref_ereg )
{
  auto &t = effectiveType( *$2->getExp() );
  $cost[0] = $cost[2];

  if ( ( t.getType() == IR_Type::LONG_LONG ) ||
       ( t.getType() == IR_Type::UNSIGNED_LONG_LONG ) )
    $cost[0] += CT( INS_ADD_32 ) + CT( INS_ADC_32 ) + 2 * CT( INS_MOV_32 );
  else

  if ( ( t.getType() == IR_Type::DOUBLE ) ||
       ( t.getType() == IR_Type::LONG_DOUBLE ) )
    $cost[0] += CT( INS_BL_32 ) + 8 * CT( INS_MOV_32 );
  else
    $cost[0] = COST_INFINITY;

  $cost[0] += ARM_LValue::storeBackWorstCost();
}
=
{
  DEBUG_RULE_ACTION( "ereg: tpm_UnaryExpPOSTDEC( deref_ereg )", $1 );

  auto lvalue = $action[2]( true, false );
  auto &t = effectiveType( *$2->getExp() );

  auto *rTmp = lvalue.getResultRegister();
  auto *r = ARMINSTRUCTIONS.CreateERegister( "" );
  auto *one = ARMINSTRUCTIONS.CreateERegister( "" );

  // Move the original value to r.
  ARMINSTRUCTIONS.insertMOV(
    r->GetFirstChild(), rTmp->GetFirstChild(), $1->getExp() );

  // Decrease it.
  if ( t.isIntegralType() ) {
    // Move a long long int 1 to the new register.
    ARMINSTRUCTIONS.insertMOV(
      one->GetNextChild( one->GetFirstChild() ), 0, $1->getExp() );
    ARMINSTRUCTIONS.insertMOV( one->GetFirstChild(), 1, $1->getExp() );

    // Increment operand.
    ARMINSTRUCTIONS.insertSUB_LL( OPER_AL, rTmp, rTmp, one, $1->getExp() );
  } else {
    // Move a double 1 to the new register. For double, the first child holds
    // high, the second low.
    unsigned long low = 0x00000000;
    unsigned long high = 0x3FF00000;

    ARMINSTRUCTIONS.insertMOV_ORR(
      one->GetNextChild( one->GetFirstChild() ), low, $1->getExp() );
    ARMINSTRUCTIONS.insertMOV_ORR( one->GetFirstChild(), high, $1->getExp() );

    // And add them.
    ARMINSTRUCTIONS.insertSUB_D( OPER_AL, rTmp, rTmp, one, $1->getExp() );
  }

  // Store back the decremented value.
  lvalue.storeBack( rTmp, $2->getExp() );

  return( r );
};


dreg: tpm_UnaryExpPREINC( deref_dreg )
{
  auto &t = effectiveType( *$2->getExp() );
  $cost[0] = $cost[2];

  if ( t.isIntegralType() ) {
    $cost[0] += CT( INS_ADD_32 );
    if ( t.bitSize() != 32 )
      $cost[0] += Cast::truncateCosts( $0->getExp()->getType() );
  } else
    $cost[0] += CT( INS_BL_32 ) + 4 * CT( INS_MOV_32 );

  $cost[0] += ARM_LValue::storeBackWorstCost();
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_UnaryExpPREINC( deref_dreg )", $1 );

  auto lvalue = $action[2]( true, false );
  auto &t = effectiveType( *$2->getExp() );

  auto *rTmp = lvalue.getResultRegister();

  // Increase it.
  if ( t.isIntegralType() ) {
    ARMINSTRUCTIONS.insertADD( OPER_AL, rTmp, rTmp, 1, $1->getExp() );

    // Truncate if needed.
    if ( t.bitSize() != 32 ) {
      auto *tmp = ARMINSTRUCTIONS.CreateRegister( "" );
      Cast::truncate( rTmp, tmp, t, $2->getExp() );
      ARMINSTRUCTIONS.insertMOV( rTmp, tmp, $1->getExp() );
    }
  } else

  if ( t.getType() == IR_Type::FLOAT ) {
    auto *tmp = ARMINSTRUCTIONS.CreateRegister( "" );
    Float f( 1.0f );

    // Move f to a register.
    ARMINSTRUCTIONS.insertMOV_ORR(
      tmp, f.getValue().getComposed(), $1->getExp() );

    // Perform add.
    ARMINSTRUCTIONS.insertADD_F( OPER_AL, rTmp, rTmp, tmp, $1->getExp() );
  }

  // Store back the incremented value.
  lvalue.storeBack( rTmp, $2->getExp() );

  return( rTmp );
};


ereg: tpm_UnaryExpPREINC( deref_ereg )
{
  auto &t = effectiveType( *$2->getExp() );
  $cost[0] = $cost[2];

  if ( ( t.getType() == IR_Type::LONG_LONG ) ||
       ( t.getType() == IR_Type::UNSIGNED_LONG_LONG ) )
    $cost[0] += CT( INS_ADD_32 ) + CT( INS_ADC_32 ) + 2 * CT( INS_MOV_32 );
  else

  if ( ( t.getType() == IR_Type::DOUBLE ) ||
       ( t.getType() == IR_Type::LONG_DOUBLE ) )
    $cost[0] += CT( INS_BL_32 ) + 8 * CT( INS_MOV_32 );
  else
    $cost[0] = COST_INFINITY;

  $cost[0] += ARM_LValue::storeBackWorstCost();
}
=
{
  DEBUG_RULE_ACTION( "ereg: tpm_UnaryExpPREINC( deref_ereg )", $1 );

  auto lvalue = $action[2]( true, false );
  auto &t = effectiveType( *$2->getExp() );

  auto *rTmp = lvalue.getResultRegister();
  auto *one = ARMINSTRUCTIONS.CreateERegister( "" );

  // Increase it.
  if ( t.isIntegralType() ) {
    // Move a long long int 1 to the new register.
    ARMINSTRUCTIONS.insertMOV(
      one->GetNextChild( one->GetFirstChild() ), 0, $1->getExp() );
    ARMINSTRUCTIONS.insertMOV( one->GetFirstChild(), 1, $1->getExp() );

    // Increment operand.
    ARMINSTRUCTIONS.insertADD_LL( OPER_AL, rTmp, rTmp, one, $1->getExp() );
  } else {
    // Move a double 1 to the new register. For double, the first child holds
    // high, the second low.
    unsigned long low = 0x00000000;
    unsigned long high = 0x3FF00000;

    ARMINSTRUCTIONS.insertMOV_ORR(
      one->GetNextChild( one->GetFirstChild() ), low, $1->getExp() );
    ARMINSTRUCTIONS.insertMOV_ORR( one->GetFirstChild(), high, $1->getExp() );

    // And add them.
    ARMINSTRUCTIONS.insertADD_D( OPER_AL, rTmp, rTmp, one, $1->getExp() );
  }

  // Store back the incremented value.
  lvalue.storeBack( rTmp, $2->getExp() );

  return( rTmp );
};


dreg: tpm_UnaryExpPREDEC( deref_dreg )
{
  auto &t = effectiveType( *$2->getExp() );
  $cost[0] = $cost[2];

  if ( t.isIntegralType() ) {
    $cost[0] += CT( INS_SUB_32 );
    if ( t.bitSize() != 32 )
      $cost[0] += Cast::truncateCosts( $0->getExp()->getType() );
  } else
    $cost[0] += CT( INS_BL_32 ) + 4 * CT( INS_MOV_32 );

  $cost[0] += ARM_LValue::storeBackWorstCost();
}
=
{
  DEBUG_RULE_ACTION( "dreg: tpm_UnaryExpPREDEC( deref_dreg )", $1 );

  auto lvalue = $action[2]( true, false );
  auto &t = effectiveType( *$2->getExp() );

  auto *rTmp = lvalue.getResultRegister();

  // Decrement it.
  if ( t.isIntegralType() ) {
    ARMINSTRUCTIONS.insertSUB( OPER_AL, rTmp, rTmp, 1, $1->getExp() );

    // Truncate if needed.
    if ( t.bitSize() != 32 ) {
      auto *tmp = ARMINSTRUCTIONS.CreateRegister( "" );
      Cast::truncate( rTmp, tmp, t, $2->getExp() );
      ARMINSTRUCTIONS.insertMOV( rTmp, tmp, $1->getExp() );
    }
  } else

  if ( t.getType() == IR_Type::FLOAT ) {
    auto *tmp = ARMINSTRUCTIONS.CreateRegister( "" );
    Float f( 1.0f );

    // Move f to a register.
    ARMINSTRUCTIONS.insertMOV_ORR(
      tmp, f.getValue().getComposed(), $1->getExp() );

    // Perform add.
    ARMINSTRUCTIONS.insertSUB_F( OPER_AL, rTmp, rTmp, tmp, $1->getExp() );
  }

  // Store back the incremented value.
  lvalue.storeBack( rTmp, $2->getExp() );

  return( rTmp );
};


ereg: tpm_UnaryExpPREDEC( deref_ereg )
{
  auto &t = effectiveType( *$2->getExp() );
  $cost[0] = $cost[2];

  if ( ( t.getType() == IR_Type::LONG_LONG ) ||
       ( t.getType() == IR_Type::UNSIGNED_LONG_LONG ) )
    $cost[0] += CT( INS_ADD_32 ) + CT( INS_ADC_32 ) + 2 * CT( INS_MOV_32 );
  else

  if ( ( t.getType() == IR_Type::DOUBLE ) ||
       ( t.getType() == IR_Type::LONG_DOUBLE ) )
    $cost[0] += CT( INS_BL_32 ) + 8 * CT( INS_MOV_32 );
  else
    $cost[0] = COST_INFINITY;

  $cost[0] += ARM_LValue::storeBackWorstCost();
}
=
{
  DEBUG_RULE_ACTION( "ereg: tpm_UnaryExpPREDEC( deref_ereg )", $1 );

  auto lvalue = $action[2]( true, false );
  auto &t = effectiveType( *$2->getExp() );

  auto *rTmp = lvalue.getResultRegister();
  auto *one = ARMINSTRUCTIONS.CreateERegister( "" );

  // Decrease it.
  if ( t.isIntegralType() ) {
    // Move a long long int 1 to the new register.
    ARMINSTRUCTIONS.insertMOV(
      one->GetNextChild( one->GetFirstChild() ), 0, $1->getExp() );
    ARMINSTRUCTIONS.insertMOV( one->GetFirstChild(), 1, $1->getExp() );

    // Increment operand.
    ARMINSTRUCTIONS.insertSUB_LL( OPER_AL, rTmp, rTmp, one, $1->getExp() );
  } else {
    // Move a double 1 to the new register. For double, the first child holds
    // high, the second low.
    unsigned long low = 0x00000000;
    unsigned long high = 0x3FF00000;

    ARMINSTRUCTIONS.insertMOV_ORR(
      one->GetNextChild( one->GetFirstChild() ), low, $1->getExp() );
    ARMINSTRUCTIONS.insertMOV_ORR( one->GetFirstChild(), high, $1->getExp() );

    // And add them.
    ARMINSTRUCTIONS.insertSUB_D( OPER_AL, rTmp, rTmp, one, $1->getExp() );
  }

  // Store back the decremented value.
  lvalue.storeBack( rTmp, $2->getExp() );

  return( rTmp );
};

include(composed_struct_bitfield_rules.m4)
