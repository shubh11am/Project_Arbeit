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


bitfield_offset: tpm_SymbolExp
{
  // This rule generates the offset from a bitfield's base pointer.
  if ( isComponentExp( *$1->getExp() ) && isBitfieldType( *$1->getExp() ) )
    $cost[0] = 0;
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "bitfield_offset: tpm_SymbolExp", $1 );

  // Get the composed type that is accessed here.
  auto *symExp = dynamic_cast<IR_SymbolExp *>( $1->getExp() );
  IR_ComposedType *compType = getParentComposedType( *symExp );

  BitfieldInfo res;

  res.byteOffset = getBitfieldByteOffset( *compType, symExp->getSymbol() );
  res.bounds.bitOffset = getBitfieldBitOffset( *compType, symExp->getSymbol() );
  res.bounds.bitLength = getBitfieldLength( symExp->getSymbol() );

  return( res );
};


deref_dreg: tpm_ComponentAccessExp( composed_type_object, bitfield_offset )
{
  if ( $0->getExp()->getType().bitSize() <= 32 )
    $cost[0] = $cost[2] + $cost[3] + readFromBitfieldCost();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION(
    "deref_dreg: tpm_ComponentAccessExp( composed_type_object, "
    "bitfield_offset )", $1 );

  auto *bitfieldType =
    dynamic_cast<IR_BitfieldType *>( &$3->getExp()->getType() );

  auto compAddr = $action[2]( dryRun );
  auto info = $action[3]();
  info.byteOffset += compAddr.getOffset();

  auto *reg_base = compAddr.getARegister();

  LLIR_Register *result = nullptr;
  if ( !dryRun && loadResult )
    result = readFromBitfield( reg_base, info, *bitfieldType, $1->getExp());

  ARM_AddressModification amod {
    compAddr.getARegister(), info.byteOffset, bitfieldType,
    AddressModification::ModTime::NONE, AddressModification::ModOper::ADD, true,
    dryRun };

  return(
    ARM_LValue {
      result, amod, info.bounds.bitOffset, info.bounds.bitLength, dryRun } );
};


init_list_element: any_reg
{
  auto *ilType = getTypeInInitList( *$1->getExp() );

  if ( ilType && isBitfieldType( *ilType ) )
    $cost[0] =
      $cost[1] + CT( INS_LDR_32 ) + CT( INS_STR_32 ) +
      insertValueIntoBitfieldCost();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "init_list_element: any_reg", $1 );

  // Get reg which has the appropriate value moved to it.
  auto *data = $action[1]();

  // Get some type information about the composed type and the current symbol.
  auto *initList = dynamic_cast<IR_InitListExp *>( $1->getExp()->getParent() );
  auto *compType = dynamic_cast<IR_ComposedType *>( &initList->getType() );
  auto *bitfieldSym = &initList->getInitializedComponent( $1->getExp() );

  // Extract bitfield parameters.
  auto bitOffset =
    static_cast<unsigned char>( getBitfieldBitOffset( *compType, *bitfieldSym ) );
  auto bitLength =
    static_cast<unsigned char>( getBitfieldLength( *bitfieldSym ) );

  // Align access to base type.
  unsigned int baseByteSize = bitfieldSym->getType().IR_Type::bitSize() / 8;
  unsigned int offset = ( ( address_increment ) / baseByteSize ) * baseByteSize;

  // Load the values of the neighboring bitfields.
  auto *old_container = ARMINSTRUCTIONS.CreateRegister( "" );

  if ( baseByteSize == 4 )
    ARMINSTRUCTIONS.insertLDR(
      OPER_IMMOFF, old_container, reg, offset, $1->getExp() );
  else

  if ( baseByteSize == 1 )
    ARMINSTRUCTIONS.insertLDRB(
      OPER_IMMOFF, old_container, reg, offset, $1->getExp() );

  // Insert the new value.
  auto *new_container =
    insertValueIntoBitfield(
      old_container, { bitOffset, bitLength }, data, $1->getExp() );

  // Store it back.
  if ( baseByteSize == 4 )
    ARMINSTRUCTIONS.insertSTR(
      OPER_IMMOFF, OPER_AL, new_container, reg, offset, $1->getExp() );
  else

  if ( baseByteSize == 1 )
    ARMINSTRUCTIONS.insertSTRB(
      OPER_IMMOFF, OPER_AL, new_container, reg, offset, $1->getExp() );
};


init_list_element: const8
{
  auto *ilType = getTypeInInitList( *$1->getExp() );

  if ( ilType && isBitfieldType( *ilType ) ) {
    $cost[0] = CT( INS_LDR_32 ) + CT( INS_STR_32 );

    auto *constExp = dynamic_cast<IR_IntConstExp *>( $1->getExp() );
    if ( constExp ) {
      int constant = constExp->getValue().getIntValue();

      // Get some type information about the composed type and the current symbol.
      auto *initList =
        dynamic_cast<IR_InitListExp *>( $1->getExp()->getParent() );
      auto *compType = dynamic_cast<IR_ComposedType *>( &initList->getType() );
      auto *bitfieldSym = &initList->getInitializedComponent( $1->getExp() );

      // Extract bitfield parameters.
      auto bitOffset =
        static_cast<unsigned char>(
          getBitfieldBitOffset( *compType, *bitfieldSym ) );
      auto bitLength =
        static_cast<unsigned char>( getBitfieldLength( *bitfieldSym ) );

      $cost[0] +=
        insertConstantIntoBitfieldCOST( { bitOffset, bitLength }, constant );
    } else
      $cost[0] = COST_INFINITY;
  } else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "init_list_element: const8", $1 );

  int constant = $action[1]().getIntValue();

  // Get some type information about the composed type and the current symbol.
  auto *initList = dynamic_cast<IR_InitListExp *>( $1->getExp()->getParent() );
  auto *compType = dynamic_cast<IR_ComposedType *>( &initList->getType() );
  auto *bitfieldSym = &initList->getInitializedComponent( $1->getExp() );

  // Extract bitfield parameters.
  auto bitOffset =
    static_cast<unsigned char>(
      getBitfieldBitOffset( *compType, *bitfieldSym ) );
  auto bitLength =
    static_cast<unsigned char>( getBitfieldLength( *bitfieldSym ) );

  // Align access to base type.
  unsigned int baseByteSize = bitfieldSym->getType().IR_Type::bitSize() / 8;
  unsigned int offset = ( ( address_increment ) / baseByteSize ) * baseByteSize;

  // Load the values of the neighboring bitfields.
  auto *old_container = ARMINSTRUCTIONS.CreateRegister( "" );

  if ( baseByteSize == 4 )
    ARMINSTRUCTIONS.insertLDR(
      OPER_IMMOFF, old_container, reg, offset, $1->getExp() );
  else

  if ( baseByteSize == 1 )
    ARMINSTRUCTIONS.insertLDRB(
      OPER_IMMOFF, old_container, reg, offset, $1->getExp() );

  // Insert the new value.
  auto *new_container =
    insertConstantIntoBitfield(
      old_container, { bitOffset, bitLength }, constant, $1->getExp() );

  // Store it back.
  if ( baseByteSize == 4 )
    ARMINSTRUCTIONS.insertSTR(
      OPER_IMMOFF, OPER_AL, new_container, reg, offset, $1->getExp() );
  else

  if ( baseByteSize == 1 )
    ARMINSTRUCTIONS.insertSTRB(
      OPER_IMMOFF, OPER_AL, new_container, reg, offset, $1->getExp() );
};
