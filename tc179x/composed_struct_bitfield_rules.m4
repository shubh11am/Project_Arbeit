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


# @author Heiko Falk <Heiko.Falk@tuhh.de>
deref_dreg: tpm_ComponentAccessExp( composed_type_object, bitfield_offset )
{
  // This rule handles bitfields with base types <= 32 bits.
  if ( $0->getExp()->getType().bitSize() <= 32 )
    $cost[0] = $cost[2] + $cost[3] + readValueFromBitfieldCost();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION(
    "deref_dreg: tpm_ComponentAccessExp( composed_type_object, bitfield_offset )",
    $1 );

  auto compAddr = $action[2]();
  auto bitfieldInfo = $action[3]();

  // Determine parameters for the bitfield access.
  const int byteOffset = compAddr.getOffset() + bitfieldInfo.byteOffset;
  const unsigned char bitOffset = bitfieldInfo.bounds.bitOffset;
  const unsigned char bitLength = bitfieldInfo.bounds.bitLength;
  auto *bitfieldType =
    dynamic_cast<IR_BitfieldType *>( &$3->getExp()->getType() );
  ufAssertT( bitfieldType, "Invalid type!" );

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateRegister( "" );
  LLIR_Register *regBase = compAddr.getARegister();

  // WIR
  auto &r = TCINSTRUCTIONS.createDReg();
  auto &bReg = compAddr.getAReg();

  if ( loadResult )
    readValueFromBitfield(
      reg, regBase, r, bReg, *bitfieldType, byteOffset, bitOffset, bitLength,
      $1->getExp() );

  return(
    TC_LValue {
      reg, &r,
      TC_AddressModification { regBase, bReg, byteOffset, bitfieldType, true },
      bitOffset, bitLength } );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
deref_llereg: tpm_ComponentAccessExp( composed_type_object, bitfield_offset )
{
  // This rule handles bitfields with base type 'long long'.
  if ( $0->getExp()->getType().bitSize() > 32 )
    $cost[0] =
      $cost[2] + $cost[3] + readValueFromBitfieldCost() +
      TC13::OperationFormat::DDC5C5.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION(
    "deref_llereg: tpm_ComponentAccessExp( composed_type_object, bitfield_offset )",
    $1 );

  auto compAddr = $action[2]();
  auto bitfieldInfo = $action[3]();

  // Determine parameters for the bitfield access.
  const int byteOffset = compAddr.getOffset() + bitfieldInfo.byteOffset;
  const unsigned char bitOffset = bitfieldInfo.bounds.bitOffset;
  const unsigned char bitLength = bitfieldInfo.bounds.bitLength;
  auto *bitfieldType =
    dynamic_cast<IR_BitfieldType *>( &$3->getExp()->getType() );
  ufAssertT( bitfieldType, "Invalid type!" );

  // LLIR
  LLIR_Register *reg = TCINSTRUCTIONS.CreateERegister( "" );
  LLIR_Register *regBase = compAddr.getARegister();

  // WIR
  auto &r = TCINSTRUCTIONS.createEReg();
  auto &bReg = compAddr.getAReg();

  if ( loadResult ) {
    // LLIR
    LLIR_Register *lowerWord = getLVLLChild( reg );
    LLIR_Register *higherWord = getHVLLChild( reg );

    readValueFromBitfield(
      lowerWord, regBase, dynamic_cast<TC_DRegV &>( r.begin()->get() ), bReg,
      *bitfieldType, byteOffset, bitOffset, bitLength, $1->getExp() );

    // LLIR
    // Fill the second register with the sign bit or set it to zero.
    if ( bitfieldType->isSignedType() )
      TCINSTRUCTIONS.insertEXTR( higherWord, lowerWord, 31, 1 );
    else
      TCINSTRUCTIONS.insertMOV( higherWord, 0 );

    // WIR
    // Fill the second register with the sign bit or set it to zero.
    if ( bitfieldType->isSignedType() )
      TCINSTRUCTIONS.insertEXTR(
        dynamic_cast<TC_DRegV &>( r.rbegin()->get() ),
        dynamic_cast<TC_DRegV &>( r.begin()->get() ), 31, 1, $1->getExp() );
    else
      TCINSTRUCTIONS.insertMOV(
        dynamic_cast<TC_DRegV &>( r.rbegin()->get() ), 0, $1->getExp() );
  }

  return(
    TC_LValue {
      reg, &r,
      TC_AddressModification { regBase, bReg, byteOffset, bitfieldType, true },
      bitOffset, bitLength } );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
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
