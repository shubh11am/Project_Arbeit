/*

   This source file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Compiler Framework

   and is property of its respective copyright holder. It must neither be used
   nor published even in parts without explicit written permission.

   Copyright 2009 - 2022

   Hamburg University of Technology (TUHH)
   Institute of Embedded Systems
   21071 Hamburg
   Germany

   http://www.tuhh.de/es/esd/research/wcc

*/


BitfieldInfo::BitfieldInfo( void )
{
  byteOffset = 0;
  bounds.bitOffset = 0;
  bounds.bitLength = 0;
};

/*
  getBitfieldByteOffset returns the offset of the word in which the bitfield
  resides seen from the start of the composed type object.
*/
int getBitfieldByteOffset( const IR_ComposedType &parentType,
                           const IR_Symbol &bitfield )
{
  DSTART(
    "int getBitfieldByteOffset(const IR_ComposedType&, const IR_Symbol&)" );

  const int byteOffsetRaw = parentType.getOffset( bitfield );

  // To get the correct address we align it to the base type size.
  unsigned int baseBitSize = bitfield.getType().IR_Type::bitSize();
  unsigned int baseByteSize = baseBitSize / 8;

  unsigned int alignedOffset = ( byteOffsetRaw / baseByteSize ) * baseByteSize;
  DOUT( "byteOffsetRaw: " << byteOffsetRaw << " alignedOffset: " <<
        alignedOffset << endl );
  return alignedOffset;
}


/*
  getBitfieldBitOffset returns the bit offset of 'bitfield' inside the word in
  which it is enclosed in the surrounding composed type.
*/
int getBitfieldBitOffset( const IR_ComposedType &parentType,
                          const IR_Symbol &bitfield )
{
  DSTART(
    "int getBitfieldBitOffset(const IR_ComposedType&, const IR_Symbol&)" );

  const int bitOffsetRaw = parentType.getBitOffset( bitfield );
  int bitOffset = bitOffsetRaw - getBitfieldByteOffset( parentType, bitfield ) * 8;

  DOUT( "bitOffsetRaw: " << bitOffsetRaw << " bitOffset: " << bitOffset << endl );

  // bitOffset is measured from beginning of the word the bitfield is stored in.
  return( bitOffset );
}


/*
  getBitfieldLength returns the length (in bits) of a bitfield.
*/
int getBitfieldLength( const IR_Symbol &bitfield )
{
  DSTART( "int getBitfieldLength(const IR_Symbol&)" );

  auto *bType = dynamic_cast<IR_BitfieldType *>( &bitfield.getType() );

  return( bType->bitSize() );
}

COST readFromBitfieldCost()
{
  return 2 * CT( INS_MOV_32 ) + CT( INS_LDR_32 );
}

LLIR_Register* readFromBitfield( LLIR_Register *address,
                                 const struct BitfieldInfo &info,
                                 const IR_Type &type, IR_Exp *exp )
{
  DSTART( "LLIR_Register* readFromBitfield( LLIR_Register *address, "
          "const struct BitfieldInfo &info, "
          "const IR_Type& type, IR_Exp *exp )" );
  LLIR_Register *result = ARMINSTRUCTIONS.CreateRegister( "" );

  unsigned char upperBound = info.bounds.bitOffset + info.bounds.bitLength;

  DOUT( "Bitfield byteOffset: " << info.byteOffset << " bitOffset: " <<
        (int) info.bounds.bitOffset << " bitLength: " << (int) info.bounds.bitLength << endl );
  ufAssertT( upperBound <= 32, "Bitfield should not exceed word boundary!" );

  unsigned int baseByteSize = type.IR_Type::bitSize() / 8;

  if ( baseByteSize == 4 ) {
    ARMINSTRUCTIONS.insertLDR( OPER_IMMOFF, result, address, info.byteOffset, exp );
  } else if ( baseByteSize == 1 ) {
    ARMINSTRUCTIONS.insertLDRB( OPER_IMMOFF, result, address, info.byteOffset, exp );
  } else {
    ufAssertT( 0, "Unsupported bitfield base type" );
  }

  // Clear higher bits: shift bit packet up
  ARMINSTRUCTIONS.insertMOV( OPER_AL, result, result, OPER_LSL, 32 - upperBound, exp );

  // Shift to beginning of word
  // Sign extend for signed types
  if ( type.isUnsignedType() )
    ARMINSTRUCTIONS.insertMOV( OPER_AL, result, result, OPER_LSR,
                             32 - info.bounds.bitLength, exp );
  else {
    ARMINSTRUCTIONS.insertMOV( OPER_AL, result, result, OPER_ASR,
                             32 - info.bounds.bitLength, exp );
  }

  return( result );
}

COST insertValueIntoBitfieldCost()
{
  return 3 * CT( INS_MOV_32 ) + CT( INS_SUB_32 ) + CT( INS_BIC_32 ) +
         CT( INS_AND_32 ) + CT( INS_ORR_32 );
}

LLIR_Register* insertValueIntoBitfield( LLIR_Register *container,
                                        const struct BitfieldBounds &bounds,
                                        LLIR_Register *source, IR_Exp* exp )
{
DSTART( "LLIR_Register* insertValueIntoBitfield( LLIR_Register *,"
        "const struct BitfieldBounds &, LLIR_Register *, IR_Exp* )" );

  DOUT( "Offset: " << (int)bounds.bitOffset << " Length: " <<
        (int) bounds.bitLength << endl );

  unsigned char upperBound = bounds.bitOffset + bounds.bitLength;
  ufAssertT( upperBound <= 32, "Bitfield over word boundary not supported!" );

  LLIR_Register *mask = ARMINSTRUCTIONS.CreateRegister( "" );
  LLIR_Register *new_container = ARMINSTRUCTIONS.CreateRegister( "" );

  // Generate Bitmask of correct length
  unsigned int one = 1 << bounds.bitLength;
  ARMINSTRUCTIONS.insertMOV( mask, one, exp );
  ARMINSTRUCTIONS.insertSUB( mask, mask, 1, exp );

  if ( bounds.bitOffset > 0 )
    ARMINSTRUCTIONS.insertMOV( OPER_AL, mask, mask, OPER_LSL, bounds.bitOffset );

  // Delete the old value using the mask
  ARMINSTRUCTIONS.insertBIC( "", OPER_AL, container, container, mask, exp );

  // Shift new value into place
  LLIR_Register *value = ARMINSTRUCTIONS.CreateRegister( "" );
  ARMINSTRUCTIONS.insertMOV( OPER_AL, "", value, source, OPER_LSL,
                           bounds.bitOffset, exp );

  // Remove any overlap into neighboring bits using mask
  ARMINSTRUCTIONS.insertAND( value, value, mask, exp );

  // Set the bits in the word containing the bitfield
  ARMINSTRUCTIONS.insertORR( new_container, container, value, exp );

  return( new_container );
}

COST insertConstantIntoBitfieldCOST( const struct BitfieldBounds &bounds,
                                     int constant )
{
  unsigned int constMask = 1 << bounds.bitLength;
  constMask -= 1;
  constMask <<= bounds.bitOffset;

  constant <<= bounds.bitOffset;
  constant &= constMask;

  return ARMINSTRUCTIONS.insertMOV_ORR_Cost( constMask ) +
         ARMINSTRUCTIONS.insertMOV_ORR_Cost( constant ) +
         CT( INS_BIC_32 ) + CT( INS_ORR_32 );
}

LLIR_Register* insertConstantIntoBitfield( LLIR_Register *container,
                                           const struct BitfieldBounds &bounds,
                                           int constant, IR_Exp* exp )
{
DSTART( "LLIR_Register* insertConstantIntoBitfield( LLIR_Register *, "
        "const struct BitfieldBounds &, int, IR_Exp* )" );

  DOUT( "Offset: " << (int)bounds.bitOffset << " Length: " <<
        (int) bounds.bitLength << endl );

  unsigned char upperBound = bounds.bitOffset + bounds.bitLength;
  ufAssertT( upperBound <= 32, "Bitfield over word boundary not supported!" );

  LLIR_Register *mask = ARMINSTRUCTIONS.CreateRegister( "" );
  LLIR_Register *constantReg = ARMINSTRUCTIONS.CreateRegister( "" );
  LLIR_Register *new_container = ARMINSTRUCTIONS.CreateRegister( "" );

  // Generate Bitmask of correct length
  unsigned int constMask = 1 << bounds.bitLength;
  constMask -= 1;
  constMask <<= bounds.bitOffset;

  ARMINSTRUCTIONS.insertMOV_ORR( mask, constMask, exp );

  // Delete the old value using the mask
  ARMINSTRUCTIONS.insertBIC( "", OPER_AL, container, container, mask, exp );

  constant <<= bounds.bitOffset;
  constant &= constMask;

  // For small constants, it would be possible to omit this MOV_ORR
  // and just ORR the constant into the new container.
  ARMINSTRUCTIONS.insertMOV_ORR( constantReg, constant, exp );

  // Set the bits in the word containing the bitfield
  ARMINSTRUCTIONS.insertORR( new_container, container, constantReg, exp );

  return( new_container );
}
