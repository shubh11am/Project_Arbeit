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

  // For the bitfield access, we need a 4-byte-aligned base address. This
  // requires that the struct's/union's base address is also 4-byte-aligned.
  // If this is not the case, we must adapt our computation here.
  return( ( byteOffsetRaw / 4 ) * 4 );
};


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

  // bitOffset is measured from beginning of the word the bitfield is stored in.
  return( bitOffsetRaw - getBitfieldByteOffset( parentType, bitfield ) * 8 );
};


/*
  getBitfieldLength returns the length (in bits) of a bitfield.
*/
int getBitfieldLength( const IR_Symbol &bitfield )
{
  DSTART( "int getBitfieldLength(const IR_Symbol&)" );

  auto *bType = dynamic_cast<IR_BitfieldType *>( &bitfield.getType() );

  return( bType->bitSize() );
};


/*
  readValueFromBitfieldCost computes the costs for reading a bitfield value.

  The exact costs cannot be computed since they depend on parameters that are
  only available when executing the action parts of the rule-set.
*/
COST readValueFromBitfieldCost( void )
{
  DSTART( "COST readValueFromBitfieldCost()" );

  return(
    TC13::OperationFormat::DAC16BOA.getSize() +
    TC13::OperationFormat::DDC5C5.getSize() );
};


/*
  readValueFromBitfield reads the bitfield value beginning in word
  [regBase] + byteOffsetFromBase into register reg using the given bitfield
  parameters.
*/
void readValueFromBitfield( LLIR_Register *reg, LLIR_Register *regBase,
                            const TC_DRegV &r, const TC_ARegV &bReg,
                            const IR_BitfieldType &t,
                            const int byteOffsetFromBase,
                            const int bitfieldOffset, const int bitfieldLength,
                            const IR_Exp *exp )
{
  DSTART(
    "void readValueFromBitfield(LLIR_Register*, LLIR_Register*, const TC_DRegV&, const TC_ARegV&, const IR_BitfieldType&, int, int, int, const IR_Exp*)" );

  unsigned char effectiveBitOffset;
  auto bitsInNextWord =
    TC_LValue::getBitsInNextWord( bitfieldOffset, bitfieldLength );

  // LLIR
  LLIR_Register *regTmp  = TCINSTRUCTIONS.CreateRegister( "" );
  if ( bitsInNextWord > 0 ) {
    // If the bitfield spans over a word boundary, we must load it with LD.D,
    // DEXTR and EXTR/EXTR.U instructions.
    LLIR_Register *eReg = TCINSTRUCTIONS.CreateERegister( "" );
    TCINSTRUCTIONS.insertLD_D(
      eReg, OPER_BASE, regBase, byteOffsetFromBase, exp );

    LLIR_Register *c1 = eReg->GetFirstChild();
    LLIR_Register *c2 = eReg->GetNextChild( c1 );
    TCINSTRUCTIONS.insertDEXTR( regTmp, c1, c2, bitsInNextWord, exp );

    effectiveBitOffset = 0;
  } else {
    // If otherwise the bitfield is in a single word, we may load it directly
    // with a LD.W and an EXTR/EXTR.U.
    TCINSTRUCTIONS.insertLD_W(
      regTmp, OPER_BASE, regBase, byteOffsetFromBase, exp );

    effectiveBitOffset = bitfieldOffset;
  }

  // Extract the bitfield value from the loaded word.
  if ( t.isUnsignedType() )
    TCINSTRUCTIONS.insertEXTR_U(
      reg, regTmp, effectiveBitOffset, bitfieldLength, exp );
  else
    TCINSTRUCTIONS.insertEXTR(
      reg, regTmp, effectiveBitOffset, bitfieldLength, exp );

  // WIR
  auto &tmpReg = TCINSTRUCTIONS.createDReg();
  if ( bitsInNextWord > 0 ) {
    // If the bitfield spans over a word boundary, we must load it with LD.D,
    // DEXTR and EXTR/EXTR.U instructions.
    auto &eReg = TCINSTRUCTIONS.createEReg();

    TCINSTRUCTIONS.insertLD_D( eReg, bReg, byteOffsetFromBase, exp );
    TCINSTRUCTIONS.insertDEXTR(
      tmpReg, dynamic_cast<TC_DRegV &>( eReg.begin()->get() ),
      dynamic_cast<TC_DRegV &>( eReg.rbegin()->get() ), bitsInNextWord, exp );

    effectiveBitOffset = 0;
  } else {
    // If otherwise the bitfield is in a single word, we may load it directly
    // with a LD.W and an EXTR/EXTR.U.
    TCINSTRUCTIONS.insertLD_W( tmpReg, bReg, byteOffsetFromBase, exp );

    effectiveBitOffset = bitfieldOffset;
  }

  // Extract the bitfield value from the loaded word.
  if ( t.isUnsignedType() )
    TCINSTRUCTIONS.insertEXTR_U(
      r, tmpReg, effectiveBitOffset, bitfieldLength, exp );
  else
    TCINSTRUCTIONS.insertEXTR(
      r, tmpReg, effectiveBitOffset, bitfieldLength, exp );
};


/*
  writeValueToBitfieldCost computes the costs for writing a bitfield value.

  The exact costs cannot be computed since they depend on parameters that are
  only available when executing the action parts of the rule-set.
*/
COST writeValueToBitfieldCost( void )
{
  DSTART( "COST writeValueToBitfieldCost()" );

  return(
    TC13::OperationFormat::EDC5C5.getSize() +
    TC13::OperationFormat::AC10EBOA.getSize() );
};


/*
  writeValueToBitfield writes the value from register reg to the bitfield in
  word [regBase] + byteOffsetFromBase using the given bitfield parameters.
*/
void writeValueToBitfield( LLIR_Register *reg, LLIR_Register *regBase,
                           AddressModification::ModTime pTime,
                           const WIR_VirtualRegister &r, const TC_ARegV &bReg,
                           const IR_BitfieldType &t,
                           const int byteOffsetFromBase,
                           const int bitfieldOffset, const int bitfieldLength,
                           const IR_Exp *exp )
{
  DSTART(
    "void writeValueToBitfield(LLIR_Register*, LLIR_Register*, AddressModification::ModTime, const WIR_VirtualRegister&, const TC_ARegV&, const IR_BitfieldType&, int, int, int, const IR_Exp*)" );

  auto bitsInNextWord =
    TC_LValue::getBitsInNextWord( bitfieldOffset, bitfieldLength );
  int bitsInFirstWord = bitfieldLength - bitsInNextWord;

  // The result value may be given in a data register or in an extended
  // register. In any case, the result is in the first register.
  const bool usesTwoRegisters = r.hasChilds();

  // LLIR
  LLIR_Register *effective_result_reg =
    usesTwoRegisters ? reg->GetFirstChild() : reg;

  // Store the part that belongs into the first word using IMASK and LDMST.
  LLIR_Register *imask_result = TCINSTRUCTIONS.CreateERegister( "" );
  TCINSTRUCTIONS.insertIMASK(
    imask_result, effective_result_reg, bitfieldOffset, bitsInFirstWord, exp );
  switch ( pTime ) {
    case AddressModification::ModTime::POST: {
      TCINSTRUCTIONS.insertLDMST(
        OPER_POSTINC, regBase, byteOffsetFromBase, imask_result, exp );
      break;
    }

    case AddressModification::ModTime::PRE: {
      TCINSTRUCTIONS.insertLDMST(
        OPER_PREINC, regBase, byteOffsetFromBase, imask_result, exp );
      break;
    }

    default: {
      TCINSTRUCTIONS.insertLDMST(
        OPER_BASE, regBase, byteOffsetFromBase, imask_result, exp );
      break;
    }
  }

  // Store the second part of the bitfield if it overlaps into the next word.
  if ( bitsInNextWord > 0 ) {
    // Extract the second part of the bitfield into a register.
    LLIR_Register *secondWordValue = TCINSTRUCTIONS.CreateRegister( "" );
    TCINSTRUCTIONS.insertSHA(
      secondWordValue, effective_result_reg, -bitsInFirstWord );

    // Store the part that belongs into the second word using IMASK and LDMST.
    TCINSTRUCTIONS.insertIMASK(
      imask_result, secondWordValue, 0, bitsInNextWord, exp );
    switch ( pTime ) {
      case AddressModification::ModTime::POST: {
        TCINSTRUCTIONS.insertLDMST(
          OPER_POSTINC, regBase, byteOffsetFromBase + 4, imask_result, exp );
        break;
      }

      case AddressModification::ModTime::PRE: {
        TCINSTRUCTIONS.insertLDMST(
          OPER_PREINC, regBase, byteOffsetFromBase + 4, imask_result, exp );
        break;
      }

      default: {
        TCINSTRUCTIONS.insertLDMST(
          OPER_BASE, regBase, byteOffsetFromBase + 4, imask_result, exp );
        break;
      }
    }
  }

  // After the write, the register that holds the value should have the same
  // content as the bitfield in memory. Therefore, we must explicitly truncate
  // and zero- or sign-extend the register's value.
  if ( bitfieldLength != 32 ) {
    ufAssertT( bitfieldLength < 32, "Invalid length!" );

    if ( t.isUnsignedType() )
      TCINSTRUCTIONS.insertEXTR_U(
        effective_result_reg, effective_result_reg, 0, bitfieldLength, exp );
    else
      TCINSTRUCTIONS.insertEXTR(
        effective_result_reg, effective_result_reg, 0, bitfieldLength, exp );

    if ( usesTwoRegisters ) {
      LLIR_Register *secondChild = reg->GetNextChild( reg->GetFirstChild() );

      if ( t.isUnsignedType() )
        TCINSTRUCTIONS.insertEXTR_U(
          secondChild, effective_result_reg, 31, 1, exp );
      else
        TCINSTRUCTIONS.insertEXTR(
          secondChild, effective_result_reg, 31, 1, exp );
    }
  }

  // WIR
  auto &resReg =
    usesTwoRegisters ?
      dynamic_cast<const TC_DRegV &>( r.begin()->get() ) :
      dynamic_cast<const TC_DRegV &>( r );

  // Store the part that belongs into the first word using IMASK and LDMST.
  auto &imaskReg = TCINSTRUCTIONS.createEReg();
  TCINSTRUCTIONS.insertIMASK(
    imaskReg, resReg, bitfieldOffset, bitsInFirstWord, exp );
  switch ( pTime ) {
    case AddressModification::ModTime::POST: {
      TCINSTRUCTIONS.insertLDMST(
        TC13::AddressingMode::post, bReg, byteOffsetFromBase, imaskReg, exp );
      break;
    }

    case AddressModification::ModTime::PRE: {
      TCINSTRUCTIONS.insertLDMST(
        TC13::AddressingMode::pre, bReg, byteOffsetFromBase, imaskReg, exp );
      break;
    }

    default: {
      TCINSTRUCTIONS.insertLDMST( bReg, byteOffsetFromBase, imaskReg, exp );
      break;
    }
  }

  // Store the second part of the bitfield if it overlaps into the next word.
  if ( bitsInNextWord > 0 ) {
    // Extract the second part of the bitfield into a register.
    auto &tmpReg = TCINSTRUCTIONS.createDReg();
    TCINSTRUCTIONS.insertSHA( tmpReg, resReg, -bitsInFirstWord );

    // Store the part that belongs into the second word using IMASK and LDMST.
    TCINSTRUCTIONS.insertIMASK( imaskReg, tmpReg, 0, bitsInNextWord, exp );
    switch ( pTime ) {
      case AddressModification::ModTime::POST: {
        TCINSTRUCTIONS.insertLDMST(
          TC13::AddressingMode::post, bReg, byteOffsetFromBase + 4, imaskReg,
          exp );
        break;
      }

      case AddressModification::ModTime::PRE: {
        TCINSTRUCTIONS.insertLDMST(
          TC13::AddressingMode::pre, bReg, byteOffsetFromBase + 4, imaskReg,
          exp );
        break;
      }

      default: {
        TCINSTRUCTIONS.insertLDMST(
          bReg, byteOffsetFromBase + 4, imaskReg, exp );
        break;
      }
    }
  }

  // After the write, the register that holds the value should have the same
  // content as the bitfield in memory. Therefore, we must explicitly truncate
  // and zero- or sign-extend the register's value.
  if ( bitfieldLength != 32 ) {
    if ( t.isUnsignedType() )
      TCINSTRUCTIONS.insertEXTR_U( resReg, resReg, 0, bitfieldLength, exp );
    else
      TCINSTRUCTIONS.insertEXTR( resReg, resReg, 0, bitfieldLength, exp );

    if ( usesTwoRegisters ) {
      auto &c2 = dynamic_cast<const TC_DRegV &>( r.rbegin()->get() );

      if ( t.isUnsignedType() )
        TCINSTRUCTIONS.insertEXTR_U( c2, resReg, 31, 1, exp );
      else
        TCINSTRUCTIONS.insertEXTR( c2, resReg, 31, 1, exp );
    }
  }
};
