/*

   This header file belongs to the

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


#ifndef _TC179x_COMPOSED_STRUCT_BITFIELD_INCL_H_
#define _TC179x_COMPOSED_STRUCT_BITFIELD_INCL_H_


//
// Include section
//

// Include local headers
#include <tc179x/tcaddressmodification.h>
#include <tc179x/cs_tc179x.h>


//
// Class forward declarations
//

class IR_BitfieldType;
class IR_ComposedType;
class IR_Exp;
class IR_Symbol;

namespace WIR {
class WIR_VirtualRegister;
class TC_ARegV;
class TC_DRegV;
}

class LLIR_Register;


//
// Header section
//

struct BitfieldBounds
{
  unsigned char bitOffset;
  unsigned char bitLength;
};


struct BitfieldInfo
{
  long byteOffset;
  BitfieldBounds bounds;

  BitfieldInfo( void );
};


/*!
  @brief getBitfieldByteOffset returns the offset of the word in which the
         bitfield resides seen from the start of the composed type object.

  @param[in] parentType A const reference to the parent composed type.
  @param[in] bitfield A const reference to the bitfield symbol.
  @return An integer denoting the byte offset of the bitfield within the parent
          composed type.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
int getBitfieldByteOffset( const IR_ComposedType &parentType,
                           const IR_Symbol &bitfield );


/*!
  @brief getBitfieldBitOffset returns the bit offset of 'bitfield' inside the
         word in which it is enclosed in the surrounding composed type.

  @param[in] parentType A const reference to the parent composed type.
  @param[in] bitfield A const reference to the bitfield symbol.
  @return An integer denoting the bit offset of the bitfield within the parent
          composed type's word.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
int getBitfieldBitOffset( const IR_ComposedType &parentType,
                          const IR_Symbol &bitfield );


/*!
  @brief getBitfieldLength returns the length (in bits) of a bitfield.

  @param[in] bitfield A const reference to the bitfield symbol.
  @return An integer denoting the bit size of the bitfield.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
int getBitfieldLength( const IR_Symbol &bitfield );


/*!
  @brief readValueFromBitfieldCost computes the costs for reading a bitfield
         value.

  @return An approximation of the costs for the instructions generated by
          readValueFromBitfield().

  The exact costs cannot be computed since they depend on parameters that are
  only available when executing the action parts of the rule-set.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
COST readValueFromBitfieldCost( void );


/*!
  @brief readValueFromBitfield reads the bitfield value beginning in word
         [regBase] + byteOffsetFromBase into register reg using the given
         bitfield parameters.

  @param[in] reg A pointer to an LLIR target register.
  @param[in] regBase A pointer to an LLIR base address register.
  @param[in] r A const reference to a virtual TriCore data register holding the
               loaded bitfield value.
  @param[in] bReg A const reference to a virtual TriCore address register
                  holding the bitfield's base address.
  @param[in] t A const reference to the IR bitfield type.
  @param[in] byteOffsetFromBase An integer holding the bitfield accesses byte
                                offset.
  @param[in] bitfieldOffset An integer holding the offset of the bitfield in
                            bits.
  @param[in] bitfieldLength An integer holding the length of the bitfield in
                            bits.
  @param[in] exp A const pointer to an IR expression to be used for the
                 generation of debug information.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
void readValueFromBitfield( LLIR_Register *reg, LLIR_Register *regBase,
                            const WIR::TC_DRegV &r, const WIR::TC_ARegV &bReg,
                            const IR_BitfieldType &t,
                            const int byteOffsetFromBase,
                            const int bitfieldOffset, const int bitfieldLength,
                            const IR_Exp *exp );


/*!
  @brief writeValueToBitfieldCost computes the costs for writing a bitfield
         value.

  @return An approximation of the costs for the instructions generated by
          writeValueToBitfield().

  The exact costs cannot be computed since they depend on parameters that are
  only available when executing the action parts of the rule-set.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
COST writeValueToBitfieldCost( void );


/*!
  @brief writeValueToBitfield writes the value from register reg to the bitfield
         in word [regBase] + byteOffsetFromBase using the given bitfield
         parameters.

  @param[in] reg A pointer to an LLIR target register.
  @param[in] regBase A pointer to an LLIR base address register.
  @param[in] pTime A specifier denoting whether pre- or post-increment
                   addressing will be applied or not.
  @param[in] r A const reference to a virtual TriCore data register holding the
               value to be written.
  @param[in] bReg A const reference to a virtual TriCore address register
                  holding the bitfield's base address.
  @param[in] t A const reference to the IR bitfield type.
  @param[in] byteOffsetFromBase An integer holding the bitfield accesses byte
                                offset.
  @param[in] bitfieldOffset An integer holding the offset of the bitfield in
                            bits.
  @param[in] bitfieldLength An integer holding the length of the bitfield in
                            bits.
  @param[in] exp A const pointer to an IR expression to be used for the
                 generation of debug information.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
void writeValueToBitfield( LLIR_Register *reg, LLIR_Register *regBase,
                           AddressModification::ModTime pTime,
                           const WIR::WIR_VirtualRegister &r,
                           const WIR::TC_ARegV &bReg, const IR_BitfieldType &t,
                           const int byteOffsetFromBase,
                           const int bitfieldOffset, const int bitfieldLength,
                           const IR_Exp *exp );

#endif  // _TC179x_COMPOSED_STRUCT_BITFIELD_INCL_H_
