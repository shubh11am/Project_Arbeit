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


#ifndef _ARM_COMPOSED_STRUCT_BITFIELD_INCL_H_
#define _ARM_COMPOSED_STRUCT_BITFIELD_INCL_H_

//
// Include section
//

// Include local headers
#include <arm7/armaddressmodification.h>
#include <arm7/cs_arm7.h>

//
// Class forward declarations
//

class IR_BitfieldType;
class IR_ComposedType;
class IR_Exp;
class IR_Symbol;


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

LLIR_Register* readFromBitfield( LLIR_Register *address,
                                 const struct BitfieldInfo &info,
                                 const IR_Type &type, IR_Exp *exp );

COST insertValueIntoBitfieldCost();

LLIR_Register* insertValueIntoBitfield( LLIR_Register *container,
                                        const struct BitfieldBounds &bounds,
                                        LLIR_Register *source, IR_Exp* exp );

COST insertConstantIntoBitfieldCOST( const struct BitfieldBounds &bounds,
                                     int constant );

LLIR_Register* insertConstantIntoBitfield( LLIR_Register *container,
                                           const struct BitfieldBounds &bounds,
                                           int constant, IR_Exp* exp );
#endif
