/*

   This source file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Compiler Framework

   and is property of its respective copyright holder. It must neither be used
   nor published even in parts without explicit written permission.

   Copyright 2021 - 2022

   Hamburg University of Technology (TUHH)
   Institute of Embedded Systems
   21071 Hamburg
   Germany

   http://www.tuhh.de/es/esd/research/wcc

*/

/*!
  @file rv32addressmodification.cc
  @brief This file implements the RISC-V RV32 class describing address register
         modifications.
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wcc.h>
#endif

// Include boost headers
#include <boost/current_function.hpp>

// Include WIR headers
#include <wir/wir.h>
#include <arch/riscv/rv32imc.h>

// Include ICD headers
#include <icd-c.h>

// Include libuseful headers
#include <libuseful/debugmacros.h>
#include <libuseful/io.h>

// Include local headers
#include <rv32/rv32addressmodification.h>
#include <rv32/rv32instructionfactory.h>
#include <rv32/rv32registrar.h>
#include <rv32/rv32_incl.h>


//
// Code section
//

using namespace std;
using namespace WIR;


namespace RV32 {

//
// Public class methods
//

/*
  Constructor initializing an empty address modification.
*/
RV32_AddressModification::RV32_AddressModification( void ) :
  AddressModification {}
{
  DSTART( "RV32_AddressModification::RV32_AddressModification()" );
};


/*
  Constructor for case with local base register and register offset.
*/
RV32_AddressModification::RV32_AddressModification( WIR::WIR_BaseRegister &reg,
                                                  WIR::WIR_BaseRegister &oReg,
                                                  IR_Type *t, ModTime mt,
                                                  ModOper mo, bool bo,
                                                  bool dr ) :
  AddressModification { nullptr,  &(const_cast<WIR_BaseRegister &>(reg)), nullptr,  &(const_cast<WIR_BaseRegister &>(oReg)), t, bo, mt, mo }
{
  DSTART( BOOST_CURRENT_FUNCTION );
};


/*
  Constructor for register+offset-based address modifications.
*/
RV32_AddressModification::RV32_AddressModification( const WIR::WIR_BaseRegister &r,
                                                    long o, IR_Type *t, bool bo,
                                                    ModOper mo ) :
  AddressModification {
    nullptr, &(const_cast<WIR_BaseRegister &>( r )), o, t, bo, ModTime::NONE,
    mo }
{
  DSTART( BOOST_CURRENT_FUNCTION );
};


/*
  Constructor for data label-based address modifications.
*/
RV32_AddressModification::RV32_AddressModification( const std::string &l, const WIR::WIR_Data &d,
                                                    IR_Type *t, bool bo ) :
  AddressModification { l, d, t, bo }
{
  DSTART( BOOST_CURRENT_FUNCTION );
};


/*
  Constructor for function label-based address modifications.
*/
RV32_AddressModification::RV32_AddressModification( const WIR::WIR_Function &f,
                                                IR_Type *t, bool bo ) :
  AddressModification { "", f, t, bo }
{
  DSTART( BOOST_CURRENT_FUNCTION );
};


/*
  Constructor for global/stack-based base register and integer offset.
*/
RV32_AddressModification::RV32_AddressModification( RV32_LValue *l, long o,
                                                IR_Type *t, bool bo,
                                                ModTime mt, ModOper mo ) :
  AddressModification { l, o, t, bo, mt, mo }
{
  DSTART(
    "RV32_AddressModification::RV32_AddressModification(RV32_LValue*, long int, "
    "IR_Type*, bool, AddressModification::ModTime, "
    "AddressModification::ModOper)" );
};


/*
  Copy constructor.
*/
RV32_AddressModification::RV32_AddressModification( const RV32_AddressModification &am ) :
  AddressModification { am }
{
  DSTART(
    "RV32_AddressModification::RV32_AddressModification(const "
    "RV32_AddressModification&)" );
};


/*
  Destructor.
*/
RV32_AddressModification::~RV32_AddressModification( void )
{
  DSTART( "virtual RV32_AddressModification::~RV32_AddressModification()" );
};


/*
  Assignment operator.
*/
RV32_AddressModification & RV32_AddressModification::operator = ( const RV32_AddressModification &am )
{
  DSTART( BOOST_CURRENT_FUNCTION );

  AddressModification::operator = ( am );

  return( *this );
};


/*
  applyModification applies the pointer arithmetics encoded by this object and
  returns the calculated register.
*/
RV_RegV *RV32_AddressModification::applyModification( IR_Exp *exp )
{
  DSTART( BOOST_CURRENT_FUNCTION );

  // You shouldn't use applyModification for already modified memory accesses.
  // Either you decide to resolve an address modification by performing pointer
  // arithmetics at runtime (the below code), or you decide to load/store
  // performing the arithmetics on the fly (as part of the LD/ST instructions).
  ufAssert( !mModApplied );

  if ( !mOReg && !mOffset ){

    // Neither an offset register nor an integer offset is provided so that
    // effectively, there is no address arithmetics to be performed. We thus
    // simply return the base address registers.
    return( &(dynamic_cast<RV_RegV &>( getBaseReg() )) );}

  // Since no pre/post-increment/decrement can be applied for RISC-V, we just
  // add base register and offset and return this.
  auto &r = RVINSTRUCTIONS.createReg();
  // if ( mModTime == ModTime::PRE ) {
  //         // Pre-indexed. The base address is the return register.
  //         r = dynamic_cast<RV_RegV &>( getBaseReg() );}

  if ( !mOReg ) {
    //A integer offset is applied.

    RVINSTRUCTIONS.insertADDI(
      r, dynamic_cast<RV_RegV &>( getBaseReg() ), getByteOffset(), exp );
  } else {
      // A register offset is applied.
      // If the typeSize is a power of two, the offset can be applied with a
      // single ADD/SUB instruction. Otherwise, things get expensive.
      int typeSize = getOffsetFactor();

      if ( ( typeSize > 0 ) && (typeSize == 0) ) {
        // Determine the exponent (i.e., perform log2(typeSize)).
        int e = 0;
        for ( ; typeSize != 1; typeSize >>= 1, ++e ) ;

        if ( mModOper == ModOper::ADD )
            RVINSTRUCTIONS.insertADD(
            r, dynamic_cast<RV_RegV &>( getBaseReg() ), dynamic_cast<RV_RegV &>( *mOReg ), exp);
          else
            RVINSTRUCTIONS.insertSUB(
            r, dynamic_cast<RV_RegV &>( getBaseReg() ), dynamic_cast<RV_RegV &>( *mOReg ), exp);

      } else {
        // This is the expensive case. First, load the typeSize into a register.
        RVINSTRUCTIONS.insertMOVConstant( r, typeSize, exp);

        // Then, perform the multiplication at runtime.
        RVINSTRUCTIONS.insertMUL( r, r, dynamic_cast<RV_RegV &>( *mOReg ), exp );


        if ( mModOper == ModOper::ADD )
            RVINSTRUCTIONS.insertADD( r, dynamic_cast<RV_RegV &>( getBaseReg() ), r, exp );
          else
            RVINSTRUCTIONS.insertSUB( r, dynamic_cast<RV_RegV &>( getBaseReg() ), r, exp );
    }
  }

  // If the base address is stored in memory (i.e., is an lvalue) and was
  // modified with this LEA instruction, we have to update its value in
  // memory.
  if ( mBaseLValueValid )
    dynamic_cast<RV32_LValue *>( mBaseLValue )->storeBack(
      exp, &(dynamic_cast<RV_RegV &>( getBaseReg() )) );

  return( &r );
};


/*
  applyModificationCost computes the costs for applying the pointer arithmetics
  encoded by this object
*/
COST RV32_AddressModification::applyModificationCost( void )
{
  DSTART( BOOST_CURRENT_FUNCTION );

  return(
    RV32I::OperationFormat::RC20_1.getSize() +
    RV32I::OperationFormat::RRC12_1.getSize() +
    RV32I::OperationFormat::RRR_1.getSize() );
};


/*
  createLoad generates code that loads the value from the effective address
  encoded by this object into the given register.
*/
void RV32_AddressModification::createLoad( WIR::WIR_BaseRegister *dst,
                                           IR_Exp *exp )
{
  DSTART( BOOST_CURRENT_FUNCTION );

  long offset = getByteOffset();

  // Determine the base register.
  auto &bReg = dynamic_cast<RV_RegV &>( getBaseReg() );

  // Load a RISC-V register.
  auto &vReg = dynamic_cast<RV_RegV &>( *dst );

  switch ( mBaseType->getType() ) {
    case IR_Type::CHAR: {
      RVINSTRUCTIONS.insertLB( vReg, offset, bReg, exp );
      break;
    }

    case IR_Type::UNSIGNED_CHAR:
    case IR_Type::BOOL: {
      RVINSTRUCTIONS.insertLBU( vReg, offset, bReg, exp );
      break;
    }

    case IR_Type::SHORT: {
      RVINSTRUCTIONS.insertLH( vReg, offset, bReg, exp );
      break;
    }

    case IR_Type::UNSIGNED_SHORT: {
      RVINSTRUCTIONS.insertLHU( vReg, offset, bReg, exp );
      break;
    }

    default: {
      RVINSTRUCTIONS.insertLW( vReg, offset, bReg, exp );
      break;
    }
  }
};


/*
  createStore generates code that stores the given register to the effective
  address encoded by this object.
*/
void RV32_AddressModification::createStore( WIR::WIR_BaseRegister *src,
                                            IR_Exp *exp, bool bfAccess,
                                            unsigned char bfOffset,
                                            unsigned char bfLength )
{
  DSTART( BOOST_CURRENT_FUNCTION );

  bool useBaseAddressing = containsAReg() || mBaseLValueValid;
  long offset = getByteOffset();

  if ( bfAccess ) {
    // Writes to bit-fields are handled separately, because they are
    // conceptually different from the accesses to standard data types.

    // TODO, see TriCore implementation!
    (void) bfOffset;
    (void) bfLength;
  } else {
    // Access to a non-bitfield type.

    switch ( mBaseType->getType() ) {
      case IR_Type::BOOL:
      case IR_Type::CHAR:
      case IR_Type::UNSIGNED_CHAR: {
        auto &r = dynamic_cast<RV_RegV &>( *src );

        if ( useBaseAddressing ) {
          // Use base+offset addressing.
          auto &bReg = dynamic_cast<RV_RegV &>( getBaseReg() );

          if ( ( mModTime == ModTime::NONE ) || isModificationApplied() )
            RVINSTRUCTIONS.insertSB( r, offset, bReg, exp );
          else
            ufAssertT(
              false, "Pre-/Post-increment semantics not yet implemented!" );
        } else {
          // Use label-based addressing.
          ufAssertT(
            false, "Label-based address modifications not yet implemented!" );
        }

        break;
      }

      case IR_Type::SHORT:
      case IR_Type::UNSIGNED_SHORT: {
        auto &r = dynamic_cast<RV_RegV &>( *src );

        if ( useBaseAddressing ) {
          // Use base+offset addressing.
          auto &bReg = dynamic_cast<RV_RegV &>( getBaseReg() );

          if ( ( mModTime == ModTime::NONE ) || isModificationApplied() )
            RVINSTRUCTIONS.insertSH( r, offset, bReg, exp );
          else
            ufAssertT(
              false, "Pre-/Post-increment semantics not yet implemented!" );
        } else {
          // Use label-based addressing.
          ufAssertT(
            false, "Label-based address modifications not yet implemented!" );
        }

        break;
      }

      case IR_Type::INT:
      case IR_Type::UNSIGNED_INT:
      case IR_Type::LONG:
      case IR_Type::UNSIGNED_LONG:
      case IR_Type::FLOAT: {
        auto &r = dynamic_cast<RV_RegV &>( *src );

        if ( useBaseAddressing ) {
          // Use base+offset addressing.
          auto &bReg = dynamic_cast<RV_RegV &>( getBaseReg() );

          if ( ( mModTime == ModTime::NONE ) || isModificationApplied() )
            RVINSTRUCTIONS.insertSW( r, offset, bReg, exp );
          else
            ufAssertT(
              false, "Pre-/Post-increment semantics not yet implemented!" );
        } else {
          // Use label-based addressing.
          auto &reg = RVINSTRUCTIONS.createReg();

          ufAssertT( mLabel != nullptr, "Missing mLable!" );
          RVINSTRUCTIONS.insertLUI(
            reg, dynamic_cast<WIR_Data &>( *mLabel ), exp );
          RVINSTRUCTIONS.insertSW(
            r, dynamic_cast<WIR_Data &>( *mLabel ), reg, exp );
        }

        break;
      }

      case IR_Type::LONG_LONG:
      case IR_Type::UNSIGNED_LONG_LONG:
      case IR_Type::LONG_DOUBLE:
      case IR_Type::DOUBLE: {
        ufAssertT( false, "64-bit data types not yet implemented!" );

        break;
      }

      case IR_Type::ARRAY:
      case IR_Type::POINTER: {
        auto &r = dynamic_cast<RV_RegV &>( *src );

        if ( useBaseAddressing ) {
          // Use base+offset addressing.
          auto &bReg = dynamic_cast<RV_RegV &>( getBaseReg() );

          if ( ( mModTime == ModTime::NONE ) || isModificationApplied() )
            RVINSTRUCTIONS.insertSW( r, offset, bReg, exp );
          else
            ufAssertT(
              false, "Pre-/Post-increment semantics not yet implemented!" );
        } else {
          // Use label-based addressing.
          auto &reg = RVINSTRUCTIONS.createReg();

          ufAssertT( mLabel != nullptr, "Missing mLable!" );
          RVINSTRUCTIONS.insertLUI(
            reg, dynamic_cast<WIR_Data &>( *mLabel ), exp );
          RVINSTRUCTIONS.insertSW(
            r, dynamic_cast<WIR_Data &>( *mLabel ), reg, exp );
        }

        break;
      }

      default:
        break;
    }
  }

  // If the base address is stored in memory (i.e., is an lvalue) and was
  // modified with this ST instruction, we have to update its value in memory.
  if ( mBaseLValueValid &&
       ( ( mModTime == ModTime::PRE ) || ( mModTime == ModTime::POST ) ) )
    dynamic_cast<RV32_LValue *>( mBaseLValue )->storeBack(
      exp, &(dynamic_cast<RV_RegV &>( getBaseReg() )) );
};


/*
  createStoreCost computes the costs for a store of a register to the effective
  address encoded by this object.
*/
COST RV32_AddressModification::createStoreCost( const IR_Exp *exp )
{
  DSTART(
    "static COST RV32_AddressModification::createStoreCost(const IR_Exp*)" );

  auto &t = exp->getType();
  COST cost = 0;

  // TODO: The costs for label-based write-back are missing here, but it seems
  //       almost impossible to determine in the costs part whether a label-
  //       based write-back has to be performed, as this is only clear when the
  //       real lvalue object was passed to the action part.
  switch ( t.getType() ) {

    case IR_Type::BOOL:
    case IR_Type::CHAR:
    case IR_Type::UNSIGNED_CHAR:
    case IR_Type::SHORT:
    case IR_Type::UNSIGNED_SHORT:
    case IR_Type::INT:
    case IR_Type::UNSIGNED_INT:
    case IR_Type::LONG:
    case IR_Type::UNSIGNED_LONG:
    case IR_Type::FLOAT: {
      cost = RV32I::OperationFormat::RC12R_2.getSize();
      break;
    }

    case IR_Type::LONG_LONG:
    case IR_Type::UNSIGNED_LONG_LONG:
    case IR_Type::LONG_DOUBLE:
    case IR_Type::DOUBLE: {
      ufAssertT( false, "64-bit data types not yet implemented!" );

      break;
    }

    case IR_Type::ARRAY: {
      ufAssertT( false, "Array data types not yet implemented!" );

      break;
    }

    case IR_Type::POINTER: {
      //ufAssertT( false, "Pointer data types not yet implemented!" );
      cost = RV32I::OperationFormat::RC12R_2.getSize();
      break;
    }

    default:
      break;

  }

  return( cost );
};


/*
  getByteOffset returns the byte offset that will be applied by the
  modification, depending on the actual offset, the size of the base IR type,
  whether a subtraction or addition is performed, or whether the modification
  has already been done or not.
*/
long RV32_AddressModification::getByteOffset( void ) const
{
  DSTART( BOOST_CURRENT_FUNCTION );

  // Depending on whether this address modification represents a subtraction or
  // an addition, mOffset is multiplied by -1 or 1, resp. If furthermore the
  // given memory location was already loaded, we must adapt the lvalue, because
  // in that case, the base address was already altered by the preceding load
  // instruction. Thus, we apply the inverted offset in order to write to the
  // same location again, which again is realized by another multiplication by
  // -1 or 1. Finally, if the address modification represents a type-wise
  // offset, the IR type-dependent scaling factor also needs to be applied.

  long offset =
    mOffset * ( ( mModOper == ModOper::SUB ) ? -1 : 1 ) *
    ( isModificationApplied() ? -1 : 1 );

  return( mIsByteOffset ? offset : offset * getOffsetFactor() );
};


//
// Protected class methods
//

/*
  clone creates a copy of an address modification object.

  clone just calls the corresponding copy constructor.
*/
AddressModification *RV32_AddressModification::clone( void ) const
{
  DSTART(
    "virtual AddressModification* RV32_AddressModification::clone() const" );

  return( new RV32_AddressModification( *this ) );
};


/*
  getOffsetFactor computes the factor by which the offset needs to be scaled
  according to a RISC-V address modification's base type.
*/
int RV32_AddressModification::getOffsetFactor( void ) const
{
  DSTART( BOOST_CURRENT_FUNCTION );

  if ( mIsByteOffset )
    // If the offset is a byteOffset, the scaling factor is always 1.
    return( 1 );

  if ( mBaseType ) {
    // If a baseType is given (it should be), then its size is the factor.
    // Exception: Base type void. In this case, it shall not be dereferenced,
    // but its offset is that of a char, i.e., 1 byte.
    if ( mBaseType->getType() == IR_Type::VOID )
      return( 1 );
    else
      return( computeSizeOf( mBaseType ) );
  } else
    // There should always be a baseType.
    ufAssertT(
      false, "No baseType specified in the RV32_AddressModification." );
};

}       // namespace RV32
