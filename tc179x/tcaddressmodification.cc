/*

   This source file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Compiler Framework

   and is property of its respective copyright holder. It must neither be used
   nor published even in parts without explicit written permission.

   Copyright 2020 - 2022

   Hamburg University of Technology (TUHH)
   Institute of Embedded Systems
   21071 Hamburg
   Germany

   http://www.tuhh.de/es/esd/research/wcc

*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wcc.h>
#endif

// Include WIR headers
#include <wir/wir.h>
#include <arch/tricore/tc131.h>

// Include ICD headers
#include <icd-c.h>
#include <llir3/llir3.h>

// Include libuseful headers
#include <libuseful/debugmacros.h>

// Include local headers
#include <tc179x/composed_struct_bitfield_incl.h>
#include <tc179x/instructionfactory.h>
#include <tc179x/registrar.h>
#include <tc179x/stack.h>
#include <tc179x/tc_incl.h>
#include <tc179x/tcaddressmodification.h>


//
// Code section
//


using namespace std;
using namespace WIR;


//
// Public class methods
//

/*
  Constructor initializing an empty address modification.
*/
TC_AddressModification::TC_AddressModification( void ) :
  AddressModification {}
{
  DSTART( "TC_AddressModification::TC_AddressModification()" );
};


/*
  Constructor for register+offset-based address modifications.
*/
TC_AddressModification::TC_AddressModification( LLIR_Register *reg,
                                                const WIR::WIR_BaseRegister &r,
                                                long o, IR_Type *t, bool bo,
                                                ModTime mt, ModOper mo ) :
  AddressModification {
    reg, &(const_cast<WIR_BaseRegister &>( r )), o, t, bo, mt, mo }
{
  DSTART(
    "TC_AddressModification::TC_AddressModification(LLIR_Register*, const WIR_BaseRegister&, long int, IR_Type*, bool, AddressModification::ModTime, AddressModification::ModOper)" );
};


/*
  Constructor for data label-based address modifications.
*/
TC_AddressModification::TC_AddressModification( const std::string &l,
                                                const WIR::WIR_Data &d,
                                                IR_Type *t, bool bo ) :
  AddressModification { l, d, t, bo }
{
  DSTART(
    "TC_AddressModification::TC_AddressModification(const string&, const WIR_Data&, IR_Type*, bool)" );
};


/*
  Constructor for function label-based address modifications.
*/
TC_AddressModification::TC_AddressModification( const std::string &l,
                                                const WIR::WIR_Function &f,
                                                IR_Type *t, bool bo ) :
  AddressModification { l, f, t, bo }
{
  DSTART(
    "TC_AddressModification::TC_AddressModification(const string&, const WIR_Function&, IR_Type*, bool)" );
};


/*
  Constructor for global/stack-based base register and integer offset.
*/
TC_AddressModification::TC_AddressModification( TC_LValue *l, long o,
                                                IR_Type *t, bool bo,
                                                ModTime mt, ModOper mo ) :
  AddressModification { l, o, t, bo, mt, mo }
{
  DSTART(
    "TC_AddressModification::TC_AddressModification(TC_LValue*, long int, IR_Type*, bool, AddressModification::ModTime, AddressModification::ModOper)" );
};


/*
  Copy constructor.
*/
TC_AddressModification::TC_AddressModification( const TC_AddressModification &am ) :
  AddressModification { am }
{
  DSTART(
    "TC_AddressModification::TC_AddressModification(const TC_AddressModification&)" );
};


/*
  Destructor.
*/
TC_AddressModification::~TC_AddressModification( void )
{
  DSTART( "virtual TC_AddressModification::~TC_AddressModification()" );
};


/*
  Assignment operator.
*/
TC_AddressModification & TC_AddressModification::operator = ( const TC_AddressModification &am )
{
  DSTART(
    "TC_AddressModification& TC_AddressModification::operator=(const TC_AddressModification&)" );

  AddressModification::operator = ( am );

  return( *this );
};


/*
  applyModification applies the pointer arithmetics encoded by this object.
*/
pair<LLIR_Register *, TC_ARegV *> TC_AddressModification::applyModification( IR_Exp *exp )
{
  DSTART(
    "pair<LLIR_Register*, TC_ARegV*> TC_AddressModification::applyModification(IR_Exp*)" );

  // You shouldn't use applyModification for already modified memory accesses.
  // Either you decide to resolve an address modification by performing pointer
  // arithmetics at runtime (the below code), or you decide to load/store
  // performing the arithmetics on the fly (as part of the LD/ST instructions).
  ufAssert( !mModApplied );

  if ( !mORegister && !mOReg && !mOffset )
    // Neither an offset register nor an integer offset is provided so that
    // effectively, there is no address arithmetics to be performed. We thus
    // simply return the base address registers.
    return(
      make_pair(
        getBaseRegister(), &(dynamic_cast<TC_ARegV &>( getBaseReg() )) ) );

  // From here on, only integer offsets are supported for TriCore. Determine the
  // actual byte offset.
  auto o = getByteOffset();

  LLIR_Register *reg = nullptr;
  TC_ARegV *r = nullptr;

  if ( mModTime == ModTime::NONE ) {
    // No pre/post-increment/decrement is applied. We just add base register and
    // offset and return this.

    // LLIR
    reg = TCINSTRUCTIONS.CreateRegister( "", true );
    TCINSTRUCTIONS.insertLEA( reg, OPER_BASE, getBaseRegister(), o, exp );

    // WIR
    r = &(TCINSTRUCTIONS.createAReg());
    TCINSTRUCTIONS.insertLEA(
      *r, dynamic_cast<TC_ARegV &>( getBaseReg() ), o, exp );
  } else

  if ( mModTime == ModTime::PRE ) {
    // Pre-increment/decrement is applied. We directly add the offset to the
    // base register and return it.

    // LLIR
    reg = getBaseRegister();
    TCINSTRUCTIONS.insertLEA( reg, OPER_BASE, reg, o, exp );

    // WIR
    r = &(dynamic_cast<TC_ARegV &>( getBaseReg() ));
    TCINSTRUCTIONS.insertLEA( *r, *r, o, exp );

    // If the base address is stored in memory (i.e., is an lvalue) and was
    // modified with this LEA instruction, we have to update its value in
    // memory.
    if ( mBaseLValueValid )
      dynamic_cast<TC_LValue *>( mBaseLValue )->storeBack( exp, reg, r );
  } else {
    // Post-increment/decrement is applied. We copy the original base to a new
    // register that is returned here and then directly add the offset to the
    // original base.

    // LLIR
    reg = TCINSTRUCTIONS.CreateRegister( "", true );
    TCINSTRUCTIONS.insertMOV_AA( reg, getBaseRegister(), exp );
    TCINSTRUCTIONS.insertLEA(
      getBaseRegister(), OPER_BASE, getBaseRegister(), o, exp );

    // WIR
    r = &(TCINSTRUCTIONS.createAReg());
    TCINSTRUCTIONS.insertMOV_AA(
      *r, dynamic_cast<TC_ARegV &>( getBaseReg() ), exp );
    TCINSTRUCTIONS.insertLEA(
      dynamic_cast<TC_ARegV &>( getBaseReg() ),
      dynamic_cast<TC_ARegV &>( getBaseReg() ), o, exp );

    // If the base address is stored in memory (i.e., is an lvalue) and was
    // modified with this LEA instruction, we have to update its value in
    // memory.
    if ( mBaseLValueValid )
      dynamic_cast<TC_LValue *>( mBaseLValue )->storeBack(
        exp, getBaseRegister(), &(dynamic_cast<TC_ARegV &>( getBaseReg() )) );
  }

  return( make_pair( reg, r ) );
};


/*
  applyModificationCost computes the costs for applying the pointer arithmetics
  encoded by this object
*/
COST TC_AddressModification::applyModificationCost( void )
{
  DSTART( "static COST TC_AddressModification::applyModificationCost()" );

  return(
    2 * TC13::OperationFormat::SAA_1.getSize() +
    TC13::OperationFormat::AAC16.getSize() +
    TC13::OperationFormat::AAC16BOA.getSize() );
};


/*
  createLoad generates code that loads the value from the effective address
  encoded by this object into the given register.
*/
void TC_AddressModification::createLoad( LLIR_Register *dest,
                                         WIR::WIR_BaseRegister *dst,
                                         IR_Exp *exp )
{
  DSTART(
    "void TC_AddressModification::createLoad(LLIR_Register*, WIR_BaseRegister*, IR_Exp*)" );

  long offset = getByteOffset();
  auto mode = getMemoryAccessMode();

  // Determine the base registers.
  auto *baseReg = getBaseRegister();
  auto &bReg = dynamic_cast<TC_ARegV &>( getBaseReg() );

  if ( mModTime == AddressModification::ModTime::NONE ) {
    // No modification is applied.

    if ( dst->getType() == TC13::RegisterType::aReg ) {
      // Load an address register.
      auto &aReg = dynamic_cast<TC_ARegV &>( *dst );

      // LLIR
      insertLD_A( dest, baseReg, offset, exp );

      // WIR
      insertLD_A( aReg, bReg, offset, exp );
    } else

    if ( dst->getType() == TC13::RegisterType::dReg ) {
      // Load a data register.
      auto &dReg = dynamic_cast<TC_DRegV &>( *dst );

      switch ( mBaseType->getType() ) {
        case IR_Type::CHAR: {
          // LLIR
          insertLD_B( dest, baseReg, offset, exp );

          // WIR
          insertLD_B( dReg, bReg, offset, exp );

          break;
        }

        case IR_Type::UNSIGNED_CHAR:
        case IR_Type::BOOL: {
          // LLIR
          insertLD_BU( dest, baseReg, offset, exp );

          // WIR
          insertLD_BU( dReg, bReg, offset, exp );

          break;
        }

        case IR_Type::SHORT: {
          // LLIR
          insertLD_H( dest, baseReg, offset, exp );

          // WIR
          insertLD_H( dReg, bReg, offset, exp );

          break;
        }

        case IR_Type::UNSIGNED_SHORT: {
          // LLIR
          insertLD_HU( dest, baseReg, offset, exp );

          // WIR
          insertLD_HU( dReg, bReg, offset, exp );

          break;
        }

        default: {
          // LLIR
          insertLD_W( dest, baseReg, offset, exp );

          // WIR
          insertLD_W( dReg, bReg, offset, exp );

          break;
        }
      }
    } else

    if ( dst->getType() == TC13::RegisterType::eReg ) {
      // Load an extended data register.
      auto &eReg = dynamic_cast<TC_ERegV &>( *dst );

      // LLIR
      insertLD_D( dest, baseReg, offset, exp );

      // WIR
      insertLD_D( eReg, bReg, offset, exp );
    } else
      ufAssertT( false, "Unsupported register type." );

  } else {
    // Pre-/Post-modification is applied.
    const TC13::AddressingMode &am =
      ( mModTime == ModTime::PRE ) ?
        TC13::AddressingMode::pre : TC13::AddressingMode::post;

    if ( dst->getType() == TC13::RegisterType::aReg ) {
      // Load an address register.
      auto &aReg = dynamic_cast<TC_ARegV &>( *dst );

      // LLIR
      insertLD_A( dest, mode, baseReg, offset, exp );

      // WIR
      insertLD_A( aReg, am, bReg, offset, exp );
    } else

    if ( dst->getType() == TC13::RegisterType::dReg ) {
      // Load a data register.
      auto &dReg = dynamic_cast<TC_DRegV &>( *dst );

      switch ( mBaseType->getType() ) {
        case IR_Type::CHAR: {
          // LLIR
          insertLD_B( dest, mode, baseReg, offset, exp );

          // WIR
          insertLD_B( dReg, am, bReg, offset, exp );

          break;
        }

        case IR_Type::UNSIGNED_CHAR:
        case IR_Type::BOOL: {
          // LLIR
          insertLD_BU( dest, mode, baseReg, offset, exp );

          // WIR
          insertLD_BU( dReg, am, bReg, offset, exp );

          break;
        }

        case IR_Type::SHORT: {
          // LLIR
          insertLD_H( dest, mode, baseReg, offset, exp );

          // WIR
          insertLD_H( dReg, am, bReg, offset, exp );

          break;
        }

        case IR_Type::UNSIGNED_SHORT: {
          // LLIR
          insertLD_HU( dest, mode, baseReg, offset, exp );

          // WIR
          insertLD_HU( dReg, am, bReg, offset, exp );

          break;
        }

        default: {
          // LLIR
          insertLD_W( dest, mode, baseReg, offset, exp );

          // WIR
          insertLD_W( dReg, am, bReg, offset, exp );

          break;
        }
      }
    } else

    if ( dst->getType() == TC13::RegisterType::eReg ) {
      // Load an extended data register.
      auto &eReg = dynamic_cast<TC_ERegV &>( *dst );

      // LLIR
      insertLD_D( dest, mode, baseReg, offset, exp );

      // WIR
      insertLD_D( eReg, am, bReg, offset, exp );
    } else
      ufAssertT( false, "Unsupported register type." );

  }

  if ( mModTime != ModTime::NONE )
    setModificationApplied();

  // If the base address is stored in memory (i.e., is an lvalue) and was
  // modified with this LD instruction, we have to update its value in memory.
  if ( mBaseLValueValid &&
       ( ( mModTime == ModTime::PRE ) || ( mModTime == ModTime::POST ) ) )
    dynamic_cast<TC_LValue *>( mBaseLValue )->storeBack( exp, baseReg, &bReg );
};


/*
  createStore generates code that stores the given register to the effective
  address encoded by this object.
*/
void TC_AddressModification::createStore( LLIR_Register *source,
                                          WIR::WIR_BaseRegister *src,
                                          IR_Exp *exp, bool bfAccess,
                                          unsigned char bfOffset,
                                          unsigned char bfLength )
{
  DSTART(
    "void TC_AddressModification::createStore(LLIR_Register*, WIR_BaseRegister*, IR_Exp*, bool, unsigned char, unsigned char)" );

  bool useBaseAddressing = containsAReg() || mBaseLValueValid;
  auto mode =
    ( useBaseAddressing && ( mModTime != ModTime::NONE ) &&
      !isModificationApplied() ) ? getMemoryAccessMode() : OPER_BASE;
  long offset = getByteOffset();

  if ( bfAccess ) {
    // Writes to bit-fields are handled separately, because they are
    // conceptually different from the accesses to standard data types.
    auto *bfType = dynamic_cast<IR_BitfieldType *>( mBaseType );
    ufAssertT( bfType, "Invalid type!" );

    auto &bReg = dynamic_cast<TC_ARegV &>( *mAReg );

    writeValueToBitfield(
      source, mARegister, mModTime, dynamic_cast<WIR_VirtualRegister &>( *src ),
      bReg, *bfType, offset, bfOffset, bfLength, exp );
  } else {
    // Access to a non-bitfield type.

    // LLIR
    LLIR_Register *iaddr = TCINSTRUCTIONS.CreateRegister( "", true );

    switch ( mBaseType->getType() ) {
      case IR_Type::BOOL:
      case IR_Type::CHAR:
      case IR_Type::UNSIGNED_CHAR: {
        if ( useBaseAddressing )
          TCINSTRUCTIONS.insertST_B(
            mode, getBaseRegister(), offset, source, exp );
        else {
          // The TriCore assembler does not accept ST.B with label offset. So,
          // we must load the address first and then access it with offset zero.
          TCINSTRUCTIONS.insertMOVH_A( iaddr, OPER_LAB_HI, mDataLabel, exp );
          TCINSTRUCTIONS.insertLEA(
            iaddr, OPER_BASE, iaddr, OPER_LAB_LO, mDataLabel, exp );
          TCINSTRUCTIONS.insertST_B( OPER_BASE, iaddr, 0, source, exp );
        }
        break;
      }

      case IR_Type::SHORT:
      case IR_Type::UNSIGNED_SHORT: {
        if ( useBaseAddressing )
          TCINSTRUCTIONS.insertST_H(
            mode, getBaseRegister(), offset, source, exp );
        else {
          // The TriCore assembler does not accept ST.H with label offset. So,
          // we must load the address first and then access it with offset zero.
          TCINSTRUCTIONS.insertMOVH_A( iaddr, OPER_LAB_HI, mDataLabel, exp );
          TCINSTRUCTIONS.insertLEA(
            iaddr, OPER_BASE, iaddr, OPER_LAB_LO, mDataLabel, exp );
          TCINSTRUCTIONS.insertST_H( OPER_BASE, iaddr, 0, source, exp );
        }
        break;
      }

      case IR_Type::INT:
      case IR_Type::UNSIGNED_INT:
      case IR_Type::LONG:
      case IR_Type::UNSIGNED_LONG:
      case IR_Type::FLOAT: {
        if ( useBaseAddressing )
          TCINSTRUCTIONS.insertST_W(
            mode, getBaseRegister(), offset, source, exp );
        else {
          TCINSTRUCTIONS.insertMOVH_A( iaddr, OPER_LAB_HI, mDataLabel, exp );
          TCINSTRUCTIONS.insertST_W(
            OPER_BASE, iaddr, OPER_LAB_LO, mDataLabel, source, exp );
        }
        break;
      }

      case IR_Type::LONG_LONG:
      case IR_Type::UNSIGNED_LONG_LONG:
      case IR_Type::LONG_DOUBLE:
      case IR_Type::DOUBLE: {
        if ( useBaseAddressing )
          TCINSTRUCTIONS.insertST_D(
            mode, getBaseRegister(), offset, source, exp );
        else {
          // The TriCore assembler does not accept ST.D with label offset. So,
          // we must load the address first and then access it with offset zero.
          TCINSTRUCTIONS.insertMOVH_A( iaddr, OPER_LAB_HI, mDataLabel, exp );
          TCINSTRUCTIONS.insertLEA(
            iaddr, OPER_BASE, iaddr, OPER_LAB_LO, mDataLabel, exp );
          TCINSTRUCTIONS.insertST_D( OPER_BASE, iaddr, 0, source, exp );
        }
        break;
      }

      case IR_Type::ARRAY:
      case IR_Type::POINTER: {
        if ( useBaseAddressing )
          TCINSTRUCTIONS.insertST_A(
            mode, getBaseRegister(), offset, source, exp );
        else {
          // The TriCore assembler does not accept ST.A with label offset. So,
          // we must load the address first and then access it with offset zero.
          TCINSTRUCTIONS.insertMOVH_A( iaddr, OPER_LAB_HI, mDataLabel, exp );
          TCINSTRUCTIONS.insertLEA(
            iaddr, OPER_BASE, iaddr, OPER_LAB_LO, mDataLabel, exp );
          TCINSTRUCTIONS.insertST_A( OPER_BASE, iaddr, 0, source, exp );
        }
        break;
      }

      default:
        break;
    }

    // WIR
    auto &mod =
      ( mModTime == ModTime::PRE ) ?
        TC13::AddressingMode::pre : TC13::AddressingMode::post;
    auto &aReg = TCINSTRUCTIONS.createAReg();

    switch ( mBaseType->getType() ) {
      case IR_Type::BOOL:
      case IR_Type::CHAR:
      case IR_Type::UNSIGNED_CHAR: {
        auto &r = dynamic_cast<TC_DRegV &>( *src );

        if ( useBaseAddressing ) {
          auto &bReg = dynamic_cast<TC_ARegV &>( getBaseReg() );

          if ( ( mModTime == ModTime::NONE ) || isModificationApplied() )
            TCINSTRUCTIONS.insertST_B( bReg, offset, r, exp );
          else
            TCINSTRUCTIONS.insertST_B( mod, bReg, offset, r, exp );
        } else {
          // The TriCore assembler does not accept ST.B with label offset. So,
          // we must load the address first and then access it with offset zero.
          TCINSTRUCTIONS.insertMOVH_A(
            aReg, dynamic_cast<WIR_Data &>( *mLabel ), exp );
          TCINSTRUCTIONS.insertLEA(
            aReg, aReg, dynamic_cast<WIR_Data &>( *mLabel ), exp );
          TCINSTRUCTIONS.insertST_B( aReg, 0, r, exp );
        }
        break;
      }

      case IR_Type::SHORT:
      case IR_Type::UNSIGNED_SHORT: {
        auto &r = dynamic_cast<TC_DRegV &>( *src );

        if ( useBaseAddressing ) {
          auto &bReg = dynamic_cast<TC_ARegV &>( getBaseReg() );

          if ( ( mModTime == ModTime::NONE ) || isModificationApplied() )
            TCINSTRUCTIONS.insertST_H( bReg, offset, r, exp );
          else
            TCINSTRUCTIONS.insertST_H( mod, bReg, offset, r, exp );
        } else {
          // The TriCore assembler does not accept ST.H with label offset. So,
          // we must load the address first and then access it with offset zero.
          TCINSTRUCTIONS.insertMOVH_A(
            aReg, dynamic_cast<WIR_Data &>( *mLabel ), exp );
          TCINSTRUCTIONS.insertLEA(
            aReg, aReg, dynamic_cast<WIR_Data &>( *mLabel ), exp );
          TCINSTRUCTIONS.insertST_H( aReg, 0, r, exp );
        }
        break;
      }

      case IR_Type::INT:
      case IR_Type::UNSIGNED_INT:
      case IR_Type::LONG:
      case IR_Type::UNSIGNED_LONG:
      case IR_Type::FLOAT: {
        auto &r = dynamic_cast<TC_DRegV &>( *src );

        if ( useBaseAddressing ) {
          auto &bReg = dynamic_cast<TC_ARegV &>( getBaseReg() );

          if ( ( mModTime == ModTime::NONE ) || isModificationApplied() )
            TCINSTRUCTIONS.insertST_W( bReg, offset, r, exp );
          else
            TCINSTRUCTIONS.insertST_W( mod, bReg, offset, r, exp );
        } else {
          TCINSTRUCTIONS.insertMOVH_A(
            aReg, dynamic_cast<WIR_Data &>( *mLabel ), exp );
          TCINSTRUCTIONS.insertST_W(
            aReg, dynamic_cast<WIR_Data &>( *mLabel ), r, exp );
        }
        break;
      }

      case IR_Type::LONG_LONG:
      case IR_Type::UNSIGNED_LONG_LONG:
      case IR_Type::LONG_DOUBLE:
      case IR_Type::DOUBLE: {
        auto &r = dynamic_cast<TC_ERegV &>( *src );

        if ( useBaseAddressing ) {
          auto &bReg = dynamic_cast<TC_ARegV &>( getBaseReg() );

          if ( ( mModTime == ModTime::NONE ) || isModificationApplied() )
            TCINSTRUCTIONS.insertST_D( bReg, offset, r, exp );
          else
            TCINSTRUCTIONS.insertST_D( mod, bReg, offset, r, exp );
        } else {
          // The TriCore assembler does not accept ST.D with label offset. So,
          // we must load the address first and then access it with offset zero.
          TCINSTRUCTIONS.insertMOVH_A(
            aReg, dynamic_cast<WIR_Data &>( *mLabel ), exp );
          TCINSTRUCTIONS.insertLEA(
            aReg, aReg, dynamic_cast<WIR_Data &>( *mLabel ), exp );
          TCINSTRUCTIONS.insertST_D( aReg, 0, r, exp );
        }
        break;
      }

      case IR_Type::ARRAY:
      case IR_Type::POINTER: {
        auto &r = dynamic_cast<TC_ARegV &>( *src );

        if ( useBaseAddressing ) {
          auto &bReg = dynamic_cast<TC_ARegV &>( getBaseReg() );

          if ( ( mModTime == ModTime::NONE ) || isModificationApplied() )
            TCINSTRUCTIONS.insertST_A( bReg, offset, r, exp );
          else
            TCINSTRUCTIONS.insertST_A( mod, bReg, offset, r, exp );
        } else {
          // The TriCore assembler does not accept ST.A with label offset. So,
          // we must load the address first and then access it with offset zero.
          TCINSTRUCTIONS.insertMOVH_A(
            aReg, dynamic_cast<WIR_Data &>( *mLabel ), exp );
          TCINSTRUCTIONS.insertLEA(
            aReg, aReg, dynamic_cast<WIR_Data &>( *mLabel ), exp );
          TCINSTRUCTIONS.insertST_A( aReg, 0, r, exp );
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
    dynamic_cast<TC_LValue *>( mBaseLValue )->storeBack(
      exp, getBaseRegister(), &(dynamic_cast<TC_ARegV &>( getBaseReg() )) );
};


/*
  createStoreCost computes the costs for a store of a register to the effective
  address encoded by this object.
*/
COST TC_AddressModification::createStoreCost( const IR_Exp *exp )
{
  DSTART(
    "static COST TC_AddressModification::createStoreCost(const IR_Exp*)" );

  auto &t = exp->getType();
  COST cost = 0;

  // TODO: The costs for label-based write-back are missing here, but it seems
  //       almost impossible to determine in the costs part whether a label-
  //       based write-back has to be performed, as this is only clear when the
  //       real lvalue object was passed to the action part.
  if ( dynamic_cast<IR_BitfieldType *>( &t ) )
    cost = writeValueToBitfieldCost();
  else
    switch ( t.getType() ) {

      case IR_Type::BOOL:
      case IR_Type::CHAR:
      case IR_Type::UNSIGNED_CHAR:
      case IR_Type::SHORT:
      case IR_Type::UNSIGNED_SHORT: {
        cost = TC13::OperationFormat::AC10DBOA_1.getSize();
        break;
      }

      case IR_Type::INT:
      case IR_Type::UNSIGNED_INT:
      case IR_Type::LONG:
      case IR_Type::UNSIGNED_LONG:
      case IR_Type::FLOAT: {
        cost = TC13::OperationFormat::AC16DBOA.getSize();
        break;
      }

      case IR_Type::LONG_LONG:
      case IR_Type::UNSIGNED_LONG_LONG:
      case IR_Type::DOUBLE:
      case IR_Type::LONG_DOUBLE: {
        cost = TC13::OperationFormat::AC10EPIA.getSize();
        break;
      }

      case IR_Type::ARRAY: {
        ufAssertT(
          dynamic_cast<const IR_SymbolExp *>(
            exp )->getSymbol().getSymbolTable().getFunction(),
          "Internal error: Arrays can only be assigned to if they are function "
          "parameters." );
        cost = TC13::OperationFormat::AC10ABOA.getSize();
        break;
      }

      case IR_Type::POINTER: {
        cost = TC13::OperationFormat::AC10ABOA.getSize();
        break;
      }

      default:
        break;

    }

  return( cost );
};


/*
  getMemoryAccessMode returns the LLIR string specifying the access mode that
  should be used during the access that performs the described modification.
*/
string TC_AddressModification::getMemoryAccessMode( void ) const
{
  DSTART( "string TC_AddressModification::getMemoryAccessMode() const" );

  return(
    ( mModTime == ModTime::PRE ) ? OPER_PREINC :
      ( mModTime == ModTime::POST ) ? OPER_POSTINC : OPER_BASE );
};


/*
  getByteOffset returns the byte offset that will be applied by the
  modification, depending on the actual offset, the size of the base IR type,
  whether a subtraction or addition is performed, or whether the modification
  has already been done or not.
*/
long TC_AddressModification::getByteOffset( void ) const
{
  DSTART( "long int TC_AddressModification::getByteOffset() const" );

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
  Inserts a LD.A instruction.

  Exact format: LD.A A[a] (def), [A[b] (use)]off (AAC16BOA)

  Handling of address offsets beyond signed 16 bits is included. Exact
  formats:

  ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
  LD.A A[a] (def), [A[x] (use)]<lower 16 bits of off> (AAC16BOA)
*/
void TC_AddressModification::insertLD_A( LLIR_Register *Aa, LLIR_Register *Ab,
                                         int off, const IR_Exp *exp )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  if ( ( off >= TC_Const16_Signed::getMinValue( 16 ) ) &&
       ( off <= TC_Const16_Signed::getMaxValue( 16 ) ) )
    TCINSTRUCTIONS.insertLD_A( Aa, OPER_BASE, Ab, off, exp );
  else {
    auto p = InstructionFactory::splitOffset( off );

    if ( !mModApplied ) {
      mPrecomputedOffset = TCINSTRUCTIONS.CreateRegister( "", true );
      TCINSTRUCTIONS.insertADDIH_A( mPrecomputedOffset, Ab, p.first, exp );
    }

    TCINSTRUCTIONS.insertLD_A(
      Aa, OPER_BASE, mPrecomputedOffset, p.second, exp );
  }
};


/*
  Inserts a LD.A instruction.

  Exact formats:

  LD.A A[a] (def), [+A[b] (defuse)]off (AAC10PIA)
  LD.A A[a] (def), [A[b] (defuse)+]off (AAC10PIA)

  Handling of address offsets larger than signed 10 bits but within signed 16
  bits is included. Exact formats for pre-increment:

  LEA A[b] (def), [A[b] (use)]off (AAC16BOA)
  LD.A A[a] (def), [A[b] (use)]0 (AAC16BOA)

  Exact formats for post-increment:

  LD.A A[a] (def), [A[b] (use)]0 (AAC16BOA)
  LEA A[b] (def), [A[b] (use)]off (AAC16BOA)

  Handling of address offsets beyond signed 16 bits is also included. Exact
  formats for pre-increment either:

  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
  LD.A A[a] (def), [+A[b] (defuse)]<lower 10 bits of off> (AAC10PIA)

  or:

  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)
  LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA)
  LD.A A[a] (def), [A[b] (use)]0 (AAC16BOA)

  Exact formats for post-increment either:

  LD.A A[a] (def), [A[b] (defuse)+]<lower 10 bits of off> (AAC10PIA) \n
  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)

  or:

  LD.A A[a] (def), [A[b] (use)]0 (AAC16BOA)
  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)
  LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA)
*/
void TC_AddressModification::insertLD_A( LLIR_Register *Aa,
                                         const std::string &m,
                                         LLIR_Register *Ab, int off,
                                         const IR_Exp *exp )
{
  DSTART(
    "void TC_AddressModification::insertLD_A(LLIR_Register*, const string&, LLIR_Register*, int, const IR_Exp*)" );

  if ( ( off >= TC_Const10_Signed::getMinValue( 10 ) ) &&
       ( off <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
    if ( m == OPER_PREINC ) {
      if ( !mModApplied ) {
        mPrecomputedOffset = Ab;
        TCINSTRUCTIONS.insertLD_A( Aa, m, Ab, off, exp );
      } else
        TCINSTRUCTIONS.insertLD_A( Aa, OPER_BASE, mPrecomputedOffset, 0, exp );
    } else {
      if ( !mModApplied ) {
        mPrecomputedOffset = TCINSTRUCTIONS.CreateRegister( "", true );
        TCINSTRUCTIONS.insertMOV_AA( mPrecomputedOffset, Ab, exp );
        TCINSTRUCTIONS.insertLD_A( Aa, m, Ab, off, exp );
      } else
        TCINSTRUCTIONS.insertLD_A( Aa, OPER_BASE, mPrecomputedOffset, 0, exp );
    }
  } else

  if ( ( off >= TC_Const16_Signed::getMinValue( 16 ) ) &&
       ( off <= TC_Const16_Signed::getMaxValue( 16 ) ) ) {
    if ( m == OPER_PREINC ) {
      if ( !mModApplied ) {
        mPrecomputedOffset = Ab;
        TCINSTRUCTIONS.insertLEA( Ab, OPER_BASE, Ab, off, exp );
      }
      TCINSTRUCTIONS.insertLD_A( Aa, OPER_BASE, mPrecomputedOffset, 0, exp );
    } else {
      if ( !mModApplied ) {
        mPrecomputedOffset = TCINSTRUCTIONS.CreateRegister( "", true );
        TCINSTRUCTIONS.insertMOV_AA( mPrecomputedOffset, Ab, exp );
        TCINSTRUCTIONS.insertLD_A( Aa, OPER_BASE, Ab, 0, exp );
        TCINSTRUCTIONS.insertLEA( Ab, OPER_BASE, Ab, off, exp );
      } else
        TCINSTRUCTIONS.insertLD_A( Aa, OPER_BASE, mPrecomputedOffset, 0, exp );
    }
  } else {
    auto p = InstructionFactory::splitOffset( off );

    if ( m == OPER_PREINC ) {
      if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
           ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
        if ( !mModApplied ) {
          mPrecomputedOffset = Ab;
          TCINSTRUCTIONS.insertADDIH_A( Ab, Ab, p.first, exp );
          TCINSTRUCTIONS.insertLD_A( Aa, m, Ab, p.second, exp );
        } else
          TCINSTRUCTIONS.insertLD_A( Aa, OPER_BASE, mPrecomputedOffset, 0, exp );
      } else {
        if ( !mModApplied ) {
          mPrecomputedOffset = Ab;
          TCINSTRUCTIONS.insertADDIH_A( Ab, Ab, p.first, exp );
          TCINSTRUCTIONS.insertLEA( Ab, OPER_BASE, Ab, p.second, exp );
        }
        TCINSTRUCTIONS.insertLD_A( Aa, OPER_BASE, mPrecomputedOffset, 0, exp );
      }
    } else {
      if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
           ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
        if ( !mModApplied ) {
          mPrecomputedOffset = TCINSTRUCTIONS.CreateRegister( "", true );
          TCINSTRUCTIONS.insertMOV_AA( mPrecomputedOffset, Ab, exp );
          TCINSTRUCTIONS.insertLD_A( Aa, m, Ab, p.second, exp );
          TCINSTRUCTIONS.insertADDIH_A( Ab, Ab, p.first, exp );
        } else
          TCINSTRUCTIONS.insertLD_A( Aa, OPER_BASE, mPrecomputedOffset, 0, exp );
      } else {
        if ( !mModApplied ) {
          mPrecomputedOffset = TCINSTRUCTIONS.CreateRegister( "", true );
          TCINSTRUCTIONS.insertMOV_AA( mPrecomputedOffset, Ab, exp );
          TCINSTRUCTIONS.insertLD_A( Aa, OPER_BASE, Ab, 0, exp );
          TCINSTRUCTIONS.insertADDIH_A( Ab, Ab, p.first, exp );
          TCINSTRUCTIONS.insertLEA( Ab, OPER_BASE, Ab, p.second, exp );
        } else
          TCINSTRUCTIONS.insertLD_A( Aa, OPER_BASE, mPrecomputedOffset, 0, exp );
      }
    }
  }
};


/*
  Inserts a LD.A instruction.

  Exact format: LD.A A[a] (def), [A[b] (use)]off (AAC16BOA)

  Handling of address offsets beyond signed 16 bits is included. Exact formats:

  ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16)
  LD.A A[a] (def), [A[x] (use)]<lower 16 bits of off> (AAC16BOA)
*/
void TC_AddressModification::insertLD_A( const WIR::TC_ARegV &Aa,
                                         const WIR::TC_ARegV &Ab, int off,
                                         const IR_Exp *exp )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  if ( ( off >= TC_Const16_Signed::getMinValue( 16 ) ) &&
       ( off <= TC_Const16_Signed::getMaxValue( 16 ) ) )
    TCINSTRUCTIONS.insertLD_A( Aa, Ab, off, exp );
  else {
    auto p = InstructionFactory::splitOffset( off );

    if ( !mModApplied ) {
      mPrecomputedOffReg = &TCINSTRUCTIONS.createAReg();
      TCINSTRUCTIONS.insertADDIH_A( *mPrecomputedOffReg, Ab, p.first, exp );
    }

    TCINSTRUCTIONS.insertLD_A( Aa, *mPrecomputedOffReg, p.second, exp );
  }
};


/*
  Inserts a LD.A instruction.

  Exact formats:

  LD.A A[a] (def), [+A[b] (defuse)]off (AAC10PIA)
  LD.A A[a] (def), [A[b] (defuse)+]off (AAC10PIA)

  Handling of address offsets larger than signed 10 bits but within signed 16
  bits is included. Exact formats for pre-increment:

  LEA A[b] (def), [A[b] (use)]off (AAC16BOA)
  LD.A A[a] (def), [A[b] (use)]0 (AAC16BOA)

  Exact formats for post-increment:

  LD.A A[a] (def), [A[b] (use)]0 (AAC16BOA)
  LEA A[b] (def), [A[b] (use)]off (AAC16BOA)

  Handling of address offsets beyond signed 16 bits is also included. Exact
  formats for pre-increment either:

  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
  LD.A A[a] (def), [+A[b] (defuse)]<lower 10 bits of off> (AAC10PIA)

  or:

  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)
  LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA)
  LD.A A[a] (def), [A[b] (use)]0 (AAC16BOA)

  Exact formats for post-increment either:

  LD.A A[a] (def), [A[b] (defuse)+]<lower 10 bits of off> (AAC10PIA) \n
  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)

  or:

  LD.A A[a] (def), [A[b] (use)]0 (AAC16BOA)
  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)
  LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA)
*/
void TC_AddressModification::insertLD_A( const WIR::TC_ARegV &Aa,
                                         const WIR::TC13::AddressingMode &m,
                                         const WIR::TC_ARegV &Ab, int off,
                                         const IR_Exp *exp )
{
  DSTART(
    "void TC_AddressModification::insertLD_A(const TC_ARegV&, const TC13::AddressingMode&, const TC_ARegV&, int, const IR_Exp*)" );

  if ( ( off >= TC_Const10_Signed::getMinValue( 10 ) ) &&
       ( off <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
    if ( m == TC13::AddressingMode::pre ) {
      if ( !mModApplied ) {
        mPrecomputedOffReg = const_cast<TC_ARegV *>( &Ab );
        TCINSTRUCTIONS.insertLD_A( Aa, m, Ab, off, exp );
      } else
        TCINSTRUCTIONS.insertLD_A( Aa, *mPrecomputedOffReg, 0, exp );
    } else {
      if ( !mModApplied ) {
        mPrecomputedOffReg = &TCINSTRUCTIONS.createAReg();
        TCINSTRUCTIONS.insertMOV_AA( *mPrecomputedOffReg, Ab, exp );
        TCINSTRUCTIONS.insertLD_A( Aa, m, Ab, off, exp );
      } else
        TCINSTRUCTIONS.insertLD_A( Aa, *mPrecomputedOffReg, 0, exp );
    }
  } else

  if ( ( off >= TC_Const16_Signed::getMinValue( 16 ) ) &&
       ( off <= TC_Const16_Signed::getMaxValue( 16 ) ) ) {
    if ( m == TC13::AddressingMode::pre ) {
      if ( !mModApplied ) {
        mPrecomputedOffReg = const_cast<TC_ARegV *>( &Ab );
        TCINSTRUCTIONS.insertLEA( Ab, Ab, off, exp );
      }
      TCINSTRUCTIONS.insertLD_A( Aa, *mPrecomputedOffReg, 0, exp );
    } else {
      if ( !mModApplied ) {
        mPrecomputedOffReg = &TCINSTRUCTIONS.createAReg();
        TCINSTRUCTIONS.insertMOV_AA( *mPrecomputedOffReg, Ab, exp );
        TCINSTRUCTIONS.insertLD_A( Aa, Ab, 0, exp );
        TCINSTRUCTIONS.insertLEA( Ab, Ab, off, exp );
      } else
        TCINSTRUCTIONS.insertLD_A( Aa, *mPrecomputedOffReg, 0, exp );
    }
  } else {
    auto p = InstructionFactory::splitOffset( off );

    if ( m == TC13::AddressingMode::pre ) {
      if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
           ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
        if ( !mModApplied ) {
          mPrecomputedOffReg = const_cast<TC_ARegV *>( &Ab );
          TCINSTRUCTIONS.insertADDIH_A( Ab, Ab, p.first, exp );
          TCINSTRUCTIONS.insertLD_A( Aa, m, Ab, p.second, exp );
        } else
          TCINSTRUCTIONS.insertLD_A( Aa, *mPrecomputedOffReg, 0, exp );
      } else {
        if ( !mModApplied ) {
          mPrecomputedOffReg = const_cast<TC_ARegV *>( &Ab );
          TCINSTRUCTIONS.insertADDIH_A( Ab, Ab, p.first, exp );
          TCINSTRUCTIONS.insertLEA( Ab, Ab, p.second, exp );
        }
        TCINSTRUCTIONS.insertLD_A( Aa, *mPrecomputedOffReg, 0, exp );
      }
    } else {
      if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
           ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
        if ( !mModApplied ) {
          mPrecomputedOffReg = &TCINSTRUCTIONS.createAReg();
          TCINSTRUCTIONS.insertMOV_AA( *mPrecomputedOffReg, Ab, exp );
          TCINSTRUCTIONS.insertLD_A( Aa, m, Ab, p.second, exp );
          TCINSTRUCTIONS.insertADDIH_A( Ab, Ab, p.first, exp );
        } else
          TCINSTRUCTIONS.insertLD_A( Aa, *mPrecomputedOffReg, 0, exp );
      } else {
        if ( !mModApplied ) {
          mPrecomputedOffReg = &TCINSTRUCTIONS.createAReg();
          TCINSTRUCTIONS.insertMOV_AA( *mPrecomputedOffReg, Ab, exp );
          TCINSTRUCTIONS.insertLD_A( Aa, Ab, 0, exp );
          TCINSTRUCTIONS.insertADDIH_A( Ab, Ab, p.first, exp );
          TCINSTRUCTIONS.insertLEA( Ab, Ab, p.second, exp );
        } else
          TCINSTRUCTIONS.insertLD_A( Aa, *mPrecomputedOffReg, 0, exp );
      }
    }
  }
};


/*
  Inserts a LD.B instruction.

  Exact format: LD.B D[a] (def), [A[b] (use)]off (DAC10BOA)

  Handling of address offsets beyond signed 10 bits is included. Exact
  formats either:

  ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
  LD.B D[a] (def), [A[x] (use)]<lower 10 bits of off> (DAC10BOA)

  or:

  ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
  LEA A[x] (def), [A[x] (use)]<lower 16 bits of off> (AAC16BOA) \n
  LD.B D[a] (def), [A[x] (use)]0 (DAC10BOA)
*/
void TC_AddressModification::insertLD_B( LLIR_Register *Da, LLIR_Register *Ab,
                                         int off, const IR_Exp *exp )
{
  DSTART(
    "void TC_AddressModification::insertLD_B(LLIR_Register*, LLIR_Register*, int, const IR_Exp*)" );

  if ( ( off >= TC_Const10_Signed::getMinValue( 10 ) ) &&
       ( off <= TC_Const10_Signed::getMaxValue( 10 ) ) )
    TCINSTRUCTIONS.insertLD_B( Da, OPER_BASE, Ab, off, exp );
  else {
    auto p = InstructionFactory::splitOffset( off );

    if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
         ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
      if ( !mModApplied ) {
        mPrecomputedOffset = TCINSTRUCTIONS.CreateRegister( "", true );
        TCINSTRUCTIONS.insertADDIH_A( mPrecomputedOffset, Ab, p.first, exp );
      }
      TCINSTRUCTIONS.insertLD_B(
        Da, OPER_BASE, mPrecomputedOffset, p.second, exp );
    } else {
      if ( !mModApplied ) {
        mPrecomputedOffset = TCINSTRUCTIONS.CreateRegister( "", true );
        TCINSTRUCTIONS.insertADDIH_A( mPrecomputedOffset, Ab, p.first, exp );
        TCINSTRUCTIONS.insertLEA(
          mPrecomputedOffset, OPER_BASE, mPrecomputedOffset, p.second, exp );
      }
      TCINSTRUCTIONS.insertLD_B( Da, OPER_BASE, mPrecomputedOffset, 0, exp );
    }
  }
};


/*
  Inserts a LD.B instruction.

  Exact formats:

  LD.B D[a] (def), [+A[b] (defuse)]off (DAC10PIA) \n
  LD.B D[a] (def), [A[b] (defuse)+]off (DAC10PIA)

  Handling of address offsets larger than signed 10 bits but within signed
  16 bits is included. Exact formats for pre-increment:

  LEA A[b] (def), [A[b] (use)]off (AAC16BOA) \n
  LD.B D[a] (def), [A[b] (use)]0 (DAC10BOA)

  Exact formats for post-increment:

  LD.B D[a] (def), [A[b] (use)]0 (DAC10BOA) \n
  LEA A[b] (def), [A[b] (use)]off (AAC16BOA)

  Handling of address offsets beyond signed 16 bits is also included. Exact
  formats for pre-increment either:

  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
  LD.B D[a] (def), [+A[b] (defuse)]<lower 10 bits of off> (DAC10PIA)

  or:

  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
  LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA) \n
  LD.B D[a] (def), [A[b] (use)]0 (DAC10BOA)

  Exact formats for post-increment either:

  LD.B D[a] (def), [A[b] (defuse)+]<lower 10 bits of off> (DAC10PIA) \n
  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)

  or:

  LD.B D[a] (def), [A[b] (use)]0 (DAC10BOA) \n
  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
  LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA)
*/
void TC_AddressModification::insertLD_B( LLIR_Register *Da,
                                         const std::string &m,
                                         LLIR_Register *Ab, int off,
                                         const IR_Exp *exp )
{
  DSTART(
    "void TC_AddressModification::insertLD_B(LLIR_Register*, const string&, LLIR_Register*, int, const IR_Exp*)" );

  if ( ( off >= TC_Const10_Signed::getMinValue( 10 ) ) &&
       ( off <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
    if ( m == OPER_PREINC ) {
      if ( !mModApplied ) {
        mPrecomputedOffset = Ab;
        TCINSTRUCTIONS.insertLD_B( Da, m, Ab, off, exp );
      } else
        TCINSTRUCTIONS.insertLD_B( Da, OPER_BASE, mPrecomputedOffset, 0, exp );
    } else {
      if ( !mModApplied ) {
        mPrecomputedOffset = TCINSTRUCTIONS.CreateRegister( "", true );
        TCINSTRUCTIONS.insertMOV_AA( mPrecomputedOffset, Ab, exp );
        TCINSTRUCTIONS.insertLD_B( Da, m, Ab, off, exp );
      } else
        TCINSTRUCTIONS.insertLD_B( Da, OPER_BASE, mPrecomputedOffset, 0, exp );
    }
  } else

  if ( ( off >= TC_Const16_Signed::getMinValue( 16 ) ) &&
       ( off <= TC_Const16_Signed::getMaxValue( 16 ) ) ) {
    if ( m == OPER_PREINC ) {
      if ( !mModApplied ) {
        mPrecomputedOffset = Ab;
        TCINSTRUCTIONS.insertLEA( Ab, OPER_BASE, Ab, off, exp );
      }
      TCINSTRUCTIONS.insertLD_B( Da, OPER_BASE, mPrecomputedOffset, 0, exp );
    } else {
      if ( !mModApplied ) {
        mPrecomputedOffset = TCINSTRUCTIONS.CreateRegister( "", true );
        TCINSTRUCTIONS.insertMOV_AA( mPrecomputedOffset, Ab, exp );
        TCINSTRUCTIONS.insertLD_B( Da, OPER_BASE, Ab, 0, exp );
        TCINSTRUCTIONS.insertLEA( Ab, OPER_BASE, Ab, off, exp );
      } else
        TCINSTRUCTIONS.insertLD_B( Da, OPER_BASE, mPrecomputedOffset, 0, exp );
    }
  } else {
    auto p = InstructionFactory::splitOffset( off );

    if ( m == OPER_PREINC ) {
      if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
           ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
        if ( !mModApplied ) {
          mPrecomputedOffset = Ab;
          TCINSTRUCTIONS.insertADDIH_A( Ab, Ab, p.first, exp );
          TCINSTRUCTIONS.insertLD_B( Da, m, Ab, p.second, exp );
        } else
          TCINSTRUCTIONS.insertLD_B( Da, OPER_BASE, mPrecomputedOffset, 0, exp );
      } else {
        if ( !mModApplied ) {
          mPrecomputedOffset = Ab;
          TCINSTRUCTIONS.insertADDIH_A( Ab, Ab, p.first, exp );
          TCINSTRUCTIONS.insertLEA( Ab, OPER_BASE, Ab, p.second, exp );
        }
        TCINSTRUCTIONS.insertLD_B( Da, OPER_BASE, mPrecomputedOffset, 0, exp );
      }
    } else {
      if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
           ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
        if ( !mModApplied ) {
          mPrecomputedOffset = TCINSTRUCTIONS.CreateRegister( "", true );
          TCINSTRUCTIONS.insertMOV_AA( mPrecomputedOffset, Ab, exp );
          TCINSTRUCTIONS.insertLD_B( Da, m, Ab, p.second, exp );
          TCINSTRUCTIONS.insertADDIH_A( Ab, Ab, p.first, exp );
        } else
          TCINSTRUCTIONS.insertLD_B( Da, OPER_BASE, mPrecomputedOffset, 0, exp );
      } else {
        if ( !mModApplied ) {
          mPrecomputedOffset = TCINSTRUCTIONS.CreateRegister( "", true );
          TCINSTRUCTIONS.insertMOV_AA( mPrecomputedOffset, Ab, exp );
          TCINSTRUCTIONS.insertLD_B( Da, OPER_BASE, Ab, 0, exp );
          TCINSTRUCTIONS.insertADDIH_A( Ab, Ab, p.first, exp );
          TCINSTRUCTIONS.insertLEA( Ab, OPER_BASE, Ab, p.second, exp );
        } else
          TCINSTRUCTIONS.insertLD_B( Da, OPER_BASE, mPrecomputedOffset, 0, exp );
      }
    }
  }
};


/*
  Inserts a LD.B instruction.

  Exact format: LD.B D[a] (def), [A[b] (use)]off (DAC10BOA)

  Handling of address offsets beyond signed 10 bits is included. Exact
  formats either:

  ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
  LD.B D[a] (def), [A[x] (use)]<lower 10 bits of off> (DAC10BOA)

  or:

  ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
  LEA A[x] (def), [A[x] (use)]<lower 16 bits of off> (AAC16BOA) \n
  LD.B D[a] (def), [A[x] (use)]0 (DAC10BOA)
*/
void TC_AddressModification::insertLD_B( const WIR::TC_DRegV &Da,
                                         const WIR::TC_ARegV &Ab, int off,
                                         const IR_Exp *exp )
{
  DSTART(
    "void TC_AddressModification::insertLD_B(const TC_DRegV&, const TC_ARegV&, int, const IR_Exp*)" );

  if ( ( off >= TC_Const10_Signed::getMinValue( 10 ) ) &&
       ( off <= TC_Const10_Signed::getMaxValue( 10 ) ) )
    TCINSTRUCTIONS.insertLD_B( Da, Ab, off, exp );
  else {
    auto p = InstructionFactory::splitOffset( off );

    if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
         ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
      if ( !mModApplied ) {
        mPrecomputedOffReg = &TCINSTRUCTIONS.createAReg();
        TCINSTRUCTIONS.insertADDIH_A( *mPrecomputedOffReg, Ab, p.first, exp );
      }
      TCINSTRUCTIONS.insertLD_B( Da, *mPrecomputedOffReg, p.second, exp );
  } else {
      if ( !mModApplied ) {
        mPrecomputedOffReg = &TCINSTRUCTIONS.createAReg();
        TCINSTRUCTIONS.insertADDIH_A( *mPrecomputedOffReg, Ab, p.first, exp );
        TCINSTRUCTIONS.insertLEA(
          *mPrecomputedOffReg, *mPrecomputedOffReg, p.second, exp );
      }
      TCINSTRUCTIONS.insertLD_B( Da, *mPrecomputedOffReg, 0, exp );
    }
  }
};


/*
  Inserts a LD.B instruction.

  Exact formats:

  LD.B D[a] (def), [+A[b] (defuse)]off (DAC10PIA) \n
  LD.B D[a] (def), [A[b] (defuse)+]off (DAC10PIA)

  Handling of address offsets larger than signed 10 bits but within signed
  16 bits is included. Exact formats for pre-increment:

  LEA A[b] (def), [A[b] (use)]off (AAC16BOA) \n
  LD.B D[a] (def), [A[b] (use)]0 (DAC10BOA)

  Exact formats for post-increment:

  LD.B D[a] (def), [A[b] (use)]0 (DAC10BOA) \n
  LEA A[b] (def), [A[b] (use)]off (AAC16BOA)

  Handling of address offsets beyond signed 16 bits is also included. Exact
  formats for pre-increment either:

  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
  LD.B D[a] (def), [+A[b] (defuse)]<lower 10 bits of off> (DAC10PIA)

  or:

  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
  LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA) \n
  LD.B D[a] (def), [A[b] (use)]0 (DAC10BOA)

  Exact formats for post-increment either:

  LD.B D[a] (def), [A[b] (defuse)+]<lower 10 bits of off> (DAC10PIA) \n
  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)

  or:

  LD.B D[a] (def), [A[b] (use)]0 (DAC10BOA) \n
  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
  LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA)
*/
void TC_AddressModification::insertLD_B( const WIR::TC_DRegV &Da,
                                         const WIR::TC13::AddressingMode &m,
                                         const WIR::TC_ARegV &Ab, int off,
                                         const IR_Exp *exp )
{
  DSTART(
    "void TC_AddressModification::insertLD_B(const TC_DRegV&, const TC13::AddressingMode&, const TC_ARegV&, int, const IR_Exp*)" );

  if ( ( off >= TC_Const10_Signed::getMinValue( 10 ) ) &&
       ( off <= TC_Const10_Signed::getMaxValue( 10 ) ) )
    if ( m == TC13::AddressingMode::pre ) {
      if ( !mModApplied ) {
        mPrecomputedOffReg = const_cast<TC_ARegV *>( &Ab );
        TCINSTRUCTIONS.insertLD_B( Da, m, Ab, off, exp );
      } else
        TCINSTRUCTIONS.insertLD_B( Da, *mPrecomputedOffReg, 0, exp );
    } else {
      if ( !mModApplied ) {
        mPrecomputedOffReg = &TCINSTRUCTIONS.createAReg();
        TCINSTRUCTIONS.insertMOV_AA( *mPrecomputedOffReg, Ab, exp );
        TCINSTRUCTIONS.insertLD_B( Da, m, Ab, off, exp );
      } else
        TCINSTRUCTIONS.insertLD_B( Da, *mPrecomputedOffReg, 0, exp );
    }
  else

  if ( ( off >= TC_Const16_Signed::getMinValue( 16 ) ) &&
       ( off <= TC_Const16_Signed::getMaxValue( 16 ) ) ) {
    if ( m == TC13::AddressingMode::pre ) {
      if ( !mModApplied ) {
        mPrecomputedOffReg = const_cast<TC_ARegV *>( &Ab );
        TCINSTRUCTIONS.insertLEA( Ab, Ab, off, exp );
      }
      TCINSTRUCTIONS.insertLD_B( Da, *mPrecomputedOffReg, 0, exp );
    } else {
      if ( !mModApplied ) {
        mPrecomputedOffReg = &TCINSTRUCTIONS.createAReg();
        TCINSTRUCTIONS.insertMOV_AA( *mPrecomputedOffReg, Ab, exp );
        TCINSTRUCTIONS.insertLD_B( Da, Ab, 0, exp );
        TCINSTRUCTIONS.insertLEA( Ab, Ab, off, exp );
      } else
        TCINSTRUCTIONS.insertLD_B( Da, *mPrecomputedOffReg, 0, exp );
    }
  } else {
    auto p = InstructionFactory::splitOffset( off );

    if ( m == TC13::AddressingMode::pre ) {
      if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
           ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
        if ( !mModApplied ) {
          mPrecomputedOffReg = const_cast<TC_ARegV *>( &Ab );
          TCINSTRUCTIONS.insertADDIH_A( Ab, Ab, p.first, exp );
          TCINSTRUCTIONS.insertLD_B( Da, m, Ab, p.second, exp );
        } else
          TCINSTRUCTIONS.insertLD_B( Da, *mPrecomputedOffReg, 0, exp );
      } else {
        if ( !mModApplied ) {
          mPrecomputedOffReg = const_cast<TC_ARegV *>( &Ab );
          TCINSTRUCTIONS.insertADDIH_A( Ab, Ab, p.first, exp );
          TCINSTRUCTIONS.insertLEA( Ab, Ab, p.second, exp );
        }
        TCINSTRUCTIONS.insertLD_B( Da, *mPrecomputedOffReg, 0, exp );
      }
    } else {
      if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
           ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
        if ( !mModApplied ) {
          mPrecomputedOffReg = &TCINSTRUCTIONS.createAReg();
          TCINSTRUCTIONS.insertMOV_AA( *mPrecomputedOffReg, Ab, exp );
          TCINSTRUCTIONS.insertLD_B( Da, m, Ab, p.second, exp );
          TCINSTRUCTIONS.insertADDIH_A( Ab, Ab, p.first, exp );
        } else
          TCINSTRUCTIONS.insertLD_B( Da, *mPrecomputedOffReg, 0, exp );
      } else {
        if ( !mModApplied ) {
          mPrecomputedOffReg = &TCINSTRUCTIONS.createAReg();
          TCINSTRUCTIONS.insertMOV_AA( *mPrecomputedOffReg, Ab, exp );
          TCINSTRUCTIONS.insertLD_B( Da, Ab, 0, exp );
          TCINSTRUCTIONS.insertADDIH_A( Ab, Ab, p.first, exp );
          TCINSTRUCTIONS.insertLEA( Ab, Ab, p.second, exp );
        } else
          TCINSTRUCTIONS.insertLD_B( Da, *mPrecomputedOffReg, 0, exp );
      }
    }
  }
};


/*
  Inserts a LD.BU instruction.

  Exact format: LD.BU D[a] (def), [A[b] (use)]off (DAC10BOA)

  Handling of address offsets beyond signed 10 bits is included. Exact
  formats either:

  ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
  LD.BU D[a] (def), [A[x] (use)]<lower 10 bits of off> (DAC10BOA)

  or:

  ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
  LEA A[x] (def), [A[x] (use)]<lower 16 bits of off> (AAC16BOA) \n
  LD.BU D[a] (def), [A[x] (use)]0 (DAC10BOA)
*/
void TC_AddressModification::insertLD_BU( LLIR_Register *Da, LLIR_Register *Ab,
                                          int off, const IR_Exp *exp )
{
  DSTART(
    "void TC_AddressModification::insertLD_BU(LLIR_Register*, LLIR_Register*, int, const IR_Exp*)" );

  if ( ( off >= TC_Const10_Signed::getMinValue( 10 ) ) &&
       ( off <= TC_Const10_Signed::getMaxValue( 10 ) ) )
    TCINSTRUCTIONS.insertLD_BU( Da, OPER_BASE, Ab, off, exp );
  else {
    auto p = InstructionFactory::splitOffset( off );

    if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
         ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
      if ( !mModApplied ) {
        mPrecomputedOffset = TCINSTRUCTIONS.CreateRegister( "", true );
        TCINSTRUCTIONS.insertADDIH_A( mPrecomputedOffset, Ab, p.first, exp );
      }
      TCINSTRUCTIONS.insertLD_BU(
        Da, OPER_BASE, mPrecomputedOffset, p.second, exp );
    } else {
      if ( !mModApplied ) {
        mPrecomputedOffset = TCINSTRUCTIONS.CreateRegister( "", true );
        TCINSTRUCTIONS.insertADDIH_A( mPrecomputedOffset, Ab, p.first, exp );
        TCINSTRUCTIONS.insertLEA(
          mPrecomputedOffset, OPER_BASE, mPrecomputedOffset, p.second, exp );
      }
      TCINSTRUCTIONS.insertLD_BU( Da, OPER_BASE, mPrecomputedOffset, 0, exp );
    }
  }
};


/*
  Inserts a LD.BU instruction.

  Exact formats:

  LD.BU D[a] (def), [+A[b] (defuse)]off (DAC10PIA) \n
  LD.BU D[a] (def), [A[b] (defuse)+]off (DAC10PIA)

  Handling of address offsets larger than signed 10 bits but within signed
  16 bits is included. Exact formats for pre-increment:

  LEA A[b] (def), [A[b] (use)]off (AAC16BOA) \n
  LD.BU D[a] (def), [A[b] (use)]0 (DAC10BOA)

  Exact formats for post-increment:

  LD.BU D[a] (def), [A[b] (use)]0 (DAC10BOA) \n
  LEA A[b] (def), [A[b] (use)]off (AAC16BOA)

  Handling of address offsets beyond signed 16 bits is also included. Exact
  formats for pre-increment either:

  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
  LD.BU D[a] (def), [+A[b] (defuse)]<lower 10 bits of off> (DAC10PIA)

  or:

  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
  LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA) \n
  LD.BU D[a] (def), [A[b] (use)]0 (DAC10BOA)

  Exact formats for post-increment either:

  LD.BU D[a] (def), [A[b] (defuse)+]<lower 10 bits of off> (DAC10PIA) \n
  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)

  or:

  LD.BU D[a] (def), [A[b] (use)]0 (DAC10BOA) \n
  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
  LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA)
*/
void TC_AddressModification::insertLD_BU( LLIR_Register *Da,
                                          const std::string &m,
                                          LLIR_Register *Ab, int off,
                                          const IR_Exp *exp )
{
  DSTART(
    "void TC_AddressModification::insertLD_BU(LLIR_Register*, const string&, LLIR_Register*, int, const IR_Exp*)" );

  if ( ( off >= TC_Const10_Signed::getMinValue( 10 ) ) &&
       ( off <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
    if ( m == OPER_PREINC ) {
      if ( !mModApplied ) {
        mPrecomputedOffset = Ab;
        TCINSTRUCTIONS.insertLD_BU( Da, m, Ab, off, exp );
      } else
        TCINSTRUCTIONS.insertLD_BU( Da, OPER_BASE, mPrecomputedOffset, 0, exp );
    } else {
      if ( !mModApplied ) {
        mPrecomputedOffset = TCINSTRUCTIONS.CreateRegister( "", true );
        TCINSTRUCTIONS.insertMOV_AA( mPrecomputedOffset, Ab, exp );
        TCINSTRUCTIONS.insertLD_BU( Da, m, Ab, off, exp );
      } else
        TCINSTRUCTIONS.insertLD_BU( Da, OPER_BASE, mPrecomputedOffset, 0, exp );
    }
  } else

  if ( ( off >= TC_Const16_Signed::getMinValue( 16 ) ) &&
       ( off <= TC_Const16_Signed::getMaxValue( 16 ) ) ) {
    if ( m == OPER_PREINC ) {
      if ( !mModApplied ) {
        mPrecomputedOffset = Ab;
        TCINSTRUCTIONS.insertLEA( Ab, OPER_BASE, Ab, off, exp );
      }
      TCINSTRUCTIONS.insertLD_BU( Da, OPER_BASE, mPrecomputedOffset, 0, exp );
    } else {
      if ( !mModApplied ) {
        mPrecomputedOffset = TCINSTRUCTIONS.CreateRegister( "", true );
        TCINSTRUCTIONS.insertMOV_AA( mPrecomputedOffset, Ab, exp );
        TCINSTRUCTIONS.insertLD_BU( Da, OPER_BASE, Ab, 0, exp );
        TCINSTRUCTIONS.insertLEA( Ab, OPER_BASE, Ab, off, exp );
      } else
        TCINSTRUCTIONS.insertLD_BU( Da, OPER_BASE, mPrecomputedOffset, 0, exp );
    }
  } else {
    auto p = InstructionFactory::splitOffset( off );

    if ( m == OPER_PREINC ) {
      if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
           ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
        if ( !mModApplied ) {
          mPrecomputedOffset = Ab;
          TCINSTRUCTIONS.insertADDIH_A( Ab, Ab, p.first, exp );
          TCINSTRUCTIONS.insertLD_BU( Da, m, Ab, p.second, exp );
        } else
          TCINSTRUCTIONS.insertLD_BU(
            Da, OPER_BASE, mPrecomputedOffset, 0, exp );
      } else {
        if ( !mModApplied ) {
          mPrecomputedOffset = Ab;
          TCINSTRUCTIONS.insertADDIH_A( Ab, Ab, p.first, exp );
          TCINSTRUCTIONS.insertLEA( Ab, OPER_BASE, Ab, p.second, exp );
        }
        TCINSTRUCTIONS.insertLD_BU( Da, OPER_BASE, mPrecomputedOffset, 0, exp );
      }
    } else {
      if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
           ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
        if ( !mModApplied ) {
          mPrecomputedOffset = TCINSTRUCTIONS.CreateRegister( "", true );
          TCINSTRUCTIONS.insertMOV_AA( mPrecomputedOffset, Ab, exp );
          TCINSTRUCTIONS.insertLD_BU( Da, m, Ab, p.second, exp );
          TCINSTRUCTIONS.insertADDIH_A( Ab, Ab, p.first, exp );
        } else
          TCINSTRUCTIONS.insertLD_BU(
            Da, OPER_BASE, mPrecomputedOffset, 0, exp );
      } else {
        if ( !mModApplied ) {
          mPrecomputedOffset = TCINSTRUCTIONS.CreateRegister( "", true );
          TCINSTRUCTIONS.insertMOV_AA( mPrecomputedOffset, Ab, exp );
          TCINSTRUCTIONS.insertLD_BU( Da, OPER_BASE, Ab, 0, exp );
          TCINSTRUCTIONS.insertADDIH_A( Ab, Ab, p.first, exp );
          TCINSTRUCTIONS.insertLEA( Ab, OPER_BASE, Ab, p.second, exp );
        } else
          TCINSTRUCTIONS.insertLD_BU(
            Da, OPER_BASE, mPrecomputedOffset, 0, exp );
      }
    }
  }
};


/*
  Inserts a LD.BU instruction.

  Exact format: LD.BU D[a] (def), [A[b] (use)]off (DAC10BOA)

  Handling of address offsets beyond signed 10 bits is included. Exact
  formats either:

  ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
  LD.BU D[a] (def), [A[x] (use)]<lower 10 bits of off> (DAC10BOA)

  or:

  ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
  LEA A[x] (def), [A[x] (use)]<lower 16 bits of off> (AAC16BOA) \n
  LD.BU D[a] (def), [A[x] (use)]0 (DAC10BOA)
*/
void TC_AddressModification::insertLD_BU( const WIR::TC_DRegV &Da,
                                          const WIR::TC_ARegV &Ab, int off,
                                          const IR_Exp *exp )
{
  DSTART(
    "void TC_AddressModification::insertLD_BU(const TC_DRegV&, const TC_ARegV&, int, const IR_Exp*)" );

  if ( ( off >= TC_Const10_Signed::getMinValue( 10 ) ) &&
       ( off <= TC_Const10_Signed::getMaxValue( 10 ) ) )
    TCINSTRUCTIONS.insertLD_BU( Da, Ab, off, exp );
  else {
    auto p = InstructionFactory::splitOffset( off );

    if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
         ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
      if ( !mModApplied ) {
        mPrecomputedOffReg = &TCINSTRUCTIONS.createAReg();
        TCINSTRUCTIONS.insertADDIH_A( *mPrecomputedOffReg, Ab, p.first, exp );
      }
      TCINSTRUCTIONS.insertLD_BU( Da, *mPrecomputedOffReg, p.second, exp );
  } else {
      if ( !mModApplied ) {
        mPrecomputedOffReg = &TCINSTRUCTIONS.createAReg();
        TCINSTRUCTIONS.insertADDIH_A( *mPrecomputedOffReg, Ab, p.first, exp );
        TCINSTRUCTIONS.insertLEA(
          *mPrecomputedOffReg, *mPrecomputedOffReg, p.second, exp );
      }
      TCINSTRUCTIONS.insertLD_BU( Da, *mPrecomputedOffReg, 0, exp );
    }
  }
};


/*
  Inserts a LD.BU instruction.

  Exact formats:

  LD.BU D[a] (def), [+A[b] (defuse)]off (DAC10PIA) \n
  LD.BU D[a] (def), [A[b] (defuse)+]off (DAC10PIA)

  Handling of address offsets larger than signed 10 bits but within signed
  16 bits is included. Exact formats for pre-increment:

  LEA A[b] (def), [A[b] (use)]off (AAC16BOA) \n
  LD.BU D[a] (def), [A[b] (use)]0 (DAC10BOA)

  Exact formats for post-increment:

  LD.BU D[a] (def), [A[b] (use)]0 (DAC10BOA) \n
  LEA A[b] (def), [A[b] (use)]off (AAC16BOA)

  Handling of address offsets beyond signed 16 bits is also included. Exact
  formats for pre-increment either:

  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
  LD.BU D[a] (def), [+A[b] (defuse)]<lower 10 bits of off> (DAC10PIA)

  or:

  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
  LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA) \n
  LD.BU D[a] (def), [A[b] (use)]0 (DAC10BOA)

  Exact formats for post-increment either:

  LD.BU D[a] (def), [A[b] (defuse)+]<lower 10 bits of off> (DAC10PIA) \n
  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)

  or:

  LD.BU D[a] (def), [A[b] (use)]0 (DAC10BOA) \n
  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
  LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA)
*/
void TC_AddressModification::insertLD_BU( const WIR::TC_DRegV &Da,
                                          const WIR::TC13::AddressingMode &m,
                                          const WIR::TC_ARegV &Ab, int off,
                                          const IR_Exp *exp )
{
  DSTART(
    "void TC_AddressModification::insertLD_BU(const TC_DRegV&, const TC13::AddressingMode&, const TC_ARegV&, int, const IR_Exp*)" );

  if ( ( off >= TC_Const10_Signed::getMinValue( 10 ) ) &&
       ( off <= TC_Const10_Signed::getMaxValue( 10 ) ) )
    if ( m == TC13::AddressingMode::pre ) {
      if ( !mModApplied ) {
        mPrecomputedOffReg = const_cast<TC_ARegV *>( &Ab );
        TCINSTRUCTIONS.insertLD_BU( Da, m, Ab, off, exp );
      } else
        TCINSTRUCTIONS.insertLD_BU( Da, *mPrecomputedOffReg, 0, exp );
    } else {
      if ( !mModApplied ) {
        mPrecomputedOffReg = &TCINSTRUCTIONS.createAReg();
        TCINSTRUCTIONS.insertMOV_AA( *mPrecomputedOffReg, Ab, exp );
        TCINSTRUCTIONS.insertLD_BU( Da, m, Ab, off, exp );
      } else
        TCINSTRUCTIONS.insertLD_BU( Da, *mPrecomputedOffReg, 0, exp );
    }
  else

  if ( ( off >= TC_Const16_Signed::getMinValue( 16 ) ) &&
       ( off <= TC_Const16_Signed::getMaxValue( 16 ) ) ) {
    if ( m == TC13::AddressingMode::pre ) {
      if ( !mModApplied ) {
        mPrecomputedOffReg = const_cast<TC_ARegV *>( &Ab );
        TCINSTRUCTIONS.insertLEA( Ab, Ab, off, exp );
      }
      TCINSTRUCTIONS.insertLD_BU( Da, *mPrecomputedOffReg, 0, exp );
    } else {
      if ( !mModApplied ) {
        mPrecomputedOffReg = &TCINSTRUCTIONS.createAReg();
        TCINSTRUCTIONS.insertMOV_AA( *mPrecomputedOffReg, Ab, exp );
        TCINSTRUCTIONS.insertLD_BU( Da, Ab, 0, exp );
        TCINSTRUCTIONS.insertLEA( Ab, Ab, off, exp );
      } else
        TCINSTRUCTIONS.insertLD_BU( Da, *mPrecomputedOffReg, 0, exp );
    }
  } else {
    auto p = InstructionFactory::splitOffset( off );

    if ( m == TC13::AddressingMode::pre ) {
      if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
           ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
        if ( !mModApplied ) {
          mPrecomputedOffReg = const_cast<TC_ARegV *>( &Ab );
          TCINSTRUCTIONS.insertADDIH_A( Ab, Ab, p.first, exp );
          TCINSTRUCTIONS.insertLD_BU( Da, m, Ab, p.second, exp );
        } else
          TCINSTRUCTIONS.insertLD_BU( Da, *mPrecomputedOffReg, 0, exp );
      } else {
        if ( !mModApplied ) {
          mPrecomputedOffReg = const_cast<TC_ARegV *>( &Ab );
          TCINSTRUCTIONS.insertADDIH_A( Ab, Ab, p.first, exp );
          TCINSTRUCTIONS.insertLEA( Ab, Ab, p.second, exp );
        }
        TCINSTRUCTIONS.insertLD_BU( Da, *mPrecomputedOffReg, 0, exp );
      }
    } else {
      if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
           ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
        if ( !mModApplied ) {
          mPrecomputedOffReg = &TCINSTRUCTIONS.createAReg();
          TCINSTRUCTIONS.insertMOV_AA( *mPrecomputedOffReg, Ab, exp );
          TCINSTRUCTIONS.insertLD_BU( Da, m, Ab, p.second, exp );
          TCINSTRUCTIONS.insertADDIH_A( Ab, Ab, p.first, exp );
        } else
          TCINSTRUCTIONS.insertLD_BU( Da, *mPrecomputedOffReg, 0, exp );
      } else {
        if ( !mModApplied ) {
          mPrecomputedOffReg = &TCINSTRUCTIONS.createAReg();
          TCINSTRUCTIONS.insertMOV_AA( *mPrecomputedOffReg, Ab, exp );
          TCINSTRUCTIONS.insertLD_BU( Da, Ab, 0, exp );
          TCINSTRUCTIONS.insertADDIH_A( Ab, Ab, p.first, exp );
          TCINSTRUCTIONS.insertLEA( Ab, Ab, p.second, exp );
        } else
          TCINSTRUCTIONS.insertLD_BU( Da, *mPrecomputedOffReg, 0, exp );
      }
    }
  }
};


/*
  Inserts a LD.D instruction.

  Exact format: LD.D D[a] (def), [A[b] (use)]off (EAC10BOA)

  Handling of address offsets beyond signed 10 bits is included. Exact
  formats either:

  ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
  LD.D E[a] (def), [A[x] (use)]<lower 10 bits of off> (EAC10BOA)

  or:

  ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
  LEA A[x] (def), [A[x] (use)]<lower 16 bits of off> (AAC16BOA) \n
  LD.D E[a] (def), [A[x] (use)]0 (EAC10BOA)
*/
void TC_AddressModification::insertLD_D( LLIR_Register *Ea, LLIR_Register *Ab,
                                         int off, const IR_Exp *exp )
{
  DSTART(
    "void TC_AddressModification::insertLD_D(LLIR_Register*, LLIR_Register*, int, const IR_Exp*)" );

  if ( ( off >= TC_Const10_Signed::getMinValue( 10 ) ) &&
       ( off <= TC_Const10_Signed::getMaxValue( 10 ) ) )
    TCINSTRUCTIONS.insertLD_D( Ea, OPER_BASE, Ab, off, exp );
  else {
    auto p = InstructionFactory::splitOffset( off );

    if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
         ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
      if ( !mModApplied ) {
        mPrecomputedOffset = TCINSTRUCTIONS.CreateRegister( "", true );
        TCINSTRUCTIONS.insertADDIH_A( mPrecomputedOffset, Ab, p.first, exp );
      }
      TCINSTRUCTIONS.insertLD_D(
        Ea, OPER_BASE, mPrecomputedOffset, p.second, exp );
    } else {
      if ( !mModApplied ) {
        mPrecomputedOffset = TCINSTRUCTIONS.CreateRegister( "", true );
        TCINSTRUCTIONS.insertADDIH_A( mPrecomputedOffset, Ab, p.first, exp );
        TCINSTRUCTIONS.insertLEA(
          mPrecomputedOffset, OPER_BASE, mPrecomputedOffset, p.second, exp );
      }
      TCINSTRUCTIONS.insertLD_D( Ea, OPER_BASE, mPrecomputedOffset, 0, exp );
    }
  }
};


/*
  Inserts a LD.D instruction.

  Exact formats:

  LD.D E[a] (def), [+A[b] (defuse)]off (EAC10PIA) \n
  LD.D E[a] (def), [A[b] (defuse)+]off (EAC10PIA)

  Handling of address offsets larger than signed 10 bits but within signed
  16 bits is included. Exact formats for pre-increment:

  LEA A[b] (def), [A[b] (use)]off (AAC16BOA) \n
  LD.D E[a] (def), [A[b] (use)]0 (EAC10BOA)

  Exact formats for post-increment:

  LD.D E[a] (def), [A[b] (use)]0 (EAC10BOA) \n
  LEA A[b] (def), [A[b] (use)]off (AAC16BOA)

  Handling of address offsets beyond signed 16 bits is also included. Exact
  formats for pre-increment either:

  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
  LD.D E[a] (def), [+A[b] (defuse)]<lower 10 bits of off> (EAC10PIA)

  or:

  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
  LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA) \n
  LD.D E[a] (def), [A[b] (use)]0 (EAC10BOA)

  Exact formats for post-increment either:

  LD.D E[a] (def), [A[b] (defuse)+]<lower 10 bits of off> (EAC10PIA) \n
  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)

  or:

  LD.D E[a] (def), [A[b] (use)]0 (EAC10BOA) \n
  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
  LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA)

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
void TC_AddressModification::insertLD_D( LLIR_Register *Ea,
                                         const std::string &m,
                                         LLIR_Register *Ab, int off,
                                         const IR_Exp *exp )
{
  DSTART(
    "void TC_AddressModification::insertLD_D(LLIR_Register*, const string&, LLIR_Register*, int, const IR_Exp*)" );

  if ( ( off >= TC_Const10_Signed::getMinValue( 10 ) ) &&
       ( off <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
    if ( m == OPER_PREINC ) {
      if ( !mModApplied ) {
        mPrecomputedOffset = Ab;
        TCINSTRUCTIONS.insertLD_D( Ea, m, Ab, off, exp );
      } else
        TCINSTRUCTIONS.insertLD_D( Ea, OPER_BASE, mPrecomputedOffset, 0, exp );
    } else {
      if ( !mModApplied ) {
        mPrecomputedOffset = TCINSTRUCTIONS.CreateRegister( "", true );
        TCINSTRUCTIONS.insertMOV_AA( mPrecomputedOffset, Ab, exp );
        TCINSTRUCTIONS.insertLD_D( Ea, m, Ab, off, exp );
      } else
        TCINSTRUCTIONS.insertLD_D( Ea, OPER_BASE, mPrecomputedOffset, 0, exp );
    }
  } else

  if ( ( off >= TC_Const16_Signed::getMinValue( 16 ) ) &&
       ( off <= TC_Const16_Signed::getMaxValue( 16 ) ) ) {
    if ( m == OPER_PREINC ) {
      if ( !mModApplied ) {
        mPrecomputedOffset = Ab;
        TCINSTRUCTIONS.insertLEA( Ab, OPER_BASE, Ab, off, exp );
      }
      TCINSTRUCTIONS.insertLD_D( Ea, OPER_BASE, mPrecomputedOffset, 0, exp );
    } else {
      if ( !mModApplied ) {
        mPrecomputedOffset = TCINSTRUCTIONS.CreateRegister( "", true );
        TCINSTRUCTIONS.insertMOV_AA( mPrecomputedOffset, Ab, exp );
        TCINSTRUCTIONS.insertLD_D( Ea, OPER_BASE, Ab, 0, exp );
        TCINSTRUCTIONS.insertLEA( Ab, OPER_BASE, Ab, off, exp );
      } else
        TCINSTRUCTIONS.insertLD_D( Ea, OPER_BASE, mPrecomputedOffset, 0, exp );
    }
  } else {
    auto p = InstructionFactory::splitOffset( off );

    if ( m == OPER_PREINC ) {
      if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
           ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
        if ( !mModApplied ) {
          mPrecomputedOffset = Ab;
          TCINSTRUCTIONS.insertADDIH_A( Ab, Ab, p.first, exp );
          TCINSTRUCTIONS.insertLD_D( Ea, m, Ab, p.second, exp );
        } else
          TCINSTRUCTIONS.insertLD_D( Ea, OPER_BASE, mPrecomputedOffset, 0, exp );
      } else {
        if ( !mModApplied ) {
          mPrecomputedOffset = Ab;
          TCINSTRUCTIONS.insertADDIH_A( Ab, Ab, p.first, exp );
          TCINSTRUCTIONS.insertLEA( Ab, OPER_BASE, Ab, p.second, exp );
        }
        TCINSTRUCTIONS.insertLD_D( Ea, OPER_BASE, mPrecomputedOffset, 0, exp );
      }
    } else {
      if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
           ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
        if ( !mModApplied ) {
          mPrecomputedOffset = TCINSTRUCTIONS.CreateRegister( "", true );
          TCINSTRUCTIONS.insertMOV_AA( mPrecomputedOffset, Ab, exp );
          TCINSTRUCTIONS.insertLD_D( Ea, m, Ab, p.second, exp );
          TCINSTRUCTIONS.insertADDIH_A( Ab, Ab, p.first, exp );
        } else
          TCINSTRUCTIONS.insertLD_D( Ea, OPER_BASE, mPrecomputedOffset, 0, exp );
      } else {
        if ( !mModApplied ) {
          mPrecomputedOffset = TCINSTRUCTIONS.CreateRegister( "", true );
          TCINSTRUCTIONS.insertMOV_AA( mPrecomputedOffset, Ab, exp );
          TCINSTRUCTIONS.insertLD_D( Ea, OPER_BASE, Ab, 0, exp );
          TCINSTRUCTIONS.insertADDIH_A( Ab, Ab, p.first, exp );
          TCINSTRUCTIONS.insertLEA( Ab, OPER_BASE, Ab, p.second, exp );
        } else
          TCINSTRUCTIONS.insertLD_D( Ea, OPER_BASE, mPrecomputedOffset, 0, exp );
      }
    }
  }
};


/*
  Inserts a LD.D instruction.

  Exact format: LD.D E[a] (def), [A[b] (use)]off (EAC10BOA)

  Handling of address offsets beyond signed 10 bits is included. Exact formats
  either:

  ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16)
  LD.D E[a] (def), [A[x] (use)]<lower 10 bits of off> (EAC10BOA)

  or:

  ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16)
  LEA A[x] (def), [A[x] (use)]<lower 16 bits of off> (AAC16BOA)
  LD.D E[a] (def), [A[x] (use)]0 (EAC10BOA)
*/
void TC_AddressModification::insertLD_D( const WIR::TC_ERegV &Ea,
                                         const WIR::TC_ARegV &Ab, int off,
                                         const IR_Exp *exp )
{
  DSTART(
    "void TC_AddressModification::insertLD_D(const TC_ERegV&, const TC_ARegV&, int, const IR_Exp*)" );

  if ( ( off >= TC_Const10_Signed::getMinValue( 10 ) ) &&
       ( off <= TC_Const10_Signed::getMaxValue( 10 ) ) )
    TCINSTRUCTIONS.insertLD_D( Ea, Ab, off, exp );
  else {
    auto p = InstructionFactory::splitOffset( off );

    if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
         ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
      if ( !mModApplied ) {
        mPrecomputedOffReg = &TCINSTRUCTIONS.createAReg();
        TCINSTRUCTIONS.insertADDIH_A( *mPrecomputedOffReg, Ab, p.first, exp );
      }
      TCINSTRUCTIONS.insertLD_D( Ea, *mPrecomputedOffReg, p.second, exp );
  } else {
      if ( !mModApplied ) {
        mPrecomputedOffReg = &TCINSTRUCTIONS.createAReg();
        TCINSTRUCTIONS.insertADDIH_A( *mPrecomputedOffReg, Ab, p.first, exp );
        TCINSTRUCTIONS.insertLEA(
          *mPrecomputedOffReg, *mPrecomputedOffReg, p.second, exp );
      }
      TCINSTRUCTIONS.insertLD_D( Ea, *mPrecomputedOffReg, 0, exp );
    }
  }
};


/*
  Inserts a LD.D instruction.

  Exact formats:

  LD.D E[a] (def), [+A[b] (defuse)]off (EAC10PIA)
  LD.D E[a] (def), [A[b] (defuse)+]off (EAC10PIA)

  Handling of address offsets larger than signed 10 bits but within signed 16
  bits is included. Exact formats for pre-increment:

  LEA A[b] (def), [A[b] (use)]off (AAC16BOA)
  LD.D E[a] (def), [A[b] (use)]0 (EAC10BOA)

  Exact formats for post-increment:

  LD.D E[a] (def), [A[b] (use)]0 (EAC10BOA)
  LEA A[b] (def), [A[b] (use)]off (AAC16BOA)

  Handling of address offsets beyond signed 16 bits is also included. Exact
  formats for pre-increment either:

  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)
  LD.D E[a] (def), [+A[b] (defuse)]<lower 10 bits of off> (EAC10PIA)

  or:

  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)
  LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA)
  LD.D E[a] (def), [A[b] (use)]0 (EAC10BOA)

  Exact formats for post-increment either:

  LD.D E[a] (def), [A[b] (defuse)+]<lower 10 bits of off> (EAC10PIA)
  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)

  or:

  LD.D E[a] (def), [A[b] (use)]0 (EAC10BOA)
  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)
  LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA)
*/
void TC_AddressModification::insertLD_D( const WIR::TC_ERegV &Ea,
                                         const WIR::TC13::AddressingMode &m,
                                         const WIR::TC_ARegV &Ab, int off,
                                         const IR_Exp *exp )
{
  DSTART(
    "void TC_AddressModification::insertLD_D(const TC_ERegV&, const TC13::AddressingMode&, const TC_ARegV&, int, const IR_Exp*)" );

  if ( ( off >= TC_Const10_Signed::getMinValue( 10 ) ) &&
       ( off <= TC_Const10_Signed::getMaxValue( 10 ) ) )
    if ( m == TC13::AddressingMode::pre ) {
      if ( !mModApplied ) {
        mPrecomputedOffReg = const_cast<TC_ARegV *>( &Ab );
        TCINSTRUCTIONS.insertLD_D( Ea, m, Ab, off, exp );
      } else
        TCINSTRUCTIONS.insertLD_D( Ea, *mPrecomputedOffReg, 0, exp );
    } else {
      if ( !mModApplied ) {
        mPrecomputedOffReg = &TCINSTRUCTIONS.createAReg();
        TCINSTRUCTIONS.insertMOV_AA( *mPrecomputedOffReg, Ab, exp );
        TCINSTRUCTIONS.insertLD_D( Ea, m, Ab, off, exp );
      } else
        TCINSTRUCTIONS.insertLD_D( Ea, *mPrecomputedOffReg, 0, exp );
    }
  else

  if ( ( off >= TC_Const16_Signed::getMinValue( 16 ) ) &&
       ( off <= TC_Const16_Signed::getMaxValue( 16 ) ) ) {
    if ( m == TC13::AddressingMode::pre ) {
      if ( !mModApplied ) {
        mPrecomputedOffReg = const_cast<TC_ARegV *>( &Ab );
        TCINSTRUCTIONS.insertLEA( Ab, Ab, off, exp );
      }
      TCINSTRUCTIONS.insertLD_D( Ea, *mPrecomputedOffReg, 0, exp );
    } else {
      if ( !mModApplied ) {
        mPrecomputedOffReg = &TCINSTRUCTIONS.createAReg();
        TCINSTRUCTIONS.insertMOV_AA( *mPrecomputedOffReg, Ab, exp );
        TCINSTRUCTIONS.insertLD_D( Ea, Ab, 0, exp );
        TCINSTRUCTIONS.insertLEA( Ab, Ab, off, exp );
      } else
        TCINSTRUCTIONS.insertLD_D( Ea, *mPrecomputedOffReg, 0, exp );
    }
  } else {
    auto p = InstructionFactory::splitOffset( off );

    if ( m == TC13::AddressingMode::pre ) {
      if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
           ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
        if ( !mModApplied ) {
          mPrecomputedOffReg = const_cast<TC_ARegV *>( &Ab );
          TCINSTRUCTIONS.insertADDIH_A( Ab, Ab, p.first, exp );
          TCINSTRUCTIONS.insertLD_D( Ea, m, Ab, p.second, exp );
        } else
          TCINSTRUCTIONS.insertLD_D( Ea, *mPrecomputedOffReg, 0, exp );
      } else {
        if ( !mModApplied ) {
          mPrecomputedOffReg = const_cast<TC_ARegV *>( &Ab );
          TCINSTRUCTIONS.insertADDIH_A( Ab, Ab, p.first, exp );
          TCINSTRUCTIONS.insertLEA( Ab, Ab, p.second, exp );
        }
        TCINSTRUCTIONS.insertLD_D( Ea, *mPrecomputedOffReg, 0, exp );
      }
    } else {
      if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
           ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
        if ( !mModApplied ) {
          mPrecomputedOffReg = &TCINSTRUCTIONS.createAReg();
          TCINSTRUCTIONS.insertMOV_AA( *mPrecomputedOffReg, Ab, exp );
          TCINSTRUCTIONS.insertLD_D( Ea, m, Ab, p.second, exp );
          TCINSTRUCTIONS.insertADDIH_A( Ab, Ab, p.first, exp );
        } else
          TCINSTRUCTIONS.insertLD_D( Ea, *mPrecomputedOffReg, 0, exp );
      } else {
        if ( !mModApplied ) {
          mPrecomputedOffReg = &TCINSTRUCTIONS.createAReg();
          TCINSTRUCTIONS.insertMOV_AA( *mPrecomputedOffReg, Ab, exp );
          TCINSTRUCTIONS.insertLD_D( Ea, Ab, 0, exp );
          TCINSTRUCTIONS.insertADDIH_A( Ab, Ab, p.first, exp );
          TCINSTRUCTIONS.insertLEA( Ab, Ab, p.second, exp );
        } else
          TCINSTRUCTIONS.insertLD_D( Ea, *mPrecomputedOffReg, 0, exp );
      }
    }
  }
};


/*
  Inserts a LD.H instruction.

  Exact format: LD.H D[a] (def), [A[b] (use)]off (DAC10BOA)

  Handling of address offsets beyond signed 10 bits is included. Exact
  formats either:

  ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
  LD.H D[a] (def), [A[x] (use)]<lower 10 bits of off> (DAC10BOA)

  or:

  ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
  LEA A[x] (def), [A[x] (use)]<lower 16 bits of off> (AAC16BOA) \n
  LD.H D[a] (def), [A[x] (use)]0 (DAC10BOA)
*/
void TC_AddressModification::insertLD_H( LLIR_Register *Da, LLIR_Register *Ab,
                                         int off, const IR_Exp *exp )
{
  DSTART(
    "void TC_AddressModification::insertLD_H(LLIR_Register*, LLIR_Register*, int, const IR_Exp*)" );

  if ( ( off >= TC_Const10_Signed::getMinValue( 10 ) ) &&
       ( off <= TC_Const10_Signed::getMaxValue( 10 ) ) )
    TCINSTRUCTIONS.insertLD_H( Da, OPER_BASE, Ab, off, exp );
  else {
    auto p = InstructionFactory::splitOffset( off );

    if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
         ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
      if ( !mModApplied ) {
        mPrecomputedOffset = TCINSTRUCTIONS.CreateRegister( "", true );
        TCINSTRUCTIONS.insertADDIH_A( mPrecomputedOffset, Ab, p.first, exp );
      }
      TCINSTRUCTIONS.insertLD_H(
        Da, OPER_BASE, mPrecomputedOffset, p.second, exp );
    } else {
      if ( !mModApplied ) {
        mPrecomputedOffset = TCINSTRUCTIONS.CreateRegister( "", true );
        TCINSTRUCTIONS.insertADDIH_A( mPrecomputedOffset, Ab, p.first, exp );
        TCINSTRUCTIONS.insertLEA(
          mPrecomputedOffset, OPER_BASE, mPrecomputedOffset, p.second, exp );
      }
      TCINSTRUCTIONS.insertLD_H( Da, OPER_BASE, mPrecomputedOffset, 0, exp );
    }
  }
};


/*
  Inserts a LD.H instruction.

  Exact formats:

  LD.H D[a] (def), [+A[b] (defuse)]off (DAC10PIA) \n
  LD.H D[a] (def), [A[b] (defuse)+]off (DAC10PIA)

  Handling of address offsets larger than signed 10 bits but within signed
  16 bits is included. Exact formats for pre-increment:

  LEA A[b] (def), [A[b] (use)]off (AAC16BOA) \n
  LD.H D[a] (def), [A[b] (use)]0 (DAC10BOA)

  Exact formats for post-increment:

  LD.H D[a] (def), [A[b] (use)]0 (DAC10BOA) \n
  LEA A[b] (def), [A[b] (use)]off (AAC16BOA)

  Handling of address offsets beyond signed 16 bits is also included. Exact
  formats for pre-increment either:

  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
  LD.H D[a] (def), [+A[b] (defuse)]<lower 10 bits of off> (DAC10PIA)

  or:

  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
  LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA) \n
  LD.H D[a] (def), [A[b] (use)]0 (DAC10BOA)

  Exact formats for post-increment either:

  LD.H D[a] (def), [A[b] (defuse)+]<lower 10 bits of off> (DAC10PIA) \n
  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)

  or:

  LD.H D[a] (def), [A[b] (use)]0 (DAC10BOA) \n
  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
  LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA)
*/
void TC_AddressModification::insertLD_H( LLIR_Register *Da,
                                         const std::string &m,
                                         LLIR_Register *Ab, int off,
                                         const IR_Exp *exp )
{
  DSTART(
    "void TC_AddressModification::insertLD_H(LLIR_Register*, const string&, LLIR_Register*, int, const IR_Exp*)" );

  if ( ( off >= TC_Const10_Signed::getMinValue( 10 ) ) &&
       ( off <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
    if ( m == OPER_PREINC ) {
      if ( !mModApplied ) {
        mPrecomputedOffset = Ab;
        TCINSTRUCTIONS.insertLD_H( Da, m, Ab, off, exp );
      } else
        TCINSTRUCTIONS.insertLD_H( Da, OPER_BASE, mPrecomputedOffset, 0, exp );
    } else {
      if ( !mModApplied ) {
        mPrecomputedOffset = TCINSTRUCTIONS.CreateRegister( "", true );
        TCINSTRUCTIONS.insertMOV_AA( mPrecomputedOffset, Ab, exp );
        TCINSTRUCTIONS.insertLD_H( Da, m, Ab, off, exp );
      } else
        TCINSTRUCTIONS.insertLD_H( Da, OPER_BASE, mPrecomputedOffset, 0, exp );
    }
  } else

  if ( ( off >= TC_Const16_Signed::getMinValue( 16 ) ) &&
       ( off <= TC_Const16_Signed::getMaxValue( 16 ) ) ) {
    if ( m == OPER_PREINC ) {
      if ( !mModApplied ) {
        mPrecomputedOffset = Ab;
        TCINSTRUCTIONS.insertLEA( Ab, OPER_BASE, Ab, off, exp );
      }
      TCINSTRUCTIONS.insertLD_H( Da, OPER_BASE, mPrecomputedOffset, 0, exp );
    } else {
      if ( !mModApplied ) {
        mPrecomputedOffset = TCINSTRUCTIONS.CreateRegister( "", true );
        TCINSTRUCTIONS.insertMOV_AA( mPrecomputedOffset, Ab, exp );
        TCINSTRUCTIONS.insertLD_H( Da, OPER_BASE, Ab, 0, exp );
        TCINSTRUCTIONS.insertLEA( Ab, OPER_BASE, Ab, off, exp );
      } else
        TCINSTRUCTIONS.insertLD_H( Da, OPER_BASE, mPrecomputedOffset, 0, exp );
    }
  } else {
    auto p = InstructionFactory::splitOffset( off );

    if ( m == OPER_PREINC ) {
      if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
           ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
        if ( !mModApplied ) {
          mPrecomputedOffset = Ab;
          TCINSTRUCTIONS.insertADDIH_A( Ab, Ab, p.first, exp );
          TCINSTRUCTIONS.insertLD_H( Da, m, Ab, p.second, exp );
        } else
          TCINSTRUCTIONS.insertLD_H( Da, OPER_BASE, mPrecomputedOffset, 0, exp );
      } else {
        if ( !mModApplied ) {
          mPrecomputedOffset = Ab;
          TCINSTRUCTIONS.insertADDIH_A( Ab, Ab, p.first, exp );
          TCINSTRUCTIONS.insertLEA( Ab, OPER_BASE, Ab, p.second, exp );
        }
        TCINSTRUCTIONS.insertLD_H( Da, OPER_BASE, mPrecomputedOffset, 0, exp );
      }
    } else {
      if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
           ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
        if ( !mModApplied ) {
          mPrecomputedOffset = TCINSTRUCTIONS.CreateRegister( "", true );
          TCINSTRUCTIONS.insertMOV_AA( mPrecomputedOffset, Ab, exp );
          TCINSTRUCTIONS.insertLD_H( Da, m, Ab, p.second, exp );
          TCINSTRUCTIONS.insertADDIH_A( Ab, Ab, p.first, exp );
        } else
          TCINSTRUCTIONS.insertLD_H( Da, OPER_BASE, mPrecomputedOffset, 0, exp );
      } else {
        if ( !mModApplied ) {
          mPrecomputedOffset = TCINSTRUCTIONS.CreateRegister( "", true );
          TCINSTRUCTIONS.insertMOV_AA( mPrecomputedOffset, Ab, exp );
          TCINSTRUCTIONS.insertLD_H( Da, OPER_BASE, Ab, 0, exp );
          TCINSTRUCTIONS.insertADDIH_A( Ab, Ab, p.first, exp );
          TCINSTRUCTIONS.insertLEA( Ab, OPER_BASE, Ab, p.second, exp );
        } else
          TCINSTRUCTIONS.insertLD_H( Da, OPER_BASE, mPrecomputedOffset, 0, exp );
      }
    }
  }
};


/*
  Inserts a LD.H instruction.

  Exact format: LD.H D[a] (def), [A[b] (use)]off (DAC10BOA)

  Handling of address offsets beyond signed 10 bits is included. Exact
  formats either:

  ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
  LD.H D[a] (def), [A[x] (use)]<lower 10 bits of off> (DAC10BOA)

  or:

  ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
  LEA A[x] (def), [A[x] (use)]<lower 16 bits of off> (AAC16BOA) \n
  LD.H D[a] (def), [A[x] (use)]0 (DAC10BOA)
*/
void TC_AddressModification::insertLD_H( const WIR::TC_DRegV &Da,
                                         const WIR::TC_ARegV &Ab, int off,
                                         const IR_Exp *exp )
{
  DSTART(
    "void TC_AddressModification::insertLD_H(const TC_DRegV&, const TC_ARegV&, int, const IR_Exp*)" );

  if ( ( off >= TC_Const10_Signed::getMinValue( 10 ) ) &&
       ( off <= TC_Const10_Signed::getMaxValue( 10 ) ) )
    TCINSTRUCTIONS.insertLD_H( Da, Ab, off, exp );
  else {
    auto p = InstructionFactory::splitOffset( off );

    if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
         ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
      if ( !mModApplied ) {
        mPrecomputedOffReg = &TCINSTRUCTIONS.createAReg();
        TCINSTRUCTIONS.insertADDIH_A( *mPrecomputedOffReg, Ab, p.first, exp );
      }
      TCINSTRUCTIONS.insertLD_H( Da, *mPrecomputedOffReg, p.second, exp );
  } else {
      if ( !mModApplied ) {
        mPrecomputedOffReg = &TCINSTRUCTIONS.createAReg();
        TCINSTRUCTIONS.insertADDIH_A( *mPrecomputedOffReg, Ab, p.first, exp );
        TCINSTRUCTIONS.insertLEA(
          *mPrecomputedOffReg, *mPrecomputedOffReg, p.second, exp );
      }
      TCINSTRUCTIONS.insertLD_H( Da, *mPrecomputedOffReg, 0, exp );
    }
  }
};


/*
  Inserts a LD.H instruction.

  Exact formats:

  LD.H D[a] (def), [+A[b] (defuse)]off (DAC10PIA) \n
  LD.H D[a] (def), [A[b] (defuse)+]off (DAC10PIA)

  Handling of address offsets larger than signed 10 bits but within signed
  16 bits is included. Exact formats for pre-increment:

  LEA A[b] (def), [A[b] (use)]off (AAC16BOA) \n
  LD.H D[a] (def), [A[b] (use)]0 (DAC10BOA)

  Exact formats for post-increment:

  LD.H D[a] (def), [A[b] (use)]0 (DAC10BOA) \n
  LEA A[b] (def), [A[b] (use)]off (AAC16BOA)

  Handling of address offsets beyond signed 16 bits is also included. Exact
  formats for pre-increment either:

  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
  LD.H D[a] (def), [+A[b] (defuse)]<lower 10 bits of off> (DAC10PIA)

  or:

  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
  LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA) \n
  LD.H D[a] (def), [A[b] (use)]0 (DAC10BOA)

  Exact formats for post-increment either:

  LD.H D[a] (def), [A[b] (defuse)+]<lower 10 bits of off> (DAC10PIA) \n
  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)

  or:

  LD.H D[a] (def), [A[b] (use)]0 (DAC10BOA) \n
  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
  LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA)
*/
void TC_AddressModification::insertLD_H( const WIR::TC_DRegV &Da,
                                         const WIR::TC13::AddressingMode &m,
                                         const WIR::TC_ARegV &Ab, int off,
                                         const IR_Exp *exp )
{
  DSTART(
    "void TC_AddressModification::insertLD_H(const TC_DRegV&, const TC13::AddressingMode&, const TC_ARegV&, int, const IR_Exp*)" );

  if ( ( off >= TC_Const10_Signed::getMinValue( 10 ) ) &&
       ( off <= TC_Const10_Signed::getMaxValue( 10 ) ) )
    if ( m == TC13::AddressingMode::pre ) {
      if ( !mModApplied ) {
        mPrecomputedOffReg = const_cast<TC_ARegV *>( &Ab );
        TCINSTRUCTIONS.insertLD_H( Da, m, Ab, off, exp );
      } else
        TCINSTRUCTIONS.insertLD_H( Da, *mPrecomputedOffReg, 0, exp );
    } else {
      if ( !mModApplied ) {
        mPrecomputedOffReg = &TCINSTRUCTIONS.createAReg();
        TCINSTRUCTIONS.insertMOV_AA( *mPrecomputedOffReg, Ab, exp );
        TCINSTRUCTIONS.insertLD_H( Da, m, Ab, off, exp );
      } else
        TCINSTRUCTIONS.insertLD_H( Da, *mPrecomputedOffReg, 0, exp );
    }
  else

  if ( ( off >= TC_Const16_Signed::getMinValue( 16 ) ) &&
       ( off <= TC_Const16_Signed::getMaxValue( 16 ) ) ) {
    if ( m == TC13::AddressingMode::pre ) {
      if ( !mModApplied ) {
        mPrecomputedOffReg = const_cast<TC_ARegV *>( &Ab );
        TCINSTRUCTIONS.insertLEA( Ab, Ab, off, exp );
      }
      TCINSTRUCTIONS.insertLD_H( Da, *mPrecomputedOffReg, 0, exp );
    } else {
      if ( !mModApplied ) {
        mPrecomputedOffReg = &TCINSTRUCTIONS.createAReg();
        TCINSTRUCTIONS.insertMOV_AA( *mPrecomputedOffReg, Ab, exp );
        TCINSTRUCTIONS.insertLD_H( Da, Ab, 0, exp );
        TCINSTRUCTIONS.insertLEA( Ab, Ab, off, exp );
      } else
        TCINSTRUCTIONS.insertLD_H( Da, *mPrecomputedOffReg, 0, exp );
    }
  } else {
    auto p = InstructionFactory::splitOffset( off );

    if ( m == TC13::AddressingMode::pre ) {
      if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
           ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
        if ( !mModApplied ) {
          mPrecomputedOffReg = const_cast<TC_ARegV *>( &Ab );
          TCINSTRUCTIONS.insertADDIH_A( Ab, Ab, p.first, exp );
          TCINSTRUCTIONS.insertLD_H( Da, m, Ab, p.second, exp );
        } else
          TCINSTRUCTIONS.insertLD_H( Da, *mPrecomputedOffReg, 0, exp );
      } else {
        if ( !mModApplied ) {
          mPrecomputedOffReg = const_cast<TC_ARegV *>( &Ab );
          TCINSTRUCTIONS.insertADDIH_A( Ab, Ab, p.first, exp );
          TCINSTRUCTIONS.insertLEA( Ab, Ab, p.second, exp );
        }
        TCINSTRUCTIONS.insertLD_H( Da, *mPrecomputedOffReg, 0, exp );
      }
    } else {
      if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
           ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
        if ( !mModApplied ) {
          mPrecomputedOffReg = &TCINSTRUCTIONS.createAReg();
          TCINSTRUCTIONS.insertMOV_AA( *mPrecomputedOffReg, Ab, exp );
          TCINSTRUCTIONS.insertLD_H( Da, m, Ab, p.second, exp );
          TCINSTRUCTIONS.insertADDIH_A( Ab, Ab, p.first, exp );
        } else
          TCINSTRUCTIONS.insertLD_H( Da, *mPrecomputedOffReg, 0, exp );
      } else {
        if ( !mModApplied ) {
          mPrecomputedOffReg = &TCINSTRUCTIONS.createAReg();
          TCINSTRUCTIONS.insertMOV_AA( *mPrecomputedOffReg, Ab, exp );
          TCINSTRUCTIONS.insertLD_H( Da, Ab, 0, exp );
          TCINSTRUCTIONS.insertADDIH_A( Ab, Ab, p.first, exp );
          TCINSTRUCTIONS.insertLEA( Ab, Ab, p.second, exp );
        } else
          TCINSTRUCTIONS.insertLD_H( Da, *mPrecomputedOffReg, 0, exp );
      }
    }
  }
};


/*
  Inserts a LD.HU instruction.

  Exact format: LD.HU D[a] (def), [A[b] (use)]off (DAC10BOA)

  Handling of address offsets beyond signed 10 bits is included. Exact
  formats either:

  ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
  LD.HU D[a] (def), [A[x] (use)]<lower 10 bits of off> (DAC10BOA)

  or:

  ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
  LEA A[x] (def), [A[x] (use)]<lower 16 bits of off> (AAC16BOA) \n
  LD.HU D[a] (def), [A[x] (use)]0 (DAC10BOA)
*/
void TC_AddressModification::insertLD_HU( LLIR_Register *Da, LLIR_Register *Ab,
                                          int off, const IR_Exp *exp )
{
  DSTART(
    "void TC_AddressModification::insertLD_HU(LLIR_Register*, LLIR_Register*, int, const IR_Exp*)" );

  if ( ( off >= TC_Const10_Signed::getMinValue( 10 ) ) &&
       ( off <= TC_Const10_Signed::getMaxValue( 10 ) ) )
    TCINSTRUCTIONS.insertLD_HU( Da, OPER_BASE, Ab, off, exp );
  else {
    auto p = InstructionFactory::splitOffset( off );

    if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
         ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
      if ( !mModApplied ) {
        mPrecomputedOffset = TCINSTRUCTIONS.CreateRegister( "", true );
        TCINSTRUCTIONS.insertADDIH_A( mPrecomputedOffset, Ab, p.first, exp );
      }
      TCINSTRUCTIONS.insertLD_HU(
        Da, OPER_BASE, mPrecomputedOffset, p.second, exp );
    } else {
      if ( !mModApplied ) {
        mPrecomputedOffset = TCINSTRUCTIONS.CreateRegister( "", true );
        TCINSTRUCTIONS.insertADDIH_A( mPrecomputedOffset, Ab, p.first, exp );
        TCINSTRUCTIONS.insertLEA(
          mPrecomputedOffset, OPER_BASE, mPrecomputedOffset, p.second, exp );
      }
      TCINSTRUCTIONS.insertLD_HU( Da, OPER_BASE, mPrecomputedOffset, 0, exp );
    }
  }
};


/*
  Inserts a LD.HU instruction.

  Exact formats:

  LD.HU D[a] (def), [+A[b] (defuse)]off (DAC10PIA) \n
  LD.HU D[a] (def), [A[b] (defuse)+]off (DAC10PIA)

  Handling of address offsets larger than signed 10 bits but within signed
  16 bits is included. Exact formats for pre-increment:

  LEA A[b] (def), [A[b] (use)]off (AAC16BOA) \n
  LD.HU D[a] (def), [A[b] (use)]0 (DAC10BOA)

  Exact formats for post-increment:

  LD.HU D[a] (def), [A[b] (use)]0 (DAC10BOA) \n
  LEA A[b] (def), [A[b] (use)]off (AAC16BOA)

  Handling of address offsets beyond signed 16 bits is also included. Exact
  formats for pre-increment either:

  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
  LD.HU D[a] (def), [+A[b] (defuse)]<lower 10 bits of off> (DAC10PIA)

  or:

  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
  LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA) \n
  LD.HU D[a] (def), [A[b] (use)]0 (DAC10BOA)

  Exact formats for post-increment either:

  LD.HU D[a] (def), [A[b] (defuse)+]<lower 10 bits of off> (DAC10PIA) \n
  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)

  or:

  LD.HU D[a] (def), [A[b] (use)]0 (DAC10BOA) \n
  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
  LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA)
*/
void TC_AddressModification::insertLD_HU( LLIR_Register *Da,
                                          const std::string &m,
                                          LLIR_Register *Ab, int off,
                                          const IR_Exp *exp )
{
  DSTART(
    "void TC_AddressModification::insertLD_HU(LLIR_Register*, const string&, LLIR_Register*, int, const IR_Exp*)" );

  if ( ( off >= TC_Const10_Signed::getMinValue( 10 ) ) &&
       ( off <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
    if ( m == OPER_PREINC ) {
      if ( !mModApplied ) {
        mPrecomputedOffset = Ab;
        TCINSTRUCTIONS.insertLD_HU( Da, m, Ab, off, exp );
      } else
        TCINSTRUCTIONS.insertLD_HU( Da, OPER_BASE, mPrecomputedOffset, 0, exp );
    } else {
      if ( !mModApplied ) {
        mPrecomputedOffset = TCINSTRUCTIONS.CreateRegister( "", true );
        TCINSTRUCTIONS.insertMOV_AA( mPrecomputedOffset, Ab, exp );
        TCINSTRUCTIONS.insertLD_HU( Da, m, Ab, off, exp );
      } else
        TCINSTRUCTIONS.insertLD_HU( Da, OPER_BASE, mPrecomputedOffset, 0, exp );
    }
  } else

  if ( ( off >= TC_Const16_Signed::getMinValue( 16 ) ) &&
       ( off <= TC_Const16_Signed::getMaxValue( 16 ) ) ) {
    if ( m == OPER_PREINC ) {
      if ( !mModApplied ) {
        mPrecomputedOffset = Ab;
        TCINSTRUCTIONS.insertLEA( Ab, OPER_BASE, Ab, off, exp );
      }
      TCINSTRUCTIONS.insertLD_HU( Da, OPER_BASE, mPrecomputedOffset, 0, exp );
    } else {
      if ( !mModApplied ) {
        mPrecomputedOffset = TCINSTRUCTIONS.CreateRegister( "", true );
        TCINSTRUCTIONS.insertMOV_AA( mPrecomputedOffset, Ab, exp );
        TCINSTRUCTIONS.insertLD_HU( Da, OPER_BASE, Ab, 0, exp );
        TCINSTRUCTIONS.insertLEA( Ab, OPER_BASE, Ab, off, exp );
      } else
        TCINSTRUCTIONS.insertLD_HU( Da, OPER_BASE, mPrecomputedOffset, 0, exp );
    }
  } else {
    auto p = InstructionFactory::splitOffset( off );

    if ( m == OPER_PREINC ) {
      if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
           ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
        if ( !mModApplied ) {
          mPrecomputedOffset = Ab;
          TCINSTRUCTIONS.insertADDIH_A( Ab, Ab, p.first, exp );
          TCINSTRUCTIONS.insertLD_HU( Da, m, Ab, p.second, exp );
        } else
          TCINSTRUCTIONS.insertLD_HU(
            Da, OPER_BASE, mPrecomputedOffset, 0, exp );
      } else {
        if ( !mModApplied ) {
          mPrecomputedOffset = Ab;
          TCINSTRUCTIONS.insertADDIH_A( Ab, Ab, p.first, exp );
          TCINSTRUCTIONS.insertLEA( Ab, OPER_BASE, Ab, p.second, exp );
        }
        TCINSTRUCTIONS.insertLD_HU( Da, OPER_BASE, mPrecomputedOffset, 0, exp );
      }
    } else {
      if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
           ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
        if ( !mModApplied ) {
          mPrecomputedOffset = TCINSTRUCTIONS.CreateRegister( "", true );
          TCINSTRUCTIONS.insertMOV_AA( mPrecomputedOffset, Ab, exp );
          TCINSTRUCTIONS.insertLD_HU( Da, m, Ab, p.second, exp );
          TCINSTRUCTIONS.insertADDIH_A( Ab, Ab, p.first, exp );
        } else
          TCINSTRUCTIONS.insertLD_HU(
            Da, OPER_BASE, mPrecomputedOffset, 0, exp );
      } else {
        if ( !mModApplied ) {
          mPrecomputedOffset = TCINSTRUCTIONS.CreateRegister( "", true );
          TCINSTRUCTIONS.insertMOV_AA( mPrecomputedOffset, Ab, exp );
          TCINSTRUCTIONS.insertLD_HU( Da, OPER_BASE, Ab, 0, exp );
          TCINSTRUCTIONS.insertADDIH_A( Ab, Ab, p.first, exp );
          TCINSTRUCTIONS.insertLEA( Ab, OPER_BASE, Ab, p.second, exp );
        } else
          TCINSTRUCTIONS.insertLD_HU(
            Da, OPER_BASE, mPrecomputedOffset, 0, exp );
      }
    }
  }
};


/*
  Inserts a LD.HU instruction.

  Exact format: LD.HU D[a] (def), [A[b] (use)]off (DAC10BOA)

  Handling of address offsets beyond signed 10 bits is included. Exact
  formats either:

  ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
  LD.HU D[a] (def), [A[x] (use)]<lower 10 bits of off> (DAC10BOA)

  or:

  ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
  LEA A[x] (def), [A[x] (use)]<lower 16 bits of off> (AAC16BOA) \n
  LD.HU D[a] (def), [A[x] (use)]0 (DAC10BOA)
*/
void TC_AddressModification::insertLD_HU( const WIR::TC_DRegV &Da,
                                          const WIR::TC_ARegV &Ab, int off,
                                          const IR_Exp *exp )
{
  DSTART(
    "void TC_AddressModification::insertLD_HU(const TC_DRegV&, const TC_ARegV&, int, const IR_Exp*)" );

  if ( ( off >= TC_Const10_Signed::getMinValue( 10 ) ) &&
       ( off <= TC_Const10_Signed::getMaxValue( 10 ) ) )
    TCINSTRUCTIONS.insertLD_HU( Da, Ab, off, exp );
  else {
    auto p = InstructionFactory::splitOffset( off );

    if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
         ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
      if ( !mModApplied ) {
        mPrecomputedOffReg = &TCINSTRUCTIONS.createAReg();
        TCINSTRUCTIONS.insertADDIH_A( *mPrecomputedOffReg, Ab, p.first, exp );
      }
      TCINSTRUCTIONS.insertLD_HU( Da, *mPrecomputedOffReg, p.second, exp );
  } else {
      if ( !mModApplied ) {
        mPrecomputedOffReg = &TCINSTRUCTIONS.createAReg();
        TCINSTRUCTIONS.insertADDIH_A( *mPrecomputedOffReg, Ab, p.first, exp );
        TCINSTRUCTIONS.insertLEA(
          *mPrecomputedOffReg, *mPrecomputedOffReg, p.second, exp );
      }
      TCINSTRUCTIONS.insertLD_HU( Da, *mPrecomputedOffReg, 0, exp );
    }
  }
};


/*
  Inserts a LD.HU instruction.

  Exact formats:

  LD.HU D[a] (def), [+A[b] (defuse)]off (DAC10PIA) \n
  LD.HU D[a] (def), [A[b] (defuse)+]off (DAC10PIA)

  Handling of address offsets larger than signed 10 bits but within signed
  16 bits is included. Exact formats for pre-increment:

  LEA A[b] (def), [A[b] (use)]off (AAC16BOA) \n
  LD.HU D[a] (def), [A[b] (use)]0 (DAC10BOA)

  Exact formats for post-increment:

  LD.HU D[a] (def), [A[b] (use)]0 (DAC10BOA) \n
  LEA A[b] (def), [A[b] (use)]off (AAC16BOA)

  Handling of address offsets beyond signed 16 bits is also included. Exact
  formats for pre-increment either:

  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
  LD.HU D[a] (def), [+A[b] (defuse)]<lower 10 bits of off> (DAC10PIA)

  or:

  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
  LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA) \n
  LD.HU D[a] (def), [A[b] (use)]0 (DAC10BOA)

  Exact formats for post-increment either:

  LD.HU D[a] (def), [A[b] (defuse)+]<lower 10 bits of off> (DAC10PIA) \n
  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)

  or:

  LD.HU D[a] (def), [A[b] (use)]0 (DAC10BOA) \n
  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
  LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA)
*/
void TC_AddressModification::insertLD_HU( const WIR::TC_DRegV &Da,
                                          const WIR::TC13::AddressingMode &m,
                                          const WIR::TC_ARegV &Ab, int off,
                                          const IR_Exp *exp )
{
  DSTART(
    "void TC_AddressModification::insertLD_HU(const TC_DRegV&, const TC13::AddressingMode&, const TC_ARegV&, int, const IR_Exp*)" );

  if ( ( off >= TC_Const10_Signed::getMinValue( 10 ) ) &&
       ( off <= TC_Const10_Signed::getMaxValue( 10 ) ) )
    if ( m == TC13::AddressingMode::pre ) {
      if ( !mModApplied ) {
        mPrecomputedOffReg = const_cast<TC_ARegV *>( &Ab );
        TCINSTRUCTIONS.insertLD_HU( Da, m, Ab, off, exp );
      } else
        TCINSTRUCTIONS.insertLD_HU( Da, *mPrecomputedOffReg, 0, exp );
    } else {
      if ( !mModApplied ) {
        mPrecomputedOffReg = &TCINSTRUCTIONS.createAReg();
        TCINSTRUCTIONS.insertMOV_AA( *mPrecomputedOffReg, Ab, exp );
        TCINSTRUCTIONS.insertLD_HU( Da, m, Ab, off, exp );
      } else
        TCINSTRUCTIONS.insertLD_HU( Da, *mPrecomputedOffReg, 0, exp );
    }
  else

  if ( ( off >= TC_Const16_Signed::getMinValue( 16 ) ) &&
       ( off <= TC_Const16_Signed::getMaxValue( 16 ) ) ) {
    if ( m == TC13::AddressingMode::pre ) {
      if ( !mModApplied ) {
        mPrecomputedOffReg = const_cast<TC_ARegV *>( &Ab );
        TCINSTRUCTIONS.insertLEA( Ab, Ab, off, exp );
      }
      TCINSTRUCTIONS.insertLD_HU( Da, *mPrecomputedOffReg, 0, exp );
    } else {
      if ( !mModApplied ) {
        mPrecomputedOffReg = &TCINSTRUCTIONS.createAReg();
        TCINSTRUCTIONS.insertMOV_AA( *mPrecomputedOffReg, Ab, exp );
        TCINSTRUCTIONS.insertLD_HU( Da, Ab, 0, exp );
        TCINSTRUCTIONS.insertLEA( Ab, Ab, off, exp );
      } else
        TCINSTRUCTIONS.insertLD_HU( Da, *mPrecomputedOffReg, 0, exp );
    }
  } else {
    auto p = InstructionFactory::splitOffset( off );

    if ( m == TC13::AddressingMode::pre ) {
      if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
           ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
        if ( !mModApplied ) {
          mPrecomputedOffReg = const_cast<TC_ARegV *>( &Ab );
          TCINSTRUCTIONS.insertADDIH_A( Ab, Ab, p.first, exp );
          TCINSTRUCTIONS.insertLD_HU( Da, m, Ab, p.second, exp );
        } else
          TCINSTRUCTIONS.insertLD_HU( Da, *mPrecomputedOffReg, 0, exp );
      } else {
        if ( !mModApplied ) {
          mPrecomputedOffReg = const_cast<TC_ARegV *>( &Ab );
          TCINSTRUCTIONS.insertADDIH_A( Ab, Ab, p.first, exp );
          TCINSTRUCTIONS.insertLEA( Ab, Ab, p.second, exp );
        }
        TCINSTRUCTIONS.insertLD_HU( Da, *mPrecomputedOffReg, 0, exp );
      }
    } else {
      if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
           ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
        if ( !mModApplied ) {
          mPrecomputedOffReg = &TCINSTRUCTIONS.createAReg();
          TCINSTRUCTIONS.insertMOV_AA( *mPrecomputedOffReg, Ab, exp );
          TCINSTRUCTIONS.insertLD_HU( Da, m, Ab, p.second, exp );
          TCINSTRUCTIONS.insertADDIH_A( Ab, Ab, p.first, exp );
        } else
          TCINSTRUCTIONS.insertLD_HU( Da, *mPrecomputedOffReg, 0, exp );
      } else {
        if ( !mModApplied ) {
          mPrecomputedOffReg = &TCINSTRUCTIONS.createAReg();
          TCINSTRUCTIONS.insertMOV_AA( *mPrecomputedOffReg, Ab, exp );
          TCINSTRUCTIONS.insertLD_HU( Da, Ab, 0, exp );
          TCINSTRUCTIONS.insertADDIH_A( Ab, Ab, p.first, exp );
          TCINSTRUCTIONS.insertLEA( Ab, Ab, p.second, exp );
        } else
          TCINSTRUCTIONS.insertLD_HU( Da, *mPrecomputedOffReg, 0, exp );
      }
    }
  }
};


/*
  Inserts a LD.W instruction.

  Exact format: LD.W D[a] (def), [A[b] (use)]off (DAC16BOA)

  Handling of address offsets beyond signed 16 bits is included. Exact
  formats:

  ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
  LD.W D[a] (def), [A[x] (use)]<lower 16 bits of off> (DAC16BOA)

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
void TC_AddressModification::insertLD_W( LLIR_Register *Da, LLIR_Register *Ab,
                                         int off, const IR_Exp *exp )
{
  DSTART(
    "void TC_AddressModification::insertLD_W(LLIR_Register*, LLIR_Register*, int, const IR_Exp*)" );

  if ( ( off >= TC_Const16_Signed::getMinValue( 16 ) ) &&
       ( off <= TC_Const16_Signed::getMaxValue( 16 ) ) )
    TCINSTRUCTIONS.insertLD_W( Da, OPER_BASE, Ab, off, exp );
  else {
    auto p = InstructionFactory::splitOffset( off );

    if ( !mModApplied ) {
      mPrecomputedOffset = TCINSTRUCTIONS.CreateRegister( "", true );
      TCINSTRUCTIONS.insertADDIH_A( mPrecomputedOffset, Ab, p.first, exp );
    }

    TCINSTRUCTIONS.insertLD_W(
      Da, OPER_BASE, mPrecomputedOffset, p.second, exp );
  }
};


/*
  Inserts a LD.W instruction.

  Exact formats:

  LD.W D[a] (def), [+A[b] (defuse)]off (DAC10PIA) \n
  LD.W D[a] (def), [A[b] (defuse)+]off (DAC10PIA)

  Handling of address offsets larger than signed 10 bits but within signed
  16 bits is included. Exact formats for pre-increment:

  LEA A[b] (def), [A[b] (use)]off (AAC16BOA) \n
  LD.W D[a] (def), [A[b] (use)]0 (DAC16BOA)

  Exact formats for post-increment:

  LD.W D[a] (def), [A[b] (use)]0 (DAC16BOA) \n
  LEA A[b] (def), [A[b] (use)]off (AAC16BOA)

  Handling of address offsets beyond signed 16 bits is also included. Exact
  formats for pre-increment either:

  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
  LD.W D[a] (def), [+A[b] (defuse)]<lower 10 bits of off> (DAC10PIA)

  or:

  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
  LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA) \n
  LD.W D[a] (def), [A[b] (use)]0 (DAC16BOA)

  Exact formats for post-increment either:

  LD.W D[a] (def), [A[b] (defuse)+]<lower 10 bits of off> (DAC10PIA) \n
  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)

  or:

  LD.W D[a] (def), [A[b] (use)]0 (DAC16BOA) \n
  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
  LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA)

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
void TC_AddressModification::insertLD_W( LLIR_Register *Da,
                                         const std::string &m,
                                         LLIR_Register *Ab, int off,
                                         const IR_Exp *exp )
{
  DSTART(
    "void TC_AddressModification::insertLD_W(LLIR_Register*, const string&, LLIR_Register*, int, const IR_Exp*)" );

  if ( ( off >= TC_Const10_Signed::getMinValue( 10 ) ) &&
       ( off <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
    if ( m == OPER_PREINC ) {
      if ( !mModApplied ) {
        mPrecomputedOffset = Ab;
        TCINSTRUCTIONS.insertLD_W( Da, m, Ab, off, exp );
      } else
        TCINSTRUCTIONS.insertLD_W( Da, OPER_BASE, mPrecomputedOffset, 0, exp );
    } else {
      if ( !mModApplied ) {
        mPrecomputedOffset = TCINSTRUCTIONS.CreateRegister( "", true );
        TCINSTRUCTIONS.insertMOV_AA( mPrecomputedOffset, Ab, exp );
        TCINSTRUCTIONS.insertLD_W( Da, m, Ab, off, exp );
      } else
        TCINSTRUCTIONS.insertLD_W( Da, OPER_BASE, mPrecomputedOffset, 0, exp );
    }
  } else

  if ( ( off >= TC_Const16_Signed::getMinValue( 16 ) ) &&
       ( off <= TC_Const16_Signed::getMaxValue( 16 ) ) ) {
    if ( m == OPER_PREINC ) {
      if ( !mModApplied ) {
        mPrecomputedOffset = Ab;
        TCINSTRUCTIONS.insertLEA( Ab, OPER_BASE, Ab, off, exp );
      }
      TCINSTRUCTIONS.insertLD_W( Da, OPER_BASE, mPrecomputedOffset, 0, exp );
    } else {
      if ( !mModApplied ) {
        mPrecomputedOffset = TCINSTRUCTIONS.CreateRegister( "", true );
        TCINSTRUCTIONS.insertMOV_AA( mPrecomputedOffset, Ab, exp );
        TCINSTRUCTIONS.insertLD_W( Da, OPER_BASE, Ab, 0, exp );
        TCINSTRUCTIONS.insertLEA( Ab, OPER_BASE, Ab, off, exp );
      } else
        TCINSTRUCTIONS.insertLD_W( Da, OPER_BASE, mPrecomputedOffset, 0, exp );
    }
  } else {
    auto p = InstructionFactory::splitOffset( off );

    if ( m == OPER_PREINC ) {
      if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
           ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
        if ( !mModApplied ) {
          mPrecomputedOffset = Ab;
          TCINSTRUCTIONS.insertADDIH_A( Ab, Ab, p.first, exp );
          TCINSTRUCTIONS.insertLD_W( Da, m, Ab, p.second, exp );
        } else
          TCINSTRUCTIONS.insertLD_W( Da, OPER_BASE, mPrecomputedOffset, 0, exp );
      } else {
        if ( !mModApplied ) {
          mPrecomputedOffset = Ab;
          TCINSTRUCTIONS.insertADDIH_A( Ab, Ab, p.first, exp );
          TCINSTRUCTIONS.insertLEA( Ab, OPER_BASE, Ab, p.second, exp );
        }
        TCINSTRUCTIONS.insertLD_W( Da, OPER_BASE, mPrecomputedOffset, 0, exp );
      }
    } else {
      if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
           ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
        if ( !mModApplied ) {
          mPrecomputedOffset = TCINSTRUCTIONS.CreateRegister( "", true );
          TCINSTRUCTIONS.insertMOV_AA( mPrecomputedOffset, Ab, exp );
          TCINSTRUCTIONS.insertLD_W( Da, m, Ab, p.second, exp );
          TCINSTRUCTIONS.insertADDIH_A( Ab, Ab, p.first, exp );
        } else
          TCINSTRUCTIONS.insertLD_W( Da, OPER_BASE, mPrecomputedOffset, 0, exp );
      } else {
        if ( !mModApplied ) {
          mPrecomputedOffset = TCINSTRUCTIONS.CreateRegister( "", true );
          TCINSTRUCTIONS.insertMOV_AA( mPrecomputedOffset, Ab, exp );
          TCINSTRUCTIONS.insertLD_W( Da, OPER_BASE, Ab, 0, exp );
          TCINSTRUCTIONS.insertADDIH_A( Ab, Ab, p.first, exp );
          TCINSTRUCTIONS.insertLEA( Ab, OPER_BASE, Ab, p.second, exp );
        } else
          TCINSTRUCTIONS.insertLD_W( Da, OPER_BASE, mPrecomputedOffset, 0, exp );
      }
    }
  }
};


/*
  Inserts a LD.W instruction.

  Exact format: LD.W D[a] (def), [A[b] (use)]off (DAC16BOA)

  Handling of address offsets beyond signed 16 bits is included. Exact
  formats:

  ADDIH.A A[x] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
  LD.W D[a] (def), [A[x] (use)]<lower 16 bits of off> (DAC16BOA)

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
void TC_AddressModification::insertLD_W( const WIR::TC_DRegV &Da,
                                         const WIR::TC_ARegV &Ab, int off,
                                         const IR_Exp *exp )
{
  DSTART(
    "void TC_AddressModification::insertLD_W(const TC_DRegV&, const TC_ARegV&, int, const IR_Exp*)" );

  if ( ( off >= TC_Const16_Signed::getMinValue( 16 ) ) &&
       ( off <= TC_Const16_Signed::getMaxValue( 16 ) ) )
    TCINSTRUCTIONS.insertLD_W( Da, Ab, off, exp );
  else {
    auto p = InstructionFactory::splitOffset( off );

    if ( !mModApplied ) {
      mPrecomputedOffReg = &TCINSTRUCTIONS.createAReg();
      TCINSTRUCTIONS.insertADDIH_A( *mPrecomputedOffReg, Ab, p.first, exp );
    }

    TCINSTRUCTIONS.insertLD_W( Da, *mPrecomputedOffReg, p.second, exp );
  }
};


/*
  Inserts a LD.W instruction.

  Exact formats:

  LD.W D[a] (def), [+A[b] (defuse)]off (AAC10PIA) \n
  LD.W D[a] (def), [A[b] (defuse)+]off (AAC10PIA)

  Handling of address offsets larger than signed 10 bits but within signed
  16 bits is included. Exact formats for pre-increment:

  LEA A[b] (def), [A[b] (use)]off (AAC16BOA) \n
  LD.W D[a] (def), [A[b] (use)]0 (DAC16BOA)

  Exact formats for post-increment:

  LD.W D[a] (def), [A[b] (use)]0 (DAC16BOA) \n
  LEA A[b] (def), [A[b] (use)]off (AAC16BOA)

  Handling of address offsets beyond signed 16 bits is also included. Exact
  formats for pre-increment either:

  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
  LD.W D[a] (def), [+A[b] (defuse)]<lower 10 bits of off> (DAC10PIA)

  or:

  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
  LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA) \n
  LD.W D[a] (def), [A[b] (use)]0 (DAC16BOA)

  Exact formats for post-increment either:

  LD.W D[a] (def), [A[b] (defuse)+]<lower 10 bits of off> (DAC10PIA) \n
  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16)

  or:

  LD.W D[a] (def), [A[b] (use)]0 (DAC16BOA) \n
  ADDIH.A A[b] (def), A[b] (use), <upper 16 bits of off> (AAC16) \n
  LEA A[b] (def), [A[b] (use)]<lower 16 bits of off> (AAC16BOA)

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
void TC_AddressModification::insertLD_W( const WIR::TC_DRegV &Da,
                                         const WIR::TC13::AddressingMode &m,
                                         const WIR::TC_ARegV &Ab, int off,
                                         const IR_Exp *exp )
{
  DSTART(
    "void TC_AddressModification::insertLD_W(const TC_DRegV&, const TC13::AddressingMode&, const TC_ARegV&, int, const IR_Exp*)" );

  if ( ( off >= TC_Const10_Signed::getMinValue( 10 ) ) &&
       ( off <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
    if ( m == TC13::AddressingMode::pre ) {
      if ( !mModApplied ) {
        mPrecomputedOffReg = const_cast<TC_ARegV *>( &Ab );
        TCINSTRUCTIONS.insertLD_W( Da, m, Ab, off, exp );
      } else
        TCINSTRUCTIONS.insertLD_W( Da, *mPrecomputedOffReg, 0, exp );
    } else {
      if ( !mModApplied ) {
        mPrecomputedOffReg = &TCINSTRUCTIONS.createAReg();
        TCINSTRUCTIONS.insertMOV_AA( *mPrecomputedOffReg, Ab, exp );
        TCINSTRUCTIONS.insertLD_W( Da, m, Ab, off, exp );
      } else
        TCINSTRUCTIONS.insertLD_W( Da, *mPrecomputedOffReg, 0, exp );
    }
  } else

  if ( ( off >= TC_Const16_Signed::getMinValue( 16 ) ) &&
       ( off <= TC_Const16_Signed::getMaxValue( 16 ) ) ) {
    if ( m == TC13::AddressingMode::pre ) {
      if ( !mModApplied ) {
        mPrecomputedOffReg = const_cast<TC_ARegV *>( &Ab );
        TCINSTRUCTIONS.insertLEA( Ab, Ab, off, exp );
      }
      TCINSTRUCTIONS.insertLD_W( Da, *mPrecomputedOffReg, 0, exp );
    } else {
      if ( !mModApplied ) {
        mPrecomputedOffReg = &TCINSTRUCTIONS.createAReg();
        TCINSTRUCTIONS.insertMOV_AA( *mPrecomputedOffReg, Ab, exp );
        TCINSTRUCTIONS.insertLD_W( Da, Ab, 0, exp );
        TCINSTRUCTIONS.insertLEA( Ab, Ab, off, exp );
      } else
        TCINSTRUCTIONS.insertLD_W( Da, *mPrecomputedOffReg, 0, exp );
    }
  } else {
    auto p = InstructionFactory::splitOffset( off );

    if ( m == TC13::AddressingMode::pre ) {
      if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
           ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
        if ( !mModApplied ) {
          mPrecomputedOffReg = const_cast<TC_ARegV *>( &Ab );
          TCINSTRUCTIONS.insertADDIH_A( Ab, Ab, p.first, exp );
          TCINSTRUCTIONS.insertLD_W( Da, m, Ab, p.second, exp );
        } else
          TCINSTRUCTIONS.insertLD_W( Da, *mPrecomputedOffReg, 0, exp );
      } else {
        if ( !mModApplied ) {
          mPrecomputedOffReg = const_cast<TC_ARegV *>( &Ab );
          TCINSTRUCTIONS.insertADDIH_A( Ab, Ab, p.first, exp );
          TCINSTRUCTIONS.insertLEA( Ab, Ab, p.second, exp );
        }
        TCINSTRUCTIONS.insertLD_W( Da, *mPrecomputedOffReg, 0, exp );
      }
    } else {
      if ( ( p.second >= TC_Const10_Signed::getMinValue( 10 ) ) &&
           ( p.second <= TC_Const10_Signed::getMaxValue( 10 ) ) ) {
        if ( !mModApplied ) {
          mPrecomputedOffReg = &TCINSTRUCTIONS.createAReg();
          TCINSTRUCTIONS.insertMOV_AA( *mPrecomputedOffReg, Ab, exp );
          TCINSTRUCTIONS.insertLD_W( Da, m, Ab, p.second, exp );
          TCINSTRUCTIONS.insertADDIH_A( Ab, Ab, p.first, exp );
        } else
          TCINSTRUCTIONS.insertLD_W( Da, *mPrecomputedOffReg, 0, exp );
      } else {
        if ( !mModApplied ) {
          mPrecomputedOffReg = &TCINSTRUCTIONS.createAReg();
          TCINSTRUCTIONS.insertMOV_AA( *mPrecomputedOffReg, Ab, exp );
          TCINSTRUCTIONS.insertLD_W( Da, Ab, 0, exp );
          TCINSTRUCTIONS.insertADDIH_A( Ab, Ab, p.first, exp );
          TCINSTRUCTIONS.insertLEA( Ab, Ab, p.second, exp );
        } else
          TCINSTRUCTIONS.insertLD_W( Da, *mPrecomputedOffReg, 0, exp );
      }
    }
  }
};


/*
  clone creates a copy of an address modification object.

  clone just calls the corresponding copy constructor.
*/
AddressModification *TC_AddressModification::clone( void ) const
{
  DSTART(
    "virtual AddressModification* TC_AddressModification::clone() const" );

  return( new TC_AddressModification( *this ) );
};


/*
  getOffsetFactor computes the factor by which the offset needs to be scaled
  according to a TC address modification's base type.
*/
int TC_AddressModification::getOffsetFactor( void ) const
{
  DSTART( "virtual int TC_AddressModification::getOffsetFactor() const" );

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
    ufAssertT( false, "No baseType specified in the TC_AddressModification." );
};
