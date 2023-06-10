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

// Include ICD headers
#include <llir3/llir3.h>

// Include libuseful headers
#include <libuseful/debugmacros.h>
#include <libuseful/io.h>

// Include local headers
#include <arm7/arm_incl.h>
#include <arm7/armaddressmodification.h>
#include <arm7/armlvalue.h>
#include <arm7/registrar.h>


//
// Code section
//


using namespace std;


//
// Public class methods
//

/*
  Constructor initializing an empty address modification.
*/
ARM_AddressModification::ARM_AddressModification( void ) :
  AddressModification {},
  mDryRun { false }
{
  DSTART( "ARM_AddressModification::ARM_AddressModification()" );
};


/*
  Constructor for case with local base register and register offset.
*/
ARM_AddressModification::ARM_AddressModification( LLIR_Register *reg,
                                                  LLIR_Register *oReg,
                                                  IR_Type *t, ModTime mt,
                                                  ModOper mo, bool bo,
                                                  bool dr ) :
  AddressModification { reg, nullptr, oReg, nullptr, t, bo, mt, mo },
  mDryRun { dr }
{
  DSTART(
    "ARM_AddressModification::ARM_AddressModification(LLIR_Register*, LLIR_Register*, IR_Type*, AddressModification::ModTime, AddressModification::ModOper, bool, bool)" );
};


/*
  Constructor for case with local base register and integer offset.
*/
ARM_AddressModification::ARM_AddressModification( LLIR_Register *reg, long o,
                                                  IR_Type *t, ModTime mt,
                                                  ModOper mo, bool bo,
                                                  bool dr ) :
  AddressModification { reg, nullptr, o, t, bo, mt, mo },
  mDryRun { dr }
{
  DSTART(
    "ARM_AddressModification::ARM_AddressModification(LLIR_Register*, long int, IR_Type*, AddressModification::ModTime, AddressModification::ModOper, bool, bool)" );
};


/*
  Constructor for case with local base register and no offset.
*/
ARM_AddressModification::ARM_AddressModification( LLIR_Register *reg,
                                                  IR_Type *t, bool dr ) :
  AddressModification {
    reg, nullptr, 0, t, false, ModTime::NONE, ModOper::ADD },
  mDryRun { dr }
{
  DSTART(
    "ARM_AddressModification::ARM_AddressModification(LLIR_Register*, IR_Type*, bool)" );
};


/*
  Constructor for case with global/stack-based base register and register
  offset.
*/
ARM_AddressModification::ARM_AddressModification( ARM_LValue *l,
                                                  LLIR_Register *oReg,
                                                  IR_Type *t, ModTime mt,
                                                  ModOper mo, bool bo,
                                                  bool dr ) :
  AddressModification { l, oReg, nullptr, t, bo, mt, mo },
  mDryRun { dr }
{
  DSTART(
    "ARM_AddressModification::ARM_AddressModification(ARM_LValue*, "
    "LLIR_Register*, IR_Type*, AddressModification::ModTime, "
    "AddressModification::ModOper, bool, bool)" );
};


/*
  Constructor for case with global/stack-based base register and integer offset.
*/
ARM_AddressModification::ARM_AddressModification( ARM_LValue *l, long o,
                                                  IR_Type *t, ModTime mt,
                                                  ModOper mo, bool bo,
                                                  bool dr ) :
  AddressModification { l, o, t, bo, mt, mo },
  mDryRun { dr }
{
  DSTART(
    "ARM_AddressModification::ARM_AddressModification(ARM_LValue*, long int, "
    "IR_Type*, AddressModification::ModTime, AddressModification::ModOper, "
    "bool, bool)" );
};


/*
  Copy constructor.
*/
ARM_AddressModification::ARM_AddressModification( const ARM_AddressModification& am ) :
  AddressModification { am },
  mDryRun { am.mDryRun }
{
  DSTART(
    "ARM_AddressModification::ARM_AddressModification(const ARM_AddressModification&)" );
};


/*
  Destructor.
*/
ARM_AddressModification::~ARM_AddressModification( void )
{
  DSTART( "virtual ARM_AddressModification::~ARM_AddressModification()" );
};


/*
  applyModification applies the pointer arithmetics encoded by this object and
  returns the calculated register.
*/
LLIR_Register *ARM_AddressModification::applyModification( IR_Exp *exp )
{
  // Never do this in a dry run!
  ufAssertT(
    !mDryRun, "Do not call 'applyModification' for a dry-run instance." );

  // Pointer to the register to be returned.
  LLIR_Register *retReg = nullptr;

  // Determine the base register.
  auto *baseReg = getBaseRegister();

  // Has a modification already been applied?
  if ( !mModApplied ) {
    // We distinguish the following three major cases.

    if ( !mORegister && !mOffset ) {
      // Case 1: No offset is applied whatsoever.
      // Well... no modification to perform, we *could* simply return the base
      // register. Yet, in some cases, the base register is the SP and some
      // other modifications will be applied on top of it (e.g., at arrays with
      // multiple indices). This is not valid, as it would change the SP.
      retReg = ARMINSTRUCTIONS.CreateRegister( "" );
      ARMINSTRUCTIONS.insertMOV( retReg, baseReg, exp );

    } else

    if ( !mORegister ) {
      // Case 2: An integer offset is applied.
      // First, calculate the actual offset that has to be added.
      auto byteOffset = mOffset * getOffsetFactor();

      // Then, check whether the size can be encoded as an immediate operand.
      if ( isValidARMImmediate( byteOffset ) ) {
        // In this case, the offset can be encoded as part of the ADD/SUB
        // operation.

        // Differentiate different indexing modes.
        if ( mModTime == ModTime::NONE ) {
          // No indexing (Immediate Offset).
          retReg = ARMINSTRUCTIONS.CreateRegister( "" );

          if ( mModOper == ModOper::ADD )
            ARMINSTRUCTIONS.insertADD(
              OPER_AL, retReg, baseReg, byteOffset, exp );
          else
            ARMINSTRUCTIONS.insertSUB(
              OPER_AL, retReg, baseReg, byteOffset, exp );
        } else

        if ( mModTime == ModTime::PRE ) {
          // Pre-indexed. The base address is the return register.
          retReg = baseReg;

          if ( mModOper == ModOper::ADD )
            ARMINSTRUCTIONS.insertADD(
              OPER_AL, baseReg, baseReg, byteOffset, exp );
          else
            ARMINSTRUCTIONS.insertSUB(
              OPER_AL, baseReg, baseReg, byteOffset, exp );

          // If the base register is a deref_dreg, store back.
          if ( mBaseLValueValid )
            dynamic_cast<ARM_LValue *>( mBaseLValue )->storeBack(
              baseReg, exp );
        } else {
          // Post-indexed. Copy the current base to return upwards.
          retReg = ARMINSTRUCTIONS.CreateRegister( "" );
          ARMINSTRUCTIONS.insertMOV( retReg, baseReg, exp );

          // Afterwards, apply the calculation.
          if ( mModOper == ModOper::ADD )
            ARMINSTRUCTIONS.insertADD(
              OPER_AL, baseReg, baseReg, byteOffset, exp );
          else
            ARMINSTRUCTIONS.insertSUB(
              OPER_AL, baseReg, baseReg, byteOffset, exp );

          // If the base register is a deref_dreg, store back.
          if ( mBaseLValueValid )
            dynamic_cast<ARM_LValue *>( mBaseLValue )->storeBack(
              baseReg, exp );
        }
      } else {
        // The offset is too big to be an immediate operand, so it has to
        // be loaded into a register first.
        LLIR_Register *tmpReg = ARMINSTRUCTIONS.CreateRegister( "" );
        ARMINSTRUCTIONS.insertMOV_ORR( tmpReg, byteOffset, exp );

        // Differentiate different indexing modes.
        if ( mModTime == ModTime::NONE ) {
          // No indexing (Immediate Offset).
          retReg = tmpReg;

          if ( mModOper == ModOper::ADD )
            ARMINSTRUCTIONS.insertADD( OPER_AL, tmpReg, baseReg, tmpReg, exp );
          else
            ARMINSTRUCTIONS.insertSUB( OPER_AL, tmpReg, baseReg, tmpReg, exp );
        } else

        if ( mModTime == ModTime::PRE ) {
          // Pre-indexed. The base address is the return register.
          retReg = baseReg;

          if ( mModOper == ModOper::ADD )
            ARMINSTRUCTIONS.insertADD( OPER_AL, baseReg, baseReg, tmpReg, exp );
          else
            ARMINSTRUCTIONS.insertSUB( OPER_AL, baseReg, baseReg, tmpReg, exp );

          // If the base register is a deref_dreg, store back.
          if ( mBaseLValueValid )
            dynamic_cast<ARM_LValue *>( mBaseLValue )->storeBack(
              baseReg, exp );
        } else {
          // Post-indexed. Copy the current base to return upwards.
          retReg = ARMINSTRUCTIONS.CreateRegister( "" );
          ARMINSTRUCTIONS.insertMOV( retReg, baseReg, exp );

          // Afterwards, apply the calculation.
          if ( mModOper == ModOper::ADD )
            ARMINSTRUCTIONS.insertADD( OPER_AL, baseReg, baseReg, tmpReg, exp );
          else
            ARMINSTRUCTIONS.insertSUB( OPER_AL, baseReg, baseReg, tmpReg, exp );

          // If the base register is a deref_dreg, store back.
          if ( mBaseLValueValid )
            dynamic_cast<ARM_LValue *>( mBaseLValue )->storeBack(
              baseReg, exp );
        }
      }
    } else {
      // Case 3: A register offset is applied.
      // If the typeSize is a power of two, the offset can be applied with a
      // single ADD/SUB instruction. Otherwise, things get expensive.
      int typeSize = getOffsetFactor();

      if ( ( typeSize > 0 ) && ((typeSize & (typeSize - 1)) == 0) ) {
        // Determine the exponent (i.e., perform log2(typeSize)).
        int e = 0;
        for ( ; typeSize != 1; typeSize >>= 1, ++e ) ;

        // Differentiate different indexing modes.
        if ( mModTime == ModTime::NONE ) {
          // No indexing (Immediate Offset).
          retReg = ARMINSTRUCTIONS.CreateRegister( "" );

          if ( mModOper == ModOper::ADD )
            ARMINSTRUCTIONS.insertADD(
              OPER_AL, retReg, baseReg, mORegister, OPER_LSL, e, exp );
          else
            ARMINSTRUCTIONS.insertSUB(
              OPER_AL, retReg, baseReg, mORegister, OPER_LSL, e, exp );
        } else

        if ( mModTime == ModTime::PRE ) {
          // Pre-indexed. The base address is the return register.
          retReg = baseReg;

          if ( mModOper == ModOper::ADD )
            ARMINSTRUCTIONS.insertADD(
              OPER_AL, baseReg, baseReg, mORegister, OPER_LSL, e, exp );
          else
            ARMINSTRUCTIONS.insertSUB(
              OPER_AL, baseReg, baseReg, mORegister, OPER_LSL, e, exp );

          // If the base register is a deref_dreg, store back.
          if ( mBaseLValueValid )
            dynamic_cast<ARM_LValue *>( mBaseLValue )->storeBack(
              baseReg, exp );
        } else {
          // Post-indexed. Copy the current base to return upwards.
          retReg = ARMINSTRUCTIONS.CreateRegister( "" );
          ARMINSTRUCTIONS.insertMOV( retReg, baseReg, exp );

          // Afterwards, apply the calculation.
          if ( mModOper == ModOper::ADD )
            ARMINSTRUCTIONS.insertADD(
              OPER_AL, baseReg, baseReg, mORegister, OPER_LSL, e, exp );
          else
            ARMINSTRUCTIONS.insertSUB(
              OPER_AL, baseReg, baseReg, mORegister, OPER_LSL, e, exp );

          // If the base register is a deref_dreg, store back.
          if ( mBaseLValueValid )
            dynamic_cast<ARM_LValue *>( mBaseLValue )->storeBack(
              baseReg, exp );
        }
      } else {
        // This is the expensive case. First, load the typeSize into a register.
        LLIR_Register *tmpReg = ARMINSTRUCTIONS.CreateRegister( "" );
        ARMINSTRUCTIONS.insertMOV_ORR( tmpReg, typeSize );

        // Then, perform the multiplication at runtime.
        ARMINSTRUCTIONS.insertMUL( OPER_AL, tmpReg, tmpReg, mORegister, exp );

        // Differentiate different indexing modes.
        if ( mModTime == ModTime::NONE ) {
          // No indexing (Immediate Offset).
          retReg = tmpReg;

          if ( mModOper == ModOper::ADD )
            ARMINSTRUCTIONS.insertADD( OPER_AL, tmpReg, baseReg, tmpReg, exp );
          else
            ARMINSTRUCTIONS.insertSUB( OPER_AL, tmpReg, baseReg, tmpReg, exp );
        } else

        if ( mModTime == ModTime::PRE ) {
          // Pre-indexed. The base address is the return register.
          retReg = baseReg;

          if ( mModOper == ModOper::ADD )
            ARMINSTRUCTIONS.insertADD( OPER_AL, baseReg, baseReg, tmpReg, exp );
          else
            ARMINSTRUCTIONS.insertSUB( OPER_AL, baseReg, baseReg, tmpReg, exp );

          // If the base register is a deref_dreg, store back.
          if ( mBaseLValueValid )
            dynamic_cast<ARM_LValue *>( mBaseLValue )->storeBack(
              baseReg, exp );
        } else {
          // Post-indexed. Copy the current base to return upwards.
          retReg = ARMINSTRUCTIONS.CreateRegister( "" );
          ARMINSTRUCTIONS.insertMOV( retReg, baseReg, exp );

          // Afterwards, apply the calculation.
          if ( mModOper == ModOper::ADD )
            ARMINSTRUCTIONS.insertADD( OPER_AL, baseReg, baseReg, tmpReg, exp );
          else
            ARMINSTRUCTIONS.insertSUB( OPER_AL, baseReg, baseReg, tmpReg, exp );

          // If the base register is a deref_dreg, store back.
          if ( mBaseLValueValid )
            dynamic_cast<ARM_LValue *>( mBaseLValue )->storeBack(
              baseReg, exp );
        }
      }
    }

  } else
    /*
      You shouldn't arrive here. Either you decide to resolve an
      ARM_AddressModification by performing pointer arithmetics at runtime (the
      above code), or you decide to load/store performing the arithmetics on the
      fly (as part of the LD/ST instructions). Don't mix the two, it'll only
      cause problems.
    */
    ufAssertT(
      false,
      "Do not call 'applyModification' after having created a load or store "
      "operation." );

  return( retReg );
};


/*
  applyModificationCost computes the costs for the pointer arithmetics encoded
  by this object.
*/
COST ARM_AddressModification::applyModificationCost( void ) const
{
  // Never calculate costs with a real run instance.
  ufAssertT(
    mDryRun, "Only call 'applyModificationCost' for a dry-run instance." );

  COST c = 0;

  // Has a modification already been applied?
  if ( !mModApplied ) {
    // We distinguish the following three major cases.

    // Case 1: No offset is applied whatsoever.
    if ( !mRegOffsetValid && !mOffset ) {
      // Well... no modification to perform. Simply return 0 costs.
    } else

    if ( !mRegOffsetValid ) {
      // Case 2: An integer offset is applied.
      // First, calculate the actual offset that has to be added.
      auto byteOffset = mOffset * getOffsetFactor();

      // Then, check whether the size can be encoded as an immediate operand.
      if ( isValidARMImmediate( byteOffset ) ) {
        // In this case, the offset can be encoded as part of the ADD/SUB
        // instruction.

        // Differentiate different indexing modes.
        if ( mModTime == ModTime::NONE )
          // No indexing (Immediate Offset).
          c +=
            ( mModOper == ModOper::ADD ) ?
              CT( INS_ADD_32 ) : CT( INS_SUB_32 );
        else

        if ( mModTime == ModTime::PRE ) {
          // Pre-indexed.
          c +=
            ( mModOper == ModOper::ADD ) ?
              CT( INS_ADD_32 ) : CT( INS_SUB_32 );

          // If the base register is a deref_dreg, store back.
          if ( mBaseLValueValid )
            c += dynamic_cast<ARM_LValue *>( mBaseLValue )->storeBackCost();
        } else {
          // Post-indexed. Copy the current base to return upwards.
          c += CT( INS_MOV_32 );

          // Afterwards, apply the calculation.
          c +=
            ( mModOper == ModOper::ADD ) ?
              CT( INS_ADD_32 ) : CT( INS_SUB_32 );

          // If the base register is a deref_dreg, store back.
          if ( mBaseLValueValid )
            c += dynamic_cast<ARM_LValue *>( mBaseLValue )->storeBackCost();
        }
      } else {
        // The offset is too big to be an immediate operand, so it has to be
        // loaded into a register first.
        c += insertMOV_ORR_Cost( byteOffset );

        // Differentiate different indexing modes.
        if ( mModTime == ModTime::NONE ) {
          // No indexing (Immediate Offset).
          c +=
            ( mModOper == ModOper::ADD ) ?
              CT( INS_ADD_32 ) : CT( INS_SUB_32 );
        } else

        if ( mModTime == ModTime::PRE ) {
          // Pre-indexed. The base address is the return register.
          c +=
            ( mModOper == ModOper::ADD ) ?
              CT( INS_ADD_32 ) : CT( INS_SUB_32 );

          // If the base register is a deref_dreg, store back.
          if ( mBaseLValueValid )
            c += dynamic_cast<ARM_LValue *>( mBaseLValue )->storeBackCost();
        } else {
          // Post-indexed. Copy the current base to return upwards.
          c += CT( INS_MOV_32 );

          // Afterwards, apply the calculation.
          c +=
            ( mModOper == ModOper::ADD ) ?
              CT( INS_ADD_32 ) : CT( INS_SUB_32 );

          // If the base register is a deref_dreg, store back.
          if ( mBaseLValueValid )
            c += dynamic_cast<ARM_LValue *>( mBaseLValue )->storeBackCost();
        }
      }
    } else {
      // Case 3: A register offset is applied.
      // If the typeSize is a power of two, the offset can be applied with a
      // single ADD/SUB instruction, otherwise things get expensive.
      int typeSize = getOffsetFactor();

      if ( ( typeSize > 0 ) && ((typeSize & (typeSize - 1)) == 0) ) {
        // Differentiate different indexing modes.
        if ( mModTime == ModTime::NONE )
          // No indexing (Immediate Offset).
          c +=
            ( mModOper == ModOper::ADD ) ?
              CT( INS_ADD_32 ) : CT( INS_SUB_32 );
        else

        if ( mModTime == ModTime::PRE ) {
          // Pre-indexed. The base address is the return register.
          c +=
            ( mModOper == ModOper::ADD ) ?
              CT( INS_ADD_32 ) : CT( INS_SUB_32 );

          // If the base register is a deref_dreg, store back.
          if ( mBaseLValueValid )
            c += dynamic_cast<ARM_LValue *>( mBaseLValue )->storeBackCost();
        } else {
          // Post-indexed. Copy the current base to return upwards.
          c += CT( INS_MOV_32 );

          // Afterwards, apply the calculation.
          c +=
            ( mModOper == ModOper::ADD ) ?
              CT( INS_ADD_32 ) : CT( INS_SUB_32 );

          // If the base register is a deref_dreg, store back.
          if ( mBaseLValueValid )
            c += dynamic_cast<ARM_LValue *>( mBaseLValue )->storeBackCost();
        }
      } else {
        // This is the expensive case. First, load the typeSize into a register.
        c += insertMOV_ORR_Cost( typeSize );

        // Then, perform the multiplication at runtime.
        c += CT( INS_MUL_32 );

        // Differentiate different indexing modes.
        if ( mModTime == ModTime::NONE )
          // No indexing (Immediate Offset).
          c +=
            ( mModOper == ModOper::ADD ) ?
              CT( INS_ADD_32 ) : CT( INS_SUB_32 );
        else

        if ( mModTime == ModTime::PRE ) {
          // Pre-indexed. The base address is the return register.
          c +=
            ( mModOper == ModOper::ADD ) ?
              CT( INS_ADD_32 ) : CT( INS_SUB_32 );

          // If the base register is a deref_dreg, store back.
          if ( mBaseLValueValid )
            c += dynamic_cast<ARM_LValue *>( mBaseLValue )->storeBackCost();
        } else {
          // Post-indexed. Copy the current base to return upwards.
          c += CT( INS_MOV_32 );

          // Afterwards, apply the calculation.
          c +=
            ( mModOper == ModOper::ADD ) ?
              CT( INS_ADD_32 ) : CT( INS_SUB_32 );

          // If the base register is a deref_dreg, store back.
          if ( mBaseLValueValid )
            c += dynamic_cast<ARM_LValue *>( mBaseLValue )->storeBackCost();
        }
      }
    }
  } else
    /*
      You shouldn't arrive here. Either you decide to resolve an
      ARM_AddressModification by performing pointer arithmetics at runtime (the
      above code), or you decide to load/store performing the arithmetics on the
      fly (as part of the LD/ST instructions). Don't mix the two, it'll only
      cause problems.
    */
    ufAssertT(
      false,
      "Do not call 'applyModificationCost' after having created a load or "
      "store operation." );

  return( c );
};


/*
  applyModificationWorstCost computes the worst-case costs for the pointer
  arithmetics encoded by this object.

  Note that this function assumes worst-case calculations are used along the
  entire tree *and* that the tree pattern matcher assumes that a value is always
  loaded from memory first and optionally stored back later (i.e.,
  loadResult = true).
*/
COST ARM_AddressModification::applyModificationWorstCost( void )
{
  COST c = 0;

  // The worst-case scenario consists of ...
  // (1) Creating a backup-copy of the original address (post-indexing).
  c += CT( INS_MOV_32 );
  // (2) Loading a massive type size.
  c += CT( INS_MOV_32 ) + 3 * CT( INS_ORR_32 );
  // (3) Multiplying at runtime.
  c += CT( INS_MUL_32 );
  // (4) Adding on top of the base address.
  c += CT( INS_ADD_32 );
  // (5) And storing back, because the base address is in memory.
  c += CT( INS_STR_32 );

  return( c );
};


/*
  createLoad generates code that loads the value from the effective address
  encoded by this object into the given register.
*/
void ARM_AddressModification::createLoad( LLIR_Register *dest, IR_Exp* exp )
{
  // Never do this in a dry run!
  ufAssertT( !mDryRun, "Do not call 'createLoad' for a dry-run instance." );

  // Call internal helper.
  createLoadStore( false, dest, exp );
};


/*
  createLoadCost computes the costs for a load of the value from the effective
  address encoded by this object into a register.
*/
COST ARM_AddressModification::createLoadCost( void )
{
  // Never calculate costs with a real run instance.
  ufAssertT( mDryRun, "Only call 'createLoadCost' for a dry-run instance." );

  // Call internal helper.
  return( createLoadStoreCost( false ) );
};


/*
  createLoadWorstCost computes the worst-case costs for a load of the value from
  the effective address encoded by this object into a register.
*/
COST ARM_AddressModification::createLoadWorstCost( void )
{
  COST c = 0;

  // The worst case scenario consists of ...
  // (1) Loading a massive type size.
  c += CT( INS_MOV_32 ) + 3 * CT( INS_ORR_32 );
  // (2) Multiplying at runtime.
  c += CT( INS_MUL_32 );
  // (3) Loading from the base address with a register offset.
  c += CT( INS_LDR_32 );
  // (4) And storing back because the base address is in memory.
  c += CT( INS_STR_32 );

  return( c );
};


/*
  createStore generates code that stores the given register to the effective
  address encoded by this object.
*/
void ARM_AddressModification::createStore( LLIR_Register *source, IR_Exp* exp )
{
  // Never do this in a dry run!
  ufAssertT( !mDryRun, "Do not call 'createStore' for a dry-run instance." );

  // Call internal helper.
  createLoadStore( true, source, exp );
};


/*
  createStoreCost computes the costs for a store of a register to the effective
  address encoded by this object.
*/
COST ARM_AddressModification::createStoreCost( void )
{
  // Never calculate costs with a real run instance.
  ufAssertT( mDryRun, "Only call 'createStoreCost' for a dry-run instance." );

  // Call internal helper.
  return( createLoadStoreCost( true ) );
};


/*
  createStoreWorstCost computes the worst-case costs for a store of a register
  to the effective address encoded by this object.

  Note that this cost is very small, because it is assumed that worst-case
  estimates are used along the entire tree. In that case, the bulk of the cost
  lies within the load, that is assumed to always take place, even if loadResult
  turns out to be false.
*/
COST ARM_AddressModification::createStoreWorstCost( void )
{
  COST c = 0;

  /*
    This function assumes that the createLoadWorstCost()-function is used as
    well during cost calculation, and is used even when no load from the address
    is performed (loadResult=false). I.e., this function assumes that the cost
    calculator during the dereference-operator always assumes the value has to
    be loaded. If a load was created, the worst-case cost would only be a single
    ST-operation, since the expensive calculations are already done (cf.
    mPrecomuptedOffset).
  */

  // The worst case scenario consists of ...
  // (1) Creating the store.
  c += CT( INS_STR_32 );

  return( c );
};


//
// Protected class methods
//

void ARM_AddressModification::createLoadStore( bool createStore,
                                               LLIR_Register *reg, IR_Exp *exp )
{
  // Never do this in a dry run!
  ufAssertT(
    !mDryRun, "Do not call 'createLoadStore' for a dry-run instance." );

  // Sanity check.
  ufAssertT( mBaseType, "BaseType missing!" );

  if ( !mModApplied ) {
    // First up is the case that this is the "first" access.
    // We differentiate between three major cases.

    if ( !mORegister && !mOffset )
      // Case 1: No offset is applied whatsoever.
      // This case is nice and simple, as no write-back has to ever be
      // performed.
      insertLdSt(
        createStore, OPER_IMMOFF, reg, nullptr, 0, "", 0, mModOper, exp );
    else

    if ( !mORegister ) {
      // Case 2: An integer offset is applied.
      // First, calculate the actual offset that has to be added.
      int byteOffset = mOffset * getOffsetFactor();

      // Then, determine the maximally allowed offset.
      // Mode 3 allows for up to 8 bits, Mode 2 for up to 12 bits.
      int maxIntOffset = isMode3() ? (1 << 8) : (1 << 12);

      if ( byteOffset < maxIntOffset ) {

        // Choose the correct indexing mode.
        string mode2;
        if ( mModTime == ModTime::NONE )
          mode2 = OPER_IMMOFF;
        else

        if ( mModTime == ModTime::PRE )
          mode2 = OPER_IMMPRE;
        else

        if ( mModTime == ModTime::POST )
          mode2 = OPER_IMMPOST;

        // Insert a single instruction.
        insertLdSt(
          createStore, mode2, reg, nullptr, byteOffset, "", 0, mModOper, exp );
      } else {
        // The more annoying case, in which we have to load the immediate
        // constant first.
        LLIR_Register *tempReg = ARMINSTRUCTIONS.CreateRegister( "" );
        ARMINSTRUCTIONS.insertMOV_ORR( tempReg, byteOffset );

        // Choose the correct indexing mode.
        string mode2;
        if ( mModTime == ModTime::NONE )
          mode2 = OPER_ROFF;
        else

        if ( mModTime == ModTime::PRE )
          mode2 = OPER_RPRE;
        else

        if ( mModTime == ModTime::POST )
          mode2 = OPER_RPOST;

        // Remember the register that contains that immediate constant, in case
        // a second load/store will be performed.
        mPrecomputedOffset = tempReg;

        // Afterwards, we insert the LD/ST.
        insertLdSt(
          createStore, mode2, reg, tempReg, 0, "", 0, mModOper, exp );
      }
    } else {
      // Case 3: A register offset is applied.
      // If the typeSize is a power of two, the arithmetics can be performed in
      // a single operation.
      int typeSize = getOffsetFactor();

      if ( ( typeSize > 0 ) && ((typeSize & (typeSize - 1)) == 0) ) {
        // Determine the exponent (i.e., perform log2(typeSize)).
        int e = 0;
        for ( ; typeSize != 1; typeSize >>= 1, ++e ) ;

        // Scaled register offsets are only possible in mode 2. For mode 3, we
        // insert an additional MOV instruction performing the shift.

        if ( isMode3() ) {
          // Perform the shift.
          LLIR_Register *tempReg = ARMINSTRUCTIONS.CreateRegister( "" );
          ARMINSTRUCTIONS.insertMOV( tempReg, mORegister, OPER_LSL, e, exp );

          // Remember the shifted offset.
          mPrecomputedOffset = tempReg;

          // Choose the correct indexing mode.
          string mode2;
          if ( mModTime == ModTime::NONE )
            mode2 = OPER_ROFF;
          else

          if ( mModTime == ModTime::PRE )
            mode2 = OPER_RPRE;
          else

          if ( mModTime == ModTime::POST )
            mode2 = OPER_RPOST;

          // Insert a single LD/ST instruction.
          insertLdSt(
            createStore, mode2, reg, tempReg, 0, "", 0, mModOper, exp );
        } else {
          // Choose the correct indexing mode.
          string mode2;
          if ( mModTime == ModTime::NONE )
            mode2 = OPER_SROFF;
          else

          if ( mModTime == ModTime::PRE )
            mode2 = OPER_SRPRE;
          else

          if ( mModTime == ModTime::POST )
            mode2 = OPER_SRPOST;

          // Insert a single LD/ST instruction.
          insertLdSt(
            createStore, mode2, reg, mORegister, 0, OPER_LSL, e, mModOper,
            exp );
        }
      } else {
        // The ugly case. First, load the typeSize into a register (costly).
        LLIR_Register *tempReg = ARMINSTRUCTIONS.CreateRegister( "" );
        ARMINSTRUCTIONS.insertMOV_ORR( tempReg, typeSize );

        // Then, perform the multiplication at runtime.
        ARMINSTRUCTIONS.insertMUL( OPER_AL, tempReg, tempReg, mORegister, exp );

        // Choose the correct indexing mode.
        string mode2;
        if ( mModTime == ModTime::NONE )
          mode2 = OPER_ROFF;
        else

        if ( mModTime == ModTime::PRE )
          mode2 = OPER_RPRE;
        else

        if ( mModTime == ModTime::POST )
          mode2 = OPER_RPOST;

        // Remember the register that contains that immediate constant, in case
        // a second load/store will be performed.
        mPrecomputedOffset = tempReg;

        // Finally, perform the load.
        insertLdSt(
          createStore, mode2, reg, tempReg, 0, "", 0, mModOper, exp );
      }
    }

  } else {
    // Now for the case that this is the second, third, etc... access.

    // Special attention has to be paid to the modification time.
    if ( mModTime == ModTime::NONE ) {
      /*
        If the offset is immediate and a second load/store is to be inserted, we
        have to apply the offset again. Luckily, any "bonus" instructions we had
        to insert do not have to be inserted again.
      */

      if ( !mORegister && !mOffset )
        // Case 1: No offset is applied whatsoever.
        // This case is nice and simple, as no write-back has to ever be
        // performed.
        insertLdSt(
          createStore, OPER_IMMOFF, reg, nullptr, 0, "", 0, mModOper, exp );
      else

      if ( !mORegister ) {
        // Case 2: An integer offset is applied.
        // First, calculate the actual offset that has to be added.
        int byteOffset = mOffset * getOffsetFactor();

        // Acquire the biggest possible immediate offset.
        int maxIntOffset = isMode3() ? (1 << 8) : (1 << 12);

        if ( byteOffset < maxIntOffset )
          // Insert a single instruction.
          insertLdSt(
            createStore, OPER_IMMOFF, reg, nullptr, byteOffset, "", 0, mModOper,
            exp );
        else
          // The more annoying case. Luckily, the immediate constant is already
          // loaded.
          insertLdSt(
            createStore, OPER_ROFF, reg, mPrecomputedOffset, 0, "", 0, mModOper,
            exp );
      } else {
        // Case 3: A register offset is applied.
        // If the typeSize is a power of two, the arithmetics can be performed
        // in a single instruction.
        int typeSize = getOffsetFactor();

        if ( ( typeSize > 0 ) && ((typeSize & (typeSize - 1)) == 0) ) {
          // Determine the exponent (i.e., perform log2(typeSize)).
          int e = 0;
          for ( ; typeSize != 1; typeSize >>= 1, ++e ) ;

          // Only mode 2 can utilize scaled register offsets.
          if ( isMode3() )
            // In this case, the offset has already been shifted into a new
            // register.
            insertLdSt(
              createStore, OPER_ROFF, reg, mPrecomputedOffset, 0, "", 0,
              mModOper, exp );
          else
            // Insert a single LD/ST instruction.
            insertLdSt(
              createStore, OPER_SROFF, reg, mORegister, 0, OPER_LSL, e,
              mModOper, exp );
        } else
          // The ugly case. Luckily, the multiplied offset is already loaded.
          insertLdSt(
            createStore, OPER_ROFF, reg, mPrecomputedOffset, 0, "", 0, mModOper,
            exp );
      }
    } else

    if ( mModTime == ModTime::PRE )
      // If the offset is pre-indexed and this is the second, third, etc...
      // access, this means that our job is done. Any offset has already been
      // applied.
      insertLdSt(
        createStore, OPER_IMMOFF, reg, nullptr, 0, "", 0, mModOper, exp );
    else

    if ( mModTime == ModTime::POST ) {
      // If the offset is post-indexed and this is the second, third, etc...
      // access, then things get ugly. The original address does not exist
      // anymore - any offsets applied are still available, however. The
      // original address can be recalculated by "backwards"-applying any
      // offset.

      // Invert the ModOper: If the change applied was previously an addition,
      // we now have to subtract and vice versa.
      auto newOp = ( mModOper == ModOper::ADD ) ? ModOper::SUB : ModOper::ADD;

      if ( !mORegister && !mOffset )
        // Case 1: No offset is applied whatsoever.
        // This case is nice and simple, as no write-back has to ever be
        // performed.
        insertLdSt(
          createStore, OPER_IMMOFF, reg, nullptr, 0, "", 0, mModOper, exp );
      else

      if ( !mORegister ) {
        // Case 2: An integer offset is applied.
        // First, calculate the actual offset that has to be added.
        int byteOffset = mOffset * getOffsetFactor();

        // Acquire the biggest possible immediate offset.
        int maxIntOffset = isMode3() ? (1 << 8) : (1 << 12);

        if ( byteOffset < maxIntOffset )
          // Insert a single instruction with inverted modOper.
          insertLdSt(
            createStore, OPER_IMMOFF, reg, nullptr, byteOffset, "", 0, newOp,
            exp );
        else
          // The more annoying case. Luckily, the constant is already loaded.
          // Afterwards, we insert the LD/ST with inverted modOper.
          insertLdSt(
            createStore, OPER_ROFF, reg, mPrecomputedOffset, 0, "", 0, newOp,
            exp );
      } else {
        // Case 3: A register offset is applied.
        // If the typeSize is a power of two, the arithmetics can be performed
        // in a single instruction.
        int typeSize = getOffsetFactor();

        if ( ( typeSize > 0 ) && ((typeSize & (typeSize - 1)) == 0) ) {
          // Determine the exponent (i.e., perform log2(typeSize)).
          int e = 0;
          for ( ; typeSize != 1; typeSize >>= 1, ++e ) ;

          // Only mode 2 can utilize scaled register offsets.
          if ( isMode3() )
            // In this case, the offset has already been shifted into a new
            // register.
            insertLdSt(
              createStore, OPER_ROFF, reg, mPrecomputedOffset, 0, "", 0, newOp,
              exp );
          else
            // Insert a single LD/ST instruction with inverted modOper.
            insertLdSt(
              createStore, OPER_SROFF, reg, mORegister, 0, OPER_LSL, e, newOp,
              exp );
        } else
          // The ugly case. Luckily, the multiplied offset is already loaded.
          // Insert the LD/ST with inverted modOper.
          insertLdSt(
            createStore, OPER_ROFF, reg, mPrecomputedOffset, 0, "", 0, newOp,
            exp );
      }
    }
  }

  // Indicate that a single access has been performed.
  mModApplied = true;
};


/*
  createLoadStoreCost computes the costs for either a load or a store of the
  given register from/to the effective address encoded by this object.
*/
COST ARM_AddressModification::createLoadStoreCost( bool createStore )
{
  // Never calculate costs with a real run instance.
  ufAssertT(
    mDryRun, "Only call 'createLoadStoreCost' for a dry-run instance." );

  COST c = 0;

  if ( !mModApplied ) {
    // First up is the case that this is the "first" access.
    // Differentiate between three major cases.

    if ( !mRegOffsetValid && !mOffset )
      // Case 1: No offset is applied whatsoever.
      // This case is nice and simple, as no write-back has to ever be
      // performed.
      c += insertLdStCost( createStore, OPER_IMMOFF );
    else

    if ( !mRegOffsetValid ) {
      // Case 2: An integer offset is applied.
      // First, calculate the actual offset that has to be added.
      int byteOffset = mOffset * getOffsetFactor();

      // Acquire the biggest possible immediate offset.
      int maxIntOffset = isMode3() ? (1 << 8) : (1 << 12);

      if ( byteOffset < maxIntOffset ) {
        // Choose the correct indexing mode.
        string mode2;
        if ( mModTime == ModTime::NONE )
          mode2 = OPER_IMMOFF;
        else

        if ( mModTime == ModTime::PRE )
          mode2 = OPER_IMMPRE;
        else

        if ( mModTime == ModTime::POST )
          mode2 = OPER_IMMPOST;

        // Insert a single instruction.
        c += insertLdStCost( createStore, mode2 );
      } else {
        // The more annoying case in which we have to load the immediate
        // constant first.
        c += insertMOV_ORR_Cost( byteOffset );

        // Choose the correct indexing mode.
        string mode2;
        if ( mModTime == ModTime::NONE )
          mode2 = OPER_ROFF;
        else

        if ( mModTime == ModTime::PRE )
          mode2 = OPER_RPRE;
        else

        if ( mModTime == ModTime::POST )
          mode2 = OPER_RPOST;

        // Afterwards, we insert the LD/ST.
        c += insertLdStCost( createStore, mode2 );
      }
    } else {
      // Case 3: A register offset is applied.
      // If the typeSize is a power of two, the arithmetics can be performed in
      // a single instruction.
      int typeSize = getOffsetFactor();

      if ( ( typeSize > 0 ) && ( (typeSize & (typeSize - 1)) == 0 ) ) {
        // Scaled register offsets are only possible in mode 2. For mode 3, we
        // insert an additional MOV instruction performing the shift.

        if ( isMode3() ) {
          // Perform the shift.
          c += CT( INS_MOV_32 );

          // Choose the correct indexing mode.
          string mode2;
          if ( mModTime == ModTime::NONE )
            mode2 = OPER_ROFF;
          else

          if ( mModTime == ModTime::PRE )
            mode2 = OPER_RPRE;
          else

          if ( mModTime == ModTime::POST )
            mode2 = OPER_RPOST;

          // Insert a single LD/ST instruction.
          c += insertLdStCost( createStore, mode2 );
        } else {
          // Choose the correct indexing mode.
          string mode2;
          if ( mModTime == ModTime::NONE )
            mode2 = OPER_SROFF;
          else

          if ( mModTime == ModTime::PRE )
            mode2 = OPER_SRPRE;
          else

          if ( mModTime == ModTime::POST )
            mode2 = OPER_SRPOST;

          // Insert a single LD/ST instruction.
          c += insertLdStCost( createStore, mode2 );
        }
      } else {
        // The ugly case. First, load the typeSize into a register (costly).
        c += insertMOV_ORR_Cost( typeSize );

        // Then, perform the multiplication at runtime.
        c += CT( INS_MUL_32 );

        // Choose the correct indexing mode.
        string mode2;
        if ( mModTime == ModTime::NONE )
          mode2 = OPER_ROFF;
        else

        if ( mModTime == ModTime::PRE )
          mode2 = OPER_RPRE;
        else

        if ( mModTime == ModTime::POST )
          mode2 = OPER_RPOST;

        // Finally, perform the load.
        c += insertLdStCost( createStore, mode2 );
      }
    }
  } else {
    // Now for the case that this is the second, third, etc... access.
    // Special attention has to be paid to towards the modification time.
    if ( mModTime == ModTime::NONE ) {
      // If the offset is immediate and a second load/store is to be inserted,
      // have to apply the offset again. Luckily, any "bonus" instructions we
      // had to insert do not have to be inserted again.

      if ( !mRegOffsetValid && !mOffset )
        // Case 1: No offset is applied whatsoever.
        // This case is nice and simple, as no write-back has to ever be
        // performed.
        c += insertLdStCost( createStore, OPER_IMMOFF );
      else

      if ( !mRegOffsetValid ) {
        // Case 2: An integer offset is applied.
        // First, calculate the actual offset that has to be added.
        int byteOffset = mOffset * getOffsetFactor();

        // Acquire the biggest possible immediate offset.
        int maxIntOffset =  isMode3() ? (1 << 8) : (1 << 12);

        if ( byteOffset < maxIntOffset )
          // Insert a single instruction.
          c += insertLdStCost( createStore, OPER_IMMOFF );
        else
          // The more annoying case. Luckily, the immediate constant is already
          // loaded.
          c += insertLdStCost( createStore, OPER_ROFF );
      } else {
        // Case 3: A register offset is applied.
        // If the typeSize is a power of two, the arithmetics can be performed
        // in a single instruction.
        int typeSize = getOffsetFactor();

        if ( ( typeSize > 0 ) && ( (typeSize & (typeSize - 1)) == 0 ) ) {
          if ( isMode3() )
            // The shift has already been performed. Insert a single LD/ST
            // instruction.
            c += insertLdStCost( createStore, OPER_ROFF );
          else
            // Insert a single LD/ST instruction.
            c += insertLdStCost( createStore, OPER_SROFF );
        } else
          // The ugly case. Luckily, the multiplied offset is already loaded.
          c += insertLdStCost( createStore, OPER_ROFF );
      }
    } else

    if ( mModTime == ModTime::PRE )
      /*
        If the offset is pre-indexed and this is the second, third, etc...
        access, this means that our job is done. Any offset has already been
        applied.
      */
      c += insertLdStCost( createStore, OPER_IMMOFF );
    else

    if ( mModTime == ModTime::POST ) {
      /*
        If the offset is post-indexed and this is the second, third, etc...
        access, then things get ugly. The original address does not exist
        anymore - any offsets applied are still available, however. The original
        address can be recalculated by applying any offset "backwards".
      */

      if ( !mRegOffsetValid && !mOffset )
        // Case 1: No offset is applied whatsoever.
        // This case is nice and simple, as no write-back has to ever be
        // performed.
        c += insertLdStCost( createStore, OPER_IMMOFF );
      else

      if ( !mRegOffsetValid ) {
        // Case 2: An integer offset is applied.
        // First, calculate the actual offset that has to be added.
        int byteOffset = mOffset * getOffsetFactor();

        // Acquire the biggest possible immediate offset.
        int maxIntOffset = isMode3() ? (1 << 8) : (1 << 12);

        if ( byteOffset < maxIntOffset )
          // Insert a single instruction with inverted modOper.
          c += insertLdStCost( createStore, OPER_IMMOFF );
        else
          // The more annoying case. Luckily, the constant is already loaded.
          // Afterwards, we insert the LD/ST with inverted modOper.
          c += insertLdStCost( createStore, OPER_ROFF );
      } else {
        // Case 3: A register offset is applied.
        // If the typeSize is a power of two, the arithmetics can be performed
        // in a single instruction.
        int typeSize = getOffsetFactor();

        if ( ( typeSize > 0 ) && ( (typeSize & (typeSize - 1)) == 0 ) ) {
          if ( isMode3() )
            // The shift has already been performed. Insert a single LD/ST
            // instruction.
            c += insertLdStCost( createStore, OPER_ROFF );
          else
            // Insert a single LD/ST instruction.
            c += insertLdStCost( createStore, OPER_SROFF );
        } else
          // The ugly case. Luckily the multiplied offset is already loaded.
          // Insert the ld/st with inverted modOper.
          c += insertLdStCost( createStore, OPER_ROFF );
      }
    }
  }

  // Indicate that a single access has been performed.
  mModApplied = true;

  return( c );
};


/*
  insertLdSt inserts a single LD/ST instruction.
*/
void ARM_AddressModification::insertLdSt( bool createStore, const string &mode2,
                                          LLIR_Register *dest,
                                          LLIR_Register *regOffset,
                                          int intOffset,
                                          const string &shift_mode, int shift,
                                          ModOper opType, IR_Exp *exp )
{
  // Never do this in a dry run!
  ufAssertT( !mDryRun, "Do not call 'insertLdSt' for a dry-run instance." );

  // Use the instance's baseType.
  IR_Type *baseType = mBaseType;

  // Determine the maximally allowed integer offset.
  int maxIntOffset = isMode3() ? (1 << 8) : (1 << 12);

  // Determine the base registers.
  auto *baseReg = getBaseRegister();

  if ( !regOffset ) {
    // Case 1: An immediate offset is used. This includes the case for
    // intOffset = 0 as well. Check that the offset size is correct.
    ufAssertT(
      intOffset < maxIntOffset, "Immediate integer offset too large." );

    // If the operation mode is subtraction, negate the integer offset here.
    if ( opType == ModOper::SUB )
      intOffset = -intOffset;

    // Differentiate between LD and ST.
    if ( !createStore ) {
      // Differentiate between baseType.
      switch ( baseType->getType() ) {
        case IR_Type::UNSIGNED_CHAR:
        case IR_Type::BOOL: {
          ARMINSTRUCTIONS.insertLDRB( mode2, dest, baseReg, intOffset, exp );
          break;
        }

        case IR_Type::CHAR: {
          ARMINSTRUCTIONS.insertLDRSB( mode2, dest, baseReg, intOffset, exp );
          break;
        }

        case IR_Type::UNSIGNED_SHORT: {
          ARMINSTRUCTIONS.insertLDRH( mode2, dest, baseReg, intOffset, exp );
          break;
        }

        case IR_Type::SHORT: {
          ARMINSTRUCTIONS.insertLDRSH( mode2, dest, baseReg, intOffset, exp );
          break;
        }

        case IR_Type::UNSIGNED_INT:
        case IR_Type::INT:
        case IR_Type::UNSIGNED_LONG:
        case IR_Type::LONG:
        case IR_Type::FLOAT:
        case IR_Type::POINTER:
        case IR_Type::ARRAY: {
          ARMINSTRUCTIONS.insertLDR( mode2, dest, baseReg, intOffset, exp );
          break;
        }

        case IR_Type::UNSIGNED_LONG_LONG:
        case IR_Type::LONG_LONG: {
          LLIR_Register* lsb;
          LLIR_Register* msb;
          // Insert two consecutive loads.
          ufAssert( dest->GetNumberOfChildren() == 2 );
          lsb = dest->GetFirstChild();
          msb = dest->GetNextChild( dest->GetFirstChild() );
          ARMINSTRUCTIONS.insertLDR( mode2, lsb, baseReg, intOffset, exp );
          ARMINSTRUCTIONS.insertLDR( mode2, msb, baseReg, intOffset + 4, exp );
          break;
        }

        case IR_Type::DOUBLE:
        case IR_Type::LONG_DOUBLE: {
          LLIR_Register* lsb;
          LLIR_Register* msb;
          // Insert two consecutive loads.
          ufAssert( dest->GetNumberOfChildren() == 2 );
          msb = dest->GetFirstChild();
          lsb = dest->GetNextChild( dest->GetFirstChild() );

          if ( mode2 == OPER_IMMOFF ) {
            ARMINSTRUCTIONS.insertLDR( mode2, lsb, baseReg, intOffset+4, exp );
            ARMINSTRUCTIONS.insertLDR( mode2, msb, baseReg, intOffset, exp );

          } else if ( mode2 == OPER_IMMPRE ) {
            ARMINSTRUCTIONS.insertLDR( mode2, msb, baseReg, intOffset, exp );
            ARMINSTRUCTIONS.insertLDR(
              OPER_IMMOFF, lsb, baseReg, intOffset + 4, exp );

          } else if ( mode2 == OPER_IMMPOST ) {
            ARMINSTRUCTIONS.insertLDR( OPER_IMMOFF, lsb, baseReg, 4, exp );
            ARMINSTRUCTIONS.insertLDR( mode2, msb, baseReg, intOffset, exp );
          } else {
            ufAssertT( false, "Not yet supported." );
          }

          break;
        }

        default: {
          ufAssertT( false, "Type not supported for LD/ST." );
          break;
        }
      }
    } else {
      // Differentiate between baseType.
      switch ( baseType->getType() ) {
        case IR_Type::UNSIGNED_CHAR:
        case IR_Type::BOOL:
        case IR_Type::CHAR: {
          ARMINSTRUCTIONS.insertSTRB(
            OPER_AL, mode2, dest, baseReg, intOffset, exp );
          break;
        }

        case IR_Type::SHORT:
        case IR_Type::UNSIGNED_SHORT: {
          ARMINSTRUCTIONS.insertSTRH(
            OPER_AL, mode2, dest, baseReg, intOffset, exp );
          break;
        }

        case IR_Type::UNSIGNED_INT:
        case IR_Type::INT:
        case IR_Type::UNSIGNED_LONG:
        case IR_Type::LONG:
        case IR_Type::FLOAT:
        case IR_Type::POINTER:
        case IR_Type::ARRAY: {
          ARMINSTRUCTIONS.insertSTR(
            OPER_AL, mode2, dest, baseReg, intOffset, exp );
          break;
        }

        case IR_Type::UNSIGNED_LONG_LONG:
        case IR_Type::LONG_LONG: {
          LLIR_Register* lsb;
          LLIR_Register* msb;
          // Insert two consecutive loads.
          ufAssert( dest->GetNumberOfChildren() == 2 );
          lsb = dest->GetFirstChild();
          msb = dest->GetNextChild( dest->GetFirstChild() );
          ARMINSTRUCTIONS.insertSTR(
            OPER_AL, mode2, lsb, baseReg, intOffset, exp );
          ARMINSTRUCTIONS.insertSTR(
            OPER_AL, mode2, msb, baseReg, intOffset + 4, exp );
          break;
        }

        case IR_Type::DOUBLE:
        case IR_Type::LONG_DOUBLE: {
          LLIR_Register* lsb;
          LLIR_Register* msb;
          // Insert two consecutive stores.
          ufAssert( dest->GetNumberOfChildren() == 2 );
          msb = dest->GetFirstChild();
          lsb = dest->GetNextChild( dest->GetFirstChild() );

          if ( mode2 == OPER_IMMOFF ) {
            ARMINSTRUCTIONS.insertSTR(
              OPER_AL, OPER_IMMOFF, lsb, baseReg, intOffset + 4, exp );
            ARMINSTRUCTIONS.insertSTR(
              OPER_AL, OPER_IMMOFF, msb, baseReg, intOffset, exp );

          } else if ( mode2 == OPER_IMMPRE ) {
            ARMINSTRUCTIONS.insertSTR(
              OPER_AL, mode2, msb, baseReg, intOffset, exp );
            ARMINSTRUCTIONS.insertSTR(
              OPER_AL, OPER_IMMOFF, lsb, baseReg, intOffset + 4, exp );

          } else if ( mode2 == OPER_IMMPOST ) {
            ARMINSTRUCTIONS.insertSTR(
              OPER_AL, OPER_IMMOFF, lsb, baseReg, 4, exp );
            ARMINSTRUCTIONS.insertSTR(
              OPER_AL, mode2, msb, baseReg, intOffset, exp );
          } else {
            ufAssertT( false, "Not yet supported." );
          }
          break;
        }

        default: {
          ufAssertT( false, "Type not supported for LD/ST." );
          break;
        }
      }
    }
  } else

  if ( shift_mode == "" ) {
    // A standard (non-scaled) register offset is used.
    // Differentiate between LD and ST.
    if ( !createStore )
      // Differentiate between baseType.
      switch ( baseType->getType() ) {
        case IR_Type::UNSIGNED_CHAR:
        case IR_Type::BOOL: {
          if ( opType == ModOper::ADD )
            ARMINSTRUCTIONS.insertLDRB( mode2, dest, baseReg, regOffset, exp );
          else
            ARMINSTRUCTIONS.insertLDRB(
              mode2, dest, baseReg, OPER_NGREG, regOffset, exp );
          break;
        }

        case IR_Type::CHAR: {
          if ( opType == ModOper::ADD )
            ARMINSTRUCTIONS.insertLDRSB( mode2, dest, baseReg, regOffset, exp );
          else
            ARMINSTRUCTIONS.insertLDRSB(
              mode2, dest, baseReg, OPER_NGREG, regOffset, exp );
          break;
        }

        case IR_Type::UNSIGNED_SHORT: {
          if ( opType == ModOper::ADD )
            ARMINSTRUCTIONS.insertLDRH( mode2, dest, baseReg, regOffset, exp );
          else
            ARMINSTRUCTIONS.insertLDRH(
              mode2, dest, baseReg, OPER_NGREG, regOffset, exp );
          break;
        }

        case IR_Type::SHORT: {
          if ( opType == ModOper::ADD )
            ARMINSTRUCTIONS.insertLDRSH( mode2, dest, baseReg, regOffset, exp );
          else
            ARMINSTRUCTIONS.insertLDRSH(
              mode2, dest, baseReg, OPER_NGREG, regOffset, exp );
          break;
        }

        case IR_Type::UNSIGNED_INT:
        case IR_Type::INT:
        case IR_Type::UNSIGNED_LONG:
        case IR_Type::LONG:
        case IR_Type::FLOAT:
        case IR_Type::POINTER:
        case IR_Type::ARRAY: {
          if ( opType == ModOper::ADD )
            ARMINSTRUCTIONS.insertLDR( mode2, dest, baseReg, regOffset, exp );
          else
            ARMINSTRUCTIONS.insertLDR(
              mode2, dest, baseReg, OPER_NGREG, regOffset, exp );
          break;
        }

        default: {
          ufAssertT( false, "Type not supported for LD/ST." );
          break;
        }
      }
    else
      // Differentiate between baseType.
      switch ( baseType->getType() ) {
        case IR_Type::UNSIGNED_CHAR:
        case IR_Type::BOOL:
        case IR_Type::CHAR: {
          if ( opType == ModOper::ADD )
            ARMINSTRUCTIONS.insertSTRB(
              OPER_AL, mode2, dest, baseReg, regOffset, exp );
          else
            ARMINSTRUCTIONS.insertSTRB(
              OPER_AL, mode2, dest, baseReg, OPER_NGREG, regOffset, exp );
          break;
        }

        case IR_Type::SHORT:
        case IR_Type::UNSIGNED_SHORT: {
          if ( opType == ModOper::ADD )
            ARMINSTRUCTIONS.insertSTRH(
              OPER_AL, mode2, dest, baseReg, regOffset, exp );
          else
            ARMINSTRUCTIONS.insertSTRH(
              OPER_AL, mode2, dest, baseReg, OPER_NGREG, regOffset, exp );
          break;
        }

        case IR_Type::UNSIGNED_INT:
        case IR_Type::INT:
        case IR_Type::UNSIGNED_LONG:
        case IR_Type::LONG:
        case IR_Type::FLOAT:
        case IR_Type::POINTER:
        case IR_Type::ARRAY: {
          if ( opType == ModOper::ADD )
            ARMINSTRUCTIONS.insertSTR(
              OPER_AL, mode2, dest, baseReg, regOffset, exp );
          else
            ARMINSTRUCTIONS.insertSTR(
              OPER_AL, mode2, dest, baseReg, OPER_NGREG, regOffset, exp );
          break;
        }

        default: {
          ufAssertT( false, "Type not supported for LD/ST." );
          break;
        }
      }
  } else {
    // A scaled register offset is used. Only mode 2 from here on.
    ufAssertT(
      !isMode3(),
      "Cannot insert a LD/ST with scaled register offset for the given base "
      "type." );

    // Differentiate between LD and ST.
    if ( !createStore )
      // Differentiate between baseType.
      switch ( baseType->getType() ) {
        case IR_Type::UNSIGNED_CHAR:
        case IR_Type::BOOL: {
          if ( opType == ModOper::ADD )
            ARMINSTRUCTIONS.insertLDRB(
              mode2, dest, baseReg, regOffset, shift_mode, shift, exp );
          else
            ARMINSTRUCTIONS.insertLDRB(
              mode2, dest, baseReg, OPER_NGREG, regOffset, shift_mode, shift,
              exp );
          break;
        }

        case IR_Type::CHAR: {
          ufAssertT(
            false, "Cannot insert LDRSB with a scaled register offset." );
          break;
        }

        case IR_Type::UNSIGNED_SHORT: {
          ufAssertT(
            false, "Cannot insert LDRH with a scaled register offset." );
          break;
        }

        case IR_Type::SHORT: {
          ufAssertT(
            false, "Cannot insert LDRSH with a scaled register offset." );
          break;
        }

        case IR_Type::UNSIGNED_INT:
        case IR_Type::INT:
        case IR_Type::UNSIGNED_LONG:
        case IR_Type::LONG:
        case IR_Type::FLOAT:
        case IR_Type::POINTER:
        case IR_Type::ARRAY: {
          if ( opType == ModOper::ADD )
            ARMINSTRUCTIONS.insertLDR(
              mode2, dest, baseReg, regOffset, shift_mode, shift, exp );
          else
            ARMINSTRUCTIONS.insertLDR(
              mode2, dest, baseReg, OPER_NGREG, regOffset, shift_mode, shift,
              exp );
          break;
        }

        default: {
          ufAssertT( false, "Type not supported for LD/ST." );
          break;
        }
      }
    else
      // Differentiate between baseType.
      switch ( baseType->getType() ) {
        case IR_Type::UNSIGNED_CHAR:
        case IR_Type::BOOL:
        case IR_Type::CHAR: {
          if ( opType == ModOper::ADD )
            ARMINSTRUCTIONS.insertSTRB(
              OPER_AL, mode2, dest, baseReg, regOffset, shift_mode, shift,
              exp );
          else
            ARMINSTRUCTIONS.insertSTRB(
              OPER_AL, mode2, dest, baseReg, OPER_NGREG, regOffset, shift_mode,
              shift, exp );
          break;
        }

        case IR_Type::SHORT:
        case IR_Type::UNSIGNED_SHORT: {
          ufAssertT(
            false, "Cannot insert STRH with a scaled register offset." );
          break;
        }

        case IR_Type::UNSIGNED_INT:
        case IR_Type::INT:
        case IR_Type::UNSIGNED_LONG:
        case IR_Type::LONG:
        case IR_Type::FLOAT:
        case IR_Type::POINTER:
        case IR_Type::ARRAY: {
          if ( opType == ModOper::ADD )
            ARMINSTRUCTIONS.insertSTR(
              OPER_AL, mode2, dest, baseReg, regOffset, shift_mode, shift,
              exp );
          else
            ARMINSTRUCTIONS.insertSTR(
              OPER_AL, mode2, dest, baseReg, OPER_NGREG, regOffset, shift_mode,
              shift, exp );
          break;
        }

        default: {
          ufAssertT( false, "Type not supported for LD/ST." );
          break;
        }
      }
  }

  // If the base address is stored in memory (i.e., is an lvalue) and was
  // modified with this LD/ST instruction, we have to update its value in
  // memory.
  if ( mBaseLValueValid &&
       ( ( mode2 == OPER_IMMPRE ) || ( mode2 == OPER_IMMPOST ) ||
         ( mode2 == OPER_RPRE ) || ( mode2 == OPER_RPOST ) ||
         ( mode2 == OPER_SRPRE ) || ( mode2 == OPER_SRPOST ) ) )
    dynamic_cast<ARM_LValue *>( mBaseLValue )->storeBack( baseReg, exp );
};


/*
  insertLdStCost computes the costs for a single LD/ST instruction.
*/
COST ARM_AddressModification::insertLdStCost( bool createStore,
                                              const string &mode2 ) const
{
  // Never calculate costs with a real run instance.
  ufAssertT( mDryRun, "Only call 'insertLdStCost' for a dry-run instance." );

  COST c = 0;

  // Use the instance's baseType.
  IR_Type *baseType = mBaseType;

  // Differentiate between LD and ST.
  if ( !createStore )
    // Differentiate between baseType.
    switch ( baseType->getType() ) {
      case IR_Type::UNSIGNED_CHAR:
      case IR_Type::BOOL: {
        c += CT( INS_LDRB_32 );
        break;
      }

      case IR_Type::CHAR: {
        c += CT( INS_LDRSB_32 );
        break;
      }

      case IR_Type::UNSIGNED_SHORT: {
        c += CT( INS_LDRH_32 );
        break;
      }

      case IR_Type::SHORT: {
        c += CT( INS_LDRSH_32 );
        break;
      }

      case IR_Type::UNSIGNED_INT:
      case IR_Type::INT:
      case IR_Type::UNSIGNED_LONG:
      case IR_Type::LONG:
      case IR_Type::FLOAT:
      case IR_Type::POINTER:
      case IR_Type::FUNCTION:
      case IR_Type::ARRAY: {
        c += CT( INS_LDR_32 );
        break;
      }

      case IR_Type::UNSIGNED_LONG_LONG:
      case IR_Type::LONG_LONG:
      case IR_Type::DOUBLE:
      case IR_Type::LONG_DOUBLE: {
        c += 2 * CT( INS_LDR_32 );
        break;
      }

      default: {
        ufAssertT( false, "Type not supported for LD/ST." );
        break;
      }
    }
  else
    // Differentiate between baseType.
    switch ( baseType->getType() ) {
      case IR_Type::UNSIGNED_CHAR:
      case IR_Type::BOOL:
      case IR_Type::CHAR: {
        c += CT( INS_STRB_32 );
        break;
      }

      case IR_Type::SHORT:
      case IR_Type::UNSIGNED_SHORT: {
        c += CT( INS_STRH_32 );
        break;
      }

      case IR_Type::UNSIGNED_INT:
      case IR_Type::INT:
      case IR_Type::UNSIGNED_LONG:
      case IR_Type::LONG:
      case IR_Type::FLOAT:
      case IR_Type::POINTER:
      case IR_Type::ARRAY: {
        c += CT( INS_STR_32 );
        break;
      }

      case IR_Type::UNSIGNED_LONG_LONG:
      case IR_Type::LONG_LONG:
      case IR_Type::DOUBLE:
      case IR_Type::LONG_DOUBLE: {
        c += 2 * CT( INS_STR_32 );
        break;
      }

      default: {
        ufAssertT( false, "Type not supported for LD/ST." );
        break;
      }
    }

  // If an lvalue supplies the base address and mode2 would modify that
  // address register, we'd have to store the new register back, incurring
  // additional cost.
  if ( mBaseLValueValid &&
       ( ( mode2 == OPER_IMMPRE ) || ( mode2 == OPER_IMMPOST ) ||
         ( mode2 == OPER_RPRE ) || ( mode2 == OPER_RPOST ) ||
         ( mode2 == OPER_SRPRE ) || ( mode2 == OPER_SRPOST ) ) )
    c += dynamic_cast<ARM_LValue *>( mBaseLValue )->storeBackCost();

  return( c );
};


/*
  insertMOV_ORR_Cost computes the costs for using insertMOV_ORR as helper.
*/
COST ARM_AddressModification::insertMOV_ORR_Cost( int value )
{
  return( ARMINSTRUCTIONS.insertMOV_ORR_Cost( value ) );
};


/*
  isMode3 determines whether addressing modes 2 or 3 are applicable.
*/
bool ARM_AddressModification::isMode3( void ) const
{
  // Only LD/ST instructions working with >words< or >unsigned bytes< can use
  // addressing mode 2. LD/ST instructions working with >halfwords< (signed or
  // unsigned) and >signed bytes< use addressing mode 3, which is less versatile
  // and does for instance not feature scaled register offsets.
  switch ( mBaseType->getType() ) {
    case IR_Type::CHAR:
    case IR_Type::SHORT:
    case IR_Type::UNSIGNED_SHORT:
    case IR_Type::UNSIGNED_LONG_LONG:
    case IR_Type::LONG_LONG:
    case IR_Type::DOUBLE:
    case IR_Type::LONG_DOUBLE: {
      return( true );
    }

    case IR_Type::UNSIGNED_CHAR:
    case IR_Type::BOOL:
    case IR_Type::UNSIGNED_INT:
    case IR_Type::INT:
    case IR_Type::UNSIGNED_LONG:
    case IR_Type::LONG:
    case IR_Type::FLOAT:
    case IR_Type::POINTER:
    case IR_Type::ARRAY: {
      return( false );
    }

    case IR_Type::VOID: {
      ufAssertT( false, "A void pointer cannot be dereferenced." );
      break;
    }

    default: {
      ufAssertT(
        false,
        "Base type currently unsupported within an ARM_AddressModification." );
      break;
    }
  }
};


/*
  clone creates a copy of an address modification object.

  clone just calls the corresponding copy constructor.
*/
AddressModification *ARM_AddressModification::clone( void ) const
{
  DSTART(
    "virtual AddressModification* ARM_AddressModification::clone() const" );

  return( new ARM_AddressModification( *this ) );
};


/*
  getOffsetFactor computes the factor by which the offset needs to be scaled
  according to an ARM address modification's base type.
*/
int ARM_AddressModification::getOffsetFactor( void ) const
{
  DSTART( "virtual int ARM_AddressModification::getOffsetFactor() const" );

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
    ufAssertT( false, "No baseType specified in the ARM_AddressModification." );
};
