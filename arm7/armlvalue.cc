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

// Include boost headers
#include <boost/current_function.hpp>

// Include WIR headers
#include <wir/wir.h>

// Include libuseful headers
#include <libuseful/debugmacros.h>
#include <libuseful/io.h>

// Include local headers
#include <arm7/armaddressmodification.h>
#include <arm7/armlvalue.h>
#include <arm7/instructionfactory.h>
#include <arm7/registrar.h>
#include <arm7/composed_struct_bitfield_incl.h>


//
// Code section
//


using namespace std;
using namespace WIR;


//
// Public class methods
//

/*
  Constructor initializing an empty lvalue.
*/
ARM_LValue::ARM_LValue( void ) :
  LValue {},
  mDryRun { false }
{
  DSTART( "ARM_LValue::ARM_LValue()" );

  mAddress.reset( new ARM_AddressModification {} );
};


/*
  Constructor initializing a base/offset-based lvalue with base address pre/post
  incr/decr.
*/
ARM_LValue::ARM_LValue( LLIR_Register *reg, const ARM_AddressModification &am,
                        bool dr ) :
  LValue { reg, nullptr, am },
  mDryRun { dr }
{
  DSTART(
    "ARM_LValue::ARM_LValue(LLIR_Register*, const ARM_AddressModification&, bool)" );
};


/*
  Constructor initializing a bitfield-based lvalue.
*/
ARM_LValue::ARM_LValue( LLIR_Register *reg, const ARM_AddressModification &am,
                        unsigned char bo, unsigned char bl, bool dr ) :
  LValue { reg, nullptr, am, bo, bl },
  mDryRun { dr }
{
  DSTART(
    "ARM_LValue::ARM_LValue(LLIR_Register*, const ARM_AddressModification&, unsigned char, unsigned char, bool)" );
};


/*
  Copy constructor.
*/
ARM_LValue::ARM_LValue( const ARM_LValue &am ) :
  LValue { am },
  mDryRun { am.mDryRun }
{
  DSTART( "ARM_LValue::ARM_LValue(const ARM_LValue&)" );
};


/*
  Destructor.
*/
ARM_LValue::~ARM_LValue( void )
{
  DSTART( "virtual ARM_LValue::~ARM_LValue()" );
};


/*
  Assignment operator.
*/
ARM_LValue & ARM_LValue::operator = ( const ARM_LValue &am )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  LValue::operator = ( am );

  mDryRun = am.mDryRun;

  return( *this );
};


/*
  getAddress returns the register that holds the base of the memory access plus
  any modification that should be applied to it.
*/
ARM_AddressModification &ARM_LValue::getAddress( void ) const
{
  DSTART( "ARM_AddressModification& ARM_LValue::getAddress() const" );

  return( dynamic_cast<ARM_AddressModification &>( *mAddress ) );
};


/*
  storeBack stores a register value back into memory.
*/
void ARM_LValue::storeBack( LLIR_Register *source, IR_Exp *exp )
{
  DSTART( "void ARM_LValue::storeBack(LLIR_Register*, IR_Exp*)" );

  // Never call this on a dry run instance.
  ufAssertT( !mDryRun, "Do not call 'storeBack' on a dry run instance." );

  if ( mIsBitfieldAccess ) {
    auto *old_container = ARMINSTRUCTIONS.CreateRegister( "" );
    getAddress().createLoad( old_container, exp );

    LLIR_Register *value =
      insertValueIntoBitfield(
        old_container, { mBitfieldOffset, mBitfieldLength }, source, exp );
    getAddress().createStore( value, exp );
  } else
    // Let the AddressModification handle this.
    getAddress().createStore( source, exp );
};


/*
  storeBackCost returns the costs for storing back into memory.
*/
COST ARM_LValue::storeBackCost( void )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  // Let the ARM_AddressModification answer this.
  return( getAddress().createStoreCost() );
};


/*
  storeBackWorstCost computes the worst-case costs for storing back into memory.

  Note that this function assumes worst-case calculations are used along the
  entire tree *and* that the tree pattern matcher assumes that a value is always
  loaded from memory first and optionally stored back later (i.e.,
  loadResult = true).
*/
COST ARM_LValue::storeBackWorstCost( void )
{
  DSTART( "static COST ARM_LValue::storeBackWorstCost()" );

  // Let the ARM_AddressModification answer this.
  return( ARM_AddressModification::createStoreWorstCost() );
};


/*
  calculateAddress applies the pointer arithmetics denoted by the embedded
  ARM_AddressModification object and returns the calculated register.

  calculateAddress requires that createLoad/createStore wasn't applied to the
  ARM_AddressModification object yet.
*/
LLIR_Register *ARM_LValue::calculateAddress( IR_Exp *exp )
{
  DSTART( "LLIR_Register* ARM_LValue::calculateAddress(IR_Exp*)" );

  // Never call this on a dry run instance.
  ufAssertT(
    !mDryRun, "Do not call 'calculateAddress' on a dry run instance." );

  // Pass this through to the ARM_AddressModification.
  return( getAddress().applyModification( exp ) );
};


/*
  calculateAddressCost returns the costs for the pointer arithmetics encoded by
  this object.
*/
COST ARM_LValue::calculateAddressCost( void ) const
{
  DSTART( "COST ARM_LValue::calculateAddressCost() const" );

  // Let the ARM_AddressModification answer this.
  return( getAddress().applyModificationCost() );
};


/*
  calculateAddressWorstCost computes the worst-case costs for the pointer
  arithmetics encoded by this object.

  Note that this function assumes worst-case calculations are used along the
  entire tree *and* that the tree pattern matcher assumes that a value is always
  loaded from memory first and optionally stored back later (i.e.,
  loadResult = true).
*/
COST ARM_LValue::calculateAddressWorstCost( void )
{
  DSTART( "static COST ARM_LValue::calculateAddressWorstCost()" );

  // Let the ARM_AddressModification class answer this.
  return( ARM_AddressModification::applyModificationWorstCost() );
};


/*
  getBitsInNextWord is a convenience method that returns
  "getBitsInNextWord( offset, length )" for the current object's values.

  If the current object does not denote a bitfield access, then this method
  returns -1.
*/
int ARM_LValue::getBitsInNextWord( void ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  return(
    mIsBitfieldAccess ?
      getBitsInNextWord( mBitfieldOffset, mBitfieldLength ) : -1 );
};


/*
  For the given bitfield offset and length, getBitsInNextWord returns the number
  of bits which span into the next word.
*/
int ARM_LValue::getBitsInNextWord( int bo, int bl )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  ufAssertT( bo < 32, "Invalid internal state." );

  const int bits = bo + bl - 32;
  return( ( bits >= 0 ) ? bits : 0 );
};


/*
  signExtendBitfieldWorstCostcomputes the worst-case costs for the
  sign-extension of a bitfield value.
*/
COST ARM_LValue::signExtendBitfieldWorstCost( void )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  return( 2 * CT( INS_MOV_32 ) );
};


/*
  For a bitfield access, signExtendBitfield sign-extends the given value to 32
  bits.
*/
LLIR_Register *ARM_LValue::signExtendBitfield( LLIR_Register *value ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  ufAssert( mIsBitfieldAccess );

  if ( mAddress->getBaseType()->isSignedType() ) {
    auto *extended = ARMINSTRUCTIONS.CreateRegister( "" );

    ARMINSTRUCTIONS.insertMOV(
      OPER_AL, extended, value, OPER_LSL, 32 - mBitfieldLength );
    ARMINSTRUCTIONS.insertMOV(
      OPER_AL, extended, extended, OPER_ASR, 32 - mBitfieldLength );
    return( extended );
  } else
    return( value );
};


//
// Protected class methods
//

/*
  clone creates a copy of an lvalue.

  clone just calls the corresponding copy constructor.
*/
LValue *ARM_LValue::clone( void ) const
{
  DSTART( "virtual LValue* ARM_LValue::clone() const" );

  return( new ARM_LValue( *this ) );
};
