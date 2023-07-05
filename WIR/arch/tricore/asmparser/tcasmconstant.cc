/*

   This source file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Intermediate Representation Framework

   and is property of its respective copyright holder. It must neither be used
   nor published even in parts without explicit written permission.

   Copyright 2015 - 2022, Heiko Falk.

   Hamburg University of Technology (TUHH)
   Institute of Embedded Systems
   21071 Hamburg
   Germany

   http://www.tuhh.de/es/esd/research/wcc

*/

/*!
  @file tcasmconstant.cc
  @brief This file implements constant assembly arguments.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include boost headers
#include <boost/current_function.hpp>

// Include libuseful headers
#include <libuseful/debugmacros.h>

// Include WIR headers
#include <arch/tricore/tcconst16signed.h>
#include <arch/tricore/tcconst16unsigned.h>

// Include local headers
#include "tcasmconstant.h"


//
// Code section
//

namespace WIR {


using namespace std;


//
// Public class methods
//

/*
  Default constructor creating a signed constant argument.
*/
TC_AsmConstant::TC_AsmConstant( signed long long v ) :
  TC_AsmArgument { determineSignedType( v ) },
  mIsSigned { true }
{
  DSTART( "TC_AsmConstant::TC_AsmConstant(long long int)" );

  mValue.sVal = v;
};


/*
  Default constructor creating an unsigned constant argument.
*/
TC_AsmConstant::TC_AsmConstant( unsigned long long v ) :
  TC_AsmArgument { determineUnsignedType( v ) },
  mIsSigned { false }
{
  DSTART( "TC_AsmConstant::TC_AsmConstant(long long unsigned int)" );

  mValue.uVal = v;
};


/*
  Copy constructor.
*/
TC_AsmConstant::TC_AsmConstant( const TC_AsmConstant &c ) :
  TC_AsmArgument { c },
  mValue { c.mValue },
  mIsSigned { c.mIsSigned }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );
};


/*
  Destructor.
*/
TC_AsmConstant::~TC_AsmConstant( void )
{
  DSTART( "virtual TC_AsmConstant::~TC_AsmConstant()" );
};


/*
  Copy-assignment operator.
*/
TC_AsmConstant & TC_AsmConstant::operator = ( const TC_AsmConstant &__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  TC_AsmArgument::operator = ( __o );

  mValue = __o.mValue;
  mIsSigned = __o.mIsSigned;

  return( *this );
};


/*
  isCompatible returns whether a constant is compatible with a given argument
  type.
*/
bool TC_AsmConstant::isCompatible( Type t ) const
{
  DSTART(
    "virtual bool TC_AsmConstant::isCompatible(TC_AsmArgument::Type) const" );

  unsigned long long tVal = static_cast<unsigned long long>( t );

  if ( !( ( static_cast<unsigned long long>( Type::NUMERIC ) |
            static_cast<unsigned long long>( Type::AMODE_ABS ) ) & tVal ) ||
       ( getType() == Type::NONE ) )
    return( false );

  if ( t == Type::AMODE_ABS ) {
    // Absolute addressing uses 18-bit offsets in the TriCore ISA with
    // { offset18[17:14], 14b'0, offset18[13:0] }.
    // The value 0x0fffc000 below masks exactly the 14 0-bits in the middle.
    if ( mIsSigned )
      return( ( mValue.sVal & 0x0fffc000 ) == 0 );
    else
      return( ( mValue.uVal & 0x0fffc000 ) == 0 );
  }

  // Smaller constants are compatible when the argument type is sufficiently
  // large.
  if ( static_cast<unsigned long long>( Type::CONST ) & tVal )
    return( static_cast<unsigned long long>( getType() ) <= tVal );

  if ( static_cast<unsigned long long>( Type::BITPOS ) & tVal ) {
    if ( mIsSigned )
      return( ( mValue.sVal >= 0 ) && ( mValue.sVal < 32 ) );
    else
      return( mValue.uVal < 32 );
  }

  if ( static_cast<unsigned long long>( Type::BFWIDTH ) & tVal ) {
    if ( mIsSigned )
      return( ( mValue.sVal >= 0 ) && ( mValue.sVal <= 32 ) );
    else
      return( mValue.uVal <= 32 );
  }

  return( false );
};


/*
  getSignedValue gets a constant's signed value.
*/
signed long long TC_AsmConstant::getSignedValue( void ) const
{
  DSTART( "long long int TC_AsmConstant::getSignedValue() const" );

  return( mValue.sVal );
};


/*
  getUnsignedValue gets a constant's unsigned value.
*/
unsigned long long TC_AsmConstant::getUnsignedValue( void ) const
{
  DSTART( "long long unsigned int TC_AsmConstant::getUnsignedValue() const" );

  return( mValue.uVal );
};


/*
  isSigned returns whether a constant argument is signed or not.
*/
bool TC_AsmConstant::isSigned( void ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  return( mIsSigned );
};


/*
  isUnsigned returns whether a constant argument is unsigned or not.
*/
bool TC_AsmConstant::isUnsigned( void ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  return( !mIsSigned );
};


//
// Protected class methods
//

/*
  clone creates a copy of a constant argument.
*/
TC_AsmConstant *TC_AsmConstant::clone( void ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  return( new TC_AsmConstant( *this ) );
};


//
// Private class methods
//

/*
  determineSignedType determines the argument type for a given signed value.
*/
TC_AsmArgument::Type TC_AsmConstant::determineSignedType( signed long long v )
{
  DSTART(
    "static TC_AsmArgument::Type TC_AsmConstant::determineSignedType(long long int)" );

  if ( ( v >= TC_Const16_Signed::getMinValue( 1 ) ) &&
       ( v <= TC_Const16_Signed::getMaxValue( 1 ) ) )
    return( Type::CONST1 );
  if ( ( v >= TC_Const16_Signed::getMinValue( 2 ) ) &&
       ( v <= TC_Const16_Signed::getMaxValue( 2 ) ) )
    return( Type::CONST2 );
  if ( ( v >= TC_Const16_Signed::getMinValue( 3 ) ) &&
       ( v <= TC_Const16_Signed::getMaxValue( 3 ) ) )
    return( Type::CONST3 );
  if ( ( v >= TC_Const16_Signed::getMinValue( 4 ) ) &&
       ( v <= TC_Const16_Signed::getMaxValue( 4 ) ) )
    return( Type::CONST4 );
  if ( ( v >= TC_Const16_Signed::getMinValue( 5 ) ) &&
       ( v <= TC_Const16_Signed::getMaxValue( 5 ) ) )
    return( Type::CONST5 );
  if ( ( v >= TC_Const16_Signed::getMinValue( 8 ) ) &&
       ( v <= TC_Const16_Signed::getMaxValue( 8 ) ) )
    return( Type::CONST8 );
  if ( ( v >= TC_Const16_Signed::getMinValue( 9 ) ) &&
       ( v <= TC_Const16_Signed::getMaxValue( 9 ) ) )
    return( Type::CONST9 );
  if ( ( v >= TC_Const16_Signed::getMinValue( 16 ) ) &&
       ( v <= TC_Const16_Signed::getMaxValue( 16 ) ) )
    return( Type::CONST16 );
  // Absolute addressing uses 18-bit offsets in the TriCore ISA with
  // { offset18[17:14], 14b'0, offset18[13:0] }.
  // The value 0x0fffc000 below masks exactly the 14 0-bits in the middle.
  if ( ( v & 0x0fffc000 ) == 0 )
    return( Type::AMODE_ABS );

  return( Type::NONE );
};


/*
  determineUnsignedType determines the argument type for a given unsigned value.
*/
TC_AsmArgument::Type TC_AsmConstant::determineUnsignedType( unsigned long long v )
{
  DSTART(
    "static TC_AsmArgument::Type TC_AsmConstant::determineUnsignedType(long long unsigned int)" );

  if ( v <= TC_Const16_Unsigned::getMaxValue( 1 ) )
    return( Type::CONST1 );
  if ( v <= TC_Const16_Unsigned::getMaxValue( 2 ) )
    return( Type::CONST2 );
  if ( v <= TC_Const16_Unsigned::getMaxValue( 3 ) )
    return( Type::CONST3 );
  if ( v <= TC_Const16_Unsigned::getMaxValue( 4 ) )
    return( Type::CONST4 );
  if ( v <= TC_Const16_Unsigned::getMaxValue( 5 ) )
    return( Type::CONST5 );
  if ( v <= TC_Const16_Unsigned::getMaxValue( 8 ) )
    return( Type::CONST8 );
  if ( v <= TC_Const16_Unsigned::getMaxValue( 9 ) )
    return( Type::CONST9 );
  if ( v <= TC_Const16_Unsigned::getMaxValue( 16 ) )
    return( Type::CONST16 );
  // Absolute addressing uses 18-bit offsets in the TriCore ISA with
  // { offset18[17:14], 14b'0, offset18[13:0] }.
  // The value 0x0fffc000 below masks exactly the 14 0-bits in the middle.
  if ( ( v & 0x0fffc000 ) == 0 )
    return( Type::AMODE_ABS );

  return( Type::NONE );
};

}       // namespace WIR
