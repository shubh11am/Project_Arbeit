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
  @file wirlocation.cc
  @brief This file implements location bits within the L4 half-order.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include standard headers
#include <utility>

// Include libuseful headers
#include <libuseful/debugmacros.h>
#include <libuseful/io.h>

// Include WIR headers
#include <wir/wir.h>


//
// Code section
//

namespace WIR {


using namespace std;


//
// Public class methods
//

/*
  Default constructor creating a location refering to a bit within a register
  produced by a given parameter.
*/
WIR_Location::WIR_Location( const WIR_RegisterParameter &rp, unsigned int b ) :
  mParam { rp },
  mType { WIR_LocationType::reg },
  mBitPosition { b }
{
  DSTART(
    "WIR_Location::WIR_Location(const WIR_RegisterParameter&, unsigned int)" );
};


/*
  Default constructor creating a location refering to a bit of the given
  symbol's address.
*/
WIR_Location::WIR_Location( const WIR_Symbol &s, unsigned int b ) :
  mSymbol { s },
  mType { WIR_LocationType::sym },
  mBitPosition { b }
{
  DSTART( "WIR_Location::WIR_Location(const WIR_Symbol&, unsigned int)" );
};


/*
  Copy constructor.
*/
WIR_Location::WIR_Location( const WIR_Location &__o ) :
  mParam { __o.mParam },
  mSymbol { __o.mSymbol },
  mType { __o.mType },
  mBitPosition { __o.mBitPosition }
{
  DSTART( "WIR_Location::WIR_Location(const WIR_Location&)" );
};


/*
  Move constructor.
*/
WIR_Location::WIR_Location( WIR_Location &&__o ) :
  mParam { std::move( __o.mParam ) },
  mSymbol { std::move( __o.mSymbol ) },
  mType { __o.mType },
  mBitPosition { __o.mBitPosition }
{
  DSTART( "WIR_Location::WIR_Location(WIR_Location&&)" );

  __o.mParam = boost::none;
  __o.mSymbol = boost::none;
  __o.mBitPosition = 0;
};


/*
  Destructor.
*/
WIR_Location::~WIR_Location( void )
{
  DSTART( "WIR_Location::~WIR_Location()" );
};


/*
  Copy-assignment operator.
*/
WIR_Location & WIR_Location::operator = ( const WIR_Location &__o )
{
  DSTART( "WIR_Location& WIR_Location::operator=(const WIR_Location&)" );

  mParam = __o.mParam;
  mSymbol = __o.mSymbol;
  mType = __o.mType;
  mBitPosition = __o.mBitPosition;

  return( *this );
};


/*
  Move-assignment operator.
*/
WIR_Location & WIR_Location::operator = ( WIR_Location &&__o )
{
  DSTART( "WIR_Location& WIR_Location::operator=(WIR_Location&&)" );

  mParam = std::move( __o.mParam );
  __o.mParam = boost::none;

  mSymbol = std::move( __o.mSymbol );
  __o.mParam = boost::none;

  mType = __o.mType;

  mBitPosition = __o.mBitPosition;
  __o.mBitPosition = 0;

  return( *this );
};


/*
  The == operator checks for equality of locations.

  Two locations are equal if and only if they are of the same type and refer to
  the very same bit of the very same register parameter or symbol, resp.
*/
bool WIR_Location::operator == ( const WIR_Location &__o ) const
{
  DSTART( "bool WIR_Location::operator==(const WIR_Location&) const" );

  if ( ( mType == __o.mType ) && ( mBitPosition == __o.mBitPosition ) &&
       ( ( ( mType == WIR_LocationType::reg ) &&
           ( mParam.get().get() == __o.mParam.get().get() ) ) ||
         ( ( mType == WIR_LocationType::sym ) &&
           ( mSymbol.get().get() == __o.mSymbol.get().get() ) ) ) )
    return( true );

  return( false );
};


/*
  The != operator checks for inequality of locations.

  Two locations are inequal if they have different type or if they refer to
  different register parameters or different symbols, resp., or if they refer to
  different bits within the same register parameter or symbol, resp.
*/
bool WIR_Location::operator != ( const WIR_Location &__o ) const
{
  DSTART( "bool WIR_Location::operator!=(const WIR_Location&) const" );

  return( !operator == ( __o ) );
};


/*
  getType returns the type of a %WIR location, i.e., whether it refers to a
  register parameter or a symbol.
*/
WIR_LocationType WIR_Location::getType( void ) const
{
  DSTART( "WIR_LocationType WIR_Location::getType() const" );

  return( mType );
};


/*
  isRegisterParameter returns whether a location refers to a register parameter.
*/
bool WIR_Location::isRegisterParameter( void ) const
{
  DSTART( "bool WIR_Location::isRegisterParameter() const" );

  return( mType == WIR_LocationType::reg );
};


/*
  isSymbol returns whether a location refers to a symbol.
*/
bool WIR_Location::isSymbol( void ) const
{
  DSTART( "bool WIR_Location::isSymbol() const" );

  return( mType == WIR_LocationType::sym );
};


/*
  getRegisterParameter returns the register parameter to which a location refers
  to.

  getRegisterParameter asserts if the location does not refer to a register
  parameter.
*/
const WIR_RegisterParameter &WIR_Location::getRegisterParameter( void ) const
{
  DSTART(
    "const WIR_RegisterParameter& WIR_Location::getRegisterParameter() const" );

  #ifdef FAILSAFEMODE
  ufAssert( mType == WIR_LocationType::reg );
  #endif

  return( mParam.get().get() );
};


/*
  getSymbol returns the symbol to which a location refers to.

  getSymbol asserts if the location does not refer to a symbol.
*/
const WIR_Symbol &WIR_Location::getSymbol( void ) const
{
  DSTART( "const WIR_Symbol& WIR_Location::getSymbol() const" );

  #ifdef FAILSAFEMODE
  ufAssert( mType == WIR_LocationType::sym );
  #endif

  return( mSymbol.get().get() );
};


/*
  getBitPosition returns a location's bit position within the refered register
  parameter or symbol address.
*/
unsigned int WIR_Location::getBitPosition( void ) const
{
  DSTART( "unsigned int WIR_Location::getBitPosition() const" );

  return( mBitPosition );
};

}       // namespace WIR
