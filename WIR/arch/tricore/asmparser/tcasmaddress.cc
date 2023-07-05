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
  @file tcasmaddress.cc
  @brief This file implements address arguments.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include standard headers
#include <stdexcept>

// Include boost headers
#include <boost/current_function.hpp>

// Include libuseful headers
#include <libuseful/debugmacros.h>

// Include WIR headers
#include <wir/wir.h>
#include <arch/tricore/tc131.h>

// Include local headers
#include "tcasmaddress.h"


//
// Code section
//

namespace WIR {


using namespace std;


//
// Public class methods
//

/*
  Default constructor creating an address argument for a given register
  argument, offset and type.

  This constructor takes over the ownership of the given pointer. The object
  pointed to will automatically be deleted during destruction of this address
  argument.

  The constructor throws std::invalid_argument if the offset is too large for
  the given argument type.
*/
TC_AsmAddress::TC_AsmAddress( TC_AsmRegister *r, long long o, Type t ) :
  TC_AsmArgument { t },
  mRegister1 { r },
  mOffset { o }
{
  DSTART(
    "TC_AsmAddress::TC_AsmAddress(TC_AsmRegister*, long long int, TC_AsmArgument::Type)" );

  switch ( t ) {

    case Type::AMODE_BASE: {
      if ( o == 0 )
        setType( Type::AMODE_REGI );
      else

      if ( ( o >= 0 ) &&
           ( o <= (long long) TC_Const16_Unsigned::getMaxValue( 4 ) ) &&
           ( TC131::isA15( r->getRegister() ) ) )
        setType( Type::AMODE_A15BASE4 );
      else

      if ( ( o >= 0 ) &&
           ( o <= (long long) TC_Const16_Unsigned::getMaxValue( 4 ) ) )
        setType( Type::AMODE_BASE4 );
      else

      if ( ( o >= 0 ) &&
           ( o <= (long long) TC_Const16_Unsigned::getMaxValue( 10 ) ) &&
           ( o % 4 == 0 ) && TC131::isSP( r->getRegister() ) )
        setType( Type::AMODE_SPREL );
      else

      if ( ( o >= TC_Const16_Signed::getMinValue( 10 ) ) &&
           ( o <= TC_Const16_Signed::getMaxValue( 10 ) ) )
        setType( Type::AMODE_BASE );
      else

      if ( ( o >= TC_Const16_Signed::getMinValue( 16 ) ) &&
           ( o <= TC_Const16_Signed::getMaxValue( 16 ) ) )
        setType( Type::AMODE_BASELO );
      else
        throw invalid_argument( "Offset larger than 16 bits." );

      break;
    }

    case Type::AMODE_POSTINC: {
      if ( o == 0 )
        setType( Type::AMODE_POSTINCCNST );
      else

      if ( ( o < TC_Const16_Signed::getMinValue( 10 ) ) ||
           ( o > TC_Const16_Signed::getMaxValue( 10 ) ) )
        throw invalid_argument( "Offset larger than 10 bits." );

      break;
    }

    case Type::AMODE_PREINC: {
      if ( ( o < TC_Const16_Signed::getMinValue( 10 ) ) ||
           ( o > TC_Const16_Signed::getMaxValue( 10 ) ) )
        throw invalid_argument( "Offset larger than 10 bits." );

      break;
    }

    case Type::AMODE_BREV: {
      auto &reg = mRegister1->getRegister();
      ufAssert( reg.isChild() );

      // Store sibling address register for bit-reverse addressing.
      mRegister2.reset(
        new TC_AsmRegister(
          reg.getParent().getLeafs().back().get(), Type::AREG ) );

      break;
    }

    case Type::AMODE_CIRC: {
      if ( ( o < TC_Const16_Signed::getMinValue( 10 ) ) ||
           ( o > TC_Const16_Signed::getMaxValue( 10 ) ) )
        throw invalid_argument( "Offset larger than 10 bits." );

      auto &reg = mRegister1->getRegister();
      ufAssert( reg.isChild() );

      // Store sibling address register for circular addressing.
      mRegister2.reset(
        new TC_AsmRegister(
          reg.getParent().getLeafs().back().get(), Type::AREG ) );

      break;
    }

    case Type::AMODE_POSTINCCNST:
    case Type::AMODE_REGI:
      break;

    default: {
      throw invalid_argument( "Illegal addressing mode." );
    }

  }
};


/*
  Default constructor creating an address argument of type AMODE_BASE for a
  given WIR register and offset.
*/
TC_AsmAddress::TC_AsmAddress( const WIR_BaseRegister &r, long long o ) :
  TC_AsmArgument { Type::AMODE_BASE },
  mRegister1 { new TC_AsmRegister( r, Type::AREG ) },
  mOffset { o }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );
};


/*
  Default constructor creating an address argument of types AMODE_BASEHILAB or
  AMODE_BASELOLAB for a given register argument and label.

  This constructor takes over the ownership of the given pointer. The object
  pointed to will automatically be deleted during destruction of this address
  argument.
*/
TC_AsmAddress::TC_AsmAddress( TC_AsmRegister *r, Type t,
                              const std::string &l ) :
  TC_AsmArgument { t },
  mRegister1 { r },
  mOffset { 0 },
  mName { l }
{
  DSTART(
    "TC_AsmAddress::TC_AsmAddress(TC_AsmRegister*, TC_AsmArgument::Type, const string&)" );

  ufAssert( ( t == Type::AMODE_BASEHILAB ) || ( t == Type::AMODE_BASELOLAB ) );
};


/*
  Copy constructor.
*/
TC_AsmAddress::TC_AsmAddress( const TC_AsmAddress &a ) :
  TC_AsmArgument { a },
  mRegister1 { new TC_AsmRegister( *(a.mRegister1) ) },
  mOffset { a.mOffset }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  if ( a.mRegister2.get() != nullptr )
    mRegister2.reset( new TC_AsmRegister( *(a.mRegister2) ) );
};


/*
  Destructor.
*/
TC_AsmAddress::~TC_AsmAddress( void )
{
  DSTART( "virtual TC_AsmAddress::~TC_AsmAddress()" );
};


/*
  Copy-assignment operator.
*/
TC_AsmAddress & TC_AsmAddress::operator = ( const TC_AsmAddress &__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  TC_AsmArgument::operator = ( __o );

  mRegister1.reset( new TC_AsmRegister( *(__o.mRegister1) ) );
  mOffset = __o.mOffset;

  if ( __o.mRegister2.get() != nullptr )
    mRegister2.reset( new TC_AsmRegister( *(__o.mRegister2) ) );

  return( *this );
};


/*
  isCompatible returns whether an address is compatible with a given argument
  type.
*/
bool TC_AsmAddress::isCompatible( Type t ) const
{
  DSTART(
    "virtual bool TC_AsmAddress::isCompatible(TC_AsmArgument::Type) const" );

  auto tVal = static_cast<unsigned long long>( t );
  auto getVal = static_cast<unsigned long long>( getType() );

  if ( !( tVal & static_cast<unsigned long long>( Type::AMODE ) ) )
    return( false );

  if ( tVal & static_cast<unsigned long long>( Type::AMODE_BASE ) )
    if ( ( static_cast<unsigned long long>( Type::AMODE_REGI ) |
           static_cast<unsigned long long>( Type::AMODE_BASE4 ) |
           static_cast<unsigned long long>( Type::AMODE_BASE ) |
           static_cast<unsigned long long>( Type::AMODE_SPREL ) |
           static_cast<unsigned long long>( Type::AMODE_A15BASE4 ) ) & getVal )
      return( true );

  if ( tVal & static_cast<unsigned long long>( Type::AMODE_BASELO ) )
    if ( ( static_cast<unsigned long long>( Type::AMODE_REGI ) |
           static_cast<unsigned long long>( Type::AMODE_BASE4 ) |
           static_cast<unsigned long long>( Type::AMODE_BASE ) |
           static_cast<unsigned long long>( Type::AMODE_BASELO ) |
           static_cast<unsigned long long>( Type::AMODE_SPREL ) |
           static_cast<unsigned long long>( Type::AMODE_A15BASE4 ) ) & getVal )
      return( true );

  if ( tVal & static_cast<unsigned long long>( Type::AMODE_POSTINC ) )
    if ( ( static_cast<unsigned long long>( Type::AMODE_POSTINCCNST ) |
           static_cast<unsigned long long>( Type::AMODE_POSTINC ) ) & getVal )
      return( true );

  if ( tVal & static_cast<unsigned long long>( Type::AMODE_BASE4 ) )
    if ( ( static_cast<unsigned long long>( Type::AMODE_REGI ) |
           static_cast<unsigned long long>( Type::AMODE_BASE4 ) |
           static_cast<unsigned long long>( Type::AMODE_A15BASE4 ) ) & getVal )
      return( true );

  return( tVal & getVal );
};


/*
  getRegister returns the register represented by an assembly address argument.
*/
const WIR_BaseRegister &TC_AsmAddress::getRegister( void ) const
{
  DSTART( "const WIR_BaseRegister& TC_AsmAddress::getRegister() const" );

  return( mRegister1->getRegister() );
};


/*
  getOffset returns an assembly address argument's offset.
*/
long long TC_AsmAddress::getOffset( void ) const
{
  DSTART( "long long int TC_AsmAddress::getOffset() const" );

  return( mOffset );
};


/*
  getName returns the label's name.
*/
std::string TC_AsmAddress::getName( void ) const
{
  DSTART( "string TC_AsmAddress::getName() const" );

  return( mName );
};


//
// Protected class methods
//

/*
  clone creates a copy of an address argument.
*/
TC_AsmAddress *TC_AsmAddress::clone( void ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  return( new TC_AsmAddress( *this ) );
};

}       // namespace WIR
