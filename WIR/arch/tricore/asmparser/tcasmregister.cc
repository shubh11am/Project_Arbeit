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
  @file tcasmregister.cc
  @brief This file implements register assembly arguments.

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
#include <wir/wir.h>

// Include local headers
#include "tcasmregister.h"


//
// Code section
//

namespace WIR {


using namespace std;


//
// Public class methods
//

/*
  Default constructor creating an empty register argument.
*/
TC_AsmRegister::TC_AsmRegister( void ) :
  TC_AsmArgument { Type::NONE },
  mRegister { nullptr }
{
  DSTART( "TC_AsmRegister::TC_AsmRegister()" );
};


/*
  Default constructor creating an assembly argument for a given WIR register and
  type.
*/
TC_AsmRegister::TC_AsmRegister( const WIR_BaseRegister &r, Type t ) :
  TC_AsmArgument { t },
  mRegister { &r }
{
  DSTART(
    "TC_AsmRegister::TC_AsmRegister(const WIR_BaseRegister&, TC_AsmArgument::Type)" );
};


/*
  Copy constructor.
*/
TC_AsmRegister::TC_AsmRegister( const TC_AsmRegister &r ) :
  TC_AsmArgument { r },
  mRegister { r.mRegister }
{
  DSTART( "TC_AsmRegister::TC_AsmRegister(const TC_AsmRegister&)" );
};


/*
  Destructor.
*/
TC_AsmRegister::~TC_AsmRegister( void )
{
  DSTART( "virtual TC_AsmRegister::~TC_AsmRegister()" );
};


/*
  Copy-assignment operator.
*/
TC_AsmRegister & TC_AsmRegister::operator = ( const TC_AsmRegister &__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  TC_AsmArgument::operator = ( __o );

  mRegister = __o.mRegister;

  return( *this );
};


/*
  isCompatible returns whether a register is compatible with a given argument
  type.
*/
bool TC_AsmRegister::isCompatible( Type t ) const
{
  DSTART(
    "virtual bool TC_AsmRegister::isCompatible(TC_AsmArgument::Type) const");

  auto tVal = static_cast<unsigned long long>( getType() );

  switch ( t ) {
    case Type::AREG:
      return( tVal & static_cast<unsigned long long>( Type::ANYAREG ) );

    case Type::DREG:
      return( tVal & static_cast<unsigned long long>( Type::ANYDREG ) );

    case Type::EREG:
      return( tVal & static_cast<unsigned long long>( Type::EREG ) );

    case Type::AREG64:
      return( tVal &
              ( static_cast<unsigned long long>( Type::ANYAREG ) |
                static_cast<unsigned long long>( Type::AREG64 ) ) );

    default:
      return( getType() == t );
  }
};


/*
  getRegister returns the register represented by an assembly operation
  argument.
*/
const WIR_BaseRegister &TC_AsmRegister::getRegister( void ) const
{
  DSTART(
    "virtual const WIR_BaseRegister& TC_AsmRegister::getRegister() const" );

  return( *mRegister );
};


//
// Protected class methods
//

/*
  clone creates a copy of a register argument.
*/
TC_AsmRegister *TC_AsmRegister::clone( void ) const
{
  DSTART( "virtual TC_AsmRegister* TC_AsmRegister::clone() const" );

  return( new TC_AsmRegister( *this ) );
};

}       // namespace WIR
