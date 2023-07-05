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
  @file wirnameapi.cc
  @brief This file implements a base class for managing names of named derived
         classes.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include standard headers
#include <iostream>

// Include libuseful headers
#include <libuseful/debugmacros.h>

// Include private headers
#include "wirnameapi.h"


//
// Code section
//

namespace WIR {


using namespace std;


//
// Public class methods
//

/*
  setName sets an object's specific name.
*/
void WIR_Name_API::setName( const std::string &s )
{
  DSTART( "void WIR_Name_API::setName(const string&)" );

  mName = s;
};


/*
  setName sets an object's specific name.
*/
void WIR_Name_API::setName( std::string &&s )
{
  DSTART( "void WIR_Name_API::setName(string&&)" );

  mName = move( s );
  s.clear();
};


/*
  getName returns an object's specific name.
*/
std::string WIR_Name_API::getName( void ) const
{
  DSTART( "string WIR_Name_API::getName() const" );

  return( mName );
};


//
// Protected class methods
//

/*
  Default constructor assigning an empty name.
*/
WIR_Name_API::WIR_Name_API( void ) :
  mName { "" }
{
  DSTART( "WIR_Name_API::WIR_Name_API()" );
};


/*
  Copy constructor.
*/
WIR_Name_API::WIR_Name_API( const WIR_Name_API &__o ) :
  mName { __o.mName }
{
  DSTART( "WIR_Name_API::WIR_Name_API(const WIR_Name_API&)" );
};


/*
  Move constructor.
*/
WIR_Name_API::WIR_Name_API( WIR_Name_API &&__o ) :
  mName { move( __o.mName ) }
{
  DSTART( "WIR_Name_API::WIR_Name_API(WIR_Name_API&&)" );

  __o.mName.clear();
};


/*
  Destructor.
*/
WIR_Name_API::~WIR_Name_API( void )
{
  DSTART( "virtual WIR_Name_API::~WIR_Name_API()" );
};


/*
  Copy-assignment operator.
*/
WIR_Name_API & WIR_Name_API::operator = ( const WIR_Name_API &__o )
{
  DSTART( "WIR_Name_API& WIR_Name_API::operator=(const WIR_Name_API&)" );

  mName = __o.mName;

  return( *this );
};


/*
  Move-assignment operator.
*/
WIR_Name_API & WIR_Name_API::operator = ( WIR_Name_API &&__o )
{
  DSTART( "WIR_Name_API& WIR_Name_API::operator=(WIR_Name_API&&)" );

  mName = move( __o.mName );
  __o.mName.clear();

  return( *this );
};

}       // namespace WIR
