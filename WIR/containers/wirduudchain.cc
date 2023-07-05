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
  @file wirduudchain.cc
  @brief This file implements WIR containers representing def-use and use-def
         chains of register parameters.

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
  Default constructor.
*/
WIR_DUUDChain::WIR_DUUDChain( void ) :
  WIR_Container<WIR_DUUDChain> {}
{
  DSTART( "WIR_DUUDChain::WIR_DUUDChain()" );
};


/*
  Destructor.
*/
WIR_DUUDChain::~WIR_DUUDChain( void )
{
  DSTART( "virtual WIR_DUUDChain::~WIR_DUUDChain()" );
};


/*
  isUnique returns whether def-use/use-def chains are unique, i.e., whether at
  most one instance of this container type can be attached to a WIR class.
*/
bool WIR_DUUDChain::isUnique( void ) const
{
  DSTART( "virtual bool WIR_DUUDChain::isUnique() const" );

  return( true );
};


/*
  insertDUChain adds a new register parameter to set mDUChains.

  A (wrapped) reference to o is added to the set.
*/
void WIR_DUUDChain::insertDUChain( const WIR_RegisterParameter &o )
{
  DSTART( "void WIR_DUUDChain::insertDUChain(const WIR_RegisterParameter&)" );

  mDUChains.insert( const_cast<WIR_RegisterParameter &>( o ) );
};


/*
  getDUChains returns the set mDUChains.
*/
const WIR_RegisterParameterSet &WIR_DUUDChain::getDUChains( void ) const
{
  DSTART(
    "const WIR_RegisterParameterSet& WIR_DUUDChain::getDUChains() const" );

  return( mDUChains );
};


/*
  insertUDChain adds a new register parameter to set mUDChains.

  A (wrapped) reference to o is added to the set.
*/
void WIR_DUUDChain::insertUDChain( const WIR_RegisterParameter &o )
{
  DSTART( "void WIR_DUUDChain::insertUDChain(const WIR_RegisterParameter&)" );

  mUDChains.insert( const_cast<WIR_RegisterParameter &>( o ) );
};


/*
  getUDChains returns the set mUDChains.
*/
const WIR_RegisterParameterSet &WIR_DUUDChain::getUDChains( void ) const
{
  DSTART(
    "const WIR_RegisterParameterSet& WIR_DUUDChain::getUDChains() const" );

  return( mUDChains );
};


/*
  insertUDInput adds a new function input to set mUDInputs.
*/
void WIR_DUUDChain::insertUDInput( const WIR_PhysicalRegister &o )
{
  DSTART( "void WIR_DUUDChain::insertUDInput(const WIR_PhysicalRegister&)" );

  mUDInputs.insert( const_cast<WIR_PhysicalRegister &>( o ) );
};


/*
  getUDInputs returns the set mUDInputs.
*/
const WIR_PhysicalRegisterSet &WIR_DUUDChain::getUDInputs( void ) const
{
  DSTART( "const WIR_PhysicalRegisterSet& WIR_DUUDChain::getUDInputs() const" );

  return( mUDInputs );
};

}       // namespace WIR
