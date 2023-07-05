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
  @file wirliveout.cc
  @brief This file implements WIR containers representing sets of registers that
         are live-out at instructions.

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

// Include boost headers
#include <boost/current_function.hpp>

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
WIR_LiveOut::WIR_LiveOut( void ) :
  WIR_Container<WIR_LiveOut> {}
{
  DSTART( "WIR_LiveOut::WIR_LiveOut()" );
};


/*
  Copy constructor.
*/
WIR_LiveOut::WIR_LiveOut( const WIR_LiveOut &__o ) :
  WIR_Container<WIR_LiveOut> { __o },
  mRegisterReferences { __o.mRegisterReferences }
{
  DSTART( "WIR_LiveOut::WIR_LiveOut(const WIR_LiveOut&)" );
};


/*
  Move constructor.
*/
WIR_LiveOut::WIR_LiveOut( WIR_LiveOut &&__o ) :
  WIR_Container<WIR_LiveOut> { move( __o ) },
  mRegisterReferences { move( __o.mRegisterReferences ) }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  __o.mRegisterReferences.clear();
};


/*
  Destructor.
*/
WIR_LiveOut::~WIR_LiveOut( void )
{
  DSTART( "virtual WIR_LiveOut::~WIR_LiveOut()" );
};


/*
  Copy-assignment operator.
*/
WIR_LiveOut & WIR_LiveOut::operator = ( const WIR_LiveOut &__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  WIR_Container<WIR_LiveOut>::operator = ( __o );

  mRegisterReferences = __o.mRegisterReferences;

  return( *this );
};


/*
  Move-assignment operator.
*/
WIR_LiveOut & WIR_LiveOut::operator = ( WIR_LiveOut &&__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  WIR_Container<WIR_LiveOut>::operator = ( move( __o ) );

  mRegisterReferences = move( __o.mRegisterReferences );
  __o.mRegisterReferences.clear();

  return( *this );
};


/*
  isUnique returns whether live-out sets are unique, i.e., whether at most one
  instance of this container type can be attached to a WIR class.
*/
bool WIR_LiveOut::isUnique( void ) const
{
  DSTART( "virtual bool WIR_LiveOut::isUnique() const" );

  return( true );
};


/*
  insertRegister adds a new register to set mRegisterReferences.

  A (wrapped) reference to o is added to the set.
*/
void WIR_LiveOut::insertRegister( const WIR_BaseRegister &o )
{
  DSTART( "void WIR_LiveOut::insertRegister(const WIR_BaseRegister&)" );

  mRegisterReferences.insert( const_cast<WIR_BaseRegister &>( o ) );
};


/*
  eraseRegister removes the specified register from set mRegisterReferences.
*/
void WIR_LiveOut::eraseRegister( const WIR_BaseRegister &o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  mRegisterReferences.erase( findRegister( o ) );
};


/*
  clearRegisters removes all elements from set mRegisterReferences.
*/
void WIR_LiveOut::clearRegisters( void )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  mRegisterReferences.clear();
};


/*
  getRegisters returns the set mRegisterReferences.
*/
const WIR_RegisterSet &WIR_LiveOut::getRegisters( void ) const
{
  DSTART( "const WIR_RegisterSet& WIR_LiveOut::getRegisters() const" );

  return( mRegisterReferences );
};


/*
  begin returns an iterator to the first register of a live-out set.
*/
WIR_RegisterSet::const_iterator WIR_LiveOut::begin( void ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  return( mRegisterReferences.begin() );
};


/*
  end returns an iterator to the end of a live-out set.
*/
WIR_RegisterSet::const_iterator WIR_LiveOut::end( void ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  return( mRegisterReferences.end() );
};


/*
  containsRegister returns whether set mRegisterReferences contains a register
  with the specified ID.
*/
bool WIR_LiveOut::containsRegister( WIR_id_t id ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  for ( WIR_BaseRegister &item : mRegisterReferences )
    if ( item.getID() == id )
      return( true );

  return( false );
};


/*
  containsRegister returns whether set mRegisterReferences contains the
  specified register.
*/
bool WIR_LiveOut::containsRegister( const WIR_BaseRegister &o ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  for ( WIR_BaseRegister &item : mRegisterReferences )
    if ( item == o )
      return( true );

  return( false );
};


/*
  findRegister finds a register with the specified ID in set
  mRegisterReferences.
*/
WIR_RegisterSet::const_iterator WIR_LiveOut::findRegister( WIR_id_t id ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  for ( auto it = mRegisterReferences.begin(); it != mRegisterReferences.end();
        ++it )
    if ( (*it).get().getID() == id )
      return( it );

  return( mRegisterReferences.end() );
};


/*
  findRegister finds the specified register in set mRegisterReferences.
*/
WIR_RegisterSet::const_iterator WIR_LiveOut::findRegister( const WIR_BaseRegister &o ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  for ( auto it = mRegisterReferences.begin(); it != mRegisterReferences.end();
        ++it )
    if ( (*it).get() == o )
      return( it );

  return( mRegisterReferences.end() );
};

}       // namespace WIR
