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
  @file wirsystemcomponent.cc
  @brief This file implements generic %WIR system components.

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
#include <libuseful/io.h>

// Include WIR headers
#include <wir/API/wirinsertionapi.h>
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
  Destructor.
*/
WIR_SystemComponent::~WIR_SystemComponent( void )
{
  DSTART( "virtual WIR_SystemComponent::~WIR_SystemComponent()" );
};


//
// API implementations.
//

WIR_INSERTION_IMPL( WIR_System, System, WIR_SystemComponent );


/*
  getName returns a system component's specific name.
*/
std::string WIR_SystemComponent::getName( void ) const
{
  DSTART( "string WIR_SystemComponent::getName() const" );

  return( mName );
};


/*
  getAddressRanges returns the set of address ranges for which this system
  component is active.
*/
const WIR_AddressRangeSet &WIR_SystemComponent::getAddressRanges( void ) const
{
  DSTART(
    "const WIR_AddressRangeSet& WIR_SystemComponent::getAddressRanges() const" );

  return( mAddressRanges );
};


/*
  getHullRange returns the convex hull over all address ranges for which this
  system component is active.

  Obviously, getHullRange produces an address range that may contain addresses
  for which this system component is not active, since these addresses lie
    between other, active ranges.
*/
const WIR_AddressRange WIR_SystemComponent::getHullRange( void ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  WIR_AddressRange res;

  for ( auto &ar : mAddressRanges )
    res.unite( ar );

  return( res );
};


/*
  isActiveInRange returns whether this system component is active for the
  specified address range.

  Derived classes may overwrite this method if they need to support different
  conditions for enabling system components other than the active address range
  assignment.
*/
bool WIR_SystemComponent::isActiveInRange( const WIR_AddressRange &r ) const
{
  DSTART(
    "virtual bool WIR_SystemComponent::isActiveInRange(const WIR_AddressRange&) const" );

  for ( auto &ar : mAddressRanges )
    if ( !r.hasEmptyIntersection( ar ) )
      return( true );

  return( false );
};


//
// Protected class methods
//

/*
  Default constructor creating a named system component.

  This constructor asserts if it is passed an empty string.
*/
WIR_SystemComponent::WIR_SystemComponent( const std::string &s ) :
  WIR_ID_API {}
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  ufAssert( !s.empty() );
  setName( s );
};


/*
  Default constructor creating a named system component.

  This constructor asserts if it is passed an empty string.
*/
WIR_SystemComponent::WIR_SystemComponent( std::string &&s ) :
  WIR_ID_API {}
{
  DSTART( "WIR_SystemComponent::WIR_SystemComponent(string&&)" );

  ufAssert( !s.empty() );
  setName( move( s ) );
};


/*
  Copy constructor.
*/
WIR_SystemComponent::WIR_SystemComponent( const WIR_SystemComponent &__o ) :
  WIR_ID_API { __o },
  mSystemPointer { nullptr },
  mName { __o.mName },
  mAddressRanges { __o.mAddressRanges }
{
  DSTART(
    "WIR_SystemComponent::WIR_SystemComponent(const WIR_SystemComponent&)" );
};


/*
  Move constructor.
*/
WIR_SystemComponent::WIR_SystemComponent( WIR_SystemComponent &&__o ) :
  WIR_ID_API { move( __o ) },
  mSystemPointer { nullptr },
  mName { move( __o.mName ) },
  mAddressRanges { move( __o.mAddressRanges ) }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  ufAssertT(
    __o.mSystemPointer == nullptr,
    "Invalid attempt to move a system component out of its owning system '" <<
    __o.getSystem().getName() << "'." );

  __o.mName.clear();
  __o.mAddressRanges.clear();
};


/*
  Copy-assignment operator.
*/
WIR_SystemComponent & WIR_SystemComponent::operator = ( const WIR_SystemComponent &__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  mName = __o.mName;
  mAddressRanges = __o.mAddressRanges;

  mSystemPointer = nullptr;

  return( *this );
};


/*
  Move-assignment operator.
*/
WIR_SystemComponent & WIR_SystemComponent::operator = ( WIR_SystemComponent &&__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  ufAssertT(
    __o.mSystemPointer == nullptr,
    "Invalid attempt to move a system component out of its owning system '" <<
    __o.getSystem().getName() << "'." );

  mName = move( __o.mName );
  __o.mName.clear();

  mAddressRanges = move( __o.mAddressRanges );
  __o.mAddressRanges.clear();

  mSystemPointer = nullptr;

  return( *this );
};


/*
  addAddressRange adds a range to the set of address ranges for which a system
  component is active.
*/
void WIR_SystemComponent::addAddressRange( const WIR_AddressRange &r )
{
  DSTART(
    "void WIR_SystemComponent::addAddressRange(const WIR_AddressRange&)" );

  mAddressRanges.unite( r );
};


/*
  setName sets an system component's specific name.
*/
void WIR_SystemComponent::setName( const std::string &s )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  mName = s;
};


/*
  setName sets an system component's specific name.
*/
void WIR_SystemComponent::setName( std::string &&s )
{
  DSTART( "void WIR_SystemComponent::setName(string&&)" );

  mName = move( s );
  s.clear();
};

}       // namespace WIR
