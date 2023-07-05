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
  @file wirbus.cc
  @brief This file implements %WIR buses.

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
  Copy constructor.
*/
WIR_Bus::WIR_Bus( const WIR_Bus &__o ) :
  WIR_SystemComponent { __o },
  mArbitrationType { __o.mArbitrationType },
  mMaxAccessTime { __o.mMaxAccessTime }
{
  DSTART( "WIR_Bus::WIR_Bus(const WIR_Bus&)" );

  mBusArbitration.reset( __o.mBusArbitration->clone() );
};


/*
  Move constructor.
*/
WIR_Bus::WIR_Bus( WIR_Bus &&__o ) :
  WIR_SystemComponent { move( __o ) },
  mArbitrationType { move( __o.mArbitrationType ) },
  mMaxAccessTime { move( __o.mMaxAccessTime ) }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  mBusArbitration = move( __o.mBusArbitration );
  __o.mMaxAccessTime = 0;
};


/*
  Destructor.
*/
WIR_Bus::~WIR_Bus( void )
{
  DSTART( "virtual WIR_Bus::~WIR_Bus()" );
};


/*
  getType returns the type of a system component, i.e., that it is a bus.
*/
WIR_SystemComponentType WIR_Bus::getType( void ) const
{
  DSTART( "virtual WIR_SystemComponentType WIR_Bus::getType() const" );

  return( WIR_SystemComponentType::bus );
};


/*
  getBusArbitration returns a buses' arbitration configuration.
*/
const WIR_BusArbitration &WIR_Bus::getBusArbitration( void ) const
{
  DSTART( "const WIR_BusArbitration& WIR_Bus::getBusArbitration() const" );

  return( *mBusArbitration );
};


/*
  getArbitrationType returns a buses' arbitration strategy.
*/
WIR_BusArbitrationType WIR_Bus::getArbitrationType( void ) const
{
  DSTART( "WIR_BusArbitrationType WIR_Bus::getArbitrationType() const" );

  return( mArbitrationType );
};


/*
  getMaxDelay returns the maximum number of clock cycles for which a bus can
  delay a memory access.

  Since the current implementation does not support hierarchies with multiple
  levels of buses, getMaxDelay simply returns d.
*/
unsigned int WIR_Bus::getMaxDelay( const unsigned int d ) const
{
  DSTART( "virtual unsigned int WIR_Bus::getMaxDelay(unsigned int) const" );

  return( d );
};


/*
  getMaxAccessTime returns the maximum access time for a single memory access
  via this bus.

  This is NOT the arbitration time, only the pure memory module access time.
  Arbitration policies can use this value to determine whether an incoming
  request can still complete in a give time frame.
*/
unsigned int WIR_Bus::getMaxAccessTime( void ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  return( mMaxAccessTime );
};


//
// Private class methods
//

/*
  Default constructor.
*/
WIR_Bus::WIR_Bus( std::string &&s, const WIR_BusArbitration &a ) :
  WIR_SystemComponent { move( s ) },
  mBusArbitration { a.clone() },
  mMaxAccessTime { 0 }
{
  DSTART( "WIR_Bus::WIR_Bus(string&&, const WIR_BusArbitration&)" );

  auto *p = &a;

  if ( dynamic_cast<const WIR_PriorityDivisionBusArbitration *>( p ) !=
         nullptr )
    mArbitrationType = WIR_BusArbitrationType::pd;
  else

  if ( dynamic_cast<const WIR_FixedPriorityBusArbitration *>( p ) != nullptr )
    mArbitrationType = WIR_BusArbitrationType::fp;
  else

  if ( dynamic_cast<const WIR_RoundRobinBusArbitration *>( p ) != nullptr )
    mArbitrationType = WIR_BusArbitrationType::rr;
  else

  if ( dynamic_cast<const WIR_TDMABusArbitration *>( p ) != nullptr )
    mArbitrationType = WIR_BusArbitrationType::tdma;
  else
    ufAssertT( false, "Unsupported bus arbiter detected." );
};


/*
  Copy-assignment operator.
*/
WIR_Bus & WIR_Bus::operator = ( const WIR_Bus &__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  WIR_SystemComponent::operator = ( __o );

  mBusArbitration.reset( __o.mBusArbitration->clone() );
  mArbitrationType = __o.mArbitrationType;
  mMaxAccessTime = __o.mMaxAccessTime;

  return( *this );
};


/*
  Move-assignment operator.
*/
WIR_Bus & WIR_Bus::operator = ( WIR_Bus &&__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  WIR_SystemComponent::operator = ( move( __o ) );

  mBusArbitration = move( __o.mBusArbitration );
  mArbitrationType = __o.mArbitrationType;
  mMaxAccessTime = move( __o.mMaxAccessTime );
  __o.mMaxAccessTime = 0;

  return( *this );
};


/*
  clone creates a copy of a %WIR bus.
*/
WIR_Bus *WIR_Bus::clone( void ) const
{
  DSTART( "virtual WIR_Bus* WIR_Bus::clone() const" );

  return( new WIR_Bus( *this ) );
};


/*
  setMaxAccessTime sets the maximum access time for a single memory access via
  this bus.

  This is NOT the arbitration time, only the pure memory module access time.
*/
void WIR_Bus::setMaxAccessTime( unsigned int t )
{
  DSTART( "void WIR_Bus::setMaxAccessTime(unsigned int)" );

  mMaxAccessTime = t;

  if ( ( getArbitrationType() == WIR_BusArbitrationType::pd ) ||
       ( getArbitrationType() == WIR_BusArbitrationType::tdma ) )
    dynamic_cast<WIR_TDMABusArbitration &>(
      *mBusArbitration ).setMaxAccessTime( t );
};

}       // namespace WIR
