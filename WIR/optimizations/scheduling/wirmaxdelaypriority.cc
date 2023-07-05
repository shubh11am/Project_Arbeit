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
  @file wirmaxdelaypriority.cc
  @brief This file implements an abstract base class computing the maximum delay
         scheduling priority for %WIR operations.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include standard headers
#include <sstream>

// Include boost headers
#include <boost/current_function.hpp>

// Include libuseful headers
#include <libuseful/debugmacros.h>
#include <libuseful/io.h>

// Include WIR headers
#include <wir/wir.h>

// Include local headers
#include "wirmaxdelaypriority.h"


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
WIR_MaxDelayPriority::WIR_MaxDelayPriority( void ) :
  WIR_SchedulingPriority {}
{
  DSTART( "WIR_MaxDelayPriority::WIR_MaxDelayPriority()" );
};


/*
  Copy constructor.
*/
WIR_MaxDelayPriority::WIR_MaxDelayPriority( const WIR_MaxDelayPriority &__o ) :
  WIR_SchedulingPriority { __o }
{
  DSTART(
    "WIR_MaxDelayPriority::WIR_MaxDelayPriority(const WIR_MaxDelayPriority&)" );
};


/*
  Destructor.
*/
WIR_MaxDelayPriority::~WIR_MaxDelayPriority( void )
{
  DSTART( "virtual WIR_MaxDelayPriority::~WIR_MaxDelayPriority()" );
};


/*
  Copy-assignment operator.
*/
WIR_MaxDelayPriority & WIR_MaxDelayPriority::operator = ( const WIR_MaxDelayPriority &__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  WIR_SchedulingPriority::operator = ( __o );

  return( *this );
};


/*
  getPriority determines an operation's scheduling priority using the maximum
  delay scheduling priority.
*/
long long WIR_MaxDelayPriority::getPriority( const WIR_Operation &o ) const
{
  DSTART(
    "virtual long long int WIR_MaxDelayPriority::getPriority(const WIR_Operation&) const" );

  // The maximum delay heuristic bases on the path length from the given
  // operation to the last operation of the region.
  auto it = mMaxDelayMap->find( o.getID() );

  #ifdef FAILSAFEMOE
  ufAssert( it != mMaxDelayMap->end() );
  #endif

  DACTION(
    stringstream str;
    str << o;
    DOUT(
      "Maximum delay priority of '" << str.str().substr( 8 ) << "' is " <<
      it->second << endl ); );

  return( it->second );
};

}       // namespace WIR
