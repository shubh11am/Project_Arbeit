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
  @file wirschedulingpriority.cc
  @brief This file implements an abstract base class computing scheduling
         priorities for %WIR operations.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include standard headers
#include <limits>
#include <sstream>

// Include boost headers
#include <boost/current_function.hpp>

// Include libuseful headers
#include <libuseful/debugmacros.h>
#include <libuseful/io.h>

// Include WIR headers
#include <wir/wir.h>

// Include local headers
#include "wirschedulingregion.h"
#include "wirschedulingpriority.h"


//
// Code section
//

namespace WIR {


using namespace std;


// invalid denotes a special, invalid scheduling priority.
const long long WIR_SchedulingPriority::invalid { numeric_limits<long long>::min() };

// penalty denotes a special, penalty scheduling priority.
const long long WIR_SchedulingPriority::penalty { numeric_limits<long long>::min() + 1 };

// skip denotes a special, skip scheduling priority.
const long long WIR_SchedulingPriority::skip { -1 };

// valid denotes a special, valid scheduling priority.
const long long WIR_SchedulingPriority::valid { 0 };


//
// Public class methods
//

/*
  Default constructor.
*/
WIR_SchedulingPriority::WIR_SchedulingPriority( void ) :
  mRegion { nullptr },
  mEarliestCycleMap { nullptr },
  mMaxDelayMap { nullptr },
  mMobilityMap { nullptr }
{
  DSTART( "WIR_SchedulingPriority::WIR_SchedulingPriority()" );
};


/*
  Copy constructor.
*/
WIR_SchedulingPriority::WIR_SchedulingPriority( const WIR_SchedulingPriority &__o ) :
  mRegion { __o.mRegion },
  mEarliestCycleMap { __o.mEarliestCycleMap },
  mMaxDelayMap { __o.mMaxDelayMap },
  mMobilityMap { __o.mMobilityMap }
{
  DSTART(
    "WIR_SchedulingPriority::WIR_SchedulingPriority(const WIR_SchedulingPriority&)" );
};


/*
  Destructor.
*/
WIR_SchedulingPriority::~WIR_SchedulingPriority( void )
{
  DSTART( "virtual WIR_SchedulingPriority::~WIR_SchedulingPriority()" );
};


/*
  Copy-assignment operator.
*/
WIR_SchedulingPriority & WIR_SchedulingPriority::operator = ( const WIR_SchedulingPriority &__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  mRegion = __o.mRegion;
  mEarliestCycleMap = __o.mEarliestCycleMap;
  mMaxDelayMap = __o.mMaxDelayMap;
  mMobilityMap = __o.mMobilityMap;

  return( *this );
};


/*
  setRegion sets the scheduling region for which priorities will be computed.
*/
void WIR_SchedulingPriority::setRegion( WIR_SchedulingRegion &r, bool asap )
{
  DSTART(
    "void WIR_SchedulingPriority::setRegion(WIR_SchedulingRegion&, bool)" );

  mRegion = &r;

  if ( asap )
    mEarliestCycleMap = &r.getEarliestCycleOpsMap();
  else
    mEarliestCycleMap = &r.getLatestCycleOpsMap();
  mMaxDelayMap = &r.getMaxDelayMap();
  mMobilityMap = &r.getMobilityMap();
};


/*
  For a given execution cycle, getBestOperations returns the sequence of
  operations of a region having the best scheduling priority.

  getBestOperations fails with an assertion if setRegion was not invoked before.
*/
std::list<WIR_Operation *> WIR_SchedulingPriority::getBestOperations( long long cycle,
                                                                      WIR_Instruction *prev )
{
  DSTART(
    "list<WIR_Operation*> WIR_SchedulingPriority::getBestOperations(long long int, WIR_Instruction*)" );

  ufAssertT(
    mRegion != nullptr,
    "setRegion must be applied before calling getBestOperations." );

  DOUT(
    string( 80, '-' ) << endl <<
    "Finding best operations for execution cycle " << cycle << "." << endl );

  // Pre-compute some priority-relevant data for the current execution cycle.
  setupCurrentCycle( cycle );

  list<WIR_Operation *> res;
  long long maxPrio = invalid;

  // Determine last operation that has previously been scheduled.
  WIR_Operation *pred = nullptr;
  if ( ( prev != nullptr ) && !prev->getOperations().empty() )
    pred = &(prev->getOperations().back().get());

  // Iterate all operations available for scheduling in the current execution
  // cycle.
  for ( auto it = mEarliestCycleMap->begin();
        it != mEarliestCycleMap->upper_bound( cycle ); ++it ) {
    for ( WIR_Operation &o : it->second ) {
      auto p = computePriority( o, pred, cycle );

      if ( ( p.first != skip ) && ( p.first > maxPrio ) ) {
        // We found a list of schedulable operations with better priority than
        // anything seen so far.
        maxPrio = p.first;
        res = std::move( p.second );

        DACTION(
          DOUT(
            "Found new operations with higher priority " << maxPrio << ": {" );
          for ( auto *op : res )
            if ( op != nullptr ) {
              stringstream str;
              str << *op;
              DOUT( " '" << str.str().substr( 8 ) << "'" );
            } else
              DOUT( " <nullptr>" );
          DOUT( " }" << endl ); );
      }
    }
  }
  DOUT( string( 80, '-' ) << endl );

  return( res );
};


//
// Protected class methods
//

/*
  setupCurrentCycle pre-computes some priority-relevant data about the
  operations of the current scheduling region.

  Since this task is processor-specific and might or might not be necessary for
  some actual processor, this method is virtual and can be overloaded if
  required.
*/
void WIR_SchedulingPriority::setupCurrentCycle( long long cycle )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  (void) cycle;
};

}       // namespace WIR
