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
  @file tcschedulingpriority.cc
  @brief This file implements a TriCore-specific class computing scheduling
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
#include <map>
#include <sstream>
#include <utility>

// Include boost headers
#include <boost/current_function.hpp>

// Include libuseful headers
#include <libuseful/debugmacros.h>
#include <libuseful/io.h>

// Include WIR headers
#include <wir/wir.h>
#include <arch/tricore/tc131.h>

// Include local headers
#include "tcblockschedulingregion.h"
#include "tcschedulinginfo.h"
#include "tcschedulingpriority.h"


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
TC_SchedulingPriority::TC_SchedulingPriority( void ) :
  WIR_SchedulingPriority {}
{
  DSTART( "TC_SchedulingPriority::TC_SchedulingPriority()" );
};


/*
  Copy constructor.
*/
TC_SchedulingPriority::TC_SchedulingPriority( const TC_SchedulingPriority &__o ) :
  WIR_SchedulingPriority { __o }
{
  DSTART(
    "TC_SchedulingPriority::TC_SchedulingPriority(const TC_SchedulingPriority&)" );
};


/*
  Destructor.
*/
TC_SchedulingPriority::~TC_SchedulingPriority( void )
{
  DSTART( "virtual TC_SchedulingPriority::~TC_SchedulingPriority()" );
};


/*
  Copy-assignment operator.
*/
TC_SchedulingPriority & TC_SchedulingPriority::operator = ( const TC_SchedulingPriority &__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  WIR_SchedulingPriority::operator = ( __o );

  return( *this );
};


//
// Protected class methods
//

/*
  setupCurrentCycle pre-computes some priority-relevant data about the
  operations of the current scheduling region.
*/
void TC_SchedulingPriority::setupCurrentCycle( long long cycle )
{
  DSTART(
    "virtual void TC_SchedulingPriority::setupCurrentCycle(long long int)" );

  mIPOperations.clear();

  // Iterate all operations available for scheduling in the current execution
  // cycle.
  for ( auto it = mEarliestCycleMap->begin();
        it != mEarliestCycleMap->upper_bound( cycle ); ++it )
    for ( WIR_Operation &o : it->second )
      if ( TC_SchedulingInfo::getType( o ) ==
             TC_SchedulingInfo::OperationType::ip )
        mIPOperations.push_back( o );
};


/*
  computePriority computes the scheduling priority for a given TriCore WIR
  operation.
*/
std::pair<long long,
          std::list<WIR_Operation *>> TC_SchedulingPriority::computePriority( const WIR_Operation &o,
                                                                              const WIR_Operation *pred,
                                                                              long long cycle )
{
  DSTART(
    "virtual pair<long long int, list<WIR_Operation*> > TC_SchedulingPriority::computePriority(const WIR_Operation&, const WIR_Operation*, long long int)" );

  (void) cycle;

  DACTION(
    stringstream str;
    str << o;
    DOUT(
      "Computing priority for '" << str.str().substr( 8 ) <<
      "' in execution cycle " << cycle << " with predecessor " );
    if ( pred != nullptr ) {
      stringstream str1;
      str1 << *pred;
      DOUT( "'" << str1.str().substr( 8 ) << "'." << endl );
    } else
      DOUT( "<nullptr>." << endl ); );

  multimap<long long, pair<WIR_Operation *, WIR_Operation *>> bundles;

  // Compute the accumulated priority of the given operation.
  long long prio = computePriority( o, pred );
  DOUT( "Initial priority set to " << prio << "." << endl );

  if ( TC_SchedulingInfo::getType( o ) ==
         TC_SchedulingInfo::OperationType::ls ) {
    // If o is an LS operation available for scheduling in the current cycle,
    // we next search for IP operations in order to bundle them with o.

    // However, we need to check first whether o has unresolvable dependences.
    // This can happen, because an IP-LS bundle is executed in the same cycle
    // with a latency of 0.
    auto unschedPreds = mRegion->getUnscheduledPredecessors( o );

    if ( unschedPreds.size() == 1 ) {
      // LS operation o depends on exactly one IP operation. We bundle these
      // two operations and do not search any other possible bundles for o.
      prio =
        computeBundlePriority( unschedPreds.begin()->get(), o, pred );

      bundles.insert(
        { prio,
          { &(unschedPreds.begin()->get()),
            const_cast<WIR_Operation *>( &o ) } } );
    } else

    if ( unschedPreds.empty() ) {
      // LS operation o has no unresolved dependences so that we can bundle it
      // with any other available IP operation.
      for ( WIR_Operation &ip : mIPOperations ) {
        prio = computeBundlePriority( ip, o, pred );

        bundles.insert( { prio, { &ip, const_cast<WIR_Operation *>( &o ) } } );
      }

      if ( bundles.empty() )
        // There is no IP operation available such that we could bundle it with
        // o. Thus o forms a bundle on its own.
        bundles.insert(
          { prio, { nullptr, const_cast<WIR_Operation *>( &o ) } } );
    }
  } else
    // For all other operation types, o forms a bundle on its own.
    bundles.insert( { prio, { const_cast<WIR_Operation *>( &o ), nullptr } } );

  if ( bundles.empty() )
    // We didn't find anything meaningful for the current execution cycle.
    return( make_pair( WIR_SchedulingPriority::skip, list<WIR_Operation *>() ) );

  // Return the last element of bundles that has the highest priority overall.
  auto &p = *(bundles.rbegin());
  return(
    make_pair(
      p.first, list<WIR_Operation *> { p.second.first, p.second.second } ) );
};


/*
  computePriority computes the scheduling priority for the given pair of TriCore
  operations.
*/
long long TC_SchedulingPriority::computePriority( const WIR_Operation &o,
                                                  const WIR_Operation *pred ) const
{
  DSTART(
    "long long int TC_SchedulingPriority::computePriority(const WIR_Operation&, const WIR_Operation*) const" );

  // Compute the accumulated priority of the given operation.
  long long prio = getPriority( o );
  DACTION(
    stringstream str;
    str << o;
    DOUT(
      "Net priority of '" << str.str().substr( 8 ) << "' is " << prio << "." <<
      endl ); );

  // Adjust o's priority based on the type of some previously scheduled
  // operation, cf. TriCore Compiler Writer's Guide page 32.
  long long factor =
    ( pred == nullptr ) ?
      TC_SchedulingInfo::getPriority( o ) :
      TC_SchedulingInfo::getPriority( o, *pred );

  DOUT( "Adjusting net priority by factor of " << factor << "." << endl );
  prio *= factor;

  // Check whether o is a silicon bug-related NOP.
  if ( o.getOpCode() == TC13::OpCode::NOP ) {
    prio =
      ( dynamic_cast<TC_BlockSchedulingRegion *>(
          mRegion )->isSiliconBugNOP( o ) ) ? 0 : 1;
    DOUT( "Handling of NOP, priority reset to " << prio << "." << endl );
  }

  return( prio );
};


/*
  computeBundlePriority computes the scheduling priority for a given bundle of
  LS-IP TriCore operations.
*/
long long TC_SchedulingPriority::computeBundlePriority( const WIR_Operation &ip,
                                                        const WIR_Operation &ls,
                                                        const WIR_Operation *pred ) const
{
  DSTART(
    "long long int TC_SchedulingPriority::computeBundlePriority(const WIR_Operation&, const WIR_Operation&, const WIR_Operation*) const" );

  return( computePriority( ip, pred ) + computePriority( ls, &ip ) );
};

}       // namespace WIR
