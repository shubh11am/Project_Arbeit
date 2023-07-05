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
  @file wirlocalscheduling.cc
  @brief This file implements an optimization performing instruction scheduling
         locally within basic blocks.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include standard headers
#include <algorithm>
#include <deque>
#include <list>
#include <sstream>

// Include boost headers
#include <boost/current_function.hpp>

// Include libuseful headers
#include <libuseful/debugmacros.h>

// Include WIR headers
#include <wir/wir.h>

// Include local headers
#include "wirblockschedulingregion.h"
#include "wirlocalscheduling.h"
#include "wirschedulingpriority.h"
#include "wirschedulingregion.h"


//
// Code section
//

namespace WIR {


using namespace std;


//
// Public class methods
//

/*
  Default constructor for system-level optimization.
*/
WIR_LocalScheduling::WIR_LocalScheduling( WIR_System &s,
                                          const WIR_SchedulingPriority &p,
                                          bool verbosity, bool keepTmpFiles ) :
  WIR_Optimization { s },
  mPriority { p.clone() },
  mReadyList { nullptr },
  mASAP { true },
  mVerbosity { verbosity },
  mKeepTmpFiles { keepTmpFiles }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );
};


/*
  Default constructor for compilation unit-level optimization.
*/
WIR_LocalScheduling::WIR_LocalScheduling( WIR_CompilationUnit &c,
                                          const WIR_SchedulingPriority &p,
                                          bool verbosity, bool keepTmpFiles ) :
  WIR_Optimization { c },
  mPriority { p.clone() },
  mReadyList { nullptr },
  mASAP { true },
  mVerbosity { verbosity },
  mKeepTmpFiles { keepTmpFiles }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );
};


/*
  Default constructor for function-level optimization.
*/
WIR_LocalScheduling::WIR_LocalScheduling( WIR_Function &f,
                                          const WIR_SchedulingPriority &p,
                                          bool verbosity, bool keepTmpFiles ) :
  WIR_Optimization { f },
  mPriority { p.clone() },
  mReadyList { nullptr },
  mASAP { true },
  mVerbosity { verbosity },
  mKeepTmpFiles { keepTmpFiles }
{
  DSTART(
    "WIR_LocalScheduling::WIR_LocalScheduling(WIR_Function&, const WIR_SchedulingPriority&, bool, bool)" );
};


/*
  Destructor.
*/
WIR_LocalScheduling::~WIR_LocalScheduling( void )
{
  DSTART( "virtual WIR_LocalScheduling::~WIR_LocalScheduling()" );
};


/*
  setASAP configures the scheduler to use as-soon-as-possible (ASAP) scheduling.
*/
void WIR_LocalScheduling::setASAP( void )
{
  DSTART( "void WIR_LocalScheduling::setASAP()" );

  mASAP = true;
};


/*
  setALAP configures the scheduler to use as-late-as-possible (ALAP) scheduling.
*/
void WIR_LocalScheduling::setALAP( void )
{
  DSTART( "void WIR_LocalScheduling::setALAP()" );

  mASAP = false;
};


//
// Protected class methods
//

/*
  runOptimization schedules instructions in the given system.
*/
void WIR_LocalScheduling::runOptimization( WIR_System &s )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  for ( WIR_CompilationUnit &c : s )
    if ( !c.getDontOptimize() )
      runOptimization( c );
};


/*
  runOptimization schedules instructions in the given compilation unit.
*/
void WIR_LocalScheduling::runOptimization( WIR_CompilationUnit &c )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  for ( WIR_Function &f : c )
    if ( !f.getDontOptimize() )
      runOptimization( f );
};


/*
  runOptimization schedules instructions in the given function.
*/
void WIR_LocalScheduling::runOptimization( WIR_Function &f )
{
  DSTART( "virtual void WIR_LocalScheduling::runOptimization(WIR_Function&)" );

  DOUT( "Processing function '" << f.getName() << "'." << endl );

  // Initialize data structures.
  init( f );

  // Do list scheduling.
  for ( auto &ptr : mRegions )
    schedule( *(ptr.get()) );
};


/*
  init initializes internal data structures before the actual instruction
  scheduling.
*/
void WIR_LocalScheduling::init( WIR_Function &f )
{
  DSTART( "virtual void WIR_LocalScheduling::init(WIR_Function&)" );

  // A small lambda to sort regions by their total operation counts in ascending
  // order.
  auto sortCriterion = [&]( const WIR_SchedulingRegion *r1,
                            const WIR_SchedulingRegion *r2 ) {
    return( r1->getOperationCount() < r2->getOperationCount() );
  };

  // Setup basic block-level scheduling regions.
  deque<WIR_BlockSchedulingRegion *> regions;
  for ( WIR_BasicBlock &b : f )
    if ( !b.getDontOptimize() )
      regions.push_back( generateBlockRegion( b ) );

  // Sort list of regions by their total instruction counts in ascending order.
  sort( regions.begin(), regions.end(), sortCriterion );

  for ( auto *r : regions )
    mRegions.push_back( unique_ptr<WIR_BlockSchedulingRegion>( r ) );
};


/*
  schedule performs list scheduling for a given scheduling region.
*/
void WIR_LocalScheduling::schedule( WIR_BlockSchedulingRegion &r )
{
  DSTART( "void WIR_LocalScheduling::schedule(WIR_BlockSchedulingRegion&)" );

  DDECLARE( list<list<WIR_id_t>> instrIDs; );
  DACTION(
    DOUT(
      string( 80, '=' ) << endl <<
      "Scheduling region with root basic block '" <<
      r.getBasicBlocks().front()->getName() << "':" << endl );
    for ( auto *b : r.getBasicBlocks() ) {
      DOUT( *b );
      for ( WIR_Instruction &i : *b ) {
        instrIDs.push_back( {} );
        for ( WIR_Operation &o : i )
          instrIDs.back().push_back( o.getID() );
      }
    } );

  // Set up the scheduling priority.
  mPriority->setRegion( r, mASAP );

  // Process the ready list.
  if ( mASAP )
    mReadyList = &r.getEarliestCycleMap();
  else
    mReadyList = &r.getLatestCycleMap();

  long long currentCycle = 1;
  WIR_Instruction *pred = nullptr;

  while ( !mReadyList->empty() ) {
    // Determine the bundle of best operations to be scheduled for the current
    // execution cycle.
    auto bestBundle = mPriority->getBestOperations( currentCycle, pred );

    if ( bestBundle.empty() )
      // The ready list is not yet empty, but no schedulable operations were
      // found. Thus, increase the cycle count and continue.
      ++currentCycle;
    else {
      // Determine the last operation among the current best bundle.
      WIR_Operation *lastOp = nullptr;
      auto it = bestBundle.rbegin();
      while ( lastOp == nullptr ) {
        lastOp = *it;
        ++it;
      }

      // Get the last operation's earliest execution cycle.
      long long lastOpCycle = mReadyList->at( lastOp->getID() );

      ufAssert( currentCycle >= lastOpCycle );

      // Move the operations of the best bundle within their scheduling region.
      pred = r.moveOperations( bestBundle, pred );
      r.updateMovedOperations( bestBundle );

      stringstream str;
      str << currentCycle;
      pred->insertContainer(
        WIR_Comment(
          "Local instruction scheduling, execution cycle no. " + str.str() +
          ":" ) );

      // Update the ready list by refreshing the current region's internal
      // earliest and latest execution cycle maps.
      currentCycle += r.updateCycleMaps( bestBundle, currentCycle );
    }
  }

  removeEmptyInstructions( r );

  // Do some final processor-specific post-processing.
  r.postProcessingHook();

  DACTION(
    bool regionChanged = false;

    list<list<WIR_id_t>> newInstrIDs;
    for ( auto *b : r.getBasicBlocks() ) {
      for ( WIR_Instruction &i : *b ) {
        newInstrIDs.push_back( {} );
        for ( WIR_Operation &o : i )
          newInstrIDs.back().push_back( o.getID() );
      }
    }

    if ( newInstrIDs.size() != instrIDs.size() )
      regionChanged = true;
    else {
      auto iIt = instrIDs.begin();
      auto iIt1 = newInstrIDs.begin();

      for ( ; iIt != instrIDs.end(); ++iIt, ++iIt1 ) {
        auto &opIDs = *iIt;
        auto &opIDs1 = *iIt1;

        if ( opIDs.size() != opIDs1.size() ) {
          regionChanged = true;
          break;
        }

        auto oIt = opIDs.begin();
        auto oIt1 = opIDs1.begin();

        for ( ; oIt != opIDs.end(); ++oIt, ++oIt1 )
          if ( *oIt != *oIt1 ) {
            regionChanged = true;
            break;
          }

        if ( regionChanged )
          break;
      }
    }

    if ( regionChanged ) {
      DOUT(
        "Finally scheduled region with root basic block '" <<
        r.getBasicBlocks().front()->getName() << "':" << endl );
      for ( auto *b : r.getBasicBlocks() )
        DOUT( *b );
    }
    DOUT( string( 80, '=' ) << endl << endl ); );
};


/*
  removeEmptyInstructions removes all instructions from a given scheduling
  region that do not contain any operation.
*/
void WIR_LocalScheduling::removeEmptyInstructions( WIR_BlockSchedulingRegion &r )
{
  DSTART(
    "void WIR_LocalScheduling::removeEmptyInstructions(WIR_BlockSchedulingRegion&)" );

  for ( auto *b : r.getBasicBlocks() )
    if ( b != nullptr ) {
      for ( auto it = b->begin(); it != b->end(); )
        if ( it->get().getOperations().empty() )
          it = b->eraseInstruction( it );
        else
          ++it;
    }
};

}       // namespace WIR
