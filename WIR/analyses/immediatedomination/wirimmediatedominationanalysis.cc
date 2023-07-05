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
  @file wirimmediatedominationanalysis.cc
  @brief This file implements the %WIR immediate domination analysis.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include standard headers
#include <list>
#include <map>

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
  Default constructor for function-level analysis.
*/
WIR_ImmediateDominationAnalysis::WIR_ImmediateDominationAnalysis( WIR_Function &f ) :
  WIR_Analysis { f },
  mCFG { f, true }
{
  DSTART(
    "WIR_ImmediateDominationAnalysis::WIR_ImmediateDominationAnalysis(WIR_Function&)" );
};


/*
  Destructor.
*/
WIR_ImmediateDominationAnalysis::~WIR_ImmediateDominationAnalysis( void )
{
  DSTART(
    "virtual WIR_ImmediateDominationAnalysis::~WIR_ImmediateDominationAnalysis()" );
};


//
// Protected class methods
//

/*
  runAnalysis performs immediate domination analysis of the given function.
*/
void WIR_ImmediateDominationAnalysis::runAnalysis( WIR_Function &f )
{
  DSTART(
    "virtual void WIR_ImmediateDominationAnalysis::runAnalysis(WIR_Function&)" );

  DOUT( "Processing function '" << f.getName() << "'." << endl );

  // Initialize data structures.
  init( f );

  // Get depth-first order of all reachable basic blocks.
  auto &dfsBBs = mCFG.getDFSOrder();

  // Prepare domination sets.
  map<WIR_id_t, WIR_BasicBlockSet> dominatorsOf;

  for ( WIR_BasicBlock &b : dfsBBs ) {
    auto &dom = b.getContainers<WIR_Domination>().begin()->get();
    dominatorsOf[ b.getID() ] = dom.getDominatorBlocks();
    dominatorsOf[ b.getID() ].erase( b );

    DACTION(
      DOUT( "dominatorsOf[ '" << b.getName() << "' ] = {" );
      for ( WIR_BasicBlock &d : dominatorsOf.at( b.getID() ) ) {
        if ( d != *(dominatorsOf.at( b.getID() ).begin()) )
          DOUT( "," );
        DOUT( " " << d.getName() );
      }
      DOUT( " }" << endl ); );
  }

  // Compute immediate domination (S. S. Muchnick, algorithm 7.15, page 184).
  for ( WIR_BasicBlock &b : dfsBBs ) {
    WIR_ImmediateDomination &res =
      b.getContainers<WIR_ImmediateDomination>().begin()->get();

    if ( !mCFG.getStartNodes().count( b ) ) {
      for ( WIR_BasicBlock &s : dominatorsOf[ b.getID() ] ) {
        list<WIR_BasicBlock *> blocksToErase;

        for ( WIR_BasicBlock &t : dominatorsOf[ b.getID() ] )
          if ( t != s ) {
            auto &dom = s.getContainers<WIR_Domination>().begin()->get();
            if ( dom.getDominatorBlocks().count( t ) )
              blocksToErase.push_back( &t );
          }

        while ( !blocksToErase.empty() ) {
          DOUT(
            "Erasing '" << blocksToErase.front()->getName() <<
            "' from dominatorsOf[ " << b.getName() << " ]." << endl );
          dominatorsOf[ b.getID() ].erase( *(blocksToErase.front()) );
          blocksToErase.pop_front();
        }
      }

      #ifdef FAILSAFEMODE
      ufAssert( dominatorsOf[ b.getID() ].size() == 1 );
      #endif

      // Store immediate dominator in container.
      res.insertImmediateDominator( dominatorsOf[ b.getID() ].begin()->get() );
    } else
      // Store CFG start node in container.
      res.insertImmediateDominator( b );
  }

  // Free some no longer needed memory.
  for ( WIR_BasicBlock &b : dfsBBs )
    b.eraseContainers( WIR_Domination::getContainerTypeID() );
};


//
// Private class methods
//

/*
  init initializes data structures by doing a domination analysis and attaching
  fresh containers.
*/
void WIR_ImmediateDominationAnalysis::init( WIR_Function &f )
{
  DSTART( "void WIR_ImmediateDominationAnalysis::init(WIR_Function&)" );

  // Do domination analysis.
  WIR_DominationAnalysis domination( f );
  domination.analyze();

  // Clear previous analysis results.
  for ( WIR_BasicBlock &b : f )
    b.eraseContainers( WIR_ImmediateDomination::getContainerTypeID() );

  // Attach fresh containers to all reachable basic blocks.
  auto &dfsBBs = mCFG.getDFSOrder();
  for ( WIR_BasicBlock &b : dfsBBs )
    b.insertContainer( new WIR_ImmediateDomination() );
};

}       // namespace WIR
