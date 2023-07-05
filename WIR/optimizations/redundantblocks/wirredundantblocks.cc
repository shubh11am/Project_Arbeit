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
  @file wirredundantblocks.cc
  @brief This file implements an optimization merging succeeding redundant basic
         blocks.

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
  Default constructor for system-level optimization.
*/
WIR_RedundantBlocks::WIR_RedundantBlocks( WIR_System &s ) :
  WIR_Optimization { s }
{
  DSTART( "WIR_RedundantBlocks::WIR_RedundantBlocks(WIR_System&)" );
};


/*
  Default constructor for compilation unit-level optimization.
*/
WIR_RedundantBlocks::WIR_RedundantBlocks( WIR_CompilationUnit &c ) :
  WIR_Optimization { c }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );
};


/*
  Default constructor for function-level optimization.
*/
WIR_RedundantBlocks::WIR_RedundantBlocks( WIR_Function &f ) :
  WIR_Optimization { f }
{
  DSTART( "WIR_RedundantBlocks::WIR_RedundantBlocks(WIR_Function&)" );
};


/*
  Destructor.
*/
WIR_RedundantBlocks::~WIR_RedundantBlocks( void )
{
  DSTART( "virtual WIR_RedundantBlocks::~WIR_RedundantBlocks()" );
};


//
// Protected class methods
//

/*
  runOptimization merges redundant basic blocks in the given system.
*/
void WIR_RedundantBlocks::runOptimization( WIR_System &s )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  for ( WIR_CompilationUnit &c : s )
    runOptimization( c );
};


/*
  runOptimization merges redundant basic blocks in the given compilation unit.
*/
void WIR_RedundantBlocks::runOptimization( WIR_CompilationUnit &c )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  for ( WIR_Function &f : c )
    runOptimization( f );
};


/*
  runOptimization merges redundant basic blocks in the given function.
*/
void WIR_RedundantBlocks::runOptimization( WIR_Function &f )
{
  DSTART( "virtual void WIR_RedundantBlocks::runOptimization(WIR_Function&)" );

  // Traverse all basic blocks of f.
  for ( auto it = f.begin(); it != f.end(); ++it ) {
    WIR_BasicBlock &b1 = it->get();

    // Check if b1 ends with a call. If so, we skip b1.
    bool b1HasCall = false;
    if ( !b1.getInstructions().empty() ) {
      WIR_Instruction &i = b1.rbegin()->get();
      for ( WIR_Operation &o : i )
        if ( o.isCall() || o.isIndirectCall() ) {
          b1HasCall = true;
          break;
        }
    }
    if ( b1HasCall )
      continue;

    bool redundantSuccessorRemoved = false;

    do {
      // Determine successor block, if any.
      redundantSuccessorRemoved = false;
      auto it2 = std::next( it );

      if ( it2 != f.getBasicBlocks().end() ) {
        WIR_BasicBlock &b2 = it2->get();

        auto b1Succs = b1.getSuccessors();
        auto b2Preds = b2.getPredecessors();

        // b1 and b2 are redundant if
        // - b1 has exactly one successor, namely b2, AND
        // - b2 has exactly one predecessor, namely b1, AND
        // - b2 is the immediate successor of b1 within the current WIR function.
        if ( ( b1Succs.size() == 1 ) && ( b1Succs.begin()->get() == b2 ) &&
             ( b2Preds.size() == 1 ) && ( b2Preds.begin()->get() == b1 ) ) {
          // Found two redundant basic blocks b1 and b2 to be merged.
          DOUT(
            "Found redundant basic blocks '" + b1.getName() + "' and '" +
            b2.getName() + "'." << endl );

          // Remove potential jump operations from b1's tail.
          if ( !b1.getInstructions().empty() ) {
            WIR_Instruction &i = b1.rbegin()->get();
            for ( auto oit = i.begin(); oit != i.end(); ) {
              WIR_Operation &o = oit->get();
              if ( o.isJump() ) {
                // Erase all bit-value containers associated with o's parameters.
                for ( WIR_Parameter &p : o )
                  if ( p.containsContainers(
                        WIR_BitValues::getContainerTypeID() ) ) {
                    // Get the current parameter's bitValue container.
                    auto &cont = p.getContainers<WIR_BitValues>().begin()->get();

                    // Iterate all incoming edges.
                    for ( auto &inEdge : cont.getInValues() ) {
                      auto &srcContainer =
                        inEdge.rp->getContainers<WIR_BitValues>().begin()->get();
                      srcContainer.eraseOutValues( p );
                    }
                    cont.clearInValues();

                    // Iterate all outgoing edges.
                    for ( auto &outEdge : cont.getOutValues() ) {
                      auto &tgtContainer =
                        outEdge.rp->getContainers<WIR_BitValues>().begin()->get();
                      tgtContainer.eraseInValues( p );
                    }
                    cont.clearOutValues();
                  }

                DOUT( "Removing jump operation " << o << endl );
                oit = i.eraseOperation( oit );
              } else
                ++oit;
            }

            if ( i.getOperations().empty() ) {
              DOUT(
                "Removing empty last instruction of basic block '" +
                b1.getName() + "'." << endl );
              b1.eraseInstruction( --(b1.end()) );
            }
          }

          // Move instructions from b2 to b1.
          bool b2HasCall = false;
          while ( !b2.getInstructions().empty() ) {
            for ( WIR_Operation &o : b2.getInstructions().front().get() )
              if ( o.isCall() || o.isIndirectCall() )
                b2HasCall = true;

            b2.getInstructions().front().get().setDontOptimize( false );
            b1.moveInstruction( b2.getInstructions().front() );
          }

          // Update flow facts.

          // Because of their redundancy, both basic blocks have the very same
          // execution frequency. All occurences of b2 in flow restrictions can
          // thus be replaced by b1.
          WIR_FlowFactUpdater::replaceBasicBlock( b2, b1 );

          // There should not be any loop bounds attached to b2 (it cannot an
          // entry of a loop), but just in case, we erase them now.
          WIR_FlowFactUpdater::eraseLoopBound( b2 );

          // Move containers from b2 to b1.
          for ( WIR_BaseContainer &c : b2.getContainers() )
            b1.insertContainer( c );
          b2.clearContainers();

          // Finally, remove now redundant basic block b2.
          DOUT(
            "Removing redundant basic block '" + b2.getName() + "'." << endl );
          f.eraseBasicBlock( it2 );
          redundantSuccessorRemoved = !b2HasCall;
        }
      }
    } while ( redundantSuccessorRemoved );
  }
};

}       // namespace WIR
