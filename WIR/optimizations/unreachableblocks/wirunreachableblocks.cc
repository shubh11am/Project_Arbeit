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
  @file wirunreachableblocks.cc
  @brief This file implements an optimization removing unreachable basic blocks.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include standard headers
#include <set>

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
WIR_UnreachableBlocks::WIR_UnreachableBlocks( WIR_System &s ) :
  WIR_Optimization { s }
{
  DSTART( "WIR_UnreachableBlocks::WIR_UnreachableBlocks(WIR_System&)" );
};


/*
  Default constructor for compilation unit-level optimization.
*/
WIR_UnreachableBlocks::WIR_UnreachableBlocks( WIR_CompilationUnit &c ) :
  WIR_Optimization { c }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );
};


/*
  Default constructor for function-level optimization.
*/
WIR_UnreachableBlocks::WIR_UnreachableBlocks( WIR_Function &f ) :
  WIR_Optimization { f }
{
  DSTART( "WIR_UnreachableBlocks::WIR_UnreachableBlocks(WIR_Function&)" );
};


/*
  Destructor.
*/
WIR_UnreachableBlocks::~WIR_UnreachableBlocks( void )
{
  DSTART( "virtual WIR_UnreachableBlocks::~WIR_UnreachableBlocks()" );
};


//
// Protected class methods
//

/*
  runOptimization removes unreachable basic blocks in the given system.
*/
void WIR_UnreachableBlocks::runOptimization( WIR_System &s )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  for ( WIR_CompilationUnit &c : s )
    runOptimization( c );
};


/*
  runOptimization removes unreachable basic blocks in the given compilation
  unit.
*/
void WIR_UnreachableBlocks::runOptimization( WIR_CompilationUnit &c )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  for ( WIR_Function &f : c )
    runOptimization( f );
};


/*
  runOptimization removes unreachable basic blocks in the given function.
*/
void WIR_UnreachableBlocks::runOptimization( WIR_Function &f )
{
  DSTART(
    "virtual void WIR_UnreachableBlocks::runOptimization(WIR_Function&)" );

  // Get list of visited basic blocks and collect their IDs.
  WIR_CFG cfg { f, true };
  set<WIR_id_t> visitedBBs;
  for ( WIR_BasicBlock &b : cfg.getDFSOrder() )
    visitedBBs.insert( b.getID() );

  // Erase all basic blocks not occuring in visitedBBs.
  auto it = f.getBasicBlocks().begin();
  while ( it != f.getBasicBlocks().end() ) {
    // Check whether the current basic block is a literal pool that hence only
    // consists of ASM data directives.
    auto &b = it->get();
    bool isLiteralPool = b.getInstructions().empty() ? false : true;
    for ( WIR_Instruction &i : b ) {
      for ( WIR_Operation &o : i )
        if ( !o.isAsmDataDirective() ) {
          isLiteralPool = false;
          break;
        }

      if ( !isLiteralPool )
        break;
    }

    // Erase current basic block if it is unreachable AND not a literal pool.
    if ( !visitedBBs.count( b.getID() ) && !isLiteralPool ) {
      DOUT(
        "Removing unreachable basic block '" << it->get().getName() << "'." <<
        endl );

      // Update flow facts.
      WIR_FlowFactUpdater::eraseUnreachableBasicBlock( b );

      // Erase all bit-value containers associated with b's parameters.
      for ( WIR_Instruction &i : b )
        for ( WIR_Operation &o : i )
          for ( WIR_Parameter &p : o )
            if ( p.containsContainers( WIR_BitValues::getContainerTypeID() ) ) {
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

      it = f.eraseBasicBlock( it );
    } else
      ++it;
  }
};

}       // namespace WIR
