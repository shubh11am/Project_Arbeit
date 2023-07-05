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
  @file wirdeadcode.cc
  @brief This file implements an optimization eliminating dead code.

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
#include <list>
#include <sstream>
#include <string>

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
WIR_DeadCode::WIR_DeadCode( WIR_System &s ) :
  WIR_Optimization { s },
  WIR_BitOpt { s }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );
};


/*
  Default constructor for compilation unit-level optimization.
*/
WIR_DeadCode::WIR_DeadCode( WIR_CompilationUnit &c ) :
  WIR_Optimization { c },
  WIR_BitOpt { c }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );
};


/*
  Default constructor for function-level optimization.
*/
WIR_DeadCode::WIR_DeadCode( WIR_Function &f ) :
  WIR_Optimization { f },
  WIR_BitOpt { f }
{
  DSTART( "WIR_DeadCode::WIR_DeadCode(WIR_Function&)" );
};


/*
  Destructor.
*/
WIR_DeadCode::~WIR_DeadCode( void )
{
  DSTART( "virtual WIR_DeadCode::~WIR_DeadCode()" );
};


//
// Protected class methods
//

/*
  runOptimization eliminates dead code in the given system.
*/
void WIR_DeadCode::runOptimization( WIR_System &s )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  for ( WIR_CompilationUnit &c : s )
    if ( !c.getDontOptimize() )
      runOptimization( c );
};


/*
  runOptimization eliminates dead code in the given compilation unit.
*/
void WIR_DeadCode::runOptimization( WIR_CompilationUnit &c )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  for ( WIR_Function &f : c )
    if ( !f.getDontOptimize() )
      runOptimization( f );
};


/*
  runOptimization eliminates dead code in the given function.
*/
void WIR_DeadCode::runOptimization( WIR_Function &f )
{
  DSTART( "virtual void WIR_DeadCode::runOptimization(WIR_Function&)" );

  // Perform bit-true data and value flow analysis first.
  if ( mRunDFA && ( mBitDFA != nullptr ) )
    mBitDFA->analyze();

  DOUT( "Processing function '" << f.getName() << "'." << endl );
  verifyLocations( f );

  // Eliminate def-use relationships of all those DFG edges where the up-values
  // only contain 'X*'.
  eliminateDefUses( f );

  // Eliminate all dead operations whose produced results are not used anywhere.
  eliminateDeadOperations( f );

  verifyLocations( f );
};


/*
  isDead determines whether a WIR operation is dead or not.

  isDead is a virtual method in order to allow overloading in derived
  processor-specific classes for particular dead code checks, if required.
*/
bool WIR_DeadCode::isDead( const WIR_Operation &o ) const
{
  DSTART( "virtual bool WIR_DeadCode::isDead(const WIR_Operation&) const" );

  // Memory writes and other operations with side effects are never dead.
  if ( o.isMemoryAccess() || o.isMemoryStore() ||
       o.isCall() || o.isIndirectCall() || o.isReturn() || o.isJump() ||
       o.hasSideEffects() ) {
    DACTION(
      stringstream sstr;
      sstr << wir << o;
      DOUT(
        "Case 1: '" << sstr.str().substr( 8 ) << "' (ID " << o.getID() <<
        ") is not dead." << endl; ); );

    return( false );
  }

  // Iterate all explicit register parameters of o.
  for ( WIR_Parameter &p : o.getExplicitParameters() )
    if ( p.getType() == WIR_ParameterType::reg ) {
      auto &src = dynamic_cast<WIR_RegisterParameter &>( p );

      // Check whether src is defined or def-used, i.e., whether src produces
      // some values that could be dead.
      if ( src.isDefined() || src.isDefUsed() ) {
        // Get the source node's bitValue container.
        auto &srcContainer =
          src.getContainers<WIR_BitValues>().begin()->get();
        auto outEdges = srcContainer.getOutValues();

        if ( !outEdges.empty() )
          for ( auto &e : outEdges ) {
            auto &tgtOp = e.rp->getOperation();

            if ( tgtOp != o ) {
              // A value produced by src is actually still used by another
              // operation other than o. o thus is not dead.
              DACTION(
                stringstream sstr;
                sstr << wir << o;
                DOUT(
                  "Case 2: '" << sstr.str().substr( 8 ) <<
                  "' (ID " << o.getID() << ") is not dead." << endl; ); );

              return( false );
            }
          }
      }
    }

  DACTION(
    stringstream sstr;
    sstr << wir << o;
    DOUT(
      "'" << sstr.str().substr( 8 ) << "' (ID " << o.getID() << ") is dead." <<
      endl; ); );

  return( true );
};


//
// Private class methods
//

/*
  eliminateDefUses eliminates def-use relationships of all those DFG edges where
  the up-values only contain 'X*'.

  For edges containing 'X*' only, the contents of the data flowing along that
  edge is completely irrelevant. De facto, no data actually flows along such an
  edge.

  After bit-true data flow analysis, the def-use relationships along DFG edges
  are modeled using bitValue containers attached to register parameters. Thus,
  in order to eliminate a dead def-use relationship, the contents of the
  involved bitValue containers is modified by eliminateDefUses.
*/
void WIR_DeadCode::eliminateDefUses( WIR_Function &f ) const
{
  DSTART( "void WIR_DeadCode::eliminateDefUses(WIR_Function&) const" );

  for ( WIR_BasicBlock &b : f )
    for ( WIR_Instruction &i : b )
      for ( WIR_Operation &o : i ) {
        DACTION(
          stringstream sstr;
          sstr << wir << o;
          DOUT(
            "Checking '" << sstr.str().substr( 8 ) << "' (ID " << o.getID() <<
            ")." << endl; ); );

        for ( WIR_Parameter &p : o.getExplicitParameters() )
          if ( p.getType() == WIR_ParameterType::reg ) {
            auto &tgt = dynamic_cast<WIR_RegisterParameter &>( p );

            // Check whether tgt is used or def-used, i.e., whether tgt has
            // incoming edges in the DFG that could be 'X*'.
            if ( tgt.isUsed() || tgt.isDefUsed() ) {
              // There are incoming edges, tgt thus is target node of such
              // edges. Get this target node's bitValue container.
              auto &tgtContainer =
                tgt.getContainers<WIR_BitValues>().begin()->get();

              // Check whether all incoming edges of the target node are X*.
              bool allIncomingEdgesX = true;

              for ( auto &e : tgtContainer.getInValues() ) {
                auto uVal = e.upVal;

                if ( tgt.getRegister().getBitWidth() < uVal.getBitWidth() )
                  uVal =
                    uVal.extract(
                      WIR_BitDFA::getHierarchicalRegisterOffset(
                        dynamic_cast<const WIR_RegisterParameter &>( *(e.rp) ),
                        tgt ),
                      tgt.getRegister().getBitWidth() );

                if ( !uVal.containsOnlyBit( WIR_L4::bX ) ) {
                  allIncomingEdgesX = false;
                  break;
                }
              }

              // All incoming edges are X so that data flowing into the target
              // node is de facto don't care.
              if ( allIncomingEdgesX ) {
                // Remove target node from all adjacent nodes.
                for ( auto &e : tgtContainer.getInValues() ) {
                  // Obtain source node of DFG edge.
                  auto &src = *(e.rp);

                  // Get the source node's bitValue container.
                  auto &srcContainer =
                    src.getContainers<WIR_BitValues>().begin()->get();

                  // Erase the edge src -> tgt.
                  srcContainer.eraseOutValues( tgt );

                  DOUT(
                    "  Eliminating incoming edge '" << src << "' -> '" << tgt <<
                    "'." << endl );
                }

                // Clear the target node's incoming edges.
                tgtContainer.clearInValues();
              }
            }
          }
      }
};


/*
  eliminateDeadOperations eliminates dead operations from a WIR function.
*/
void WIR_DeadCode::eliminateDeadOperations( WIR_Function &f ) const
{
  DSTART( "void WIR_DeadCode::eliminateDeadOperations(WIR_Function&) const" );

  bool functionModified;

  do {
    functionModified = false;

    for ( auto bIt = f.rbegin(); bIt != f.rend(); ++bIt ) {
      WIR_BasicBlock &b = bIt->get();

      if ( b.getDontOptimize() )
        continue;

      list<WIR_Instruction *> instrs;

      // Check all instructions of b.
      auto iIt = b.rbegin();

      while ( iIt != b.rend() ) {
        WIR_Instruction &i = iIt->get();

        if ( i.getDontOptimize() ) {
          ++iIt;
          continue;
        }

        // Check all operations of i.
        auto oIt = i.begin();

        while ( oIt != i.end() ) {
          WIR_Operation &o = oIt->get();

          bool dontOptimizeParams = false;
          for ( WIR_Parameter &p : o )
            if ( p.getDontOptimize() ) {
              dontOptimizeParams = true;
              break;
            }

          // Remove a dead operation from i if possible.
          if ( o.getDontOptimize() || dontOptimizeParams || !isDead( o ) )
            ++oIt;
          else {
            DACTION(
              stringstream sstr;
              sstr << wir << o;
              DOUT(
                "Eliminating dead operation '" << sstr.str().substr( 8 ) <<
                "' (ID " << o.getID() << ")." << endl; ); );

            // Remove all incoming edges of o.
            for ( WIR_Parameter &p : o )
              if ( p.getType() == WIR_ParameterType::reg ) {
                auto &tgt = dynamic_cast<WIR_RegisterParameter &>( p );

                // Get the target node's bitValue container.
                auto &tgtContainer =
                  tgt.getContainers<WIR_BitValues>().begin()->get();

                // Iterate incoming edges.
                for ( auto &e : tgtContainer.getInValues() ) {
                  // Obtain source node of current DFG edge.
                  auto &src = *(e.rp);

                  // Get the source node's bitValue container.
                  auto &srcContainer =
                    src.getContainers<WIR_BitValues>().begin()->get();

                  // Erase the edge src -> tgt.
                  if ( srcContainer.getID() != tgtContainer.getID() ) {
                    srcContainer.eraseOutValues( tgt );

                    DOUT(
                      "  Eliminating incoming edge '" << src << "' -> '" <<
                      tgt << "'." << endl );
                  }
                }
              }

            oIt = i.eraseOperation( oIt );
            functionModified = true;
          }
        }

        if ( i.getOperations().empty() )
          instrs.push_back( &i );

        ++iIt;
      }

      // Erase all identified empty instructions.
      while ( !instrs.empty() ) {
        WIR_Instruction &i = *(instrs.front());
        instrs.pop_front();

        DOUT(
          "Eliminating empty instruction '" << i << "' (ID " << i.getID() <<
          ")." << endl );

        b.eraseInstruction( b.findInstruction( i ) );
      }
    }
  } while ( functionModified );
};

}       // namespace WIR
