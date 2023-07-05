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
  @file wirduudchainanalysis.cc
  @brief This file implements the %WIR def-use/use-def chain analysis.

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
  Default constructor for function-level analysis.
*/
WIR_DUUDChainAnalysis::WIR_DUUDChainAnalysis( WIR_Function &f ) :
  WIR_Analysis { f },
  mVirtualRegistersOnly { false }
{
  DSTART( "WIR_DUUDChainAnalysis::WIR_DUUDChainAnalysis(WIR_Function&)" );
};


/*
  Destructor.
*/
WIR_DUUDChainAnalysis::~WIR_DUUDChainAnalysis( void )
{
  DSTART( "virtual WIR_DUUDChainAnalysis::~WIR_DUUDChainAnalysis()" );
};


/*
  setVirtualRegistersOnly sets whether the analysis should consider only virtual
  registers or whether both virtual and physical registers are analyzed.
*/
void WIR_DUUDChainAnalysis::setVirtualRegistersOnly( bool b )
{
  DSTART( "void WIR_DUUDChainAnalysis::setVirtualRegistersOnly(bool)" );

  mVirtualRegistersOnly = b;
};


/*
  getVirtualRegistersOnly returns whether only virtual or both virtual and
  physical registers are analyzed.
*/
bool WIR_DUUDChainAnalysis::getVirtualRegistersOnly( void ) const
{
  DSTART( "bool WIR_DUUDChainAnalysis::getVirtualRegistersOnly() const" );

  return( mVirtualRegistersOnly );
};


//
// Protected class methods
//

/*
  runAnalysis performs def-use/use-def chain analysis of the given function.
*/
void WIR_DUUDChainAnalysis::runAnalysis( WIR_Function &f )
{
  DSTART( "virtual void WIR_DUUDChainAnalysis::runAnalysis(WIR_Function&)" );

  // Initialize parameter-level data structures.
  init( f );

  // Build reverse-map of the function's precolors, i.e., a map mapping a
  // physical register 'pr' to the set of virtual registers that are all
  // precolored with 'pr'.
  map<WIR_id_t, WIR_VirtualRegisterSet> precolRMap;

  for ( WIR_VirtualRegister &vr : f.getVirtualRegisters() )
    for ( WIR_VirtualRegister &leaf : vr.getLeafs() )
      if ( leaf.isPrecolored() ) {
        WIR_PhysicalRegister &pr = leaf.getPrecolor();

        auto &vregSet = precolRMap[ pr.getID() ];
        vregSet.insert( ref( leaf ) );
      }

  // Determine def-use and use-def chains.
  for ( WIR_BasicBlock &b : f )
    for ( WIR_Instruction &i : b ) {
      for ( WIR_Operation &o : i )
        for ( WIR_Parameter &p : o )
          if ( p.getType() == WIR_ParameterType::reg ) {
            WIR_RegisterParameter &rp =
              dynamic_cast<WIR_RegisterParameter &>( p );

            if ( rp.isUsed() || rp.isDefUsed() ) {
              WIR_BaseRegister &r = rp.getRegister();

              // Determine the IDs of all leaf registers of r.
              set<WIR_id_t> usedLeafs;
              if ( r.isVirtual() )
                for ( WIR_VirtualRegister &leaf :
                        dynamic_cast<WIR_VirtualRegister &>( r ).getLeafs() )
                  usedLeafs.insert( leaf.getID() );
              else
                for ( WIR_PhysicalRegister &leaf :
                        dynamic_cast<WIR_PhysicalRegister &>( r ).getLeafs() )
                  usedLeafs.insert( leaf.getID() );

              // Get definitions reaching the current instruction i.
              auto &reachingDefinitions =
                i.getContainers<WIR_ReachingDefinitions>().begin()->get()
                 .getReachingDefinitions();

              DACTION(
                DOUT(
                  endl << "Analyzing USE of " << r.getName() << " in" << endl );
                unsigned int bbPos = 1;
                for ( auto it = b.getInstructions().begin();
                      (*it).get() != i; ++it, ++bbPos ) ;
                DOUT( "  (" << b.getName() << "/" << bbPos << ")" );
                DOUT( wir << implicitparams << defuse << i ); );

              // Iterate over all definition parameters reaching the current
              // use.
              for ( WIR_RegisterParameter &def : reachingDefinitions ) {
                WIR_BaseRegister &r = def.getRegister();

                DACTION(
                  DOUT( "  Checking reaching DEF from" << endl );
                  unsigned int bbPos = 1;
                  WIR_Instruction &i1 = def.getOperation().getInstruction();
                  for ( auto it = i1.getBasicBlock().getInstructions().begin();
                        (*it).get() != i1; ++it, ++bbPos ) ;
                  DOUT(
                    "    (" << i1.getBasicBlock().getName() << "/" << bbPos <<
                    ")" << wir << implicitparams << defuse << i1 ); );

                // Check all leaf registers involved in the current reaching
                // definition p.
                bool duudChainFound = false;

                if ( r.isVirtual() )
                  for ( WIR_VirtualRegister &leaf :
                          dynamic_cast<WIR_VirtualRegister &>( r ).getLeafs() ) {
                    if ( usedLeafs.count( leaf.getID() ) ) {
                      duudChainFound = true;
                      break;
                    }

                    // Also consider precolorings if desired.
                    if ( !mVirtualRegistersOnly && leaf.isPrecolored() &&
                         usedLeafs.count( leaf.getPrecolor().getID() ) ) {
                      duudChainFound = true;
                      break;
                    }
                  }
                else

                if ( !mVirtualRegistersOnly )
                  for ( WIR_PhysicalRegister &leaf :
                          dynamic_cast<WIR_PhysicalRegister &>( r ).getLeafs() ) {
                    if ( usedLeafs.count( leaf.getID() ) ) {
                      duudChainFound = true;
                      break;
                    }

                    // Also consider precolorings.
                    auto it = precolRMap.find( leaf.getID() );
                    if ( it != precolRMap.end() )
                      for ( WIR_VirtualRegister &vr : it->second )
                        if ( usedLeafs.count( vr.getID() ) ) {
                          duudChainFound = true;
                          break;
                        }
                  }

                // If the definition p also occurs in usedLeafs, the def and the
                // use intersect, we have found a def-use/use-def chain.
                if ( duudChainFound ) {
                  DOUT( "    Added DEF to du- and ud-chains." << endl );

                  // Store def-use and use-def chain in containers.
                  WIR_DUUDChain &defContainer =
                    def.getContainers<WIR_DUUDChain>().begin()->get();
                  defContainer.insertDUChain( rp );

                  WIR_DUUDChain &rpContainer =
                    rp.getContainers<WIR_DUUDChain>().begin()->get();
                  rpContainer.insertUDChain( def );
                }
              }

              // Get external function inputs reaching the current instruction i.
              auto &reachingInputs =
                i.getContainers<WIR_ReachingDefinitions>().begin()->get()
                 .getReachingInputs();

              // Iterate over all external inputs reaching the current use.
              for ( WIR_PhysicalRegister &pReg : reachingInputs ) {
                DOUT(
                  "  Checking reaching External Input " << pReg.getName() <<
                  endl );

                // Check all leaf registers involved in the current external
                // input.
                bool udChainFound = false;

                for ( WIR_PhysicalRegister &leaf : pReg.getLeafs() ) {
                  if ( usedLeafs.count( leaf.getID() ) ) {
                    udChainFound = true;
                    break;
                  }

                  // Also consider precolorings.
                  auto it = precolRMap.find( leaf.getID() );
                  if ( it != precolRMap.end() )
                    for ( WIR_VirtualRegister &vr : it->second )
                      if ( usedLeafs.count( vr.getID() ) ) {
                        udChainFound = true;
                        break;
                      }
                }

                // If the external input also occurs in usedLeafs, it intersects
                // with the use, we have found a use-def chain.
                if ( udChainFound ) {
                  DOUT( "    Added External Input to du-chains." << endl );

                  // Store use-def chain in container.
                  WIR_DUUDChain &rpContainer =
                    rp.getContainers<WIR_DUUDChain>().begin()->get();
                  rpContainer.insertUDInput( pReg );
                }
              }
            }
          }

      // Free some no longer needed memory.
      i.eraseContainers( WIR_ReachingDefinitions::getContainerTypeID() );
    }
};


//
// Private class methods
//

/*
  init initializes data structures by attaching fresh containers and doing a
  reaching-definitions analysis.
*/
void WIR_DUUDChainAnalysis::init( WIR_Function &f )
{
  DSTART( "void WIR_DUUDChainAnalysis::init(WIR_Function&)" );

  // Clear previous analysis results by attaching fresh containers.
  for ( WIR_BasicBlock &b : f )
    for ( WIR_Instruction &i : b )
      for ( WIR_Operation &o : i )
        for ( WIR_Parameter &p : o )
          if ( p.getType() == WIR_ParameterType::reg )
            p.insertContainer( new WIR_DUUDChain() );

  // Do reaching definitions analysis.
  WIR_ReachingDefinitionsAnalysis reachdef( f );
  reachdef.setVirtualRegistersOnly( mVirtualRegistersOnly );
  reachdef.analyze();
};

}       // namespace WIR
