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
  @file tcgraphcoloring.cc
  @brief This file implements a TriCore-specific graph-coloring based register
         allocator.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include standard headers
#include <iterator>
#include <limits>
#include <map>
#include <sstream>
#include <stack>

// Include boost headers
#include <boost/current_function.hpp>

// Include libuseful headers
#include <libuseful/debugmacros.h>
#include <libuseful/exceptions.h>
#include <libuseful/io.h>

// Include WIR headers
#include <wir/wir.h>
#include <arch/tricore/tc13.h>
#include <arch/tricore/analyses/bit/tcbitdfa.h>

// Include local headers
#include "tcgraphcoloring.h"


//
// Preprocessor macros
//

#define BBPOS( __i )                                                           \
  WIR_BasicBlock &__b = __i.getBasicBlock();                                   \
  unsigned int __bbPos = 1;                                                    \
  for ( auto it = __b.getInstructions().begin(); it->get() != __i;             \
        ++it, ++__bbPos ) ;

#define BBID                                                                   \
  "(" << __b.getName() << "/" << __bbPos << ")" << endl


//
// Code section
//

namespace WIR {


using namespace boost;
using namespace std;


//
// Public class methods
//

/*
  Default constructor for system-level optimization.
*/
TC_GraphColoring::TC_GraphColoring( WIR_System &s, bool verbosity, bool uc ) :
  WIR_Optimization { s },
  WIR_GraphColoring { s, verbosity },
  mStackAccessLatency { 0 },
  mUseOnlyUC { uc }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );
};


/*
  Default constructor for compilation unit-level optimization.
*/
TC_GraphColoring::TC_GraphColoring( WIR_CompilationUnit &c, bool verbosity,
                                    bool uc ) :
  WIR_Optimization { c },
  WIR_GraphColoring { c, verbosity },
  mStackAccessLatency { 0 },
  mUseOnlyUC { uc }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );
};


/*
  Default constructor for function-level optimization.
*/
TC_GraphColoring::TC_GraphColoring( WIR_Function &f, bool verbosity, bool uc ) :
  WIR_Optimization { f },
  WIR_GraphColoring { f, verbosity },
  mStackAccessLatency { 0 },
  mUseOnlyUC { uc }
{
  DSTART( "TC_GraphColoring::TC_GraphColoring(WIR_Function&, bool, bool)" );
};


/*
  Destructor.
*/
TC_GraphColoring::~TC_GraphColoring( void )
{
  DSTART( "virtual TC_GraphColoring::~TC_GraphColoring()" );
};


/*
  setUseOnlyUC (de-) activates whether the register allocator should only use
  physical registers from the TriCore's upper context, or not.
*/
void TC_GraphColoring::setUseOnlyUC( bool uc )
{
  DSTART( "void TC_GraphColoring::setUseOnlyUC(bool)" );

  mUseOnlyUC = uc;
};


/*
  getUseOnlyUC returns whether the register allocator should only use physical
  registers from the TriCore's upper context, or not.
*/
bool TC_GraphColoring::getUseOnlyUC( void ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  return( mUseOnlyUC );
};


//
// Protected class methods
//

/*
  runOptimization allocates registers in the given function.
*/
void TC_GraphColoring::runOptimization( WIR_Function &f )
{
  DSTART( "virtual void TC_GraphColoring::runOptimization(WIR_Function&)" );

  TC_BitDFA analyzer { f };
  setDFA( analyzer );

  WIR_GraphColoring::runOptimization( f );
};


/*
  saveBestSolutionHook allows to save processor-specific allocation data in the
  course of Bernstein's best-of-three spilling heuristic.

  Here, saveBestSolutionHook is used to save TriCore-specific information on
  lower-context registers alive across function calls.
*/
void TC_GraphColoring::saveBestSolutionHook( void )
{
  DSTART( "virtual void TC_GraphColoring::saveBestSolutionHook()" );

  mBestLCRegsAliveAcrossCall = std::move( mLocalLCRegsAliveAcrossCall );
  mBestLocalDummyParameters = std::move( mLocalDummyParameters );

  mLocalLCRegsAliveAcrossCall.clear();
  mLocalDummyParameters.clear();
};


/*
  restoreBestSolutionHook allows to restore processor-specific allocation data
  in the course of Bernstein's best-of-three spilling heuristic.

  Here, restoreBestSolutionHook is used to restore TriCore-specific information
  on lower-context registers alive across function calls.
*/
void TC_GraphColoring::restoreBestSolutionHook( void )
{
  DSTART( "virtual void TC_GraphColoring::restoreBestSolutionHook()" );

  mLocalLCRegsAliveAcrossCall = std::move( mBestLCRegsAliveAcrossCall );
  mLocalDummyParameters = std::move( mBestLocalDummyParameters );

  mBestLCRegsAliveAcrossCall.clear();
  mBestLocalDummyParameters.clear();
};


/*
  createPhregs sets up the lists mPhregs and mPhregsForPrecoloringOnly of all
  TriCore-specific physical registers contained in the specified WIR function.
*/
void TC_GraphColoring::createPhregs( WIR_Function &f )
{
  DSTART( "virtual void TC_GraphColoring::createPhregs(WIR_Function&)" );

  // Determine the involved processor core.
  WIR_System &sys = f.getCompilationUnit().getSystem();
  const WIR_Section &sec = sys.findSymbol( f ).getSection();
  WIR_BaseProcessor &p = sec.getProcessor();

  // It must be a TriCore, of course.
  ufAssert( p.getISAName().find( "TC1.3" ) == 0 );
  TC13 &tc = dynamic_cast<TC13 &>( p );

  // Determine memory access latency of the stack.
  ufAssert( tc.containsSection( ".stack" ) );
  WIR_MemoryRegion &stackMem = tc.findSection( ".stack" )->get().getRegion();
  mStackAccessLatency = stackMem.getMaxDelay();

  // Determine the code access latencies of all basic blocks.
  mBBAccessLatency.clear();
  for ( WIR_BasicBlock &b : f ) {
    auto &region = sys.findSymbol( b ).getSection().getRegion();
    mBBAccessLatency[ b.getID() ] = region.getMaxDelay();
  }

  mPhregs = {
    // TriCore data registers subject to register allocation.
    const_cast<TC_DRegP &>( tc.D0() ), const_cast<TC_DRegP &>( tc.D1() ),
    const_cast<TC_DRegP &>( tc.D2() ), const_cast<TC_DRegP &>( tc.D3() ),
    const_cast<TC_DRegP &>( tc.D4() ), const_cast<TC_DRegP &>( tc.D5() ),
    const_cast<TC_DRegP &>( tc.D6() ), const_cast<TC_DRegP &>( tc.D7() ),
    const_cast<TC_DRegP &>( tc.D8() ), const_cast<TC_DRegP &>( tc.D9() ),
    const_cast<TC_DRegP &>( tc.D10() ), const_cast<TC_DRegP &>( tc.D11() ),
    const_cast<TC_DRegP &>( tc.D12() ), const_cast<TC_DRegP &>( tc.D13() ),
    const_cast<TC_DRegP &>( tc.D14() ), const_cast<TC_DRegP &>( tc.D15() ),

    // TriCore address registers subject to register allocation.
    // We do not include A10 and A11 here, since these are the stack and return
    // address pointers, resp. Furthermore, A0, A1, A8 and A9 are omitted, since
    // they are global registers that do neither belong to a function's upper
    // nor to its lower context (cf. TriCore EABI, section 2.2.1.4 "System
    // Global Registers": "Address registers A[0], A[1], A[8], and A[9] are
    // designated as system global registers. They are not part of either
    // context partition and are not saved/restored across calls. [...] By
    // convention, A[0] and A[1] are reserved for compiler use, while A[8] and
    // A[9] are reserved for OS or application use. A[0] is intended as a base
    // pointer to the "small" data section [...]. A[1] is intended as a base
    // pointer to the "literal data section"."
    const_cast<TC_ARegP &>( tc.A2() ), const_cast<TC_ARegP &>( tc.A3() ),
    const_cast<TC_ARegP &>( tc.A4() ), const_cast<TC_ARegP &>( tc.A5() ),
    const_cast<TC_ARegP &>( tc.A6() ), const_cast<TC_ARegP &>( tc.A7() ),
    const_cast<TC_ARegP &>( tc.A12() ), const_cast<TC_ARegP &>( tc.A13() ),
    const_cast<TC_ARegP &>( tc.A14() ), const_cast<TC_ARegP &>( tc.A15() ) };

  // TriCore registers subject to pre-coloring (e.g., if used in some snippet of
  // inline assembly) but NOT subject to register allocation. Here, we list all
  // address registers not included in mPhregs, see the comment on the TriCore
  // EABI above.
  mPhregsForPrecoloringOnly = {
    const_cast<TC_ARegP &>( tc.A0() ), const_cast<TC_ARegP &>( tc.A1() ),
    const_cast<TC_ARegP &>( tc.A8() ), const_cast<TC_ARegP &>( tc.A9() ),
    const_cast<TC_ARegP &>( tc.A10() ), const_cast<TC_ARegP &>( tc.A11() ) };

  // Initialize some TriCore-specific data structures.
  mLCRegsAliveAcrossCall.clear();
  mDummyParameters.clear();

  mOrderedAREGsUC = {
    const_cast<TC_ARegP &>( tc.A15() ), const_cast<TC_ARegP &>( tc.A14() ),
    const_cast<TC_ARegP &>( tc.A13() ), const_cast<TC_ARegP &>( tc.A12() ) };

  mOrderedDREGsUC = {
    const_cast<TC_DRegP &>( tc.D15() ), const_cast<TC_DRegP &>( tc.D14() ),
    const_cast<TC_DRegP &>( tc.D13() ), const_cast<TC_DRegP &>( tc.D12() ),
    const_cast<TC_DRegP &>( tc.D11() ), const_cast<TC_DRegP &>( tc.D10() ),
    const_cast<TC_DRegP &>( tc.D9() ), const_cast<TC_DRegP &>( tc.D8() ) };

  mOrderedEREGsUC = {
    const_cast<TC_ERegP &>( tc.E14() ), const_cast<TC_ERegP &>( tc.E12() ),
    const_cast<TC_ERegP &>( tc.E10() ), const_cast<TC_ERegP &>( tc.E8() ) };

  mOrderedPREGsUC = {
    const_cast<TC_PRegP &>( tc.P14() ), const_cast<TC_PRegP &>( tc.P12() ) };

  mOrderedAREGsAliveAcrossCall = {
    const_cast<TC_ARegP &>( tc.A15() ), const_cast<TC_ARegP &>( tc.A14() ),
    const_cast<TC_ARegP &>( tc.A13() ), const_cast<TC_ARegP &>( tc.A12() ),
    const_cast<TC_ARegP &>( tc.A3() ), const_cast<TC_ARegP &>( tc.A7() ),
    const_cast<TC_ARegP &>( tc.A6() ), const_cast<TC_ARegP &>( tc.A5() ),
    const_cast<TC_ARegP &>( tc.A2() ), const_cast<TC_ARegP &>( tc.A4() ) };

  mOrderedDREGsAliveAcrossCall = {
    const_cast<TC_DRegP &>( tc.D15() ), const_cast<TC_DRegP &>( tc.D14() ),
    const_cast<TC_DRegP &>( tc.D13() ), const_cast<TC_DRegP &>( tc.D12() ),
    const_cast<TC_DRegP &>( tc.D11() ), const_cast<TC_DRegP &>( tc.D10() ),
    const_cast<TC_DRegP &>( tc.D9() ), const_cast<TC_DRegP &>( tc.D8() ),
    const_cast<TC_DRegP &>( tc.D1() ), const_cast<TC_DRegP &>( tc.D0() ),
    const_cast<TC_DRegP &>( tc.D3() ), const_cast<TC_DRegP &>( tc.D7() ),
    const_cast<TC_DRegP &>( tc.D6() ), const_cast<TC_DRegP &>( tc.D5() ),
    const_cast<TC_DRegP &>( tc.D2() ), const_cast<TC_DRegP &>( tc.D4() ) };

  mOrderedEREGsAliveAcrossCall = {
    const_cast<TC_ERegP &>( tc.E14() ), const_cast<TC_ERegP &>( tc.E12() ),
    const_cast<TC_ERegP &>( tc.E10() ), const_cast<TC_ERegP &>( tc.E8() ),
    const_cast<TC_ERegP &>( tc.E6() ), const_cast<TC_ERegP &>( tc.E4() ),
    const_cast<TC_ERegP &>( tc.E2() ), const_cast<TC_ERegP &>( tc.E0() ) };

  mOrderedPREGsAliveAcrossCall = {
    const_cast<TC_PRegP &>( tc.P14() ), const_cast<TC_PRegP &>( tc.P12() ),
    const_cast<TC_PRegP &>( tc.P6() ), const_cast<TC_PRegP &>( tc.P4() ),
    const_cast<TC_PRegP &>( tc.P2() ) };
};


/*
  initializationHook allows to perform processor-specific actions before doing
  some actual coloring or spilling.

  Here, initializationHook is used to perform the bit-true data flow analysis
  for TriCore before jumping into the actual graph coloring.
*/
void TC_GraphColoring::initializationHook( WIR_Function &f )
{
  DSTART( "virtual void TC_GraphColoring::initializationHook(WIR_Function&)" );

  (void) f;

  if ( getRematerialization() )
    mBitDFA->analyze();
};


/*
  isStackPointer returns whether the specified WIR register is the TriCore's
  stack pointer.
*/
bool TC_GraphColoring::isStackPointer( const WIR_BaseRegister &r ) const
{
  DSTART(
    "virtual bool TC_GraphColoring::isStackPointer(const WIR_BaseRegister&) const" );

  return( TC13::isSP( r ) );
};


/*
  checkLCRegsAliveAcrossCall checks whether the two specified virtual leaf
  registers are mapped to a lower-context physical register and whether they are
  alive across a function call.

  If so, the respective function call and the physical register are stored in
  mLCRegsAliveAcrossCall.
*/
void TC_GraphColoring::checkLCRegsAliveAcrossCall( const WIR_PhysicalRegister &phreg,
                                                   const WIR_RegisterSet &vregs_c1,
                                                   const WIR_RegisterSet &vregs_c2 )
{
  DSTART(
    "void TC_GraphColoring::checkLCRegsAliveAcrossCall(const WIR_PhysicalRegister&, const WIR_RegisterSet&, const WIR_RegisterSet&)" );

  if ( TC13::isLCReg( phreg ) ) {
    // If a lower-context register was chosen, we have to check whether the
    // associated virtual register is live across some function call. If so, we
    // must make sure to save/restore the chosen lower-context register across
    // such a function call later.
    TC13 &tc = dynamic_cast<TC13 &>( phreg.getProcessor() );

    for ( auto p : mVregsAliveAcrossCall ) {
      WIR_Instruction &theCall = p.first.get();
      WIR_Operation &theCallOp = theCall.getOperations().front().get();
      auto &liveVregs = p.second;

      // First, determine whether the current call instruction defines registers
      // D2, D3 or E2, i.e., whether the call returns some result via these
      // registers.
      bool callDefinesD2 = false;
      bool callDefinesD3 = false;
      bool callDefinesE2 = false;

      for ( WIR_Parameter &p : theCallOp )
        if ( p.getType() == WIR_ParameterType::reg ) {
          auto &rp = dynamic_cast<WIR_RegisterParameter &>( p );
          auto &r = rp.getRegister();

          if ( rp.isDefined() ) {
            if ( r == tc.D2() )
              callDefinesD2 = true;
            else

            if ( r == tc.D3() )
              callDefinesD3 = true;
            else

            if ( r == tc.E2() )
              callDefinesE2 = callDefinesD2 = callDefinesD3 = true;
          }
        }

      if ( callDefinesD2 && callDefinesD3 )
        callDefinesE2 = true;

      if ( vregs_c2.empty() ) {
        // We are dealing with a simple, non-hierarchical register or with a
        // root of a hierarchical register.
        for ( WIR_BaseRegister &r : vregs_c1 )
          if ( r.isVirtual() &&
               liveVregs.count( dynamic_cast<WIR_VirtualRegister &>( r ) ) ) {
            // The current virtual register is live across the call.
            mLocalLCRegsAliveAcrossCall[ theCall ].insert(
              const_cast<WIR_PhysicalRegister &>( phreg ) );

            DACTION(
              BBPOS( theCall );
              DOUT(
                "Storing LC register " << r.getName() << "/" <<
                phreg.getName() << " to be caller-saved across call " << BBID <<
                theCallOp << endl ); );

            // If phreg is one of D2, D3 or E2, and if this phreg is also
            // defined by the current call, then we have to pay attention. In
            // this particular situation, D2/D3/E2 is alive across a function
            // call, but the call itself overwrites this phreg. Later on, in the
            // post-processing hook, dedicated store and load instructions are
            // generated before and after the call in order to save/restore this
            // phreg:
            //
            //   <some instruction defining, e.g., D2>
            //   ...
            //   st.w [SP] offset, D2;        # this store is missing here!
            //   call func;                   # function call overwrites D2!
            //
            // However, the save instruction storing D2/D3/E2 before the call
            // is not generated NOW. This means that the current call
            // instruction defines/overwrites D2/D3/E2, while a store
            // instruction making use of the original value of D2/D3/E2 and
            // saving it on the stack is still missing. This has the effect that
            // liveness information computed during subsequent iterations of the
            // graph coloring allocator is WRONG here. Liveness analysis only
            // sees the DEF of D2/D3/E2 by the call instruction, but a USE of
            // these registers immediately before the call is missing, which
            // finally leads to eventually missing edges in the interference
            // graph.
            // In order to resolve this issues, we add a dummy implicit
            // parameter using D2/D3/E2 to the call instruction in order to
            // model the liveness of D2/D3/E2 correctly. Later on in the post-
            // processing hook, all these dummy USE parameters will be properly
            // removed again.
            if ( ( ( phreg == tc.D2() ) && callDefinesD2 ) ||
                 ( ( phreg == tc.D3() ) && callDefinesD3 ) ||
                 ( ( phreg == tc.E2() ) && callDefinesE2 ) )
              mLocalDummyParameters.push_back(
                { const_cast<WIR_PhysicalRegister &>( phreg ), theCall } );
          }
      } else {
        // We are dealing with a hierarchical register.
        for ( WIR_BaseRegister &r : vregs_c1 )
          if ( r.isVirtual() &&
               liveVregs.count( dynamic_cast<WIR_VirtualRegister &>( r ) ) ) {
            WIR_PhysicalRegister &child1 =
              const_cast<WIR_PhysicalRegister &>( phreg.begin()->get() );

            // The first child of the extended virtual register is live across
            // the call.
            mLocalLCRegsAliveAcrossCall[ theCall ].insert( child1 );

            DACTION(
              BBPOS( theCall );
              DOUT(
                "Storing LC register " << r.getName() << "/" <<
                child1.getName() << " to be caller-saved across call " <<
                BBID << theCallOp << endl ); );

            // See lengthy comment above.
            if ( ( child1 == tc.D2() ) && callDefinesD2 )
              mLocalDummyParameters.push_back( { child1, theCall } );
          }

        for ( WIR_BaseRegister &r : vregs_c2 )
          if ( r.isVirtual() &&
               liveVregs.count( dynamic_cast<WIR_VirtualRegister &>( r ) ) ) {
            WIR_PhysicalRegister &child2 =
              const_cast<WIR_PhysicalRegister &>( phreg.rbegin()->get() );

            // The second child of the extended virtual register is live across
            // the call.
            mLocalLCRegsAliveAcrossCall[ theCall ].insert( child2 );

            DACTION(
              BBPOS( theCall );
              DOUT(
                "Storing LC register " << r.getName() << "/" <<
                child2.getName() << " to be caller-saved across call " <<
                BBID << theCallOp << endl ); );

            // See lengthy comment above.
            if ( ( child2 == tc.D3() ) && callDefinesD3 )
              mLocalDummyParameters.push_back( { child2, theCall } );
          }
      }

      WIR_RegisterSet &lcRegs = mLocalLCRegsAliveAcrossCall[ theCall ];

      if ( lcRegs.count( const_cast<TC_DRegP &>( tc.D0() ) ) &&
           lcRegs.count( const_cast<TC_DRegP &>( tc.D1() ) ) ) {
        lcRegs.erase( const_cast<TC_DRegP &>( tc.D0() ) );
        lcRegs.erase( const_cast<TC_DRegP &>( tc.D1() ) );
        lcRegs.insert( const_cast<TC_ERegP &>( tc.E0() ) );
      }

      if ( lcRegs.count( const_cast<TC_ERegP &>( tc.E0() ) ) &&
           lcRegs.count( const_cast<TC_DRegP &>( tc.D0() ) ) )
        lcRegs.erase( const_cast<TC_DRegP &>( tc.D0() ) );
      if ( lcRegs.count( const_cast<TC_ERegP &>( tc.E0() ) ) &&
           lcRegs.count( const_cast<TC_DRegP &>( tc.D1() ) ) )
        lcRegs.erase( const_cast<TC_DRegP &>( tc.D1() ) );

      if ( lcRegs.count( const_cast<TC_DRegP &>( tc.D2() ) ) &&
           lcRegs.count( const_cast<TC_DRegP &>( tc.D3() ) ) ) {
        lcRegs.erase( const_cast<TC_DRegP &>( tc.D2() ) );
        lcRegs.erase( const_cast<TC_DRegP &>( tc.D3() ) );
        lcRegs.insert( const_cast<TC_ERegP &>( tc.E2() ) );
      }

      if ( lcRegs.count( const_cast<TC_ERegP &>( tc.E2() ) ) &&
           lcRegs.count( const_cast<TC_DRegP &>( tc.D2() ) ) )
        lcRegs.erase( const_cast<TC_DRegP &>( tc.D2() ) );
      if ( lcRegs.count( const_cast<TC_ERegP &>( tc.E2() ) ) &&
           lcRegs.count( const_cast<TC_DRegP &>( tc.D3() ) ) )
        lcRegs.erase( const_cast<TC_DRegP &>( tc.D3() ) );

      if ( lcRegs.count( const_cast<TC_DRegP &>( tc.D4() ) ) &&
           lcRegs.count( const_cast<TC_DRegP &>( tc.D5() ) ) ) {
        lcRegs.erase( const_cast<TC_DRegP &>( tc.D4() ) );
        lcRegs.erase( const_cast<TC_DRegP &>( tc.D5() ) );
        lcRegs.insert( const_cast<TC_ERegP &>( tc.E4() ) );
      }

      if ( lcRegs.count( const_cast<TC_ERegP &>( tc.E4() ) ) &&
           lcRegs.count( const_cast<TC_DRegP &>( tc.D4() ) ) )
        lcRegs.erase( const_cast<TC_DRegP &>( tc.D4() ) );
      if ( lcRegs.count( const_cast<TC_ERegP &>( tc.E4() ) ) &&
           lcRegs.count( const_cast<TC_DRegP &>( tc.D5() ) ) )
        lcRegs.erase( const_cast<TC_DRegP &>( tc.D5() ) );

      if ( lcRegs.count( const_cast<TC_DRegP &>( tc.D6() ) ) &&
           lcRegs.count( const_cast<TC_DRegP &>( tc.D7() ) ) ) {
        lcRegs.erase( const_cast<TC_DRegP &>( tc.D6() ) );
        lcRegs.erase( const_cast<TC_DRegP &>( tc.D7() ) );
        lcRegs.insert( const_cast<TC_ERegP &>( tc.E6() ) );
      }

      if ( lcRegs.count( const_cast<TC_ERegP &>( tc.E6() ) ) &&
           lcRegs.count( const_cast<TC_DRegP &>( tc.D6() ) ) )
        lcRegs.erase( const_cast<TC_DRegP &>( tc.D6() ) );
      if ( lcRegs.count( const_cast<TC_ERegP &>( tc.E6() ) ) &&
           lcRegs.count( const_cast<TC_DRegP &>( tc.D7() ) ) )
        lcRegs.erase( const_cast<TC_DRegP &>( tc.D7() ) );

      if ( lcRegs.count( const_cast<TC_ARegP &>( tc.A2() ) ) &&
           lcRegs.count( const_cast<TC_ARegP &>( tc.A3() ) ) ) {
        lcRegs.erase( const_cast<TC_ARegP &>( tc.A2() ) );
        lcRegs.erase( const_cast<TC_ARegP &>( tc.A3() ) );
        lcRegs.insert( const_cast<TC_PRegP &>( tc.P2() ) );
      }

      if ( lcRegs.count( const_cast<TC_PRegP &>( tc.P2() ) ) &&
           lcRegs.count( const_cast<TC_ARegP &>( tc.A2() ) ) )
        lcRegs.erase( const_cast<TC_ARegP &>( tc.A2() ) );
      if ( lcRegs.count( const_cast<TC_PRegP &>( tc.P2() ) ) &&
           lcRegs.count( const_cast<TC_ARegP &>( tc.A3() ) ) )
        lcRegs.erase( const_cast<TC_ARegP &>( tc.A3() ) );

      if ( lcRegs.count( const_cast<TC_ARegP &>( tc.A4() ) ) &&
           lcRegs.count( const_cast<TC_ARegP &>( tc.A5() ) ) ) {
        lcRegs.erase( const_cast<TC_ARegP &>( tc.A4() ) );
        lcRegs.erase( const_cast<TC_ARegP &>( tc.A5() ) );
        lcRegs.insert( const_cast<TC_PRegP &>( tc.P4() ) );
      }

      if ( lcRegs.count( const_cast<TC_PRegP &>( tc.P4() ) ) &&
           lcRegs.count( const_cast<TC_ARegP &>( tc.A4() ) ) )
        lcRegs.erase( const_cast<TC_ARegP &>( tc.A4() ) );
      if ( lcRegs.count( const_cast<TC_PRegP &>( tc.P4() ) ) &&
           lcRegs.count( const_cast<TC_ARegP &>( tc.A5() ) ) )
        lcRegs.erase( const_cast<TC_ARegP &>( tc.A5() ) );

      if ( lcRegs.count( const_cast<TC_ARegP &>( tc.A6() ) ) &&
           lcRegs.count( const_cast<TC_ARegP &>( tc.A7() ) ) ) {
        lcRegs.erase( const_cast<TC_ARegP &>( tc.A6() ) );
        lcRegs.erase( const_cast<TC_ARegP &>( tc.A7() ) );
        lcRegs.insert( const_cast<TC_PRegP &>( tc.P6() ) );
      }

      if ( lcRegs.count( const_cast<TC_PRegP &>( tc.P6() ) ) &&
           lcRegs.count( const_cast<TC_ARegP &>( tc.A6() ) ) )
        lcRegs.erase( const_cast<TC_ARegP &>( tc.A6() ) );
      if ( lcRegs.count( const_cast<TC_PRegP &>( tc.P6() ) ) &&
           lcRegs.count( const_cast<TC_ARegP &>( tc.A7() ) ) )
        lcRegs.erase( const_cast<TC_ARegP &>( tc.A7() ) );
    }
  }
};


/*
  buildProcessorSpecificInterferences adds edges to the interference graph
  expressing TriCore-specific interferences.

  Furthermore, buildProcessorSpecificInterferences checks which virtual
  registers are live-out across function calls inside f.
*/
void TC_GraphColoring::buildProcessorSpecificInterferences( WIR_Function &f,
                                                            WIR_InterferenceGraph &igraph )
{
  DSTART(
    "virtual void TC_GraphColoring::buildProcessorSpecificInterferences(WIR_Function&, WIR_InterferenceGraph&)" );

  static map<WIR_id_t, bool> firstInvocation;

  if ( firstInvocation.find( f.getID() ) == firstInvocation.end() )
    firstInvocation[ f.getID() ] = true;

  // Update the interference graph first.
  for ( WIR_VirtualRegister &r : f.getVirtualRegisters() )
    if ( !r.isChild() && igraph.containsNode( r ) && !isPrecolored( r ) )
      // We have found a virtual register which is either a simple,
      // non-hierarchical register or is the root of a complex register
      // hierarchy. Now, add interferences between a virtual DREG and all
      // physical AREGs, and vice versa.
      for ( WIR_VirtualRegister &l : r.getLeafs() )
        for ( const WIR_PhysicalRegister &preg : mPhregs )
          if ( !igraph.interfere( l, preg ) &&
               !igraph.areSameNodes( l, preg ) &&
               ( ( ( l.getType() == TC13::RegisterType::dReg ) &&
                   ( preg.getType() == TC13::RegisterType::aReg ) ) ||
                 ( ( l.getType() == TC13::RegisterType::aReg ) &&
                   ( preg.getType() == TC13::RegisterType::dReg ) ) ) ) {
            DOUT(
              "Adding interference " << l.getName() << " <-> " <<
              preg.getName() << endl );
            igraph.addInterference( l, preg, 1 );
          }

  // Next, check which virtual registers are alive across function calls. This
  // information is required later in order to correctly handle the TriCore's
  // lower-context registers during function calls.
  // Simultaneously, we check for LOOP instructions and mark all address
  // registers used by them as high-priority registers.
  mVregsAliveAcrossCall.clear();

  for ( WIR_BasicBlock &b : f )
    for ( WIR_Instruction &i : b )
      if ( i.getOperations().front().get().isCall() ||
           i.getOperations().front().get().isIndirectCall() ) {
        auto &ltaContainer = i.getContainers<WIR_LiveOut>().begin()->get();
        auto &live = ltaContainer.getRegisters();

        for ( WIR_BaseRegister &r : live )
          if ( r.isVirtual() )
            mVregsAliveAcrossCall[ i ].insert(
              dynamic_cast<WIR_VirtualRegister &>( r ) );

        DACTION(
          if ( mVregsAliveAcrossCall[ i ].size() != 0 ) {
            BBPOS( i );
            DOUT(
              endl << "Virtual registers live across function call " << BBID <<
              i << "{" );
            for ( WIR_VirtualRegister &r : mVregsAliveAcrossCall[ i ] )
              DOUT( " " << r.getName() );
            DOUT( " }" << endl );
          } );

        // Furthermore, we count how many move operations storing a function
        // call's result occur immediately after the call.
        if ( firstInvocation[ f.getID() ] ) {
          WIR_BasicBlock &succBB = b.getSuccessors().begin()->get();
          auto &instrs = succBB.getInstructions();

          if ( instrs.size() >= 2 ) {
            auto iit = instrs.begin();
            auto opit1 = iit->get().getOperations().begin();

            if ( ( opit1 != iit->get().getOperations().end() ) &&
                 isFunctionReturnMove( opit1->get() ) ) {
              ++iit;
              auto opit2 = iit->get().getOperations().begin();

              if ( ( opit2 != iit->get().getOperations().end() ) &&
                   isFunctionReturnMove( opit2->get() ) )
                mFunctionReturnMoves[ i.getID() ] = 2;
              else
                mFunctionReturnMoves[ i.getID() ] = 1;
            } else
              mFunctionReturnMoves[ i.getID() ] = 0;
          } else

          if ( instrs.size() == 1 ) {
            auto iit = instrs.begin();
            auto opit1 = iit->get().getOperations().begin();

            if ( ( opit1 != iit->get().getOperations().end() ) &&
                 isFunctionReturnMove( opit1->get() ) )
              mFunctionReturnMoves[ i.getID() ] = 1;
            else
              mFunctionReturnMoves[ i.getID() ] = 0;
          } else
            mFunctionReturnMoves [ i.getID() ] = 0;
        }
      } else

      if ( i.getOperations().front().get().getOpCode() == TC13::OpCode::LOOP ) {
        WIR_Parameter &p =
          i.getOperations().front().get().getParameters().front().get();
        auto &rp = dynamic_cast<WIR_RegisterParameter &>( p );
        mHighPriorityRegs.insert( rp.getRegister().getID() );
      }

  firstInvocation[ f.getID() ] = false;

  // Clear some data structures at the beginning of each and every allocation
  // round.
  mLocalLCRegsAliveAcrossCall.clear();
  mLocalDummyParameters.clear();
};


/*
  isFunctionReturnMove checks whether the move operation stores the result of a
  function call somewhere.
*/
bool TC_GraphColoring::isFunctionReturnMove( const WIR_Operation &o ) const
{
  DSTART(
    "bool TC_GraphColoring::isFunctionReturnMove(const WIR_Operation&) const" );

  if ( !o.isMove() )
    return( false );

  WIR_BaseRegister &useReg = getUseOfMove( o );

  // First, verify that the register used by the move is D2, D3 or E2.
  bool isD2D3E2 = false;

  if ( useReg.isPhysical() &&
       ( ( useReg.getName() == "D2" ) || ( useReg.getName() == "D3" ) ||
         ( useReg.getName() == "E2" ) ) )
    isD2D3E2 = true;

  if ( useReg.isVirtual() && isPrecolored( useReg ) ) {
    WIR_PhysicalRegister &preg = mPrecolored.at( useReg ).get();

    if ( ( preg.getName() == "D2" ) || ( preg.getName() == "D3" ) ||
         ( preg.getName() == "E2" ) )
      isD2D3E2 = true;
  }

  if ( !isD2D3E2 )
    return( false );

  // Next, verify that the predecessor basic blocks all end with a call that
  // define useReg.
  WIR_Instruction &i = o.getInstruction();
  WIR_BasicBlock &b = i.getBasicBlock();

  for ( WIR_BasicBlock &pred : b.getPredecessors() ) {
    WIR_Instruction &lastIns = *(pred.getInstructions().rbegin());

    if ( lastIns.getOperations().front().get().isCall() ||
         lastIns.getOperations().front().get().isIndirectCall() ) {
      bool rIsDefinedByCall = false;

      for ( WIR_Parameter &p :
              lastIns.getOperations().front().get().getParameters() )
        if ( p.getType() == WIR_ParameterType::reg ) {
          auto &rp = dynamic_cast<WIR_RegisterParameter &>( p );

          if ( ( rp.isDefined() || rp.isDefUsed() ) &&
               ( ( rp.getRegister() == useReg ) ||
                 useReg.isChildOf( rp.getRegister() ) ||
                 rp.getRegister().isChildOf( useReg ) ) ) {
            rIsDefinedByCall = true;
            break;
          }
        }

      if ( !rIsDefinedByCall )
        return( false );
    } else
      return( false );
  }

  // Finally, verify that o is either the very first instruction within b, or
  // that o is the second instruction AND the first one is also a
  // function-return move.
  auto it = b.getInstructions().begin();
  if ( ( i == it->get() ) ||
       ( ( i == (++it)->get() ) &&
         ( isFunctionReturnMove(
             b.getInstructions().front().get().getOperations().front().get() ) ) ) )
    return( true );

  return( false );
};


/*
  isPriorityRegister returns whether a given register has high priority for
  color assignment.

  For the TriCore register allocator, high-priority registers are kept in set
  mHighPriorityRegs. Thus, this method only checks whether r is in
  mHighPriorityRegs or not.
*/
bool TC_GraphColoring::isPriorityRegister( const WIR_VirtualRegister &r ) const
{
  DSTART(
    "virtual bool TC_GraphColoring::isPriorityRegister(const WIR_VirtualRegister&) const" );

  return( mHighPriorityRegs.count( r.getID() ) );
};


/*
  getSpillLoadCosts returns the TriCore-specific costs of one single spill-load
  for the specified register parameter.
*/
unsigned int TC_GraphColoring::getSpillLoadCosts( const WIR_RegisterParameter &p ) const
{
  DSTART(
    "virtual unsigned int TC_GraphColoring::getSpillLoadCosts(const WIR_RegisterParameter&) const" );

  auto &b = p.getOperation().getInstruction().getBasicBlock();
  auto blockLatency = mBBAccessLatency.at( b.getID() );

  auto &r = dynamic_cast<WIR_VirtualRegister &>( p.getRegister() );

  if ( r.getType() == TC13::RegisterType::aReg )
    return( mStackAccessLatency + blockLatency );
  else

  if ( r.getType() == TC13::RegisterType::dReg )
    return( mStackAccessLatency + blockLatency );
  else

  if ( r.getType() == TC13::RegisterType::eReg )
    return( ( 2 * mStackAccessLatency ) + blockLatency );
  else

  if ( r.getType() == TC13::RegisterType::pReg )
    return( ( 2 * mStackAccessLatency ) + blockLatency );

  throw( ufFatalError( "This should never happen...", false ) );
};


/*
  getSpillStoreCosts returns the TriCore-specific costs of one single
  spill-store for the specified register parameter.
*/
unsigned int TC_GraphColoring::getSpillStoreCosts( const WIR_RegisterParameter &p ) const
{
  DSTART(
    "virtual unsigned int TC_GraphColoring::getSpillStoreCosts(const WIR_RegisterParameter&) const" );

  return( getSpillLoadCosts( p ) );
};


/*
  getMoveCosts returns the TriCore-specific costs of one single move operation
  that can be omitted due to spilling.
*/
unsigned int TC_GraphColoring::getMoveCosts( const WIR_Operation &o ) const
{
  DSTART(
    "virtual unsigned int TC_GraphColoring::getMoveCosts(const WIR_Operation&) const" );

  auto &b = o.getInstruction().getBasicBlock();

  if ( ( o.getOpCode() == TC13::OpCode::MOV_RR ) ||
       ( o.getOpCode() == TC13::OpCode::MOV_AA ) )
    return( mBBAccessLatency.at( b.getID() ) );

  throw( ufFatalError( "This should never happen...", false ) );
};


/*
  getUseOfMove returns the used register of the specified TriCore WIR move
  operation.
*/
WIR_BaseRegister &TC_GraphColoring::getUseOfMove( const WIR_Operation &o ) const
{
  DSTART(
    "virtual WIR_BaseRegister& TC_GraphColoring::getUseOfMove(const WIR_Operation&) const" );

  return( dynamic_cast<WIR_RegisterParameter &>(
    o.getExplicitParameter( 2 ) ).getRegister() );
};


/*
  getDefOfMove returns the defined register of the specified TriCore WIR move
  operation.
*/
WIR_BaseRegister &TC_GraphColoring::getDefOfMove( const WIR_Operation &o ) const
{
  DSTART(
    "virtual WIR_BaseRegister& TC_GraphColoring::getDefOfMove(const WIR_Operation&) const" );

  return(
    dynamic_cast<WIR_RegisterParameter &>(
      o.getExplicitParameter( 1 ) ).getRegister() );
};


/*
  avoidCoalescing checks whether the two given registers which are both
  move-related must not be coalesced due to TriCore-specific reasons.

  Coalescing will be avoided if
  - the register used by the move is D2, D3 or E3, and
  - the move immediately follows a call instruction.

  or if
  - one of the two registers is pre-colored and the other not,
  - the pre-colored register is a lower-context register, and
  - the non pre-colored register is alive across a function call,

  In the first case, the move is used to store away a result of a function call.
  We keep these explicit moves since otherwise, it would cause us trouble when
  implementing the caller-saving/restoring of LC registers later.

  If we would allow coalescing in the second case, the non pre-colored register
  will be allocated to the lower-context register specified in the pre-color.
  However, since it is alive across a function call, caller-saving stack loads
  and stores must be added which is bad. Instead of inserting these costly
  memory loads and stores, we simply keep the move instruction and do not
  coalesce.
*/
bool TC_GraphColoring::avoidCoalescing( const WIR_Operation &o,
                                        const WIR_BaseRegister &r1,
                                        const WIR_BaseRegister &r2,
                                        const WIR_InterferenceGraph &igraph ) const
{
  DSTART(
    "virtual bool TC_GraphColoring::avoidCoalescing(const WIR_Operation&, const WIR_BaseRegister&, const WIR_BaseRegister&, const WIR_InterferenceGraph&) const"  );

  // Check first case.
  if ( isFunctionReturnMove( o ) )
    return( true );

  // Check second case.
  if ( ( isPrecolored( r1 ) && isPrecolored( r2 ) ) ||
       ( !isPrecolored( r1 ) && !isPrecolored( r2 ) ) )
    return( false );

  WIR_PhysicalRegister *phreg = nullptr;
  WIR_BaseRegister *uncoloredVreg = nullptr;

  if ( isPrecolored( r1 ) ) {
    auto it = mPrecolored.find( const_cast<WIR_BaseRegister &>( r1 ) );
    phreg = &(it->second.get());
    uncoloredVreg = const_cast<WIR_BaseRegister *>( &r2 );
  } else {
    auto it = mPrecolored.find( const_cast<WIR_BaseRegister &>( r2 ) );
    phreg = &(it->second.get());
    uncoloredVreg = const_cast<WIR_BaseRegister *>( &r1 );
  }

  if ( TC13::isLCReg( *phreg ) ) {
    // Retrieve all coalescing aliases of the uncolored virtual register and its
    // root.
    WIR_RegisterSet aliases = igraph.getCoalescedAliases( *uncoloredVreg );
    aliases.insert( *uncoloredVreg );
    WIR_RegisterSet rootAliases =
      igraph.getCoalescedAliases( uncoloredVreg->getRoot() );
    for ( WIR_BaseRegister &r : rootAliases )
      aliases.insert( r );
    aliases.insert( uncoloredVreg->getRoot() );

    // If a lower-context register is used for pre-coloring, we have to check
    // whether the uncolored virtual register is live across some function call.
    for ( auto p : mVregsAliveAcrossCall ) {
      WIR_VirtualRegisterSet &liveVregs = p.second;

      for ( WIR_BaseRegister &r : aliases )
        if ( r.isVirtual() &&
             liveVregs.count( dynamic_cast<WIR_VirtualRegister &>( r ) ) )
          // The current virtual register is live across the call.
          return( true );
    }
  }

  return( false );
};


/*
  getRematerializationCosts returns the TriCore-specific costs of one single
  recomputation of the specified used parameter.
*/
unsigned int TC_GraphColoring::getRematerializationCosts( const WIR_RegisterParameter &p ) const
{
  DSTART(
    "virtual unsigned int TC_GraphColoring::getRematerializationCosts(const WIR_RegisterParameter&) const" );

  auto &b = p.getOperation().getInstruction().getBasicBlock();
  auto blockLatency = mBBAccessLatency.at( b.getID() );

  // Check whether the value carried along p's register is a constant.
  int constValue = getRematerializationConstant( p );

  if ( constValue != numeric_limits<int>::max() ) {
    // It now remains to determine how many TriCore machine instructions it
    // takes to recompute this constant.
    TC_ERegV e;
    bool eReg = p.getRegister().getType() == e.getType();

    DDECLARE( ostringstream __sstr );
    DACTION(
      __sstr << "Rematerialization cost of " << p.getRegister().getName()
             << " (const = " << constValue << ") in " << p.getOperation()
             << ": "; );

    if ( ( constValue >= TC_Const16_Signed::getMinValue( 16 ) ) &&
         ( constValue <= TC_Const16_Signed::getMaxValue( 16 ) ) ) {
      // In this case, only a simple MOV instruction is sufficient.
      DOUT(
        __sstr.str() << ( eReg ? 2 * blockLatency : blockLatency ) << endl );
      return( eReg ? 2 * blockLatency : blockLatency );
    }

    if ( ( constValue >= 0 ) &&
         ( (unsigned int) constValue <=
             TC_Const16_Unsigned::getMaxValue( 16 ) ) ) {
      // In this case, one simple MOV.U instruction is sufficient.
      DOUT(
        __sstr.str() << ( eReg ? 2 * blockLatency : blockLatency ) << endl );
      return( eReg ? 2 * blockLatency : blockLatency );
    }

    // Extract lowest 16 bits from constant, use 2's-complement representation.
    int low = constValue & 0x0000FFFF;

    if ( low > TC_Const16_Signed::getMaxValue( 16 ) )
      low =
        TC_Const16_Signed::getMinValue( 16 ) +
        ( low - TC_Const16_Signed::getMaxValue( 16 ) ) - 1;

    if ( low != 0 ) {
      // We need a MOVH and ADDI combination.
      DOUT(
        __sstr.str() << ( eReg ? 3 * blockLatency : 2 * blockLatency ) <<
        endl );
      return( eReg ? 3 * blockLatency : 2 * blockLatency );
    } else {
      // We only need a MOVH.
      DOUT(
        __sstr.str() << ( eReg ? 2 * blockLatency : blockLatency ) << endl );
      return( eReg ? 2 * blockLatency : blockLatency );
    }
  }

  return( numeric_limits<unsigned int>::max() );
};


/*
  getRematerializationConstant returns a constant integer value that is
  equivalent to the specified WIR parameter.
*/
int TC_GraphColoring::getRematerializationConstant( const WIR_RegisterParameter &p ) const
{
  DSTART(
    "int TC_GraphColoring::getRematerializationConstant(const WIR_RegisterParameter&) const" );

  int res = numeric_limits<int>::max();

  // If p refers to an AREG or an APREG, stop here. Rematerialization currently
  // only supports constants in DREGs or EREGs.
  if ( ( p.getRegister().getType() == TC13::RegisterType::aReg ) ||
       ( p.getRegister().getType() == TC13::RegisterType::pReg ) )
    return( res );

  // Combine incoming up values of p.
  map<WIR_id_t, WIR_UpDownValue> inValue;
  combineInValues( p, inValue );

  auto it = inValue.find( p.getID() );
  if ( it == inValue.end() )
    return( res );

  auto &combinedInValue = it->second;
  if ( !combinedInValue.isInteger() )
    return( res );

  // Replace all don't care bits of combinedInValue by 0.
  for ( unsigned int i = 0; i < combinedInValue.getBitWidth(); ++i )
    if ( combinedInValue.at( i ) == WIR_L4::bX )
      combinedInValue.setBit( i, WIR_L4::b0 );

  // At this point, we know that the value carried along p's register is a
  // constant.
  res = combinedInValue.getSignedValue();

  DACTION(
    stringstream sstr;
    sstr << wir << p.getOperation();

    DOUT(
      "Rematerialization constant of parameter '" << p << "' in operation '" <<
      sstr.str().substr( 8 ) << "' = " << res << endl ); );

  return( res );
};


/*
  getRematerializationInstructions returns a list of TriCore instructions for
  one single recomputation of the specified used parameter.
*/
std::list<WIR_Instruction *> TC_GraphColoring::getRematerializationInstructions( const WIR_RegisterParameter &p ) const
{
  DSTART(
    "virtual list<WIR_Instruction*> TC_GraphColoring::getRematerializationInstructions(const WIR_RegisterParameter&) const" );

  list<WIR_Instruction *> res;
  WIR_VirtualRegister &r =
    dynamic_cast<WIR_VirtualRegister &>( p.getRegister() );

  // Check whether the value carried along p's register is a constant.
  int constValue = getRematerializationConstant( p );

  if ( constValue != numeric_limits<int>::max() ) {
    // It now remains to list the TriCore machine instructions it takes to
    // recompute this constant.
    TC_ERegV e;
    bool eReg = p.getRegister().getType() == e.getType();
    TC_DRegV d;
    bool dReg = p.getRegister().getType() == d.getType();

    if ( ( constValue >= TC_Const16_Signed::getMinValue( 16 ) ) &&
         ( constValue <= TC_Const16_Signed::getMaxValue( 16 ) ) ) {
      // In this case, only a simple MOV instruction is sufficient.
      res.push_back( new WIR_Instruction(
        { TC13::OpCode::MOV, TC13::OperationFormat::DC16_1,
          WIR_RegisterParameter(
            ( dReg ? r : r.begin()->get() ), WIR_Usage::def ),
          TC_Const16_Signed( constValue ) } ) );

      DOUT(
        "For parameter " << p.getID() << "/" << r.getName() <<
        ", generated rematerialization instruction" << *(res.back()) << endl );

      if ( eReg ) {
        res.push_back( new WIR_Instruction(
          { TC13::OpCode::MOV, TC13::OperationFormat::DC16_1,
            WIR_RegisterParameter( r.rbegin()->get(), WIR_Usage::def ),
            TC_Const16_Signed( 0 ) } ) );

        DOUT(
          "For parameter " << p.getID() << "/" << r.getName() <<
          ", generated rematerialization instruction" << *(res.back()) <<
          endl );
      }
    } else

    if ( ( constValue >= 0 ) &&
         ( (unsigned int) constValue <=
             TC_Const16_Unsigned::getMaxValue( 16 ) ) ) {
      // In this case, one simple MOV.U instruction is sufficient.
      res.push_back( new WIR_Instruction(
        { TC13::OpCode::MOV_U, TC13::OperationFormat::DC16_2,
          WIR_RegisterParameter(
            ( dReg ? r : r.begin()->get() ), WIR_Usage::def ),
          TC_Const16_Unsigned( constValue ) } ) );

      DOUT(
        "For parameter " << p.getID() << "/" << r.getName() <<
        ", generated rematerialization instruction" << *(res.back()) << endl );

      if ( eReg ) {
        res.push_back( new WIR_Instruction(
          { TC13::OpCode::MOV, TC13::OperationFormat::DC16_1,
            WIR_RegisterParameter( r.rbegin()->get(), WIR_Usage::def ),
            TC_Const16_Signed( 0 ) } ) );

        DOUT(
          "For parameter " << p.getID() << "/" << r.getName() <<
          ", generated rematerialization instruction" << *(res.back()) <<
          endl );
      }
    } else {
      int low, high;

      // Extract lowest 16 bits from constant, use 2's-complement
      // representation.
      low = constValue & 0x0000FFFF;

      if ( low > TC_Const16_Signed::getMaxValue( 16 ) )
        low =
          TC_Const16_Signed::getMinValue( 16 ) +
          ( low - TC_Const16_Signed::getMaxValue( 16 ) ) - 1;

      // Extract upper part of constant.
      high = constValue - low;

      // We don't use '>> 16' here, since C/C++ does not specify whether >>
      // performs shifting with or without sign extension.
      for ( int i = 0; i < 16; ++i )
        high /= 2;

      if ( high < 0 )
        high += TC_Const16_Unsigned::getMaxValue( 16 ) + 1;

      // Generate MOVH instruction.
      res.push_back( new WIR_Instruction(
        { TC13::OpCode::MOVH, TC13::OperationFormat::DC16_2,
          WIR_RegisterParameter(
            ( dReg ? r : r.begin()->get() ), WIR_Usage::def ),
          TC_Const16_Unsigned( high ) } ) );

      DOUT(
        "For parameter " << p.getID() << "/" << r.getName() <<
        ", generated rematerialization instruction" << *(res.back()) << endl );

      // Generate ADDI instruction.
      if ( low != 0 ) {
        res.push_back( new WIR_Instruction(
          { TC13::OpCode::ADDI, TC13::OperationFormat::DDC16_1,
            WIR_RegisterParameter(
              ( dReg ? r : r.begin()->get() ), WIR_Usage::def ),
            WIR_RegisterParameter(
              ( dReg ? r : r.begin()->get() ), WIR_Usage::use ),
            TC_Const16_Unsigned( low ) } ) );

        DOUT(
          "For parameter " << p.getID() << "/" << r.getName() <<
          ", generated rematerialization instruction" << *(res.back()) <<
          endl );
      }

      if ( eReg ) {
        res.push_back( new WIR_Instruction(
          { TC13::OpCode::MOV, TC13::OperationFormat::DC16_1,
            WIR_RegisterParameter( r.rbegin()->get(), WIR_Usage::def ),
            TC_Const16_Unsigned( 0 ) } ) );

        DOUT(
          "For parameter " << p.getID() << "/" << r.getName() <<
          ", generated rematerialization instruction" << *(res.back()) <<
          endl );
      }
    }
  }

  return( res );
};



/*
  selectColors assigns actual colors to the TriCore leaf registers in the
  specified vector.

  This method must not yet assign colors to the interference graph - this is
  done elsewhere. In order to determine feasible colors for the leaf registers,
  this method should make use of WIR_InterferenceGraph::getPossibleColors(). It
  must be ensured that the returned map is either empty or contains exactly one
  entry per leaf. It must hold that none of the colors used in this returned map
  is already used for adjacent interference graph nodes.

  This method checks whether the leafs to be processed are TriCore data or
  address registers, and whether they are simple or extended registers. It first
  tries to select the implicit registers D15 and A15, since this is beneficial
  for the generation of 16-bit instructions. If D15/A15 are not available, other
  data/address registers from the TriCore's upper context are checked next. This
  is done, because upper context registers are automatically saved across
  function calls without any additional context saving code. If the entire upper
  context is not available, the registers of the lower context are finally
  checked.
*/
WIR_GraphColoring::WIR_ColorMap TC_GraphColoring::selectColors( const std::vector<std::reference_wrapper<WIR_VirtualRegister>> &leafs,
                                                                const WIR_InterferenceGraph &igraph )
{
  DSTART(
    "virtual WIR_GraphColoring::WIR_ColorMap TC_GraphColoring::selectColors(const vector<reference_wrapper<WIR_VirtualRegister>>&, const WIR_InterferenceGraph&)" );

  WIR_GraphColoring::WIR_ColorMap res;

  ufAssert( ( leafs.size() == 1 ) || ( leafs.size() == 2 ) );
  set<unsigned int> possibleColors = igraph.getPossibleColors( leafs.front() );

  WIR_VirtualRegister &c1 = leafs.front().get();
  WIR_VirtualRegister &c2 = leafs.back().get();

  vector<std::reference_wrapper<WIR_PhysicalRegister>> *orderedPhregs = nullptr;

  // Determine whether the VREG to be colored is alive across function calls.
  DACTION(
    bool vregIsAliveAcrossCall = false;
    for ( auto p : mVregsAliveAcrossCall )
      if ( p.second.count( c1 ) || ( ( c2 != c1 ) && p.second.count( c2 ) ) ) {
        vregIsAliveAcrossCall = true;
        break;
      }

    DOUT(
      "c1 = " << c1.getName() << "\t\tc2 = " <<
      ( c2 != c1 ? c2.getName() : "NULL" ) << "\tis " <<
      ( vregIsAliveAcrossCall ? "" : "not " ) << "alive across call." <<
      endl ); );

  if ( leafs.size() == 1 ) {
    // A simple non-hierarchical register has to be colored. We distinguish
    // between DREGs and AREGs.
    if ( c1.getType() == TC13::RegisterType::dReg )
      orderedPhregs =
        mUseOnlyUC ? &mOrderedDREGsUC : &mOrderedDREGsAliveAcrossCall;
    else
      orderedPhregs =
        mUseOnlyUC ? &mOrderedAREGsUC : &mOrderedAREGsAliveAcrossCall;

    // Now, check the phregs in the specified precedence order.
    for ( WIR_PhysicalRegister &phreg : *orderedPhregs ) {
      unsigned int color = igraph.getColorOfPhreg( phreg );

      if ( possibleColors.count( color ) ) {
        res[ c1 ] = color;

        WIR_RegisterSet aliases_c1 = igraph.getCoalescedAliases( c1 );
        aliases_c1.insert( c1 );
        WIR_RegisterSet aliases_c2;

        checkLCRegsAliveAcrossCall( phreg, aliases_c1, aliases_c2 );

        break;
      }
    }
  } else {
    // An extended hierarchical register has to be colored. We again distinguish
    // between DREGs and AREGs.
    ufAssert(
      ( ( c1.getType() == TC13::RegisterType::dReg ) &&
        ( c2.getType() == TC13::RegisterType::dReg ) ) ||
      ( ( c1.getType() == TC13::RegisterType::aReg ) &&
        ( c2.getType() == TC13::RegisterType::aReg ) ) );

    if ( c1.getType() == TC13::RegisterType::dReg )
      orderedPhregs =
        mUseOnlyUC ? &mOrderedEREGsUC : &mOrderedEREGsAliveAcrossCall;
    else
      orderedPhregs =
        mUseOnlyUC ? &mOrderedPREGsUC : &mOrderedPREGsAliveAcrossCall;

    // Now, check the phregs in the specified precedence order.
    for ( WIR_PhysicalRegister &phreg : *orderedPhregs ) {
      unsigned int color1 = igraph.getColorOfPhreg( phreg.begin()->get() );
      unsigned int color2 = igraph.getColorOfPhreg( phreg.rbegin()->get() );

      if ( possibleColors.count( color1 ) && possibleColors.count( color2 ) ) {
        res[ c1 ] = color1;
        res[ c2 ] = color2;

        WIR_RegisterSet aliases_root =
          igraph.getCoalescedAliases( c1.getRoot() );
        aliases_root.insert( c1.getRoot() );
        WIR_RegisterSet emptySet;

        checkLCRegsAliveAcrossCall( phreg, aliases_root, emptySet );

        WIR_RegisterSet aliases_c1 = igraph.getCoalescedAliases( c1 );
        aliases_c1.insert( c1 );
        WIR_RegisterSet aliases_c2 = igraph.getCoalescedAliases( c2 );
        aliases_c2.insert( c2 );

        checkLCRegsAliveAcrossCall( phreg, aliases_c1, aliases_c2 );

        break;
      }
    }
  }

  return( res );
};


/*
  isSpillStore checks whether the specified instruction spill-stores a certain
  virtual register in the TriCore ISA.
*/
bool TC_GraphColoring::isSpillStore( const WIR_Instruction &i,
                                     const WIR_VirtualRegister &r ) const
{
  DSTART(
    "virtual bool TC_GraphColoring::isSpillStore(const WIR_Instruction&, const WIR_VirtualRegister&) const" );

  WIR_Operation &o = i.getOperations().front().get();

  if ( ( ( o.getOpCode() == TC13::OpCode::ST_A ) &&
         ( o.getOperationFormat() == TC13::OperationFormat::AC10ABOA ) ) ||
       ( ( o.getOpCode() == TC13::OpCode::ST_DA ) &&
         ( o.getOperationFormat() == TC13::OperationFormat::AC10PBOA ) ) ||
       ( ( o.getOpCode() == TC13::OpCode::ST_W ) &&
         ( ( o.getOperationFormat() == TC13::OperationFormat::AC10DBOA_1 ) ||
           ( o.getOperationFormat() == TC13::OperationFormat::AC16DBOA ) ) ) ||
       ( ( o.getOpCode() == TC13::OpCode::ST_D ) &&
         ( o.getOperationFormat() == TC13::OperationFormat::AC10EBOA ) ) ) {
    WIR_BaseRegister &sp =
      dynamic_cast<WIR_RegisterParameter &>(
        o.getExplicitParameter( 1 ) ).getRegister();
    WIR_BaseRegister &d =
      dynamic_cast<WIR_RegisterParameter &>(
        o.getExplicitParameter( 3 ) ).getRegister();

    if ( isStackPointer( sp ) && ( d == r ) )
      return( true );
  }

  return( false );
};


/*
  isSpillLoad checks whether the specified instruction spill-loads a certain
  virtual register in the TriCore ISA.
*/
bool TC_GraphColoring::isSpillLoad( const WIR_Instruction &i,
                                    const WIR_VirtualRegister &r ) const
{
  DSTART(
    "virtual bool TC_GraphColoring::isSpillLoad(const WIR_Instruction&, const WIR_VirtualRegister&) const" );

  WIR_Operation &o = i.getOperations().front().get();

  if ( ( ( o.getOpCode() == TC13::OpCode::LD_A ) &&
         ( o.getOperationFormat() == TC13::OperationFormat::AAC10BOA ) ) ||
       ( ( o.getOpCode() == TC13::OpCode::LD_DA ) &&
         ( o.getOperationFormat() == TC13::OperationFormat::PAC10BOA ) ) ||
       ( ( o.getOpCode() == TC13::OpCode::LD_W ) &&
         ( ( o.getOperationFormat() == TC13::OperationFormat::DAC10BOA ) ||
           ( o.getOperationFormat() == TC13::OperationFormat::DAC16BOA ) ) ) ||
       ( ( o.getOpCode() == TC13::OpCode::LD_D ) &&
         ( o.getOperationFormat() == TC13::OperationFormat::EAC10BOA ) ) ) {
    WIR_BaseRegister &d =
      dynamic_cast<WIR_RegisterParameter &>(
        o.getExplicitParameter( 1 ) ).getRegister();
    WIR_BaseRegister &sp =
      dynamic_cast<WIR_RegisterParameter &>(
        o.getExplicitParameter( 2 ) ).getRegister();

    if ( isStackPointer( sp ) && ( d == r ) )
      return( true );
  }

  return( false );
};


/*
  resolveSpillCoalescingConflict resolves a conflict when two registers with
  different positions in a register hierarchy shall be coalesced during
  computeStackLocations.
*/
std::pair<std::reference_wrapper<WIR_VirtualRegister>,
          std::reference_wrapper<WIR_VirtualRegister>> TC_GraphColoring::resolveSpillCoalescingConflict( const WIR_VirtualRegister &r1,
                                                                                                         const WIR_VirtualRegister &r2 ) const
{
  DSTART(
    "virtual pair<reference_wrapper<WIR_VirtualRegister>, reference_wrapper<WIR_VirtualRegister> > TC_GraphColoring::resolveSpillCoalescingConflict(const WIR_VirtualRegister&, const WIR_VirtualRegister&) const" );

  WIR_VirtualRegister &reg1 = const_cast<WIR_VirtualRegister &>( r1 );
  WIR_VirtualRegister &reg2 = const_cast<WIR_VirtualRegister &>( r2 );

  unsigned int leafs1 = r1.getLeafs().size();
  unsigned int leafs2 = r2.getLeafs().size();

  if ( leafs1 != leafs2 ) {
    WIR_VirtualRegister *large = ( leafs1 > leafs2 ? &reg1 : &reg2 );
    WIR_VirtualRegister *small = ( *large == reg1 ? &reg2 : &reg1 );

    return(
      pair<std::reference_wrapper<WIR_VirtualRegister>,
           std::reference_wrapper<WIR_VirtualRegister>>(
        large->begin()->get(), *small ) );
  } else
    return(
      pair<std::reference_wrapper<WIR_VirtualRegister>,
           std::reference_wrapper<WIR_VirtualRegister>>( reg1, reg2 ) );
};


/*
  getStackPosOfSubReg returns the stack position of some child register, if the
  root of the entire register hierarchy is located in the specified stack
  position.
*/
unsigned int TC_GraphColoring::getStackPosOfSubReg( const WIR_VirtualRegister &r,
                                                    unsigned int rootPos ) const
{
  DSTART(
    "virtual unsigned int TC_GraphColoring::getStackPosOfSubReg(const WIR_VirtualRegister&, unsigned int) const" );

  if ( !r.isChild() || ( r == r.getRoot().begin()->get() ) )
    return( rootPos );
  else
    return( rootPos + 4 );
};


/*
  insertSpillLoad inserts TriCore code for a spill-load of a register into the
  WIR.

  insertSpillLoad is responsible to add all generated spill-load instructions to
  map mSpillLoads.
*/
void TC_GraphColoring::insertSpillLoad( const WIR_BaseRegister &clone,
                                        const WIR_BaseRegister &r,
                                        int stackPos, WIR_BasicBlock &b,
                                        std::list<std::reference_wrapper<WIR_Instruction>>::const_iterator pos )
{
  DSTART(
    "virtual void TC_GraphColoring::insertSpillLoad(const WIR_BaseRegister&, const WIR_BaseRegister&, int, WIR_BasicBlock&, list<reference_wrapper<WIR_Instruction> >::const_iterator)"  );

  // Determine the size of the function's overflow area. According to the
  // TriCore EABI (section 2.2.2.1), the argument overflow area for outgoing
  // arguments must be located at the bottom (low address end) of the frame,
  // with the first overflow argument at zero offset from the stack pointer. We
  // must not overwrite the overflow area by spilled registers so that we
  // increase the stack positions of spills by the size of the overflow area.
  int overflowAreaSize = b.getFunction().getFrameSize();

  DACTION(
    DOUT( "Inserting spill-load of register '" << clone.getName() << "'" );
    if ( r != clone )
      DOUT(  "/'" << r.getName() << "'" );
    DOUT(
      " from stack offset " << stackPos + overflowAreaSize << " (stackPos = " <<
      stackPos << ", overflowAreaSize = " << overflowAreaSize << ") " );

    if ( pos != b.getInstructions().end() ) {
      BBPOS( pos->get() );
      DOUT( "before position " << tricore << BBID << pos->get() << endl );
    } else
      DOUT( "at end of block '" << b.getName() << "'." << endl ); );

  DDECLARE(
    // Save the current number of spill instructions in mInsertedSpillCode.
    const size_t oldPos = mInsertedSpillCode.size(); );

  WIR_BasicBlock &newBB =
    insertSpillCode( clone, r, stackPos + overflowAreaSize, b, pos, false );

  DACTION(
    auto it = mInsertedSpillCode.begin();
    for ( std::advance( it, oldPos ); it != mInsertedSpillCode.end(); ++it )
      cout << tricore << it->get(); );

  if ( newBB != b ) {
    // TODO: Add update of back-annotation mapping!
  }
};


/*
  insertSpillStore inserts TriCore code for a spill-store of a register into the
  WIR.

  insertSpillStore is responsible to add all generated spill-store instructions
  to map mSpillStores.
*/
void TC_GraphColoring::insertSpillStore( const WIR_BaseRegister &clone,
                                         const WIR_BaseRegister &r,
                                         int stackPos, WIR_BasicBlock &b,
                                         std::list<std::reference_wrapper<WIR_Instruction>>::const_iterator pos )
{
  DSTART(
    "virtual void TC_GraphColoring::insertSpillStore(const WIR_BaseRegister&, const WIR_BaseRegister&, int, WIR_BasicBlock&, list<reference_wrapper<WIR_Instruction> >::const_iterator)" );

  // Determine the size of the function's overflow area. According to the
  // TriCore EABI (section 2.2.2.1), the argument overflow area for outgoing
  // arguments must be located at the bottom (low address end) of the frame,
  // with the first overflow argument at zero offset from the stack pointer. We
  // must not overwrite the overflow area by spilled registers so that we
  // increase the stack positions of spills by the size of the overflow area.
  int overflowAreaSize = b.getFunction().getFrameSize();

  DACTION(
    DOUT( "Inserting spill-store of register '" << clone.getName() << "'" );
    if ( r != clone )
      DOUT(  "/'" << r.getName() << "'" );
    DOUT(
      " from stack offset " << stackPos + overflowAreaSize << " (stackPos = " <<
      stackPos << ", overflowAreaSize = " << overflowAreaSize << ") " );

    if ( pos != b.getInstructions().end() ) {
      BBPOS( pos->get() );
      DOUT( "before position " << tricore << BBID << pos->get() << endl );
    } else
      DOUT( "at end of block '" << b.getName() << "'." << endl ); );

  DDECLARE(
    // Save the current number of spill instructions in mInsertedSpillCode.
    const size_t oldPos = mInsertedSpillCode.size(); );

  WIR_BasicBlock &newBB =
    insertSpillCode( clone, r, stackPos + overflowAreaSize, b, pos, true );

  DACTION(
    auto it = mInsertedSpillCode.begin();
    for ( std::advance( it, oldPos ); it != mInsertedSpillCode.end(); ++it )
      cout << tricore << it->get(); );

  if ( newBB != b ) {
    // TODO: Add update of back-annotation mapping!
  }
};


/*
  insertSpillCode inserts TriCore code for a spill-load or spill-store of a
  register into the WIR.
*/
WIR_BasicBlock &TC_GraphColoring::insertSpillCode( const WIR_BaseRegister &clone,
                                                   const WIR_BaseRegister &r,
                                                   int stackPos,
                                                   WIR_BasicBlock &b,
                                                   std::list<std::reference_wrapper<WIR_Instruction>>::const_iterator pos,
                                                   bool spillStore )
{
  DSTART(
    "virtual WIR_BasicBlock& TC_GraphColoring::insertSpillCode(const WIR_BaseRegister&, const WIR_BaseRegister&, int, WIR_BasicBlock&, list<reference_wrapper<WIR_Instruction> >::const_iterator, bool)" );

  std::reference_wrapper<WIR_BasicBlock> res = b;

  if ( pos == b.getInstructions().end() ) {
    // Someone wants to insert spill code at the end of basic block b. So, we
    // have to check whether b's very last instruction is a jump or a call.
    bool isJump = false;
    bool isCall = false;

    if ( !b.getInstructions().empty() )
      for ( WIR_Operation &o : b.getInstructions().back().get() ) {
        if ( o.isJump() )
          isJump = true;
        if ( o.isCall() )
          isCall = true;
      }

    if ( isJump ) {
      // OK, we shall insert spill code after a jump. However, this spill code
      // cannot be inserted in b itself. Instead, it has to be placed in all
      // successors of b. However, this is only valid if all these successors
      // have only one predecessor, namely b.
      bool successorBBsAreValid = true;
      for ( WIR_BasicBlock &succ : b.getSuccessors() )
        if ( succ.getPredecessors().size() > 1 ) {
          successorBBsAreValid = false;
          break;
        }

      ufAssert( successorBBsAreValid );

      // Insert spill code at the beginning of each successor block.
      for ( WIR_BasicBlock &succ : b.getSuccessors() )
        insertSpillCode( clone, r, stackPos, succ, succ.begin(), spillStore );

      return( b );
    } else

    if ( isCall ) {
      // If the preceding instruction is a function call, insert the new spill
      // code at the beginning of the sole succeeding basic block. If this
      // succeeding basic block has, however, more than one predecessor, it may
      // be entered from somewhere else, too. But the spill code to be generated
      // must not be executed if the successor block is not reached from the
      // current basic block b. In that case, insert a new basic block so that
      // the new spill code is inserted immediately after the function call and
      // this new successor basic block has exactly one predecessor which is b.
      WIR_BasicBlock &succ = *(b.getSuccessors().begin());

      if ( succ.getPredecessors().size() > 1 ) {
        WIR_Function &f = b.getFunction();

        // Create a new basic block between the current block b and its succ.
        res = f.insertBasicBlock( f.findBasicBlock( succ ), {} )->get();

        b = res;
        pos = b.getInstructions().end();
      }
    }
  }

  // Due to potential adjustments of the stack pointer, spill-load and -store
  // instructions can be surrounded by address calculation instructions that
  // set and reset the stack pointer. If pos refers to such an already existing
  // load or store instruction with a stack pointer-relative addressing mode, or
  // to one of these address calculation instructions, the new spill code must
  // not be placed between the old existing address calculation instructions.
  if ( isAdjustedLoadOrStoreInstruction( b, pos ) )
    // New spill code shall be inserted before the already existing surrounded
    // load or store instruction. Thus, insert the new spill code before the
    // first LEA instruction.
    --pos;
  else

  if ( ( pos != b.getInstructions().begin() ) &&
       isAdjustedLoadOrStoreInstruction( b, std::prev( pos ) ) )
    // New spill code shall be inserted after the already existing surrounded
    // load or store instruction. Thus, insert the new spill code after the
    // second LEA instruction.
    ++pos;

  // Now that we have correctly adjusted the position before which to insert a
  // new spill instruction, let's generate the spill code.

  list<std::reference_wrapper<WIR_Instruction>> newSpill;

  DOUT(
    "Extending " << string( spillStore ? "mSpillStores" : "mSpillLoads" ) <<
    "[ " << r.getRoot().getID() << " ] by" << endl );

  // Determine the involved processor core.
  WIR_Function &f = b.getFunction();
  WIR_System &sys = f.getCompilationUnit().getSystem();
  const WIR_Section &sec = sys.findSymbol( f ).getSection();
  TC13 &tc = dynamic_cast<TC13 &>( sec.getProcessor() );
  auto &stackPointer = tc.SP();

  // If the stack offset is too large to be encoded as immediate argument in the
  // spill instruction, insert additional address arithmetic instructions before
  // and after the spill code to change the stack pointer temporarily. Pay
  // attention that every offset is sign extended.
  short addressUpperHalf =
    ( ( stackPos + 0x8000 ) / (unsigned int) 0x10000 ) & 0xFFFF;
  short addressLowerHalf = stackPos & 0xFFFF;
  short offset10 = stackPos & 0x3FF;
  short &offset16 = addressLowerHalf;

  auto insertedInsns = mInsertedSpillCode.end();

  if ( ( stackPos < TC_Const16_Signed::getMinValue( 16 ) )  ||
       ( stackPos > TC_Const16_Signed::getMaxValue( 16 ) ) ) {
    // Insert ADDIH.A instructions.
    auto newInstr =
      mInsertedSpillCode.insert(
        insertedInsns,
        markSpillInstruction(
          b.insertInstruction( pos,
            { { TC13::OpCode::ADDIH_A, TC13::OperationFormat::AAC16,
                WIR_RegisterParameter( stackPointer, WIR_Usage::def ),
                WIR_RegisterParameter( stackPointer, WIR_Usage::use ),
                TC_Const16_Signed( addressUpperHalf ) } } )->get(),
          clone ) );
    newSpill.push_back( *newInstr );
    DOUT( *newInstr );

    insertedInsns =
      mInsertedSpillCode.insert(
        insertedInsns,
        markSpillInstruction(
          b.insertInstruction( pos,
            { { TC13::OpCode::ADDIH_A, TC13::OperationFormat::AAC16,
                WIR_RegisterParameter( stackPointer, WIR_Usage::def ),
                WIR_RegisterParameter( stackPointer, WIR_Usage::use ),
                TC_Const16_Signed( -addressUpperHalf ) } } )->get(),
          clone ) );
    newSpill.push_back( *insertedInsns );
    DOUT( *insertedInsns );

    --pos;
  }

  if ( ( ( stackPos < TC_Const10_Signed::getMinValue( 10 ) )  ||
         ( stackPos > TC_Const10_Signed::getMaxValue( 10 ) ) ) &&
       ( ( ( clone.getType() == TC13::RegisterType::aReg ) && spillStore ) ||
         ( clone.getType() == TC13::RegisterType::eReg ) ||
         ( clone.getType() == TC13::RegisterType::pReg ) ) &&
       // Test if sign_ext( addressLowerHalf ) != sign_ext( offset10 ).
       // If so, the bits addressLowerHalf[16:10] will be either all zero or all
       // one.
       ( ( addressLowerHalf & 0xFE00 ) != 0 ) &&
       ( ( ( addressLowerHalf / 0x200 ) & 0x7F ) != 0x7F ) ) {
    // The current stack position does not fit into the 10 bits available in the
    // foreseen spill instruction. Thus, insert stack pointer-adjusting LEA
    // instructions.
    auto newInstr =
      mInsertedSpillCode.insert(
        insertedInsns,
        markSpillInstruction(
          b.insertInstruction( pos,
            { { TC13::OpCode::LEA, TC13::OperationFormat::AAC16BOA,
                WIR_RegisterParameter( stackPointer, WIR_Usage::def ),
                WIR_RegisterParameter( stackPointer, WIR_Usage::use ),
                TC_Const16_Signed( addressLowerHalf ) } } )->get(),
          clone ) );
    newSpill.push_back( *newInstr );
    DOUT( *newInstr );

    insertedInsns =
      mInsertedSpillCode.insert(
        insertedInsns,
        markSpillInstruction(
          b.insertInstruction( pos,
            { { TC13::OpCode::LEA, TC13::OperationFormat::AAC16BOA,
                WIR_RegisterParameter( stackPointer, WIR_Usage::def ),
                WIR_RegisterParameter( stackPointer, WIR_Usage::use ),
                TC_Const16_Signed( -addressLowerHalf ) } } )->get(),
          clone ) );
    newSpill.push_back( *insertedInsns );
    DOUT( *insertedInsns );

    --pos;

    offset10 = 0;
  }

  // Insert the spill instruction itself.
  auto newInstr = mInsertedSpillCode.end();

  if ( spillStore ) {

    if ( clone.getType() == TC13::RegisterType::aReg )
      newInstr =
        mInsertedSpillCode.insert(
          insertedInsns,
          markSpillInstruction(
            b.insertInstruction( pos,
              { { TC13::OpCode::ST_A, TC13::OperationFormat::AC10ABOA,
                  WIR_RegisterParameter( stackPointer, WIR_Usage::use ),
                  TC_Const10_Signed( offset10 ),
                  WIR_RegisterParameter( clone, WIR_Usage::use ) } } )->get(),
            clone ) );
    else

    if ( clone.getType() == TC13::RegisterType::dReg )
      newInstr =
        mInsertedSpillCode.insert(
          insertedInsns,
          markSpillInstruction(
            b.insertInstruction( pos,
              { { TC13::OpCode::ST_W, TC13::OperationFormat::AC16DBOA,
                  WIR_RegisterParameter( stackPointer, WIR_Usage::use ),
                  TC_Const16_Signed( offset16 ),
                  WIR_RegisterParameter( clone, WIR_Usage::use ) } } )->get(),
            clone ) );
    else

    if ( clone.getType() == TC13::RegisterType::eReg )
      newInstr =
        mInsertedSpillCode.insert(
          insertedInsns,
          markSpillInstruction(
            b.insertInstruction( pos,
              { { TC13::OpCode::ST_D, TC13::OperationFormat::AC10EBOA,
                  WIR_RegisterParameter( stackPointer, WIR_Usage::use ),
                  TC_Const10_Signed( offset10 ),
                  WIR_RegisterParameter( clone, WIR_Usage::use ) } } )->get(),
            clone ) );
    else

    if ( clone.getType() == TC13::RegisterType::pReg )
      newInstr =
        mInsertedSpillCode.insert(
          insertedInsns,
          markSpillInstruction(
            b.insertInstruction( pos,
              { { TC13::OpCode::ST_DA, TC13::OperationFormat::AC10PBOA,
                  WIR_RegisterParameter( stackPointer, WIR_Usage::use ),
                  TC_Const10_Signed( offset10 ),
                  WIR_RegisterParameter( clone, WIR_Usage::use ) } } )->get(),
            clone ) );
  } else {
    if ( clone.getType() == TC13::RegisterType::aReg )
      newInstr =
        mInsertedSpillCode.insert(
          insertedInsns,
          markSpillInstruction(
            b.insertInstruction( pos,
              { { TC13::OpCode::LD_A, TC13::OperationFormat::AAC16BOA,
                  WIR_RegisterParameter( clone, WIR_Usage::def ),
                  WIR_RegisterParameter( stackPointer, WIR_Usage::use ),
                  TC_Const16_Signed( offset16 ) } } )->get(),
            clone ) );
    else

    if ( clone.getType() == TC13::RegisterType::dReg )
      newInstr =
        mInsertedSpillCode.insert(
          insertedInsns,
          markSpillInstruction(
            b.insertInstruction( pos,
              { { TC13::OpCode::LD_W, TC13::OperationFormat::DAC16BOA,
                  WIR_RegisterParameter( clone, WIR_Usage::def ),
                  WIR_RegisterParameter( stackPointer, WIR_Usage::use ),
                  TC_Const16_Signed( offset16 ) } } )->get(),
            clone ) );
    else

    if ( clone.getType() == TC13::RegisterType::eReg )
      newInstr =
        mInsertedSpillCode.insert(
          insertedInsns,
          markSpillInstruction(
            b.insertInstruction( pos,
              { { TC13::OpCode::LD_D, TC13::OperationFormat::EAC10BOA,
                  WIR_RegisterParameter( clone, WIR_Usage::def ),
                  WIR_RegisterParameter( stackPointer, WIR_Usage::use ),
                  TC_Const10_Signed( offset10 ) } } )->get(),
            clone ) );
    else

    if ( clone.getType() == TC13::RegisterType::pReg )
      newInstr =
        mInsertedSpillCode.insert(
          insertedInsns,
          markSpillInstruction(
            b.insertInstruction( pos,
              { { TC13::OpCode::LD_DA, TC13::OperationFormat::PAC10BOA,
                  WIR_RegisterParameter( clone, WIR_Usage::def ),
                  WIR_RegisterParameter( stackPointer, WIR_Usage::use ),
                  TC_Const10_Signed( offset10 ) } } )->get(),
            clone ) );
  }

  newSpill.push_back( *newInstr );

  for ( WIR_Instruction &i : newSpill )
    mStackOffsetOfSpillInstruction[ i.getID() ] = stackPos;

  auto &spillCode =
    spillStore ?
      mSpillStores[ r.getRoot().getID() ] : mSpillLoads[ r.getRoot().getID() ];
  spillCode.push_back( std::move( newSpill ) );

  DOUT( *newInstr );

  return( res.get() );
};


/*
  isAdjustedLoadOrStoreInstruction returns whether the given iterator refers to
  a load or a store instruction with a stack pointer relative addressing mode
  which is surrounded by LEA instructions that adjust the stack pointer.

  isAdjustedLoadOrStoreInstruction checks if the code looks like

    lea    A10, A10, const1             # with const1 > 0
    ld__   __, [A10] __                 # pos
    lea    A10, A10, -const2            # with const2 > 0

  or

    lea    A10, A10, const1             # with const1 > 0
    st__   [A10] __, __                 # pos
    lea    A10, A10, -const2            # with const2 > 0
*/
bool TC_GraphColoring::isAdjustedLoadOrStoreInstruction( const WIR_BasicBlock &b,
                                                         std::list<std::reference_wrapper<WIR_Instruction>>::const_iterator pos ) const
{
  DSTART(
    "bool TC_GraphColoring::isAdjustedLoadOrStoreInstruction(const WIR_BasicBlock&, list<reference_wrapper<WIR_Instruction> >::const_iterator) const" );

  auto &iList = b.getInstructions();

  // If pos refers to the very first instruction within b or to b's very last
  // instruction or if pos is the end() iterator or if pos contains no
  // operation, the above code structure is impossible.
  if ( ( pos == iList.begin() ) || ( pos == iList.end() ) ||
       ( pos->get() == iList.back().get() ) ||
       pos->get().getOperations().empty() )
    return( false );

  const WIR_Instruction &i = pos->get();

  // Check whether pos refers to a load or store instruction.
  if ( !i.getOperations().front().get().isMemoryLoad() &&
       !i.getOperations().front().get().isMemoryStore() )
    return( false );

  const WIR_Operation &o = i.getOperations().front().get();

  // Check whether the load/store uses the stack pointer.
  bool usesSP = false;
  for ( WIR_Parameter &p : o )
    if ( p.getType() == WIR_ParameterType::reg ) {
      auto &rp = dynamic_cast<WIR_RegisterParameter &>( p );
      if ( ( rp.isUsed() || rp.isDefUsed() ) &&
           isStackPointer( rp.getRegister() ) ) {
        usesSP = true;
        break;
      }
    }

  if ( !usesSP )
    return( false );

  // Check whether predecessor and successor are stack-adjusting LEAs.
  const WIR_Instruction &predI = std::prev( pos )->get();
  const WIR_Instruction &succI = std::next( pos )->get();

  if ( predI.getOperations().empty() || succI.getOperations().empty() )
    return( false );
  auto &predO = predI.begin()->get();
  auto &succO = succI.begin()->get();

  if ( !TC13::isStackPointerLEA( predO ) || !TC13::isStackPointerLEA( succO ) )
    return( false );

  signed long long predVal =
    ( predO.getOperationFormat() == TC13::OperationFormat::AAC10BOA ) ?
      dynamic_cast<const TC_Const10_Signed &>(
        predO.getExplicitParameters().back().get() ).getSignedValue() :
      dynamic_cast<const TC_Const16_Signed &>(
        predO.getExplicitParameters().back().get() ).getSignedValue();
  signed long long succVal =
    ( succO.getOperationFormat() == TC13::OperationFormat::AAC10BOA ) ?
      dynamic_cast<const TC_Const10_Signed &>(
        succO.getExplicitParameters().back().get() ).getSignedValue() :
      dynamic_cast<const TC_Const16_Signed &>(
        succO.getExplicitParameters().back().get() ).getSignedValue();

  return( ( predVal > 0 ) && ( succVal < 0 ) );
};


/*
  rewriteProgramHook allows to perform TriCore-specific actions after having
  transformed the %WIR code.

  Here, rewriteProgramHook is used to realize handling of dummy parameters that
  need to be attached to CALL instruction in certain cases (see also the lenghty
  comment inside method checkLCRegsAliveAcrossCall).
*/
void TC_GraphColoring::rewriteProgramHook( WIR_Function &f )
{
  DSTART( "virtual void TC_GraphColoring::rewriteProgramHook(WIR_Function&)" );

  (void) f;

  // All lower-context registers alive during function calls identified in this
  // current allocation round are now made permanent by storing them in
  // mLCRegsAliveAcrossCall.
  for ( auto p : mLocalLCRegsAliveAcrossCall )
    for ( WIR_BaseRegister &r : p.second )
      mLCRegsAliveAcrossCall[ p.first ].insert( r );
  mLocalLCRegsAliveAcrossCall.clear();

  // All dummy parameters identified in this current allocation round are now
  // made permanent by storing them in mDummyParameters.
  for ( auto p : mLocalDummyParameters ) {
    WIR_PhysicalRegister &phreg = p.first.get();
    WIR_Instruction &theCall = p.second.get();

    mDummyParameters.push_back(
      theCall.begin()->get().pushBackParameter(
        WIR_RegisterParameter( phreg, WIR_Usage::use, true ) ) );
  }

  mLocalDummyParameters.clear();
};


/*
  getCandidatePhregs returns a set of physical registers that could be used for
  the specified virtual register according to the TriCore's ISA.
*/
WIR_PhysicalRegisterSet TC_GraphColoring::getCandidatePhregs( const WIR_VirtualRegister &r )
{
  DSTART(
    "virtual WIR_PhysicalRegisterSet TC_GraphColoring::getCandidatePhregs(const WIR::WIR_VirtualRegister&)" );

  WIR_PhysicalRegisterSet res;

  // Determine the involved processor core.
  WIR_Function &f = r.getFunction();
  WIR_System &sys = f.getCompilationUnit().getSystem();
  const WIR_Section &sec = sys.findSymbol( f ).getSection();
  TC13 &tc = dynamic_cast<TC13 &>( sec.getProcessor() );

  if ( r.getType() == TC13::RegisterType::aReg ) {
    if ( !mUseOnlyUC ) {
      res.insert( const_cast<TC_ARegP &>( tc.A2() ) );
      res.insert( const_cast<TC_ARegP &>( tc.A3() ) );
      res.insert( const_cast<TC_ARegP &>( tc.A4() ) );
      res.insert( const_cast<TC_ARegP &>( tc.A5() ) );
      res.insert( const_cast<TC_ARegP &>( tc.A6() ) );
      res.insert( const_cast<TC_ARegP &>( tc.A7() ) );
    }
    res.insert( const_cast<TC_ARegP &>( tc.A12() ) );
    res.insert( const_cast<TC_ARegP &>( tc.A13() ) );
    res.insert( const_cast<TC_ARegP &>( tc.A14() ) );
    res.insert( const_cast<TC_ARegP &>( tc.A15() ) );
  } else

  if ( r.getType() == TC13::RegisterType::dReg ) {
    if ( !mUseOnlyUC ) {
      res.insert( const_cast<TC_DRegP &>( tc.D0() ) );
      res.insert( const_cast<TC_DRegP &>( tc.D1() ) );
      res.insert( const_cast<TC_DRegP &>( tc.D2() ) );
      res.insert( const_cast<TC_DRegP &>( tc.D3() ) );
      res.insert( const_cast<TC_DRegP &>( tc.D4() ) );
      res.insert( const_cast<TC_DRegP &>( tc.D5() ) );
      res.insert( const_cast<TC_DRegP &>( tc.D6() ) );
      res.insert( const_cast<TC_DRegP &>( tc.D7() ) );
    }
    res.insert( const_cast<TC_DRegP &>( tc.D8() ) );
    res.insert( const_cast<TC_DRegP &>( tc.D9() ) );
    res.insert( const_cast<TC_DRegP &>( tc.D10() ) );
    res.insert( const_cast<TC_DRegP &>( tc.D11() ) );
    res.insert( const_cast<TC_DRegP &>( tc.D12() ) );
    res.insert( const_cast<TC_DRegP &>( tc.D13() ) );
    res.insert( const_cast<TC_DRegP &>( tc.D14() ) );
    res.insert( const_cast<TC_DRegP &>( tc.D15() ) );
  } else

  if ( r.getType() == TC13::RegisterType::eReg ) {
    if ( !mUseOnlyUC ) {
      res.insert( const_cast<TC_ERegP &>( tc.E0() ) );
      res.insert( const_cast<TC_ERegP &>( tc.E2() ) );
      res.insert( const_cast<TC_ERegP &>( tc.E4() ) );
      res.insert( const_cast<TC_ERegP &>( tc.E6() ) );
    }
    res.insert( const_cast<TC_ERegP &>( tc.E8() ) );
    res.insert( const_cast<TC_ERegP &>( tc.E10() ) );
    res.insert( const_cast<TC_ERegP &>( tc.E12() ) );
    res.insert( const_cast<TC_ERegP &>( tc.E14() ) );
  } else {
    if ( !mUseOnlyUC ) {
      res.insert( const_cast<TC_PRegP &>( tc.P2() ) );
      res.insert( const_cast<TC_PRegP &>( tc.P4() ) );
      res.insert( const_cast<TC_PRegP &>( tc.P6() ) );
    }
    res.insert( const_cast<TC_PRegP &>( tc.P12() ) );
    res.insert( const_cast<TC_PRegP &>( tc.P14() ) );
  }

  // Let's store the current uncolored spill for later use (see method
  // getCandidatePhreg below).
  mUncoloredSpill = const_cast<WIR_VirtualRegister *>( &r );

  return( res );
};


/*
  getCandidatePhreg returns one element from the specified set of registers that
  will finally be used within allocateUncoloredActualSpills for spilling.
*/
const WIR_PhysicalRegister &TC_GraphColoring::getCandidatePhreg( const WIR_PhysicalRegisterSet &candidates )
{
  DSTART(
    "virtual const WIR_PhysicalRegister& TC_GraphColoring::getCandidatePhreg(const WIR_PhysicalRegisterSet&)" );

  if ( candidates.size() == 1 )
    return( candidates.begin()->get() );

  TC13 &tc = dynamic_cast<TC13 &>( candidates.begin()->get().getProcessor() );
  std::reference_wrapper<const WIR_PhysicalRegister> res(
    candidates.begin()->get() );

  list<std::reference_wrapper<const WIR_PhysicalRegister>> orderedPhregs;

  // If possible, we prefer the implicit data/address registers D15 or A15.
  orderedPhregs.push_back( tc.D15() );
  orderedPhregs.push_back( tc.A15() );

  // Next, we prefer the upper-context registers.
  orderedPhregs.push_back( tc.D14() );
  orderedPhregs.push_back( tc.D13() );
  orderedPhregs.push_back( tc.D12() );
  orderedPhregs.push_back( tc.D11() );
  orderedPhregs.push_back( tc.D10() );
  orderedPhregs.push_back( tc.D9() );
  orderedPhregs.push_back( tc.D8() );
  orderedPhregs.push_back( tc.A14() );
  orderedPhregs.push_back( tc.A13() );
  orderedPhregs.push_back( tc.A12() );
  orderedPhregs.push_back( tc.E14() );
  orderedPhregs.push_back( tc.E12() );
  orderedPhregs.push_back( tc.E10() );
  orderedPhregs.push_back( tc.E8() );
  orderedPhregs.push_back( tc.P14() );
  orderedPhregs.push_back( tc.P12() );

  // Finally, we use some lower-context register.
  if ( !mUseOnlyUC ) {
    orderedPhregs.push_back( tc.D1() );
    orderedPhregs.push_back( tc.D0() );
    orderedPhregs.push_back( tc.D3() );
    orderedPhregs.push_back( tc.D7() );
    orderedPhregs.push_back( tc.D6() );
    orderedPhregs.push_back( tc.D5() );
    orderedPhregs.push_back( tc.D2() );
    orderedPhregs.push_back( tc.D4() );
    orderedPhregs.push_back( tc.A3() );
    orderedPhregs.push_back( tc.A7() );
    orderedPhregs.push_back( tc.A6() );
    orderedPhregs.push_back( tc.A5() );
    orderedPhregs.push_back( tc.A2() );
    orderedPhregs.push_back( tc.A4() );
    orderedPhregs.push_back( tc.E0() );
    orderedPhregs.push_back( tc.E6() );
    orderedPhregs.push_back( tc.E2() );
    orderedPhregs.push_back( tc.E4() );
    orderedPhregs.push_back( tc.P6() );
    orderedPhregs.push_back( tc.P2() );
    orderedPhregs.push_back( tc.P4() );

    if ( candidates.count( const_cast<TC_PRegP &>( tc.P6() ) ) )
      return( tc.P6() );
    if ( candidates.count( const_cast<TC_PRegP &>( tc.P4() ) ) )
      return( tc.P4() );
    if ( candidates.count( const_cast<TC_PRegP &>( tc.P2() ) ) )
      return( tc.P2() );
  }

  // Now check the phregs in the specified precedence order.
  for ( const WIR_PhysicalRegister &phreg : orderedPhregs )
    if ( candidates.count( const_cast<WIR_PhysicalRegister &>( phreg ) ) ) {
      res = phreg;
      break;
    }

  WIR_VirtualRegister &vregRoot = mUncoloredSpill->getRoot();
  WIR_RegisterSet root;
  root.insert( vregRoot );
  WIR_RegisterSet emptySet;
  checkLCRegsAliveAcrossCall( res, root, emptySet );

  if ( vregRoot.isHierarchical() ) {
    WIR_RegisterSet c1;
    c1.insert( vregRoot.begin()->get() );
    WIR_RegisterSet c2;
    c2.insert( vregRoot.rbegin()->get() );
    checkLCRegsAliveAcrossCall( res, c1, c2 );
  }

  return( res.get() );
};


/*
  postProcessingHook allows to perform TriCore-specific actions after having
  done register allocation for a function, using e.g., the set of inserted spill
  operations mInsertedSpillCode.

  Here, postProcessingHook is used to realize the TriCore-specific calling
  conventions afterwards.
*/
void TC_GraphColoring::postProcessingHook( WIR_Function &f )
{
  DSTART( "virtual void TC_GraphColoring::postProcessingHook(WIR_Function&)" );

  // First of all, we free some memory that is no longer used.
  f.eraseContainers( WIR_BitValues::getContainerTypeID(), true );

  // We now finally have to ensure that f as a calling function is responsible
  // for preserving any values residing in lower-context registers that are live
  // across a function call (TriCore EABI, section 2.2.1.5).

  mVregsAliveAcrossCall.clear();

  // First, we determine how many additional stack space we need in order to
  // temporarily save lower-context registers on the stack during a function
  // call.
  unsigned int additionalStackSpace = 0;
  for ( auto p : mLCRegsAliveAcrossCall ) {
    WIR_RegisterSet &lcRegs = p.second;
    unsigned int requiredSpace = 0;

    for ( WIR_BaseRegister &r : lcRegs )
      requiredSpace += getStackSize( r );

    if ( requiredSpace > additionalStackSpace )
      additionalStackSpace = requiredSpace;
  }

  if ( additionalStackSpace == 0 )
    return;

  mAdditionalStackSpace += additionalStackSpace;

  DOUT(
    "Additional stack space for caller-saved registers in function '" <<
    f.getName() << "': " << additionalStackSpace << "." << endl );

  // Now, we finally generate the instructions in order to caller-save lower-
  // context registers in f.
  for ( auto p : mLCRegsAliveAcrossCall ) {
    WIR_Instruction &theCall = p.first.get();
    WIR_RegisterSet &lcRegs = p.second;
    unsigned int stackPos = mAdditionalStackSpace - additionalStackSpace;
    WIR_BasicBlock &b = theCall.getBasicBlock();
    auto posi = b.findInstruction( theCall );

    std::reference_wrapper<WIR_BasicBlock> succBB =
      b.getSuccessors().begin()->get();
    if  ( ( mFunctionReturnMoves[ theCall.getID() ] == 0 ) &&
          ( succBB.get().getPredecessors().size() > 1 ) )
      // Create a new basic block between the current block b and its succ.
      succBB =
        f.insertBasicBlock(
          f.findBasicBlock( succBB.get() ), WIR_BasicBlock {} )->get();

    for ( WIR_BaseRegister &r : lcRegs ) {
      // We have to determine a correct position where to insert the required
      // caller-restore AFTER the call. Since a call might define some register,
      // there might be some move instructions immediately after the call that
      // move the function call's result away. Our caller-restore instruction to
      // be generated here must be placed after such a move.
      auto pos = succBB.get().begin();
      std::advance( pos, mFunctionReturnMoves[ theCall.getID() ] );

      // For each lower-context register alive across a call, insert a store
      // immediately before the call, and a load immediately after.
      insertSpillStore( r, r, stackPos, b, posi );
      insertSpillLoad( r, r, stackPos, succBB.get(), pos );

      DACTION(
        BBPOS( theCall );
        DOUT(
          "Inserting caller-saving instructions for function call " << BBID <<
          theCall << endl << std::prev( posi )->get() <<
          std::prev( pos )->get() ); );

      stackPos += getStackSize( r );
    }
  }

  // Remove all artificially inserted dummy parameters of call instructions.
  while ( !mDummyParameters.empty() ) {
    WIR_Parameter &p = mDummyParameters.front();
    mDummyParameters.pop_front();

    WIR_Operation &o = p.getOperation();
    o.eraseParameter( o.findParameter( p ) );
  }

  mLCRegsAliveAcrossCall.clear();
};


/*
  adjustStack allocates additional space in the specified function's stack frame
  and adjusts all stack-related memory accesses accordingly.

  According to the TriCore EABI (section 2.2.2.1), the stack pointer points to
  the bottom (low address) of the stack frame. The stack pointer alignment is 8
  bytes. The argument overflow area for outgoing arguments must be located at
  the bottom (low address end) of the frame, with the first overflow argument at
  zero offset from the stack pointer:

  (Stack
   growing
   direction)
       |
       |   +-------------------------+        (high address)
       |   | Local Variables Frame 1 |
       |   +-------------------------+
       |   | Argument Overflow Area, |
       |   | Function 2 Arguments    |        (first argument passed on stack)
       |   +-------------------------+
       |   | Local Variables Frame 2 |
       |   +-------------------------+
       |   | Argument Overflow Area, |
       |   | Function 3 Arguments    |
       |   +-------------------------+   <--- Stack Pointer (SP) at entry
       V   | Local Variables Frame 3 |        (CALL) to Function 3
           +-------------------------+
           | Argument Overflow Area  |
           +-------------------------+        (low address)
*/
void TC_GraphColoring::adjustStack( WIR_Function &f )
{
  DSTART( "virtual void TC_GraphColoring::adjustStack(WIR_Function&)" );

  if ( ( mAdditionalStackSpace > 0 ) && getVerbosity() )
    ufProgrMsg << ufFile() << "Adjusting stack by " << mAdditionalStackSpace
               << " bytes." << endl;

  TC13::adjustStack( f, mAdditionalStackSpace, mInsertedSpillCode );

  mInsertedSpillCode.clear();
};


/*
  postRACleanup allows to perform very final TriCore-specific cleanup actions,
  particularly after stack frame reorganization.

  Here, postRACleanup is used to remove redundant MOV and SWAP.W instructions.
  Furthermore, don't optimize flags of parameters (indicating accesses to the
  TriCore's argument overflow stack region) are reset.
*/
void TC_GraphColoring::postRACleanup( WIR_Function &f )
{
  DSTART( "virtual void TC_GraphColoring::postRACleanup(WIR_Function&)" );

  if ( getVerbosity() )
    ufProgrMsg << ufFile() << "Simplifying code." << endl;

  for ( WIR_BasicBlock &b : f ) {
    auto it = b.begin();

    while ( it != b.end() ) {
      WIR_Instruction &i1 = it->get();

      if ( i1.getOperations().empty() ) {
        ++it;
        continue;
      }

      WIR_Operation &o1 = i1.getOperations().front().get();

      for ( WIR_Parameter &p : o1 )
        p.setDontOptimize( false );

      // Redundant moves:
      //   mov          <reg>, <reg>                            or
      //   mov.aa       <reg>, <reg>
      if ( o1.isMove() && ( getUseOfMove( o1 ) == getDefOfMove( o1 ) ) ) {
        DACTION(
          BBPOS( i1 );
          DOUT(
            "Removing redundant move " << BBID << tricore << o1 << endl ); );
        it = b.eraseInstruction( it );
        continue;
      }

      if ( i1 == b.getInstructions().back().get() ) {
        ++it;
        continue;
      }

      auto pos = std::next( it );
      WIR_Instruction &i2 = pos->get();
      if ( i2.getOperations().empty() ) {
        ++it;
        continue;
      }
      WIR_Operation &o2 = i2.getOperations().front().get();

      // Redundant MOV.A/MOV.D:
      //   mov.a        <reg1>, <reg2>
      //   mov.d        <reg2>, <reg1>                          or
      //
      //   mov.d        <reg1>, <reg2>
      //   mov.a        <reg2>, <reg1>
      if ( ( ( o1.getOpCode() == TC13::OpCode::MOV_A ) &&
             ( o1.getOperationFormat() != TC13::OperationFormat::SAC4_1 ) &&
             ( o2.getOpCode() == TC13::OpCode::MOV_D ) ) ||
           ( ( o1.getOpCode() == TC13::OpCode::MOV_D ) &&
             ( o2.getOpCode() == TC13::OpCode::MOV_A ) &&
             ( o2.getOperationFormat() != TC13::OperationFormat::SAC4_1 ) ) ) {
        WIR_BaseRegister &r11 =
          dynamic_cast<WIR_RegisterParameter &>(
            o1.getExplicitParameter( 1 ) ).getRegister();
        WIR_BaseRegister &r12 =
          dynamic_cast<WIR_RegisterParameter &>(
            o1.getExplicitParameter( 2 ) ).getRegister();

        WIR_BaseRegister &r21 =
          dynamic_cast<WIR_RegisterParameter &>(
            o2.getExplicitParameter( 1 ) ).getRegister();
        WIR_BaseRegister &r22 =
          dynamic_cast<WIR_RegisterParameter &>(
            o2.getExplicitParameter( 2 ) ).getRegister();

        if ( ( r11 == r22 ) && ( r12 == r21 ) ) {
          DACTION(
            BBPOS( i2 );
            DOUT(
              "Removing redundant mov.a/mov.d " << BBID << tricore << o2 <<
              endl ); );
          b.eraseInstruction( pos );
          continue;
        }
      }

      // Redundant SWAP.W/SWAP.W:
      //   swap.w       [<areg>]c, <reg>
      //   swap.w       [<areg>]c, <reg>
      if ( ( o1.getOpCode() == TC13::OpCode::SWAP_W ) &&
           ( o1.getOperationFormat() == TC13::OperationFormat::AC10DBOA_2 ) &&
           ( o2.getOpCode() == TC13::OpCode::SWAP_W ) &&
           ( o2.getOperationFormat() == TC13::OperationFormat::AC10DBOA_2 ) ) {
        WIR_BaseRegister &r11 =
          dynamic_cast<WIR_RegisterParameter &>(
            o1.getExplicitParameter( 1 ) ).getRegister();
        long long offset1 =
          dynamic_cast<WIR_BaseImmediateParameter &>(
            o1.getExplicitParameter( 2 ) ).getSignedValue();
        WIR_BaseRegister &r12 =
          dynamic_cast<WIR_RegisterParameter &>(
            o1.getExplicitParameter( 3 ) ).getRegister();

        WIR_BaseRegister &r21 =
          dynamic_cast<WIR_RegisterParameter &>(
            o2.getExplicitParameter( 1 ) ).getRegister();
        long long offset2 =
          dynamic_cast<WIR_BaseImmediateParameter &>(
            o2.getExplicitParameter( 2 ) ).getSignedValue();
        WIR_BaseRegister &r22 =
          dynamic_cast<WIR_RegisterParameter &>(
            o2.getExplicitParameter( 3 ) ).getRegister();

        if ( ( r11 == r21 ) && ( offset1 == offset2 ) && ( r12 == r22 ) ) {
          DACTION(
            BBPOS( i2 );
            DOUT(
              "Removing redundant swap.w " << BBID << tricore << o2 <<
              endl ); );
          b.eraseInstruction( pos );
          DACTION(
            BBPOS( i1 );
            DOUT(
              "Removing redundant swap.w " << BBID << tricore << o1 <<
              endl ); );
          it = b.eraseInstruction( it );
          continue;
        }
      }

      ++it;
    }
  }
};


/*
  For a list of instructions implementing one spill-load or -store,
  getPhregOfSpill determines that physical register that is actually
  spill-loaded or -stored.
*/
WIR_PhysicalRegister &TC_GraphColoring::getPhregOfSpill( const std::list<std::reference_wrapper<WIR_Instruction>> &spill )
{
  DSTART(
    "virtual WIR_PhysicalRegister& TC_GraphColoring::getPhregOfSpill(const list<reference_wrapper<WIR_Instruction> >&)" );

  for ( WIR_Instruction &i : spill )
    for ( WIR_Operation &o : i )
      if ( o.isMemoryLoad() || o.isMemoryStore() ) {
        for ( WIR_Parameter &p : o )
          if ( p.getType() == WIR_ParameterType::reg ) {
            auto &regP = dynamic_cast<WIR_RegisterParameter &>( p );

            if ( regP.getRegister().isPhysical() &&
                 !TC13::isSP( regP.getRegister() ) )
              return(
                dynamic_cast<WIR_PhysicalRegister &>( regP.getRegister() ) );
          }
      }

  // If we reach this point, no spill-load/-store was observed up 'till now so
  // that the current spill must be a rematerialization. In this case, we
  // determine the physical register that is defined by the rematerialization
  // instructions.

  // Check the very last rematerialization instruction.
  auto it = spill.rbegin();
  WIR_Operation &o = it->get().begin()->get();
  auto pIt = o.begin();
  for ( ; pIt != o.end(); ++pIt )
    if ( pIt->get().getType() == WIR_ParameterType::reg ) {
      auto &regP = dynamic_cast<WIR_RegisterParameter &>( pIt->get() );

      if ( regP.getRegister().isPhysical() && regP.isDefined() )
        break;
    }

  auto &lastPhreg =
    dynamic_cast<WIR_PhysicalRegister &>(
      dynamic_cast<WIR_RegisterParameter &>( pIt->get() ).getRegister() );

  // Check the second last rematerialization instruction, if any.
  ++it;
  WIR_PhysicalRegister *prevPhreg = nullptr;
  if ( it != spill.rend() ) {
    o = it->get().begin()->get();

    for ( WIR_Parameter &p : o )
      if ( p.getType() == WIR_ParameterType::reg ) {
        auto &regP = dynamic_cast<WIR_RegisterParameter &>( p );

        if ( regP.getRegister().isPhysical() && regP.isDefined() ) {
          prevPhreg =
            dynamic_cast<WIR_PhysicalRegister *>( &regP.getRegister() );
          break;
        }
      }
  }

  if ( ( prevPhreg == nullptr ) ||
       ( prevPhreg->getRoot() != lastPhreg.getRoot() ) )
    return( lastPhreg );
  else
    return( lastPhreg.getRoot() );
};

}       // namespace WIR
