/*

   This source file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Intermediate Representation Framework

   and is property of its respective copyright holder. It must neither be used
   nor published even in parts without explicit written permission.

   Copyright 2021 - 2022

   Hamburg University of Technology (TUHH)
   Institute of Embedded Systems
   21071 Hamburg
   Germany

   http://www.tuhh.de/es/esd/research/wcc

*/

/*!
  @file rvgraphcoloring.cc
  @brief This file implements a RISC-V-specific graph-coloring based register
         allocator.

  @author Rasmus Mecklenburg <Rasmus.Mecklenburg@tuhh.de>
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
#include <stack>

// Include boost headers
#include <boost/current_function.hpp>

// Include libuseful headers
#include <libuseful/debugmacros.h>
#include <libuseful/exceptions.h>
#include <libuseful/io.h>

// Include WIR headers
#include <wir/wir.h>
#include <arch/riscv/rv32imc.h>

// Include local headers
#include "rvgraphcoloring.h"


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
RV_GraphColoring::RV_GraphColoring( WIR_System &s, bool verbosity ) :
  WIR_Optimization { s },
  WIR_GraphColoring { s, verbosity },
  mUncoloredSpill { nullptr },
  mStackAccessLatency { 0 }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );
};


/*
  Default constructor for compilation unit-level optimization.
*/
RV_GraphColoring::RV_GraphColoring( WIR_CompilationUnit &c, bool verbosity ) :
  WIR_Optimization { c },
  WIR_GraphColoring { c, verbosity },
  mUncoloredSpill { nullptr },
  mStackAccessLatency { 0 }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );
};


/*
  Default constructor for function-level optimization.
*/
RV_GraphColoring::RV_GraphColoring( WIR_Function &f, bool verbosity ) :
  WIR_Optimization { f },
  WIR_GraphColoring { f, verbosity },
  mUncoloredSpill { nullptr },
  mStackAccessLatency { 0 }
{
  DSTART( "RV_GraphColoring::RV_GraphColoring(WIR_Function&, bool)" );
};


/*
  Destructor.
*/
RV_GraphColoring::~RV_GraphColoring( void )
{
  DSTART( "virtual RV_GraphColoring::~RV_GraphColoring()" );
};


//
// Protected class methods
//

/*
  runOptimization allocates registers in the given function.
*/
void RV_GraphColoring::runOptimization( WIR_Function &f )
{
  DSTART( "virtual void RV_GraphColoring::runOptimization(WIR_Function&)" );

  // TODO: Add bit-true data flow analysis for RISC-V.

  WIR_GraphColoring::runOptimization( f );
};


/*
  saveBestSolutionHook allows to save processor-specific allocation data in the
  course of Bernstein's best-of-three spilling heuristic.

  Here, saveBestSolutionHook is used to save TriCore-specific information on
  lower-context registers alive across function calls.
*/
void RV_GraphColoring::saveBestSolutionHook( void )
{
  DSTART( "virtual void RV_GraphColoring::saveBestSolutionHook()" );

  mBestLocalCallerSavedRegsAliveAcrossCall =
    std::move( mLocalCallerSavedRegsAliveAcrossCall );
  mBestLocalDummyParameters = std::move( mLocalDummyParameters );

  mLocalCallerSavedRegsAliveAcrossCall.clear();
  mLocalDummyParameters.clear();
};


/*
  restoreBestSolutionHook allows to restore processor-specific allocation data
  in the course of Bernstein's best-of-three spilling heuristic.

  Here, restoreBestSolutionHook is used to restore TriCore-specific information
  on lower-context registers alive across function calls.
*/
void RV_GraphColoring::restoreBestSolutionHook( void )
{
  DSTART( "virtual void RV_GraphColoring::restoreBestSolutionHook()" );

  mLocalCallerSavedRegsAliveAcrossCall =
    std::move( mBestLocalCallerSavedRegsAliveAcrossCall );
  mLocalDummyParameters = std::move( mBestLocalDummyParameters );

  mBestLocalCallerSavedRegsAliveAcrossCall.clear();
  mBestLocalDummyParameters.clear();
};


/*
  createPhregs sets up the lists mPhregs and mPhregsForPrecoloringOnly of all
  RISC-V-specific physical registers contained in the specified WIR function.
*/
void RV_GraphColoring::createPhregs( WIR_Function &f )
{
  DSTART( "virtual void RV_GraphColoring::createPhregs(WIR_Function&)" );

  // Determine the involved processor core.
  WIR_System &sys = f.getCompilationUnit().getSystem();
  const WIR_Section &sec = sys.findSymbol( f ).getSection();
  WIR_BaseProcessor &p = sec.getProcessor();

  // It must be a RISC-V, of course.
  ufAssert( p.getISAName().find( "RV32I" ) == 0 );
  RV32I &rv = dynamic_cast<RV32I &>( p );

  // Determine memory access latency of the stack.
  ufAssert( rv.containsSection( ".stack" ) );
  WIR_MemoryRegion &stackMem = rv.findSection( ".stack" )->get().getRegion();
  mStackAccessLatency = stackMem.getMaxDelay();

  // Determine the code access latencies of all basic blocks.
  mBBAccessLatency.clear();
  for ( WIR_BasicBlock &b : f ) {
    auto &region = sys.findSymbol( b ).getSection().getRegion();
    mBBAccessLatency[ b.getID() ] = region.getMaxDelay();
  }

  // RISC-V registers subject to register allocation.
  mPhregs = {
    const_cast<RV_RegP &>( rv.x5() ), const_cast<RV_RegP &>( rv.x6() ),
    const_cast<RV_RegP &>( rv.x7() ),
    const_cast<RV_RegP &>( rv.x9() ), const_cast<RV_RegP &>( rv.x10() ),
    const_cast<RV_RegP &>( rv.x11() ), const_cast<RV_RegP &>( rv.x12() ),
    const_cast<RV_RegP &>( rv.x13() ), const_cast<RV_RegP &>( rv.x14() ),
    const_cast<RV_RegP &>( rv.x15() ), const_cast<RV_RegP &>( rv.x16() ),
    const_cast<RV_RegP &>( rv.x17() ), const_cast<RV_RegP &>( rv.x18() ),
    const_cast<RV_RegP &>( rv.x19() ), const_cast<RV_RegP &>( rv.x20() ),
    const_cast<RV_RegP &>( rv.x21() ), const_cast<RV_RegP &>( rv.x22() ),
    const_cast<RV_RegP &>( rv.x23() ), const_cast<RV_RegP &>( rv.x24() ),
    const_cast<RV_RegP &>( rv.x25() ), const_cast<RV_RegP &>( rv.x26() ),
    const_cast<RV_RegP &>( rv.x27() ), const_cast<RV_RegP &>( rv.x28() ),
    const_cast<RV_RegP &>( rv.x29() ), const_cast<RV_RegP &>( rv.x30() ),
    const_cast<RV_RegP &>( rv.x31() ) };

  // RISC-V registers subject to pre-coloring (e.g., if used in some snippet of
  // inline assembly) but NOT subject to register allocation. Here, we list all
  // registers not included in mPhregs.
  mPhregsForPrecoloringOnly = {
    const_cast<RV_RegP &>( rv.x0() ), const_cast<RV_RegP &>( rv.x1() ),
    const_cast<RV_RegP &>( rv.x2() ), const_cast<RV_RegP &>( rv.x3() ),
    const_cast<RV_RegP &>( rv.x4() ), const_cast<RV_RegP &>( rv.x8() ) };

  // Initialize some RISC-V-specific data structures.
  mCallerSavedRegsAliveAcrossCall.clear();
  mDummyParameters.clear();

  // mOrderedREGsAliveAcrossCall contains all RISC-V registers which are
  // preserved across calls, i.e., which are callee-saved according to the
  // RISC-V ABI, section 1.1.
  mOrderedREGsAliveAcrossCall = {
    const_cast<RV_RegP &>( rv.x8() ), const_cast<RV_RegP &>( rv.x9() ),
    const_cast<RV_RegP &>( rv.x18() ), const_cast<RV_RegP &>( rv.x19() ),
    const_cast<RV_RegP &>( rv.x20() ), const_cast<RV_RegP &>( rv.x21() ),
    const_cast<RV_RegP &>( rv.x22() ), const_cast<RV_RegP &>( rv.x23() ),
    const_cast<RV_RegP &>( rv.x24() ), const_cast<RV_RegP &>( rv.x25() ),
    const_cast<RV_RegP &>( rv.x26() ), const_cast<RV_RegP &>( rv.x27() ) };

  // All physical registers to be used for coloring, in their precedence order.
  mOrderedPhregs = {
    // We prefer the temporary registers x5-x7 and x28-31, because they are
    // caller-saved and thus not alive across function calls.
    const_cast<RV_RegP &>( rv.x5() ), const_cast<RV_RegP &>( rv.x6() ),
    const_cast<RV_RegP &>( rv.x7() ), const_cast<RV_RegP &>( rv.x28() ),
    const_cast<RV_RegP &>( rv.x29() ), const_cast<RV_RegP &>( rv.x30() ),
    const_cast<RV_RegP &>( rv.x31() ),

    // Next, we use the callee-saved registers x9 and x18 - x27.
    const_cast<RV_RegP &>( rv.x9() ),
    const_cast<RV_RegP &>( rv.x18() ), const_cast<RV_RegP &>( rv.x19() ),
    const_cast<RV_RegP &>( rv.x20() ), const_cast<RV_RegP &>( rv.x21() ),
    const_cast<RV_RegP &>( rv.x22() ), const_cast<RV_RegP &>( rv.x23() ),
    const_cast<RV_RegP &>( rv.x24() ), const_cast<RV_RegP &>( rv.x25() ),
    const_cast<RV_RegP &>( rv.x26() ), const_cast<RV_RegP &>( rv.x27() ),

    // Finally, we use the argument registers x10 - x17.
    const_cast<RV_RegP &>( rv.x10() ), const_cast<RV_RegP &>( rv.x11() ),
    const_cast<RV_RegP &>( rv.x12() ), const_cast<RV_RegP &>( rv.x13() ),
    const_cast<RV_RegP &>( rv.x14() ), const_cast<RV_RegP &>( rv.x15() ),
    const_cast<RV_RegP &>( rv.x16() ), const_cast<RV_RegP &>( rv.x17() ) };
};


/*
  isCallerSaved checks wether the specified physical register is caller-saved.
*/
bool RV_GraphColoring::isCallerSaved( const WIR_PhysicalRegister &r ) const
{
  DSTART(
    "bool RV_GraphColoring::isCallerSaved(const WIR_PhysicalRegister&) const" );

  // mOrderedREGsAliveAcrossCall contains all registers that are not
  // caller-saved and is defined in createPhregs.
  for ( auto &phregAliveAcrossCall : mOrderedREGsAliveAcrossCall )
    if ( r == phregAliveAcrossCall )
      return( false );

  return ( true );
};


/*
  isStackPointer returns whether the specified WIR register is the RISC-V's
  stack pointer.
*/
bool RV_GraphColoring::isStackPointer( const WIR_BaseRegister &r ) const
{
  DSTART( "bool RV_GraphColoring::isStackPointer( const WIR_BaseRegister &r ) "
          "const" );

  return( RV32I::isSP( r ) );
};


/*
  checkCallerSavedRegsAliveAcrossCall checks whether the two specified virtual
  leaf registers are mapped to a caller-saved physical register and whether they
  are alive across a function call.

  If so, the respective function call and the physical register are stored in
  mCallerSavedRegsAliveAcrossCall.
*/
void RV_GraphColoring::checkCallerSavedRegsAliveAcrossCall( const WIR_PhysicalRegister &phreg,
                                                            const WIR_RegisterSet &vregs )
{
  DSTART(
    "void RV_GraphColoring::checkCallerSavedRegsAliveAcrossCall(const "
    "WIR_PhysicalRegister&, const WIR_RegisterSet&)" );

  if ( isCallerSaved( phreg ) ) {
    // If a caller-saved register was chosen, we have to check whether the
    // associated virtual register is live across some function call. If so, we
    // must make sure to save/restore the chosen caller-saved register across
    // such a function call later.
    RV32I &rv = dynamic_cast<RV32I &>( phreg.getProcessor() );

    for ( auto p : mVregsAliveAcrossCall ) {
      WIR_Instruction &theCall = p.first.get();
      WIR_Operation &theCallOp = theCall.getOperations().front().get();
      auto &liveVregs = p.second;

      // First, determine whether the current call instruction defines register
      // R10, i.e., whether the call returns some result via this register.
      bool callDefinesR10 = false;

      for ( WIR_Parameter &p : theCallOp ){
        if ( p.getType() == WIR_ParameterType::reg ) {
          auto &rp = dynamic_cast<WIR_RegisterParameter &>( p );
          auto &r = rp.getRegister();

          if ( rp.isDefined() ) {
            if ( r == rv.x10() )
              callDefinesR10 = true;

          }
        }
      }

      for ( WIR_BaseRegister &r : vregs ) {
        if ( r.isVirtual() &&
             liveVregs.count( dynamic_cast<WIR_VirtualRegister &>( r ) ) ) {
          // The current virtual register is live across the call.
          mLocalCallerSavedRegsAliveAcrossCall[ theCall ].insert(
            const_cast<WIR_PhysicalRegister &>( phreg ) );

          DACTION(
            BBPOS( theCall );
            DOUT(
              "Storing caller-saved register " << r.getName() << "/" <<
              phreg.getName() << " to be caller-saved across call " << BBID <<
              theCallOp << endl ); );

          // See lengthy comment above.
           if ( ( phreg == rv.x10() ) && callDefinesR10 )
              mLocalDummyParameters.push_back(
                { const_cast<WIR_PhysicalRegister &>( phreg ), theCall } );
        }
      }
    }
  }
};


/*
  rewriteProgramHook allows to perform TriCore-specific actions after having
  transformed the %WIR code.

  Here, rewriteProgramHook is used to realize handling of dummy parameters that
  need to be attached to CALL instruction in certain cases (see also the lenghty
  comment inside method checkCallerSavedRegsAliveAcrossCall).
*/
void RV_GraphColoring::rewriteProgramHook( WIR_Function &f )
{
  DSTART( "virtual void RV_GraphColoring::rewriteProgramHook(WIR_Function&)" );

  (void) f;

  // All lower-context registers alive during function calls identified in this
  // current allocation round are now made permanent by storing them in
  // mCallerSavedRegsAliveAcrossCall.
  for ( auto p : mLocalCallerSavedRegsAliveAcrossCall )
    for ( WIR_BaseRegister &r : p.second )
      mCallerSavedRegsAliveAcrossCall[ p.first ].insert( r );
  mLocalCallerSavedRegsAliveAcrossCall.clear();

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
  buildProcessorSpecificInterferences adds edges to the interference graph
  expressing RISC-V-specific interferences.

  Furthermore, buildProcessorSpecificInterferences checks which virtual
  registers are live-out across function calls inside f.
*/
void RV_GraphColoring::buildProcessorSpecificInterferences( WIR_Function &f,
                                                            WIR_InterferenceGraph &igraph )
{
  DSTART(
    "virtual void RV_GraphColoring::buildProcessorSpecificInterferences("
    "WIR_Function&, WIR_InterferenceGraph&)" );

  // For RISC-V, there are no processor-specific interferences to be added to
  // igraph.
  (void) igraph;

  static map<WIR_id_t, bool> firstInvocation;

  if ( firstInvocation.find( f.getID() ) == firstInvocation.end() )
    firstInvocation[ f.getID() ] = true;

  // Next, check which virtual registers are alive across function calls. This
  // information is required later in order to correctly handle the RISC-V's
  // caller-saved registers during function calls.
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
      }

  firstInvocation[ f.getID() ] = false;

  // Clear some data structures at the beginning of each and every allocation
  // round.
  mLocalCallerSavedRegsAliveAcrossCall.clear();
  mLocalDummyParameters.clear();
};


/*
  isFunctionReturnMove checks whether the move operation stores the result of a
  function call somewhere.
*/
bool RV_GraphColoring::isFunctionReturnMove( const WIR_Operation &o ) const
{
  DSTART(
    "bool RV_GraphColoring::isFunctionReturnMove(const WIR_Operation&) const" );

  if ( !o.isMove() )
    return( false );

  WIR_BaseRegister &useReg = getUseOfMove( o );

  // First, verify that the register used by the move is D2, D3 or E2.
  bool isR10 = false;

  if ( useReg.isPhysical() && ( useReg.getName() == "10" )  )
    isR10 = true;

  if ( useReg.isVirtual() && isPrecolored( useReg ) ) {
    WIR_PhysicalRegister &preg = mPrecolored.at( useReg ).get();

    if ( preg.getName() == "10" )
      isR10 = true;
  }

  if ( !isR10 )
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

  For the RISC-V register allocator, high-priority registers are kept in set
  mHighPriorityRegs. Thus, this method only checks whether r is in
  mHighPriorityRegs or not.
*/
bool RV_GraphColoring::isPriorityRegister( const WIR_VirtualRegister &r ) const
{
  DSTART(
    "virtual bool RV_GraphColoring::isPriorityRegister(const "
    "WIR_VirtualRegister&) const" );

  return( mHighPriorityRegs.count( r.getID() ) );
};


/*
  getSpillLoadCosts returns the RISC-V-specific costs of one single spill-load
  for the specified register parameter.
*/
unsigned int RV_GraphColoring::getSpillLoadCosts( const WIR_RegisterParameter &p ) const
{
  DSTART(
    "virtual unsigned int RV_GraphColoring::getSpillLoadCosts(const "
    "WIR_RegisterParameter&) const" );

  auto &b = p.getOperation().getInstruction().getBasicBlock();
  auto blockLatency = mBBAccessLatency.at( b.getID() );

  return( mStackAccessLatency + blockLatency );
};


/*
  getSpillStoreCosts returns the RISC-V-specific costs of one single spill-store
  for the specified register parameter.
*/
unsigned int RV_GraphColoring::getSpillStoreCosts( const WIR_RegisterParameter &p ) const
{
  DSTART(
    "virtual unsigned int RV_GraphColoring::getSpillStoreCosts(const "
    "WIR_RegisterParameter&) const" );

  return( getSpillLoadCosts( p ) );
};


/*
  getMoveCosts returns the RISC-V-specific costs of one single move operation
  that can be omitted due to spilling.
*/
unsigned int RV_GraphColoring::getMoveCosts( const WIR_Operation &o ) const
{
  DSTART(
    "virtual unsigned int RV_GraphColoring::getMoveCosts(const "
    "WIR_Operation&) const" );

  auto &b = o.getInstruction().getBasicBlock();

  if ( ( o.getOpCode() == RV32I::OpCode::MOV ) ||
       ( o.getOpCode() == RV32IC::OpCode::CMV ) )
    return( mBBAccessLatency.at( b.getID() ) );

  throw( ufFatalError( "This should never happen...", false ) );
};


/*
  getUseOfMove returns the used register of the specified RISC-V WIR move
  operation.
*/
WIR_BaseRegister &RV_GraphColoring::getUseOfMove( const WIR_Operation &o ) const
{
  DSTART(
    "virtual WIR_BaseRegister& RV_GraphColoring::getUseOfMove(const "
    "WIR_Operation&) const" );

  return(
    dynamic_cast<WIR_RegisterParameter &>(
      o.getExplicitParameter( 2 ) ).getRegister() );
};


/*
  getDefOfMove returns the defined register of the specified RISC-V WIR move
  operation.
*/
WIR_BaseRegister &RV_GraphColoring::getDefOfMove( const WIR_Operation &o ) const
{
  DSTART(
    "virtual WIR_BaseRegister& RV_GraphColoring::getDefOfMove(const "
    "WIR_Operation&) const" );

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
bool RV_GraphColoring::avoidCoalescing( const WIR_Operation &o,
                                        const WIR_BaseRegister &r1,
                                        const WIR_BaseRegister &r2,
                                        const WIR_InterferenceGraph &igraph
                                        ) const
{
  DSTART( "virtual bool RV_GraphColoring::avoidCoalescing(const WIR_Operation&,"
          " const WIR_BaseRegister&, const WIR_BaseRegister&, const"
          " WIR_InterferenceGraph&) const"  );

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

  if ( isCallerSaved( *phreg ) ) {
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
  getRematerializationInstructions returns a list of RISC-V instructions for
  one single recomputation of the specified used parameter.
*/
std::list<WIR_Instruction *> RV_GraphColoring::getRematerializationInstructions( const WIR_RegisterParameter &p ) const
{
  DSTART(
    "virtual list<WIR_Instruction*> RISCV_GraphColoring::"
    "getRematerializationInstructions(const WIR_RegisterParameter&) const" );

  (void) p;

  return( list<WIR_Instruction *> {} );
};


/*
  selectColors assigns actual colors to the RISC-V leaf registers in the
  specified vector.

  This method must not yet assign colors to the interference graph - this is
  done elsewhere. In order to determine feasible colors for the leaf registers,
  this method should make use of WIR_InterferenceGraph::getPossibleColors(). It
  must be ensured that the returned map is either empty or contains exactly one
  entry per leaf. It must hold that none of the colors used in this returned map
  is already used for adjacent interference graph nodes.

  The registers are prioritized according to mOrderedPhregs.
*/
WIR_GraphColoring::WIR_ColorMap RV_GraphColoring::selectColors( const std::vector<std::reference_wrapper<WIR_VirtualRegister>> &leafs,
                                                                const WIR_InterferenceGraph &igraph)
{
  DSTART(
    "virtual WIR_GraphColoring::WIR_ColorMap RV_GraphColoring::selectColors("
    "const vector<reference_wrapper<WIR_VirtualRegister>>&, const "
    "WIR_InterferenceGraph&)" );

  WIR_GraphColoring::WIR_ColorMap res;

  ufAssert( leafs.size() == 1 );
  set<unsigned int> possibleColors = igraph.getPossibleColors( leafs.front() );

  WIR_VirtualRegister &c1 = leafs.front().get();

  // Determine whether the VREG to be colored is alive across function calls.
  DACTION(
    bool vregIsAliveAcrossCall = false;
    for ( auto p : mVregsAliveAcrossCall )
      if ( p.second.count( c1 )  ) {
        vregIsAliveAcrossCall = true;
        break;
      }

    DOUT(
      "c1 = " << c1.getName() << "\tis " <<
      ( vregIsAliveAcrossCall ? "" : "not " ) << "alive across call." <<
      endl ); );

  // Check the phregs in the specified precedence order.
  for ( WIR_PhysicalRegister &phreg : mOrderedPhregs ) {
    unsigned int color = igraph.getColorOfPhreg( phreg );

    if ( possibleColors.count( color ) ) {
      res[ c1 ] = color;

      WIR_RegisterSet aliases_c1 = igraph.getCoalescedAliases( c1 );
      aliases_c1.insert( c1 );

      // Check if the selected color is a caller-saved register and alive across
      // calls. If this is the case, it is saved in
      // mCallerSavedRegsAliveAcrossCall.
      checkCallerSavedRegsAliveAcrossCall( phreg, aliases_c1 );

      break;
    }
  }

  return( res );
};


/*
  isSpillStore checks whether the specified instruction spill-stores a certain
  virtual register in the RISC-V ISA.
*/
bool RV_GraphColoring::isSpillStore( const WIR_Instruction &i,
                                     const WIR_VirtualRegister &r ) const
{
  DSTART(
    "virtual bool RV_GraphColoring::isSpillStore(const WIR_Instruction&, "
    "const WIR_VirtualRegister&) const" );

  WIR_Operation &o = i.getOperations().front().get();

  if ( o.getOpCode() == RV32I::OpCode::SW ) {
    WIR_BaseRegister &sp =
      dynamic_cast<WIR_RegisterParameter &>(
        o.getExplicitParameter( 3 ) ).getRegister();
    WIR_BaseRegister &d =
      dynamic_cast<WIR_RegisterParameter &>(
        o.getExplicitParameter( 1 ) ).getRegister();

    return ( isStackPointer( sp ) && ( d == r ) );
  }

  return( false );
};


/*
  isSpillLoad checks whether the specified instruction spill-loads a certain
  virtual register in the RISC-V ISA.
*/
bool RV_GraphColoring::isSpillLoad( const WIR_Instruction &i,
                                    const WIR_VirtualRegister &r ) const
{
  DSTART(
    "virtual bool RV_GraphColoring::isSpillLoad(const WIR_Instruction&, "
    "const WIR_VirtualRegister&) const" );

  WIR_Operation &o = i.getOperations().front().get();

  if ( o.getOpCode() == RV32I::OpCode::LW ) {
    WIR_BaseRegister &sp =
      dynamic_cast<WIR_RegisterParameter &>(
        o.getExplicitParameter( 3 ) ).getRegister();
    WIR_BaseRegister &d =
      dynamic_cast<WIR_RegisterParameter &>(
        o.getExplicitParameter( 1 ) ).getRegister();

    return ( isStackPointer( sp ) && ( d == r ) );
  }

  return( false );
};


/*
  getStackPosOfSubReg returns the stack position of some child register, if the
  root of the entire register hierarchy is located in the specified stack
  position.

  Note: Since RISC-V registers are not hierarchical and thus have no child
        register, this method always returns the position of the given root
        register on the stack.
*/
unsigned int RV_GraphColoring::getStackPosOfSubReg( const WIR_VirtualRegister &r,
                                                    unsigned int rootPos ) const
{
  DSTART(
    "virtual unsigned int RV_GraphColoring::getStackPosOfSubReg(const "
    "WIR_VirtualRegister&, unsigned int) const" );

  (void) r;

  return( rootPos );
};


/*
  insertSpillLoad inserts RISC-V code for a spill-load of a register into the
  WIR.

  insertSpillLoad is responsible to add all generated spill-load instructions to
  map mSpillLoads.
*/
void RV_GraphColoring::insertSpillLoad( const WIR_BaseRegister &clone,
                                        const WIR_BaseRegister &r,
                                        int stackPos, WIR_BasicBlock &b,
                                        std::list<std::reference_wrapper<WIR_Instruction>>::const_iterator pos )
{
  DSTART(
    "virtual void RV_GraphColoring::insertSpillLoad(const WIR_BaseRegister&, "
    "const WIR_BaseRegister&, int, WIR_BasicBlock&, "
    "list<reference_wrapper<WIR_Instruction> >::const_iterator)" );

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
      DOUT( "before position " << riscv << BBID << pos->get() << endl );
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
      cout << riscv << it->get(); );

  if ( newBB != b ) {
    // TODO: Add update of back-annotation mapping!
  }
};


/*
  insertSpillStore inserts RISC-V code for a spill-store of a register into the
  WIR.

  insertSpillStore is responsible to add all generated spill-store instructions
  to map mSpillStores.
*/
void RV_GraphColoring::insertSpillStore( const WIR_BaseRegister &clone,
                                         const WIR_BaseRegister &r,
                                         int stackPos, WIR_BasicBlock &b,
                                         std::list<std::reference_wrapper<WIR_Instruction>>::const_iterator pos )
{
  DSTART(
    "virtual void RV_GraphColoring::insertSpillStore(const WIR_BaseRegister&, "
    "const WIR_BaseRegister&, int, WIR_BasicBlock&, "
    "list<reference_wrapper<WIR_Instruction> >::const_iterator)" );

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
      DOUT( "before position " << riscv << BBID << pos->get() << endl );
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
      cout << riscv << it->get(); );

  if ( newBB != b ) {
    // TODO: Add update of back-annotation mapping!
  }
};


/*
  insertSpillCode inserts RISC-V code for a spill-load or spill-store of a
  register into the WIR.
*/
WIR_BasicBlock &RV_GraphColoring::insertSpillCode( const WIR_BaseRegister &clone,
                                                   const WIR_BaseRegister &r,
                                                   int stackPos,
                                                   WIR_BasicBlock &b,
                                                   std::list<std::reference_wrapper<WIR_Instruction>>::const_iterator pos,
                                                   bool spillStore )
{
  DSTART(
    "virtual WIR_BasicBlock& RV_GraphColoring::insertSpillCode(const "
    "WIR_BaseRegister&, const WIR_BaseRegister&, int, WIR_BasicBlock&, "
    "list<reference_wrapper<WIR_Instruction> >::const_iterator, bool)" );

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
    // first ADDI instruction.
    --pos;
  else

  if ( ( pos != b.getInstructions().begin() ) &&
       isAdjustedLoadOrStoreInstruction( b, std::prev( pos ) ) )
    // New spill code shall be inserted after the already existing surrounded
    // load or store instruction. Thus, insert the new spill code after the
    // second ADDI instruction.
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
  RV32I &rv = dynamic_cast<RV32I &>( sec.getProcessor() );
  auto &stackPointer = rv.SP();

  short offset12 = stackPos & 0xFFF;

  auto insertedInsns = mInsertedSpillCode.end();

  if ( stackPos > RV_Const12_Signed::getMaxValue( 12 ) ) {
    // The stack offset is too large to fit into a const12. As a remedy, we need
    // to move the stack pointer closer to the destination.

    // stackPos, with the last 12 bits set to zero.
    // The stack pointer will have to be moved by this amount.
    int stackPointerMoveAmount =
      ( stackPos / (unsigned int) 0x1000 ) * (unsigned int) 0x1000;

    // TODO: The following code must be broken. If stackPos does not fit into
    //       12 bits, then simply clearing the least-significant 12 bits above
    //       in stackPointerMoveAmount still yields a value > getMaxValue( 12 )
    //       which does not fit into the two ADDI instructions below.
    auto newInstr =
      mInsertedSpillCode.insert(
        insertedInsns,
        markSpillInstruction(
          b.insertInstruction( pos,
            { { RV32I::OpCode::ADDI, RV32I::OperationFormat::RRC12_1,
                WIR_RegisterParameter( stackPointer, WIR_Usage::def ),
                WIR_RegisterParameter( stackPointer, WIR_Usage::use ),
                RV_Const12_Signed( stackPointerMoveAmount ) } } )->get(),
          clone ) );
    newSpill.push_back( *newInstr );
    DOUT( *newInstr );

    insertedInsns =
      mInsertedSpillCode.insert(
        insertedInsns,
        markSpillInstruction(
          b.insertInstruction( pos,
            { { RV32I::OpCode::ADDI, RV32I::OperationFormat::RRC12_1,
                WIR_RegisterParameter( stackPointer, WIR_Usage::def ),
                WIR_RegisterParameter( stackPointer, WIR_Usage::use ),
                RV_Const12_Signed( -stackPointerMoveAmount ) } } )->get(),
          clone ) );
    newSpill.push_back( *insertedInsns );
    DOUT( *insertedInsns );

    --pos;
  }

  // Insert the spill instruction itself.
  auto newInstr = mInsertedSpillCode.end();

  if ( spillStore ) {
    newInstr =
      mInsertedSpillCode.insert(
        insertedInsns,
        markSpillInstruction(
          b.insertInstruction( pos,
            { { RV32I::OpCode::SW, RV32I::OperationFormat::RC12R_2,
                WIR_RegisterParameter( clone, WIR_Usage::use ),
                RV_Const12_Signed( offset12 ),
                WIR_RegisterParameter( stackPointer, WIR_Usage::use ) } } )->get(),
          clone ) );
  } else {
    newInstr =
      mInsertedSpillCode.insert(
        insertedInsns,
        markSpillInstruction(
          b.insertInstruction( pos,
            { { RV32I::OpCode::LW, RV32I::OperationFormat::RC12R_1,
                WIR_RegisterParameter( clone, WIR_Usage::def ),
                RV_Const12_Signed( offset12 ),
                WIR_RegisterParameter( stackPointer, WIR_Usage::use ) } } )->get(),
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
  which is surrounded by ADDI instructions that adjust the stack pointer.

  isAdjustedLoadOrStoreInstruction checks if the code looks like

    addi   x2, x2, const1               # with const1 > 0
    l__    __, __(x2)                   # pos
    addi   x2, x2, -const2              # with const2 > 0

  or

    addi   x2, x2, const1               # with const1 > 0
    s__    __, __(x2)                   # pos
    addi   x2, x2, -const2              # with const2 > 0

  TODO: Since the current implementation of handling offsets larger than 12 bits
        violates the RISC-V ABI, this needs to be rewritten once that the
        implementation is changed to be ABI-compatible.
*/
bool RV_GraphColoring::isAdjustedLoadOrStoreInstruction( const WIR_BasicBlock &b,
                                                         std::list<std::reference_wrapper<WIR_Instruction>>::const_iterator pos ) const
{
  DSTART(
    "bool RV_GraphColoring::isAdjustedLoadOrStoreInstruction(const "
    "WIR_BasicBlock&, "
    "list<reference_wrapper<WIR_Instruction> >::const_iterator) const" );

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
      if ( rp.isUsed() || rp.isDefUsed() &&
           isStackPointer( rp.getRegister() ) ) {
        usesSP = true;
        break;
      }
    }

  if ( !usesSP )
    return ( false );

  // Check whether predecessor and successor are stack-adjusting ADDIs.
  const WIR_Instruction &predI = std::prev( pos )->get();
  const WIR_Instruction &succI = std::next( pos )->get();

  if ( predI.getOperations().empty() || succI.getOperations().empty() )
    return( false );
  auto &predO = predI.begin()->get();
  auto &succO = succI.begin()->get();

  if ( !RV32I::isStackPointerADDI( predO ) ||
       !RV32I::isStackPointerADDI( succO ) )
    return ( false );

  signed long long predVal =
    dynamic_cast<const RV_Const12_Signed &>(
      predO.getExplicitParameter( 3 ) ).getSignedValue();
  signed long long succVal =
    dynamic_cast<const RV_Const12_Signed &>(
      succO.getExplicitParameter( 3 ) ).getSignedValue();

  return ( ( predVal > 0 ) && ( succVal < 0 ) );
};


/*
  getCandidatePhregs returns a set of physical registers that could be used for
  the specified virtual register according to the RISC-V's ISA.
*/
WIR_PhysicalRegisterSet RV_GraphColoring::getCandidatePhregs( const WIR_VirtualRegister &r )
{
  DSTART(
    "virtual WIR_PhysicalRegisterSet RV_GraphColoring::getCandidatePhregs("
    "const WIR::WIR_VirtualRegister&)" );

  WIR_PhysicalRegisterSet res;
  WIR_Function &f = r.getFunction();
  WIR_System &sys = f.getCompilationUnit().getSystem();
  const WIR_Section &sec = sys.findSymbol( f ).getSection();
  RV32I &rv = dynamic_cast<RV32I &>( sec.getProcessor() );

  res.insert( const_cast<RV_RegP &>( rv.x5() ) );
  res.insert( const_cast<RV_RegP &>( rv.x6() ) );
  res.insert( const_cast<RV_RegP &>( rv.x7() ) );
  res.insert( const_cast<RV_RegP &>( rv.x9() ) );
  res.insert( const_cast<RV_RegP &>( rv.x10() ) );
  res.insert( const_cast<RV_RegP &>( rv.x11() ) );
  res.insert( const_cast<RV_RegP &>( rv.x12() ) );
  res.insert( const_cast<RV_RegP &>( rv.x13() ) );
  res.insert( const_cast<RV_RegP &>( rv.x14() ) );
  res.insert( const_cast<RV_RegP &>( rv.x15() ) );
  res.insert( const_cast<RV_RegP &>( rv.x16() ) );
  res.insert( const_cast<RV_RegP &>( rv.x17() ) );
  res.insert( const_cast<RV_RegP &>( rv.x18() ) );
  res.insert( const_cast<RV_RegP &>( rv.x19() ) );
  res.insert( const_cast<RV_RegP &>( rv.x20() ) );
  res.insert( const_cast<RV_RegP &>( rv.x21() ) );
  res.insert( const_cast<RV_RegP &>( rv.x21() ) );
  res.insert( const_cast<RV_RegP &>( rv.x22() ) );
  res.insert( const_cast<RV_RegP &>( rv.x23() ) );
  res.insert( const_cast<RV_RegP &>( rv.x24() ) );
  res.insert( const_cast<RV_RegP &>( rv.x25() ) );
  res.insert( const_cast<RV_RegP &>( rv.x26() ) );
  res.insert( const_cast<RV_RegP &>( rv.x27() ) );
  res.insert( const_cast<RV_RegP &>( rv.x28() ) );
  res.insert( const_cast<RV_RegP &>( rv.x29() ) );
  res.insert( const_cast<RV_RegP &>( rv.x30() ) );
  res.insert( const_cast<RV_RegP &>( rv.x31() ) );

  // Let's store the current uncolored spill for later use (see method
  // getCandidatePhreg below).
  mUncoloredSpill = const_cast<WIR_VirtualRegister *>( &r );

  return( res );
};


/*
  getCandidatePhreg returns one element from the specified set of registers that
  will finally be used within allocateUncoloredActualSpills for spilling.
*/
const WIR_PhysicalRegister &RV_GraphColoring::getCandidatePhreg(const WIR_PhysicalRegisterSet &candidates )
{
  DSTART(
    "virtual const WIR_PhysicalRegister& "
    "RV_GpaphColoring::getCandidatePhreg(const WIR_PhysicalRegisterSet&" );

  if ( candidates.size() == 1 )
    return( candidates.begin()->get() );

  std::reference_wrapper<const WIR_PhysicalRegister> res(
    candidates.begin()->get() );

  vector<std::reference_wrapper<WIR_PhysicalRegister>> *orderedPhregs =
    &mOrderedPhregs;

  // Now check the phregs in the specified precedence order.
  for ( WIR_PhysicalRegister &phreg : *orderedPhregs )
    if ( candidates.count( const_cast<WIR_PhysicalRegister &>( phreg ) ) ) {
      res = phreg;
      break;
    }

  WIR_VirtualRegister &vregRoot = mUncoloredSpill->getRoot();
  WIR_RegisterSet root;
  root.insert( vregRoot );
  checkCallerSavedRegsAliveAcrossCall( res, root );

  return( res.get() );
};



/*
  postProcessingHook allows to perform TriCore-specific actions after having
  done register allocation for a function, using e.g., the set of inserted spill
  operations mInsertedSpillCode.

  Here, postProcessingHook is used to realize the TriCore-specific calling
  conventions afterwards.
*/
void RV_GraphColoring::postProcessingHook( WIR_Function &f )
{
  DSTART( "virtual void RV_GraphColoring::postProcessingHook(WIR_Function&)" );

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
  for ( auto p : mCallerSavedRegsAliveAcrossCall ) {
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
  for ( auto p : mCallerSavedRegsAliveAcrossCall ) {
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

  mCallerSavedRegsAliveAcrossCall.clear();
};


/*
  adjustStack allocates additional space in the specified function's stack frame
  and adjusts all stack-related memory accesses accordingly.

  According to the RISC-V ABI (section 2.1, Integer Calling Convention), the
  stack grows downwards (towards lower addresses) and the stack pointer shall be
  aligned to a 128-bit boundary upon procedure entry. The first argument passed
  on the stack is located at offset zero of the stack pointer on function entry;
  following arguments are stored at correspondingly higher addresses.

  In the standard ABI, the stack pointer must remain aligned throughout
  procedure execution. [...]

  Procedures must not rely upon the persistence of stack-allocated data whose
  addresses lie below the stack pointer.

  (Stack
   growing
   direction)
       |
       |   +-------------------------+      (high address)
       |   | Local Variables Func 1  |
       |   +-------------------------+
       |   | Argument Area for func- |
       |   | tions called by Func 1  |      (first argument passed on stack)
       |   +-------------------------+
       |   | Local Variables Func 2  |
       |   +-------------------------+
       |   | Argument Area for func- |
       |   | tions called by Func 2  |
       |   +-------------------------+ <--- Stack Pointer (SP) at entry
       V   | Local Variables Func 3  |      (CALL) to Function 3
           +-------------------------+
           | Argument Area for func- |
           | tions called by Func 3  |
           +-------------------------+ <--- Stack Pointer (SP) after stack
           |                         |      allocation of Function 3
           |           ...           |
           +-------------------------+      (low address)
*/
void RV_GraphColoring::adjustStack( WIR_Function &f )
{
  DSTART( "virtual void RV_GraphColoring::adjustStack(WIR_Function&)" );

  if ( ( mAdditionalStackSpace > 0 ) && getVerbosity() )
    ufProgrMsg << ufFile() << "Adjusting stack by " << mAdditionalStackSpace
               << " bytes." << endl;

  RV32I::adjustStack( f, mAdditionalStackSpace, mInsertedSpillCode );
  mInsertedSpillCode.clear();
};


/*
  postRACleanup allows to perform very final TriCore-specific cleanup actions,
  particularly after stack frame reorganization.

  Here, postRACleanup is used to remove redundant MOV and SWAP.W instructions.
  Furthermore, don't optimize flags of parameters (indicating accesses to the
  TriCore's argument overflow stack region) are reset.
*/
void RV_GraphColoring::postRACleanup( WIR_Function &f )
{
  DSTART( "virtual void RV_GraphColoring::postRACleanup(WIR_Function&)" );

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

      ++it;
    }
  }
};


/*
  For a list of instructions implementing one spill-load or -store,
  getPhregOfSpill determines that physical register that is actually
  spill-loaded or -stored.
*/
WIR_PhysicalRegister &RV_GraphColoring::getPhregOfSpill( const std::list<std::reference_wrapper<WIR_Instruction>> &spill )
{
  DSTART(
    "virtual WIR_PhysicalRegister& RV_GraphColoring::getPhregOfSpill("
    "const list<reference_wrapper<WIR_Instruction> >&)" );

  for ( WIR_Instruction &i : spill )
    for ( WIR_Operation &o : i )
      if ( o.isMemoryLoad() || o.isMemoryStore() ) {
        for ( WIR_Parameter &p : o )
          if ( p.getType() == WIR_ParameterType::reg ) {
            auto &regP = dynamic_cast<WIR_RegisterParameter &>( p );

            if ( regP.getRegister().isPhysical() &&
                 !RV32I::isSP( regP.getRegister() ) )
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
