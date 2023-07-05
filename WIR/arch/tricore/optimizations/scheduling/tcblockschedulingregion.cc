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
  @file tcblockschedulingregion.cc
  @brief This file implements a TriCore-specific class representing basic block
         regions in which scheduling is performed.

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
#include <sstream>
#include <string>

// Include boost headers
#include <boost/current_function.hpp>

// Include libuseful headers
#include <libuseful/debugmacros.h>

// Include WIR headers
#include <wir/wir.h>
#include <arch/tricore/tc131.h>

// Include local headers
#include "tcblockschedulingregion.h"
#include "tcschedulinginfo.h"


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
TC_BlockSchedulingRegion::TC_BlockSchedulingRegion( WIR_BasicBlock &b,
                                                    bool verbosity,
                                                    bool keepTmpFiles ) :
  WIR_BlockSchedulingRegion { b, verbosity, keepTmpFiles },
  mCallRetSituation { false }
{
  DSTART(
    "TC_BlockSchedulingRegion::TC_BlockSchedulingRegion(WIR_BasicBlock&, bool, bool)" );

  determineCallRetSituation();
};


/*
  Destructor.
*/
TC_BlockSchedulingRegion::~TC_BlockSchedulingRegion( void )
{
  DSTART( "virtual TC_BlockSchedulingRegion::~TC_BlockSchedulingRegion()" );
};


/*
  moveOperations moves a bundle of TriCore operations to a new position after a
  given instruction within a scheduling region.

  All operations of a bundle are assumed to be executed in the same execution
  cycle, i.e., in parallel. Thus, moveOperations groups them altogether into a
  single, novel WIR instruction.
*/
WIR_Instruction *TC_BlockSchedulingRegion::moveOperations( std::list<WIR_Operation *> &b,
                                                           const WIR_Instruction *pred )
{
  DSTART(
    "virtual WIR_Instruction* TC_BlockSchedulingRegion::moveOperations(list<WIR_Operation*>&, const WIR_Instruction*)" );

  // A TriCore bundle always consists of 2 operations. However, one of them
  // could potentially be a nullptr that we have to check accordingly.
  ufAssert( b.size() == 2 );

  auto *o1 = b.front();
  auto *o2 = b.back();

  // Create a new instruction and insert it immediately after pred.
  WIR_BasicBlock &bb = *(mBasicBlocks.front());
  auto itPred = ( pred != nullptr ) ? bb.findInstruction( *pred ) : bb.end();
  auto itNew = bb.insertInstruction( next( itPred ), WIR_Instruction {} );
  auto &i = itNew->get();

  // Move the first operation.
  if ( o1 != nullptr ) {
    // Ensure that the first operation has no unresolved dependences.
    ufAssert( getUnscheduledPredecessors( *o1 ).empty() );

    // Move the bundle's first operation.
    DACTION(
      stringstream str;
      str << *o1;

      if ( pred != nullptr ) {
        stringstream str1;
        str1 << *pred;
        DOUT(
          "Moving '" << str.str().substr( 8 ) << "' behind '" <<
          str1.str().substr( 8 ) << "'." << endl );
      } else
        DOUT(
          "Moving '" << str.str().substr( 8 ) << "' at beginning of basic " <<
          "block '" << bb.getName() << "'." << endl ); );
    i.moveOperation( *o1 );
    o1 = b.front() = &(i.getOperations().back().get());
  }

  // Move the second operation.
  if ( o2 != nullptr ) {
    auto unschedPreds = getUnscheduledPredecessors( *o2 );

    if ( !unschedPreds.empty() ) {
      // If o2 has unscheduled predecessors, there must only be exactly one of
      // them, namely o1.
      ufAssert( unschedPreds.size() == 1 );
      ufAssert( ( o1 != nullptr ) && ( unschedPreds.begin()->get() == *o1 ) );
    }

    DACTION(
      stringstream str;
      str << *o2;

      if ( o1 != nullptr ) {
        stringstream str1;
        str1 << *o1;
        DOUT(
          "Moving '" << str.str().substr( 8 ) << "' behind '" <<
          str1.str().substr( 8 ) << "'." << endl );
      } else

      if ( pred != nullptr ) {
        stringstream str1;
        str1 << *pred;
        DOUT(
          "Moving '" << str.str().substr( 8 ) << "' behind '" <<
          str1.str().substr( 8 ) << "'." << endl );
      } else
        DOUT(
          "Moving '" << str.str().substr( 8 ) << "' at beginning of basic " <<
          "block '" << bb.getName() << "'." << endl ); );
    i.moveOperation( *o2 );
    b.back() = &(i.getOperations().back().get());
  }

  return( &i );
};


/*
  isSiliconBugNOP determines whether an operation is a NOP related to a silicon
  bug.
*/
bool TC_BlockSchedulingRegion::isSiliconBugNOP( const WIR_Operation &o ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  return( mSiliconBugNOPs.count( const_cast<WIR_Operation &>( o ) ) );
};


/*
  postProcessingHook performs TriCore-specific actions after having done
  instruction scheduling for a block region.

  This method removes NOP operations originally required as silicon bug fix, and
  that became obsolete after instruction scheduling.
*/
void TC_BlockSchedulingRegion::postProcessingHook( void )
{
  DSTART( "virtual void TC_BlockSchedulingRegion::postProcessingHook()" );

  for ( WIR_Operation &nop : mSiliconBugNOPs ) {
    auto &nopInstr = nop.getInstruction();

    WIR_Operation &o = mSiliconBugNOPPartner.at( nop.getID() );
    auto &oInstr = o.getInstruction();

    // Check whether o and nop are still direct successors in their basic block.
    bool neighbors = false;

    auto nopIt = nopInstr.getBasicBlock().findInstruction( nopInstr );
    auto oIt = oInstr.getBasicBlock().findInstruction( oInstr );

    if ( ( next( nopIt ) == oIt ) || ( next( oIt ) == nopIt ) )
      neighbors = true;

    if ( !neighbors )
      // Something has been scheduled between o and nop, turning the nop
      // obsolete.
      nopInstr.getBasicBlock().eraseInstruction( nopIt );
  }
};


//
// Protected class methods
//

/*
  checkUCert checks whether there is an uncertain TriCore-specific dependence
  between an operation and some register parameter.
*/
bool TC_BlockSchedulingRegion::checkUCert( const WIR_Operation &o1,
                                           const WIR_RegisterParameter &rp2 ) const
{
  DSTART(
    "virtual bool TC_BlockSchedulingRegion::checkUCert(const WIR_Operation&, const WIR_RegisterParameter&) const" );

  auto &o2 = rp2.getOperation();

  // In case of memory accesses, we assume a dependence for safety reasons.
  return(
    ( o1.isMemoryLoad() && o2.isMemoryStore() ) ||
    ( o1.isMemoryStore() && o2.isMemoryLoad() ) ||
    ( o1.isMemoryStore() && o2.isMemoryStore() ) );
};


/*
  checkCtrl checks whether there is a TriCore-specific control dependence
  between an operation and some register parameter.
*/
bool TC_BlockSchedulingRegion::checkCtrl( const WIR_Operation &o1,
                                          const WIR_RegisterParameter &rp2 ) const
{
  DSTART(
    "virtual bool TC_BlockSchedulingRegion::checkCtrl(const WIR_Operation&, const WIR_RegisterParameter&) const" );

  // Control dependences occur for any kind of function call/return or
  // conditional/unconditional jump, irrespective of the nature of operation o1.
  (void) o1;
  auto &o2 = rp2.getOperation();

  return( o2.isCall() || o2.isIndirectCall() || o2.isReturn() || o2.isJump() );
};


/*
  computeLatency computes the TriCore-specific latency between two dependent
  WIR operations.
*/
long long TC_BlockSchedulingRegion::computeLatency( const WIR_Operation &o1,
                                                    const WIR_Operation &o2,
                                                    WIR_DGEdgeType t )
{
  DSTART(
    "virtual long long int TC_BlockSchedulingRegion::computeLatency(const WIR_Operation&, const WIR_Operation&, WIR_DGEdgeType)" );

  long long lat = TC_SchedulingInfo::getLatency( o1, o2 );
  long long stall = computeSiliconBugStall( o1, o2 );
  long long res = 0;

  // Check latency for write-after-read dependences.
  if ( ( t == WIR_DGEdgeType::war ) && ( res < lat + stall ) )
    res = lat + stall;

  // Check latency for write-after-write dependences.
  if ( t == WIR_DGEdgeType::waw ) {
    // The minimal latency in case of a WAW dependence is 0, if both operations
    // are a pair of IP/LS operations executed in parallel.
    // If the operations are a pair of LS/IP operations, however, one stall
    // cycle is counted.
    long long tmpStall = stall;
    if ( ( lat == 0 ) && ( tmpStall == 0 ) )
      tmpStall = 1;

    if ( res < lat + tmpStall ) {
      stall = tmpStall;
      res = lat + stall;
    }
  }

  // Check latency for read-after-write dependences.
  if ( t == WIR_DGEdgeType::raw ) {
    long long tmpStall = stall;

    // Address register loads have a built-in stall cycle (cf. TriCore Compiler
    // Writer's Guide, section 2.1.2.4, page 35).
    if ( ( o1.getOpCode() == TC13::OpCode::LD_A ) ||
         ( o1.getOpCode() == TC13::OpCode::LD_DA ) )
      if ( tmpStall == 0 )
        tmpStall = 1;

    // MAC operations followed by a dependent store also induce a stall cycle.
    if ( ( TC_SchedulingInfo::getType( o1 ) ==
             TC_SchedulingInfo::OperationType::mac ) &&
         o2.isMemoryStore() )
      if ( tmpStall == 0 )
        tmpStall = 1;

    if ( res < lat + tmpStall ) {
      stall = tmpStall;
      res = lat + stall;
    }
  }

  // Check latency for uncertain dependences.
  if ( ( t == WIR_DGEdgeType::ucert ) && ( res < lat + stall ) )
    res = lat + stall;

  // Check latency for control dependences.
  if ( t == WIR_DGEdgeType::ctrl ) {
    long long tmpStall = stall;

    // Store operations directly followed by a function return that involves a
    // context restore have a built-in stall cycle (cf. TriCore Compiler
    // Writer's Guide, section 2.1.2.9, page 38).
    if ( o1.isMemoryStore() && o2.isReturn() )
      if ( tmpStall == 0 )
        tmpStall = 1;

    if ( res < lat + tmpStall ) {
      stall = tmpStall;
      res = lat + stall;
    }
  }

  return( res );
};


/*
  getLatency determines the number of clock cycles it takes from issuing the
  given operation until availability of its results.
*/
long long TC_BlockSchedulingRegion::getLatency( const WIR_Operation &o )
{
  DSTART(
    "virtual long long int TC_BlockSchedulingRegion::getLatency(const WIR_Operation&)" );

  return( TC_SchedulingInfo::getLatency( o ) );
};


/*
  computeSiliconBugStall computes the amount of additional stall cycles to be
  accounted for if two operations depend on each other due to a silicon bug fix.
*/
long long TC_BlockSchedulingRegion::computeSiliconBugStall( const WIR_Operation &o1,
                                                            const WIR_Operation &o2 )
{
  DSTART(
    "long long int TC_BlockSchedulingRegion::computeSiliconBugStall(const WIR_Operation&, const WIR_Operation&)" );

  auto &i1 = o1.getInstruction();
  auto &i2 = o2.getInstruction();

  // Check for silicon bug-related comment attached to both instructions.
  if ( i1.containsContainers( WIR_Comment::getContainerTypeID() ) &&
       i2.containsContainers( WIR_Comment::getContainerTypeID() ) ) {
    auto &c1 = i1.getContainers<WIR_Comment>();
    auto &c2 = i2.getContainers<WIR_Comment>();

    string s;

    for ( WIR_Comment &c : c1 )
      if ( c.getText().substr( 0, 20 ) == "Fix for silicon bug " ) {
        s = c.getText();
        break;
      }

    if ( !s.empty() )
      for ( WIR_Comment &c : c2 )
        if ( c.getText() == s ) {
          // Both operations refer to the same silicon bug fix. Account for one
          // stall cycle if none of the operations is a NOP.
          if ( ( o1.getOpCode() != TC13::OpCode::NOP ) &&
               ( o2.getOpCode() != TC13::OpCode::NOP ) )
            return( 1 );

          // Add silicon bug-related NOPs to internal map.
          if ( o1.getOpCode() == TC13::OpCode::NOP ) {
            mSiliconBugNOPs.insert( const_cast<WIR_Operation &>( o1 ) );
            mSiliconBugNOPPartner.insert(
              { o1.getID(), const_cast<WIR_Operation &>( o2 ) } );
          }
          if ( o2.getOpCode() == TC13::OpCode::NOP ) {
            mSiliconBugNOPs.insert( const_cast<WIR_Operation &>( o2 ) );
            mSiliconBugNOPPartner.insert(
              { o2.getID(), const_cast<WIR_Operation &>( o1 ) } );
          }
        }
  }

  return( 0 );
};


/*
  getStartCycle determines a TriCore operation's absolutely earliest execution
  cycle if that operation were scheduled as the region's very first operation.
*/
long long TC_BlockSchedulingRegion::getStartCycle( const WIR_Operation &o ) const
{
  DSTART(
    "virtual long long int TC_BlockSchedulingRegion::getStartCycle(const WIR_Operation&) const" );

  if ( mCallRetSituation &&
       ( TC_SchedulingInfo::getType( o ) ==
           TC_SchedulingInfo::OperationType::ls ) ) {
    // Create one cycle of gap between the very first LS operation and a
    // preceding CALL/RET operation in order to schedule around the CALL/RET if
    // possible.
    DACTION(
      stringstream str;
      str << o;
      DOUT(
        "Start cycle of '" << str.str().substr( 8 ) << "' is 2." << endl ); );
    return( 2 );
  }

  DACTION(
    stringstream str;
    str << o;
    DOUT(
      "Start cycle of '" << str.str().substr( 8 ) << "' is 1." << endl ); );
  return( 1 );
};


/*
  determineCallRetSituation determines whether the current region is a direct
  successor of some CALL or RET operation, or whether the region itself ends
  with a RET.
*/
void TC_BlockSchedulingRegion::determineCallRetSituation( void )
{
  DSTART( "void TC_BlockSchedulingRegion::determineCallRetSituation()" );

  WIR_BasicBlock &b = *(mBasicBlocks.front());

  // A small lambda to determine a basic block's last operation.
  auto lastOp = []( const WIR_BasicBlock &b ) -> WIR_Operation * {
    WIR_Operation *oPtr = nullptr;

    for ( auto rIt = b.rbegin(); rIt != b.rend(); ++rIt ) {
      auto &i = rIt->get();
      if ( !i.getOperations().empty() ) {
        oPtr = &(i.begin()->get());
        break;
      }
    }

    return( oPtr );
  };

  for ( WIR_BasicBlock &pred : b.getPredecessors() ) {
    auto *op = lastOp( pred );

    if ( op && ( op->isCall() || op->isIndirectCall() || op->isReturn() ) ) {
      mCallRetSituation = true;
      DOUT(
        "Block region '" << b.getName() << "' is " <<
        string( mCallRetSituation ? "" : "not " ) <<
        "in a CALL/RET scenario." << endl );
      return;
    }
  }

  // If b is a function's very first basic block, it naturally is preceded by a
  // CALL.
  mCallRetSituation = ( b.getFunction().begin()->get() == b );
  DOUT(
    "Block region '" << b.getName() << "' is " <<
    string( mCallRetSituation ? "" : "not " ) << "in a CALL/RET scenario." <<
    endl );
};

}       // namespace WIR
