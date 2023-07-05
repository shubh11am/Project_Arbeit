/*

   This source file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Compiler Framework

   and is property of its respective copyright holder. It must neither be used
   nor published even in parts without explicit written permission.

   Copyright 2022

   Hamburg University of Technology (TUHH)
   Institute of Embedded Systems
   21071 Hamburg
   Germany

   http://www.tuhh.de/es/esd/research/wcc

*/

/*!
  @file wirflowfactupdater.cc
  @brief This file implements various functions to update and delete %WIR flow
         facts.

  @author Til Mauersberger <Til.Mauersberger@tuhh.de>
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
#include <libuseful/io.h>

// Inlcude WIR headers
#include <wir/wir.h>


//
// Code section
//

namespace WIR {


using namespace std;


/*
  Namespace WIR_FlowFactUpdater provides update mechanisms for WIR flow facts.

  Because of the strong dependence of flow facts on a program's control flow,
  flow facts have to be updated every time the (control flow of the) program may
  be changed. For this purpose, WIR_FlowFactUpdater provides some functions
  which have to be used in WIR optimizations. The programmer of such
  optimizations is responsible for the correct use of the update mechanisms.

  Warning: A wrong or a forgotten update of flow facts within a WIR optimization
           could result in unsafe WCET estimates without any warning or hint!
*/
namespace WIR_FlowFactUpdater {

//
// Update of flow restrictions
//

/*
  replaceBasicBlock replaces a basic block in flow restrictions by another one.

  replaceBasicBlock should be used if a basic block is available which has
  exactly the same execution frequency as the original one has had, and if both
  basic blocks belong to the same function of the programm.

  replaceBasicBlock examines all flow restrictions that b refers to via a
  WIR_FlowFactRef and replaces all occurrences of b by bNew therein.

  Loop bounds are not touched.
*/
void replaceBasicBlock( WIR_BasicBlock &b, WIR_BasicBlock &bNew )
{
  DSTART(
    "WIR_FlowFactUpdater::replaceBasicBlock(WIR::WIR_BasicBlock&, "
    "WIR::WIR_BasicBlock&)" );

  DOUT(
    "WIR_FlowFactUpdater::replaceBasicBlock( '" << b.getName() << "', '" <<
    bNew.getName() << "' )" << endl );

  // Get FlowFactRef.
  WIR_FlowFactRef &ref = WIR_FlowFactRef::get( b );

  if ( ref.getFlowFacts().empty() ) {
    DOUT( "Nothing to do." << endl );
    return;
  }

  // Update flow restrictions.
  auto flowRestrictions = ref.getFlowFacts<WIR_FlowRestriction>();
  for ( WIR_FlowRestriction &fr : flowRestrictions ) {
    DOUT( "Changing flow restriction '" << fr << "' to" << endl );

    fr.replaceSummand( b, bNew );

    DOUT( "                          '" << fr << "'." << endl );
  }

  return;
};


/*
  estimateBasicBlock estimates a basic block in flow restrictions by another one
  or by zero.

  estimateBasicBlock should be used if no basic blocks with the same execution
  frequency as b are available.

  All occurrences of b in the less-equal sides of flow restrictions will be
  estimated by zero, occurrences in the greater-equal sides will be replaced by
  bNew. For this purpose, the execution frequency of bNew has to be
  greater-equal than the one of b.

  Loop bounds are not touched.
*/
void estimateBasicBlock( WIR_BasicBlock &b, WIR_BasicBlock &bNew )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  DOUT(
    "WIR_FlowFactUpdater::estimateBasicBlock( '" << b.getName() << "', '" <<
    bNew.getName() << "' )" << endl );

  // Get FlowFactRef.
  WIR_FlowFactRef &ref = WIR_FlowFactRef::get( b );

  if ( ref.getFlowFacts().empty() ) {
    DOUT( "Nothing to do." << endl );
    return;
  }

  // Do symbolic update of flow restrictions.
  auto flowRestrictions = ref.getFlowFacts<WIR_FlowRestriction>();
  for ( WIR_FlowRestriction &fr : flowRestrictions ) {
    if ( fr.isPartOfLeq( b ) ) {
      // Less-equal side: Estimate matching summands by zero.
      DOUT( "Changing flow restriction '" << fr << "' to" << endl );

      fr.eraseSummand( b );

      DOUT( "                          '" << fr << "'." << endl );
    }

    if ( fr.isPartOfGeq( b ) ) {
      // Greater-equal side: Estimate matching summands by bNew.
      DOUT( "Changing flow restriction '" << fr << "' to" << endl );

      fr.replaceSummand( b, bNew );

      DOUT( "                          '" << fr << "'." << endl );
    }
  }

  return;
};


/*
  eraseFlowRestrictions erases all flow restrictions referring to a given basic
  block.

  eraseFlowRestrictions does not erase a reference of a flow restriction to the
  given basic block, but it instead erases the entire flow restriction as a
  whole.
*/
void eraseFlowRestrictions( WIR_BasicBlock &b )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  DOUT(
    "WIR_FlowFactUpdater::eraseFlowRestrictions( '" << b.getName() << "' )" <<
    endl );

  // Get FlowFactRef.
  WIR_FlowFactRef &ref = WIR_FlowFactRef::get( b );

  if ( ref.getFlowFacts().empty() ) {
    DOUT( "Nothing to do." << endl );
    return;
  }

  // Update flow restrictions.
  auto flowRestrictions = ref.getFlowFacts<WIR_FlowRestriction>();
  for ( WIR_FlowRestriction &fr : flowRestrictions )
    // Remove and delete flow restriction.
    if ( fr.isInserted() ) {
      DOUT( "Erasing flow restriction '" << fr << "'." << endl );
      WIR_System &sys = fr.getSystem();
      sys.eraseFlowFact( fr.getID() );
    }

  return;
};


//
// Update of both flow restrictions and loop bounds
//

/*
  eraseUnreachableBasicBlock updates flow facts for basic blocks which are never
  executed.

  An unreachable basic block has an execution frequency of zero. Due to this, a
  loop bound can simply be deleted (never executed). Furthermore, each reference
  to this basic block in flow restrictions can be replaced by zero, i.e., the
  corresponding summand of the flow restriction can be erased.
*/
void eraseUnreachableBasicBlock( WIR_BasicBlock &b )
{
  DSTART(
    "WIR_FlowFactUpdater::eraseUnreachableBasicBlock(WIR::WIR_BasicBlock&)" );

  DOUT(
    "WIR_FlowFactUpdater::eraseUnreachableBasicBlock( '" << b.getName() <<
    "' )" << endl );

  // Get FlowFactRef.
  WIR_FlowFactRef &ref = WIR_FlowFactRef::get( b );

  if ( ref.getFlowFacts().empty() ) {
    DOUT( "Nothing to do." << endl );
    return;
  }

  // Update flow restrictions.
  auto flowRestrictions = ref.getFlowFacts<WIR_FlowRestriction>();
  for ( WIR_FlowRestriction &fr : flowRestrictions ) {
    DOUT( "Changing flow restriction '" << fr << "' to" << endl );

    fr.eraseSummand( b );

    DOUT( "                          '" << fr << "'." << endl );
  }

  // Update loop bounds.
  auto loopBounds = ref.getFlowFacts<WIR_LoopBound>();
  for ( WIR_LoopBound &lb : loopBounds )
    if ( ( lb.getLoop() == b ) && lb.isInserted() ) {
      DOUT( "Erasing loop bound '" << lb << "'." << endl );
      WIR_System &sys = lb.getSystem();
      sys.eraseFlowFact( lb.getID() );
    }

  return;
};


//
// Update of loop bounds
//

/*
  moveLoopBounds moves all loop bounds from one loop to another.

  All loop bounds of a loop given by b will be moved to the loop denoted by bNew
  (which may also be the same loop).

  Warning: It is in the programmer's responsibility to ensure that the target
           basic block bNew is member of an unannotated loop.
*/
void moveLoopBounds( WIR_BasicBlock &b, WIR_BasicBlock &bNew )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  DOUT(
    "WIR_FlowFactUpdater::moveLoopBounds( '" << b.getName() << "', '" <<
    bNew.getName() << "' )" << endl );

  // Get FlowFactRef.
  WIR_FlowFactRef &ref = WIR_FlowFactRef::get( b );

  if ( ref.getFlowFacts().empty() ) {
    DOUT( "Nothing to do." << endl );
    return;
  }

  // Update loop bounds.
  auto loopBounds = ref.getFlowFacts<WIR_LoopBound>();
  for ( WIR_LoopBound &lb : loopBounds ) {
    DOUT( "Changing loop bound '" << lb << "' to" << endl );

    lb.setLoop( bNew, lb.getLoopControlType() );

    DOUT( "                    '" << lb << "'." << endl );
  }

  return;
};


/*
  createLoopBound creates a loop bound for a given basic block.

  It is in the programmer's responsibility to ensure that:
  - the given basic block b is part of a loop;
  - that loop is not already annotated by another loop bound;
  - that loop is not a multientry loop, because the WCET analyzer aiT does not
    recognize them;
  - a head-controlled loop is annotated with n+1 if the loop body executes for n
    iterations.
*/
void createLoopBound( WIR_BasicBlock &b, int min, int max )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  DOUT(
    "WIR_FlowFactUpdater::createLoopBound( '" << b.getName() << "', " << min <<
    ", " << max << " )" << endl );

  // Check whether a loop bound for b already exists.
  WIR_FlowFactRef &ref = WIR_FlowFactRef::get( b );

  if ( !ref.getFlowFacts<WIR_LoopBound>().empty() ) {
    DOUT(
      "WARNING in createLoopBound(): Invalid attempt to create more than " <<
      "one loop bound for loop basic block '" << b.getName() << "'." << endl );

    ufErrMsg << ufFile() << "Invalid attempt to create more than one loop "
             << "bound for loop basic block '" << b.getName() << "'.";
  }

  // Create new loop bound and add it to the system. b's loop could already be
  // annotated via another basic block, but this has to be checked by the user
  // of this function before.
  ufAssert(
    b.isInserted() && b.getFunction().isInserted() &&
    b.getFunction().getCompilationUnit().isInserted() );
  auto &sys = b.getFunction().getCompilationUnit().getSystem();
  auto &lb = sys.pushBackFlowFact( WIR_LoopBound( min, max, b ) );

  (void) lb;
  DOUT( "Added loop bound '" << lb << "'." << endl );

  return;
};


/*
  eraseLoopBound erases all loop bounds referring to a given basic block.

  Applying eraseLoopBound does not mean that the loop is guaranteed to be no
  longer annotated any more, because another basic block of this loop may be
  annotated.
*/
void eraseLoopBound( WIR_BasicBlock &b )
{
  DSTART( "WIR_FlowFactUpdater::eraseLoopBound(WIR::WIR_BasicBlock&)" );

  DOUT(
    "WIR_FlowFactUpdater::eraseLoopBound( '" << b.getName() << "' )" << endl );

  // Get FlowFactRef.
  WIR_FlowFactRef &ref = WIR_FlowFactRef::get( b );

  if ( ref.getFlowFacts().empty() ) {
    DOUT( "Nothing to do." << endl );
    return;
  }

  // Update loop bounds.
  auto loopBounds = ref.getFlowFacts<WIR_LoopBound>();
  for ( WIR_LoopBound &lb : loopBounds )
    if ( lb.isInserted() ) {
      DOUT( "Erasing loop bound '" << lb << "'." << endl );
      WIR_System &sys = lb.getSystem();
      sys.eraseFlowFact( lb.getID() );
    }

  return;
};

}       // namespace WIR_FlowFactUpdater

}       // namespace WIR
