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
  @file tc1796_cpu_tc_070.cc
  @brief This file implements a peephole optimizer for silicon bug TC1796
         CPU_TC.070 detection and correction.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include libuseful headers
#include <libuseful/debugmacros.h>

// Include WIR headers
#include <wir/wir.h>
#include <arch/tricore/tc13.h>

// Include local headers
#include "tc1796_cpu_tc_070.h"


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
TC1796_CPU_TC_070::TC1796_CPU_TC_070( WIR_Function &f ) :
  WIR_Optimization { f },
  WIR_SiliconBugs { f, "TC1796 CPU_TC.070", 2, true }
{
  DSTART( "TC1796_CPU_TC_070::TC1796_CPU_TC_070(WIR_Function&)" );

  addPeepholeSize( 3 );
};


/*
  Destructor.
*/
TC1796_CPU_TC_070::~TC1796_CPU_TC_070( void )
{
  DSTART( "virtual TC1796_CPU_TC_070::~TC1796_CPU_TC_070()" );
};


//
// Protected class methods
//

/*
  matchSiliconBug determines whether the specified peephole matches with silicon
  bug CPU_TC.070.
*/
bool TC1796_CPU_TC_070::matchSiliconBug( const WIR_Peephole::peephole &p )
{
  DSTART( "virtual bool TC1796_CPU_TC_070::matchSiliconBug(const peephole&)" );

  vector<WIR_Operation *> ops;

  for ( auto it : p )
    for ( WIR_Operation &o : it->get() )
      if ( ops.size() < 4 )
        ops.push_back( &o );

  if ( ( ops[ 0 ]->getInstruction() == p[ 0 ]->get() ) &&
       isCJump( *ops[ 0 ] ) && !mMatchedPeepholes.count( ops[ 0 ]->getID() ) &&
       isLOOP( *ops[ 1 ] ) ) {
    mMatchedPeepholes.insert( ops[ 0 ]->getID() );
    return( true );
  }

  if ( ( ops.size() >= 3 ) && ( ops[ 1 ]->getInstruction() == p[ 0 ]->get() ) &&
       isCJump( *ops[ 1 ] ) && !mMatchedPeepholes.count( ops[ 1 ]->getID() ) &&
       isLOOP( *ops[ 2 ] ) ) {
    mMatchedPeepholes.insert( ops[ 1 ]->getID() );
    return( true );
  }

  if ( ( ops.size() >= 3 ) && ( ops[ 0 ]->getInstruction() == p[ 0 ]->get() ) &&
       isDJump( *ops[ 0 ] ) && !mMatchedPeepholes.count( ops[ 0 ]->getID() ) &&
       ( ops[ 1 ]->getOpCode() == TC13::OpCode::NOP ) && isLOOP( *ops[ 2 ] ) ) {
    mMatchedPeepholes.insert( ops[ 0 ]->getID() );
    return( true );
  }

  if ( ( ops.size() == 4 ) && ( ops[ 1 ]->getInstruction() == p[ 0 ]->get() ) &&
       isDJump( *ops[ 1 ] ) && !mMatchedPeepholes.count( ops[ 1 ]->getID() ) &&
       ( ops[ 2 ]->getOpCode() == TC13::OpCode::NOP ) && isLOOP( *ops[ 3 ] ) ) {
    mMatchedPeepholes.insert( ops[ 1 ]->getID() );
    return( true );
  }

  return( false );
};


/*
  fixSiliconBug fixes silicon bug CPU_TC.070.
*/
WIR_Peephole::peephole TC1796_CPU_TC_070::fixSiliconBug( const WIR_Peephole::peephole &p ) const
{
  DSTART(
    "virtual WIR_Peephole::peephole TC1796_CPU_TC_070::fixSiliconBug(const peephole&) const" );

  WIR_Peephole::peephole res;

  WIR_BasicBlock &b = p[ 1 ]->get().getBasicBlock();

  vector<WIR_Operation *> ops;

  for ( auto it : p )
    for ( WIR_Operation &o : it->get() )
      if ( ops.size() < 4 )
        ops.push_back( &o );

  res.push_back( p[ 0 ] );

  res.push_back(
    b.insertInstruction(
      p[ 1 ],
      { { TC13::OpCode::NOP,
          m16BitOperations ?
            TC13::OperationFormat::S : TC13::OperationFormat::SYS } } ) );
  markInstruction( res[ 1 ]->get() );

  res.push_back( p[ 1 ] );

  if ( isCJump( *ops[ 0 ] ) && isLOOP( *ops[ 1 ] ) ) {
    if ( isDJump( *ops[ 0 ] ) )
      res[ 1 ]->get().pushBackOperation(
        { TC13::OpCode::NOP,
          m16BitOperations ?
            TC13::OperationFormat::S : TC13::OperationFormat::SYS } );
  } else

  if ( isCJump( *ops[ 1 ] ) && isLOOP( *ops[ 2 ] ) ) {
    if ( isDJump( *ops[ 1 ] ) )
      res[ 1 ]->get().pushBackOperation(
        { TC13::OpCode::NOP,
          m16BitOperations ?
            TC13::OperationFormat::S : TC13::OperationFormat::SYS } );
  } else

  if ( isDJump( *ops[ 0 ] ) && ( ops[ 1 ]->getOpCode() == TC13::OpCode::NOP ) &&
       isLOOP( *ops[ 2 ] ) ) {
    if ( ops[ 2 ]->getInstruction() != ops[ 1 ]->getInstruction() )
      res.push_back( p[ 2 ] );
  } else

  if ( isDJump( *ops[ 1 ] ) && ( ops[ 2 ]->getOpCode() == TC13::OpCode::NOP ) &&
       isLOOP( *ops[ 3 ] ) ) {
    if ( ops[ 3 ]->getInstruction() != ops[ 2 ]->getInstruction() )
      res.push_back( p[ 2 ] );
  }

  if ( res.size() == 3 )
    b.insertContainer(
      WIR_SchedulingConstraint {
        WIR_SchedulingConstraintType::sequential,
        res[ 1 ]->get(), res[ 2 ]->get() } );
  else
    b.insertContainer(
      WIR_SchedulingConstraint {
        WIR_SchedulingConstraintType::sequential,
        res[ 1 ]->get(), res[ 2 ]->get(), res[ 3 ]->get() } );

  return( res );
};


//
// Private class methods
//

/*
  isCJump determines whether an operation is a conditional jump subject to
  silicon bug CPU_TC.070.
*/
bool TC1796_CPU_TC_070::isCJump( const WIR_Operation &o ) const
{
  DSTART( "bool TC1796_CPU_TC_070::isCJump(const WIR_Operation&) const" );

  WIR_BasicBlock &b = o.getInstruction().getBasicBlock();
  WIR_System &sys = b.getFunction().getCompilationUnit().getSystem();

  return(
    o.isConditionalJump() && ( o.getSize() == 4 ) &&
    ( o.getOpCode() != TC13::OpCode::LOOP ) &&
    ( sys.findSymbol(
        dynamic_cast<WIR_LabelParameter &>(
          o.getExplicitParameters().rbegin()->get() ).getBasicBlock() ).
        getBaseAddress() >=
        sys.findSymbol( b ).getBaseAddress() + b.getSize() ) );
};


/*
  isDJump determines whether an operation is a conditional jump depending on a
  data register, subject to silicon bug CPU_TC.070.
*/
bool TC1796_CPU_TC_070::isDJump( const WIR_Operation &o ) const
{
  DSTART( "bool TC1796_CPU_TC_070::isDJump(const WIR_Operation&) const" );

  return(
    isCJump( o ) && ( o.getOperationFormat() != TC13::OperationFormat::AAL ) &&
    ( o.getOperationFormat() != TC13::OperationFormat::AL_2 ) );
};


/*
  isLOOP determines whether an operation is a LOOP/LOOPU operation subject to
  silicon bug CPU_TC.070.
*/
bool TC1796_CPU_TC_070::isLOOP( const WIR_Operation &o ) const
{
  DSTART( "bool TC1796_CPU_TC_070::isLOOP(const WIR_Operation&) const" );

  return(
    ( o.getOpCode() == TC13::OpCode::LOOP ) ||
    ( o.getOpCode() == TC13::OpCode::LOOPU ) );
};

}       // namespace WIR
