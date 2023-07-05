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
  @file tc1796_cpu_tc_096.cc
  @brief This file implements a peephole optimizer for silicon bug TC1796
         CPU_TC.096 detection and correction.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include standard headers
#include <vector>

// Include libuseful headers
#include <libuseful/debugmacros.h>

// Include WIR headers
#include <wir/wir.h>
#include <arch/tricore/tc13.h>

// Include local headers
#include "tc1796_cpu_tc_096.h"


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
TC1796_CPU_TC_096::TC1796_CPU_TC_096( WIR_Function &f ) :
  WIR_Optimization { f },
  WIR_SiliconBugs { f, "TC1796 CPU_TC.096", 2, false }
{
  DSTART( "TC1796_CPU_TC_096::TC1796_CPU_TC_096(WIR_Function&)" );

  addPeepholeSize( 3 );
};


/*
  Destructor.
*/
TC1796_CPU_TC_096::~TC1796_CPU_TC_096( void )
{
  DSTART( "virtual TC1796_CPU_TC_096::~TC1796_CPU_TC_096()" );
};


//
// Protected class methods
//

/*
  matchSiliconBug determines whether the specified peephole matches with silicon
  bug CPU_TC.096.
*/
bool TC1796_CPU_TC_096::matchSiliconBug( const WIR_Peephole::peephole &p )
{
  DSTART( "virtual bool TC1796_CPU_TC_096::matchSiliconBug(const peephole&)" );

  // A small lambda to check whether a loop operation targets another operation.
  auto loopTgt = [&]( WIR_Operation &lp, WIR_Operation &o ) {
    auto &tgt =
      dynamic_cast<WIR_LabelParameter &>(
        lp.rbegin()->get() ).getBasicBlock();
    auto &b = o.getInstruction().getBasicBlock();

    if ( ( b.begin()->get().begin()->get() == o ) && ( tgt == b ) )
      return( true );

    return( false );
  };

  vector<WIR_Operation *> ops;

  for ( auto it : p )
    for ( WIR_Operation &o : it->get() )
      if ( ops.size() < 3 )
        ops.push_back( &o );

  if ( TC13::isIP( *ops[ 0 ] ) &&
       !mMatchedPeepholes.count( ops[ 0 ]->getID() ) &&
       TC13::isLP( *ops[ 1 ] ) &&
       loopTgt( *ops[ 1 ], *ops[ 0 ] ) ) {
    mMatchedPeepholes.insert( ops[ 0 ]->getID() );
    return( true );
  }

  if ( ( ops.size() == 3 ) && TC13::isIP( *ops[ 0 ] ) &&
       !mMatchedPeepholes.count( ops[ 0 ]->getID() ) &&
        ( ( ops[ 1 ]->getOpCode() == TC13::OpCode::NOP ) ||
          TC13::isLS( *ops[ 1 ] ) ) &&
        TC13::isLP( *ops[ 2 ] ) && loopTgt( *ops[ 2 ], *ops[ 0 ] ) ) {
    mMatchedPeepholes.insert( ops[ 0 ]->getID() );
    return( true );
  }

  return( false );
};


/*
  fixSiliconBug fixes silicon bug CPU_TC.096.
*/
WIR_Peephole::peephole TC1796_CPU_TC_096::fixSiliconBug( const WIR_Peephole::peephole &p ) const
{
  DSTART(
    "virtual WIR_Peephole::peephole TC1796_CPU_TC_096::fixSiliconBug(const peephole&) const" );

  WIR_Peephole::peephole res;

  WIR_BasicBlock &b = p[ 0 ]->get().getBasicBlock();

  vector<WIR_Operation *> ops;

  for ( auto it : p )
    for ( WIR_Operation &o : it->get() )
      if ( ops.size() < 4 )
        ops.push_back( &o );

  res.push_back( p[ 0 ] );

  if ( TC13::isLP( *ops[ 1 ] ) ) {
    res.push_back(
      b.insertInstruction(
        p[ 1 ],
        WIR_Instruction {
          WIR_Operation {
            TC13::OpCode::NOP,
            m16BitOperations ?
                TC13::OperationFormat::S : TC13::OperationFormat::SYS },
          WIR_Operation {
            TC13::OpCode::NOP,
            m16BitOperations ?
                TC13::OperationFormat::S : TC13::OperationFormat::SYS } } ) );
    markInstruction( res[ 1 ]->get() );

    if ( *ops[ 1 ] == p[ 0 ]->get().getOperations().back().get() ) {
      res.push_back( b.insertInstruction( p[ 1 ], {} ) );
      res[ 2 ]->get().moveOperation( *ops[ 1 ] );
    } else
      res.push_back( p[ 1 ] );

    b.insertContainer(
      WIR_SchedulingConstraint {
        WIR_SchedulingConstraintType::sequential,
        res[ 1 ]->get(), res[ 2 ]->get() } );
  } else {
    if ( *ops[ 2 ] == p[ 1 ]->get().getOperations().front().get() ) {
      res.push_back(
        b.insertInstruction(
          p[ 1 ],
          { { TC13::OpCode::NOP,
              m16BitOperations ?
                TC13::OperationFormat::S : TC13::OperationFormat::SYS } } ) );
      markInstruction( res[ 1 ]->get() );

      res.push_back( p[ 1 ] );

      b.insertContainer(
        WIR_SchedulingConstraint {
          WIR_SchedulingConstraintType::sequential,
          res[ 1 ]->get(), res[ 2 ]->get() } );
    } else

    if ( *ops[ 2 ] == p[ 1 ]->get().getOperations().back().get() ) {
      res.push_back(
        b.insertInstruction(
          next( p[ 1 ] ),
          { { TC13::OpCode::NOP,
              m16BitOperations ?
                TC13::OperationFormat::S : TC13::OperationFormat::SYS } } ) );
      markInstruction( res[ 1 ]->get() );

      res.push_back( b.insertInstruction( next( p[ 1 ] ), {} ) );
      res[ 2 ]->get().moveOperation( *ops[ 2 ] );

      b.insertContainer(
        WIR_SchedulingConstraint {
          WIR_SchedulingConstraintType::sequential,
          res[ 1 ]->get(), res[ 2 ]->get() } );
    } else {
      res.push_back( p[ 1 ] );

      res.push_back(
        b.insertInstruction(
          p[ 2 ],
          { { TC13::OpCode::NOP,
              m16BitOperations ?
                TC13::OperationFormat::S : TC13::OperationFormat::SYS } } ) );
      markInstruction( res[ 2 ]->get() );

      res.push_back( p[ 2 ] );

      b.insertContainer(
        WIR_SchedulingConstraint {
          WIR_SchedulingConstraintType::sequential,
          res[ 2 ]->get(), res[ 3 ]->get() } );
    }
  }

  return( res );
};

}       // namespace WIR
