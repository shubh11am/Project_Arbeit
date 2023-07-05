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
  @file tc179x_cpu_tc_105.cc
  @brief This file implements a peephole optimizer for silicon bug
         TC1796/TC1797 CPU_TC.105 detection and correction.

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
#include "tc179x_cpu_tc_105.h"


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
TC179x_CPU_TC_105::TC179x_CPU_TC_105( WIR_Function &f ) :
  WIR_Optimization { f },
  WIR_SiliconBugs { f, "TC179x CPU_TC.105", 2, true }
{
  DSTART( "TC179x_CPU_TC_105::TC179x_CPU_TC_105(WIR_Function&)" );
};


/*
  Destructor.
*/
TC179x_CPU_TC_105::~TC179x_CPU_TC_105( void )
{
  DSTART( "virtual ::~TC179x_CPU_TC_105()" );
};


//
// Protected class methods
//

/*
  matchSiliconBug determines whether the specified peephole matches with silicon
  bug CPU_TC.105.
*/
bool TC179x_CPU_TC_105::matchSiliconBug( const WIR_Peephole::peephole &p )
{
  DSTART( "virtual bool TC179x_CPU_TC_105::matchSiliconBug(const peephole&)" );

  vector<WIR_Operation *> ops;

  for ( auto it : p )
    for ( WIR_Operation &o : it->get() )
      if ( ops.size() < 3 )
        ops.push_back( &o );

  if ( ( ops[ 0 ]->getInstruction() == p[ 0 ]->get() ) &&
       ( ops[ 0 ]->getOpCode() != TC13::OpCode::DSYNC ) && isMTCR( *ops[ 1 ] ) )
    return( true );

  if ( ( ops.size() == 3 ) && ( ops[ 1 ]->getInstruction() == p[ 0 ]->get() ) &&
       ( ops[ 1 ]->getOpCode() != TC13::OpCode::DSYNC ) && isMTCR( *ops[ 2 ] ) )
    return( true );

  return( false );
};


/*
  fixSiliconBug fixes silicon bug CPU_TC.105.
*/
WIR_Peephole::peephole TC179x_CPU_TC_105::fixSiliconBug( const WIR_Peephole::peephole &p ) const
{
  DSTART(
    "virtual WIR_Peephole::peephole TC179x_CPU_TC_105::fixSiliconBug(const peephole&) const" );

  WIR_Peephole::peephole res;

  auto &b = p[ 1 ]->get().getBasicBlock();

  vector<WIR_Operation *> ops;

  for ( auto it : p )
    for ( WIR_Operation &o : it->get() )
      if ( ops.size() < 3 )
        ops.push_back( &o );

  res.push_back(
    b.insertInstruction(
      p[ 1 ], { { TC13::OpCode::DSYNC, TC13::OperationFormat::SYS } } ) );
  markInstruction( res[ 0 ]->get() );

  if ( isMTCR( *ops[ 1 ] ) ) {
    if ( ops[ 1 ]->getInstruction() != ops[ 0 ]->getInstruction() )
      res.push_back( p[ 1 ] );
    else {
      res.push_back( b.insertInstruction( p[ 1 ], {} ) );
      res[ 1 ]->get().moveOperation( *ops[ 1 ] );
    }
  } else
    res.push_back( p[ 1 ] );

  b.insertContainer(
    WIR_SchedulingConstraint {
      WIR_SchedulingConstraintType::sequential,
      res[ 0 ]->get(), res[ 1 ]->get() } );

  return( res );
};


//
// Protected class methods
//

/*
  isMTCR determines whether an operation is an MTCR subject to silicon bug
  CPU_TC.105.
*/
bool TC179x_CPU_TC_105::isMTCR( const WIR_Operation &o ) const
{
  DSTART( "bool TC179x_CPU_TC_105::isMTCR(const WIR_Operation&) const" );

  return(
    ( o.getOpCode() == TC13::OpCode::MTCR ) &&
    ( dynamic_cast<TC_Const16_Unsigned &>(
        o.getExplicitParameters().front().get() ).getUnsignedValue() ==
        0xFE04 ) );
};

}       // namespace WIR
