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
  @file tc1796_cpu_tc_069.cc
  @brief This file implements a peephole optimizer for silicon bug TC1796
         CPU_TC.069 detection and correction.

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
#include "tc1796_cpu_tc_069.h"


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
TC1796_CPU_TC_069::TC1796_CPU_TC_069( WIR_Function &f ) :
  WIR_Optimization { f },
  WIR_SiliconBugs { f, "TC1796 CPU_TC.069", 2, true }
{
  DSTART( "TC1796_CPU_TC_069::TC1796_CPU_TC_069(WIR_Function&)" );

  addPeepholeSize( 3 );
};


/*
  Destructor.
*/
TC1796_CPU_TC_069::~TC1796_CPU_TC_069( void )
{
  DSTART( "virtual TC1796_CPU_TC_069::~TC1796_CPU_TC_069()" );
};


//
// Protected class methods
//

/*
  matchSiliconBug determines whether the specified peephole matches with silicon
  bug CPU_TC.069.
*/
bool TC1796_CPU_TC_069::matchSiliconBug( const WIR_Peephole::peephole &p )
{
  DSTART( "virtual bool TC1796_CPU_TC_069::matchSiliconBug(const peephole&)" );

  // This lambda determines whether the given operation uses TriCore registers
  // A0 or A1.
  auto isA0A1 = [&]( WIR_Operation &o ) {
    for ( WIR_Parameter &p : o.getExplicitParameters() )
      if ( p.getType() == WIR_ParameterType::reg ) {
        auto &rp = dynamic_cast<const WIR_RegisterParameter &>( p );

        if ( rp.isUsed() || rp.isDefUsed() ) {
          auto &r = rp.getRegister();

          if ( r.isVirtual() ) {
            auto &vr = dynamic_cast<const WIR_VirtualRegister &>( r );

            if ( vr.isPrecolored() &&
                ( ( vr.getPrecolor().getName() == "a0" ) ||
                  ( vr.getPrecolor().getName() == "a1" ) ) )
              return( true );
          } else
            return( ( r.getName() == "a0" ) || ( r.getName() == "a1" ) );
        }
      }

    return( false );
  };

  vector<WIR_Operation *> ops;

  for ( auto it : p )
    for ( WIR_Operation &o : it->get() )
      if ( ops.size() < 4 )
        ops.push_back( &o );

  if ( ( ops[ 0 ]->getInstruction() == p[ 0 ]->get() ) &&
       isRSLCX( *ops[ 0 ] ) && !mMatchedPeepholes.count( ops[ 0 ]->getID() ) &&
       isLSLP( *ops[ 1 ] ) && isA0A1( *ops[ 1 ] ) ) {
    mMatchedPeepholes.insert( ops[ 0 ]->getID() );
    return( true );
  }

  if ( ( ops.size() >= 3 ) && ( ops[ 1 ]->getInstruction() == p[ 0 ]->get() ) &&
       isRSLCX( *ops[ 1 ] ) && !mMatchedPeepholes.count( ops[ 1 ]->getID() ) &&
       isLSLP( *ops[ 2 ] ) && isA0A1( *ops[ 2 ] ) ) {
    mMatchedPeepholes.insert( ops[ 1 ]->getID() );
    return( true );
  }

  if ( ( ops.size() >= 3 ) && ( ops[ 0 ]->getInstruction() == p[ 0 ]->get() ) &&
       isRSLCX( *ops[ 0 ] ) && !mMatchedPeepholes.count( ops[ 0 ]->getID() ) &&
       isIP( *ops[ 1 ] ) && isLSLP( *ops[ 2 ] ) && isA0A1( *ops[ 2 ] ) ) {
    mMatchedPeepholes.insert( ops[ 0 ]->getID() );
    return( true );
  }

  if ( ( ops.size() == 4 ) && ( ops[ 1 ]->getInstruction() == p[ 0 ]->get() ) &&
       isRSLCX( *ops[ 1 ] ) && !mMatchedPeepholes.count( ops[ 1 ]->getID() ) &&
       isIP( *ops[ 2 ] ) && isLSLP( *ops[ 3 ] ) && isA0A1( *ops[ 3 ] ) ) {
    mMatchedPeepholes.insert( ops[ 1 ]->getID() );
    return( true );
  }

  return( false );
};


/*
  fixSiliconBug fixes silicon bug CPU_TC.069.
*/
WIR_Peephole::peephole TC1796_CPU_TC_069::fixSiliconBug( const WIR_Peephole::peephole &p ) const
{
  DSTART(
    "virtual WIR_Peephole::peephole TC1796_CPU_TC_069::fixSiliconBug(const peephole&) const" );

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

  if ( isRSLCX( *ops[ 0 ] ) && isLSLP( *ops[ 1 ] ) ) {
    if ( ops[ 1 ]->getInstruction() != ops[ 0 ]->getInstruction() )
      res.push_back( p[ 1 ] );
    else {
      res.push_back( b.insertInstruction( p[ 1 ], {} ) );
      res[ 2 ]->get().moveOperation( *ops[ 1 ] );
    }
  } else

  if ( isRSLCX( *ops[ 1 ] ) && isLSLP( *ops[ 2 ] ) )
    res.push_back( p[ 1 ] );
  else

  if ( isRSLCX( *ops[ 0 ] ) && isIP( *ops[ 1 ] ) && isLSLP( *ops[ 2 ] ) ) {
    if ( ops[ 1 ]->getInstruction() != ops[ 0 ]->getInstruction() )
      res.push_back( p[ 1 ] );
    else {
      res.push_back( b.insertInstruction( p[ 1 ], {} ) );
      res[ 2 ]->get().moveOperation( *ops[ 1 ] );
    }

    if ( ops[ 2 ]->getInstruction() != ops[ 1 ]->getInstruction() )
      res.push_back(
        ( ops[ 2 ]->getInstruction() == p[ 1 ]->get() ) ? p[ 1 ] : p[ 2 ] );
  } else

  if ( isRSLCX( *ops[ 1 ] ) && isIP( *ops[ 2 ] ) && isLSLP( *ops[ 3 ] ) ) {
    res.push_back( p[ 1 ] );

    if ( ops[ 3 ]->getInstruction() != ops[ 2 ]->getInstruction() )
      res.push_back( p[ 2 ] );
  }

  b.insertContainer(
    WIR_SchedulingConstraint {
      WIR_SchedulingConstraintType::sequential,
      res[ 1 ]->get(), res[ 2 ]->get() } );

  return( res );
};


//
// Private class methods
//

/*
  isRSLCX determines whether an operation is an RSLCX operation subject to
  silicon bug CPU_TC.069.
*/
bool TC1796_CPU_TC_069::isRSLCX( const WIR_Operation &o ) const
{
  DSTART( "bool TC1796_CPU_TC_069::isRSLCX(const WIR_Operation&) const" );

  return( o.getOpCode() == TC13::OpCode::RSLCX );
};


/*
  isLSLP determines whether an operation is a LS or LP operation subject to
  silicon bug CPU_TC.069.
*/
bool TC1796_CPU_TC_069::isLSLP( const WIR_Operation &o ) const
{
  DSTART( "bool TC1796_CPU_TC_069::isLSLP(const WIR_Operation&) const" );

  return( TC13::isLS( o ) || TC13::isLP( o ) );
};


/*
  isIP determines whether an operation is an IP operation subject to silicon bug
  CPU_TC.069.
*/
bool TC1796_CPU_TC_069::isIP( const WIR_Operation &o ) const
{
  DSTART( "bool TC1796_CPU_TC_069::isIP(const WIR_Operation&) const" );

  return( TC13::isIP( o ) && ( o.getOpCode() != TC13::OpCode::NOP ) );
};

}       // namespace WIR
