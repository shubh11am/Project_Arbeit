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
  @file tc1796_cpu_tc_048.cc
  @brief This file implements a peephole optimizer for silicon bug TC1796
         CPU_TC.048 detection and correction.

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
#include "tc1796_cpu_tc_048.h"


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
TC1796_CPU_TC_048::TC1796_CPU_TC_048( WIR_Function &f ) :
  WIR_Optimization { f },
  WIR_SiliconBugs { f, "TC1796 CPU_TC.048", 2, true }
{
  DSTART( "TC1796_CPU_TC_048::TC1796_CPU_TC_048(WIR_Function&)" );

  addPeepholeSize( 3 );
};


/*
  Destructor.
*/
TC1796_CPU_TC_048::~TC1796_CPU_TC_048( void )
{
  DSTART( "virtual TC1796_CPU_TC_048::~TC1796_CPU_TC_048()" );
};


//
// Protected class methods
//

/*
  matchSiliconBug determines whether the specified peephole matches with silicon
  bug CPU_TC.048.
*/
bool TC1796_CPU_TC_048::matchSiliconBug( const WIR_Peephole::peephole &p )
{
  DSTART( "virtual bool TC1796_CPU_TC_048::matchSiliconBug(const peephole&)" );

  // A small lambda to check whether two operations use the same register.
  auto sameReg = [&]( WIR_Operation &o0, WIR_Operation &o1 ) {
    auto &aReg0 =
      dynamic_cast<WIR_RegisterParameter &>( o0.begin()->get() ).getRegister();
    auto &aReg1 =
      dynamic_cast<WIR_RegisterParameter &>( o1.begin()->get() ).getRegister();

    if ( aReg0 == aReg1 )
      return( true );

    if ( aReg0.hasChilds() ) {
      auto childs = aReg0.getLeafs();
      if ( ( childs[ 0 ].get() == aReg1 ) || ( childs[ 1 ].get() == aReg1 ) )
        return( true );
    }

    return( false );
  };

  vector<WIR_Operation *> ops;

  for ( auto it : p )
    for ( WIR_Operation &o : it->get() )
      if ( ops.size() < 4 )
        ops.push_back( &o );

  if ( ( ops[ 0 ]->getInstruction() == p[ 0 ]->get() ) && isLDA( *ops[ 0 ] ) &&
       !mMatchedPeepholes.count( ops[ 0 ]->getID() ) &&
       isIndirect( *ops[ 1 ] ) && sameReg( *ops[ 0 ], *ops[ 1 ] ) ) {
    mMatchedPeepholes.insert( ops[ 0 ]->getID() );
    return( true );
  }

  if ( ( ops.size() >= 3 ) && ( ops[ 1 ]->getInstruction() == p[ 0 ]->get() ) &&
       !mMatchedPeepholes.count( ops[ 1 ]->getID() ) && isLDA( *ops[ 1 ] ) &&
       isIndirect( *ops[ 2 ] ) && sameReg( *ops[ 1 ], *ops[ 2 ] ) ) {
    mMatchedPeepholes.insert( ops[ 1 ]->getID() );
    return( true );
  }

  if ( ( ops.size() >= 3 ) && ( ops[ 0 ]->getInstruction() == p[ 0 ]->get() ) &&
       !mMatchedPeepholes.count( ops[ 0 ]->getID() ) && isLDA( *ops[ 0 ] ) &&
       isIP( *ops[ 1 ] ) && isIndirect( *ops[ 2 ] ) &&
       sameReg( *ops[ 0 ], isIndirect( *ops[ 1 ] ) ? *ops[ 1 ] : *ops[ 2 ] ) ) {
    mMatchedPeepholes.insert( ops[ 0 ]->getID() );
    return( true );
  }

  if ( ( ops.size() == 4 ) && ( ops[ 1 ]->getInstruction() == p[ 0 ]->get() ) &&
       !mMatchedPeepholes.count( ops[ 1 ]->getID() ) && isLDA( *ops[ 1 ] ) &&
       isIP( *ops[ 2 ] ) && isIndirect( *ops[ 3 ] ) &&
       sameReg( *ops[ 1 ], isIndirect( *ops[ 2 ] ) ? *ops[ 2 ] : *ops[ 3 ] ) ) {
    mMatchedPeepholes.insert( ops[ 1 ]->getID() );
    return( true );
  }

  return( false );
};


/*
  fixSiliconBug fixes silicon bug CPU_TC.048.
*/
WIR_Peephole::peephole TC1796_CPU_TC_048::fixSiliconBug( const WIR_Peephole::peephole &p ) const
{
  DSTART(
    "virtual WIR_Peephole::peephole TC1796_CPU_TC_048::fixSiliconBug(const peephole&) const" );

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

  if ( isLDA( *ops[ 0 ] ) && isIndirect( *ops[ 1 ] ) ) {
    if ( ops[ 1 ]->getInstruction() != ops[ 0 ]->getInstruction() )
      res.push_back( p[ 1 ] );
    else {
      res.push_back( b.insertInstruction( p[ 1 ], {} ) );
      res[ 2 ]->get().moveOperation( *ops[ 1 ] );
    }
  } else

  if ( isLDA( *ops[ 1 ] ) && isIndirect( *ops[ 2 ] ) )
    res.push_back( p[ 1 ] );
  else

  if ( isLDA( *ops[ 0 ] ) && isIP( *ops[ 1 ] ) && isIndirect( *ops[ 2 ] ) ) {
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

  if ( isLDA( *ops[ 1 ] ) && isIP( *ops[ 2 ] ) && isIndirect( *ops[ 3 ] ) ) {
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
  isLDA determines whether an operation is an address register load subject to
  silicon bug CPU_TC.048.
*/
bool TC1796_CPU_TC_048::isLDA( const WIR_Operation &o ) const
{
  DSTART( "bool TC1796_CPU_TC_048::isLDA(const WIR_Operation&) const" );

  return(
    ( o.getOpCode() == TC13::OpCode::LD_A ) ||
    ( o.getOpCode() == TC13::OpCode::LD_DA ) );
};


/*
  isIndirect determines whether an operation is an indirect jump or call subject
  to silicon bug CPU_TC.048.
*/
bool TC1796_CPU_TC_048::isIndirect( const WIR_Operation &o ) const
{
  DSTART( "bool TC1796_CPU_TC_048::isIndirect(const WIR_Operation&) const" );

  return( o.isIndirectCall() || o.isIndirectJump() );
};


/*
  isIP determines whether an operation is an IP operation subject to silicon bug
  CPU_TC.048.
*/
bool TC1796_CPU_TC_048::isIP( const WIR_Operation &o ) const
{
  DSTART( "bool TC1796_CPU_TC_048::isIP(const WIR_Operation&) const" );

  return( TC13::isIP( o ) && ( o.getOpCode() != TC13::OpCode::NOP ) );
};

}       // namespace WIR
