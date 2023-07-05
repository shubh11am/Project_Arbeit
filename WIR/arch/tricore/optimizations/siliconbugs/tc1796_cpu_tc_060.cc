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
  @file tc1796_cpu_tc_060.cc
  @brief This file implements a peephole optimizer for silicon bug TC1796
         CPU_TC.060 detection and correction.

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
#include "tc1796_cpu_tc_060.h"


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
TC1796_CPU_TC_060::TC1796_CPU_TC_060( WIR_Function &f ) :
  WIR_Optimization { f },
  WIR_SiliconBugs { f, "TC1796 CPU_TC.060", 2, true }
{
  DSTART( "TC1796_CPU_TC_060::TC1796_CPU_TC_060(WIR_Function&)" );
};


/*
  Destructor.
*/
TC1796_CPU_TC_060::~TC1796_CPU_TC_060( void )
{
  DSTART( "virtual TC1796_CPU_TC_060::~TC1796_CPU_TC_060()" );
};


//
// Protected class methods
//

/*
  matchSiliconBug determines whether the specified peephole matches with silicon
  bug CPU_TC.060.
*/
bool TC1796_CPU_TC_060::matchSiliconBug( const WIR_Peephole::peephole &p )
{
  DSTART( "virtual bool TC1796_CPU_TC_060::matchSiliconBug(const peephole&)" );

  // A small lambda to check whether two operations use the same register.
  auto sameReg = [&]( WIR_Operation &o0, WIR_Operation &o1 ) {
    auto &aReg0 =
      dynamic_cast<WIR_RegisterParameter &>( o0.begin()->get() ).getRegister();

    for ( WIR_Parameter &p : o1.getExplicitParameters() )
      if ( p.getType() == WIR_ParameterType::reg ) {
        auto &rp = dynamic_cast<WIR_RegisterParameter &>( p );
        if ( rp.isUsed() || rp.isDefUsed() ) {
          auto &r = rp.getRegister();

          if ( aReg0 == r )
            return( true );

          if ( aReg0.hasChilds() ) {
            auto childs = r.getLeafs();
            if ( ( childs[ 0 ].get() == r ) || ( childs[ 1 ].get() == r ) )
              return( true );
          }
        }
      }

    return( false );
  };

  vector<WIR_Operation *> ops;

  for ( auto it : p )
    for ( WIR_Operation &o : it->get() )
      if ( ops.size() < 3 )
        ops.push_back( &o );

  if ( ( ops[ 0 ]->getInstruction() == p[ 0 ]->get() ) && isLDA( *ops[ 0 ] ) &&
       isLDAW( *ops[ 1 ] ) && sameReg( *ops[ 0 ], *ops[ 1 ] ) )
    return( true );

  if ( ( ops.size() == 3 ) && ( ops[ 1 ]->getInstruction() == p[ 0 ]->get() ) &&
       isLDA( *ops[ 1 ] ) && isLDAW( *ops[ 2 ] ) &&
       sameReg( *ops[ 1 ], *ops[ 2 ] ) )
    return( true );

  return( false );
};


/*
  fixSiliconBug fixes silicon bug CPU_TC.060.
*/
WIR_Peephole::peephole TC1796_CPU_TC_060::fixSiliconBug( const WIR_Peephole::peephole &p ) const
{
  DSTART(
    "virtual WIR_Peephole::peephole TC1796_CPU_TC_060::fixSiliconBug(const peephole&) const" );

  WIR_Peephole::peephole res;

  WIR_BasicBlock &b = p[ 1 ]->get().getBasicBlock();

  vector<WIR_Operation *> ops;

  for ( auto it : p )
    for ( WIR_Operation &o : it->get() )
      if ( ops.size() < 3 )
        ops.push_back( &o );

  res.push_back( p[ 0 ] );

  res.push_back(
    b.insertInstruction(
      p[ 1 ],
      { { TC13::OpCode::NOP,
          m16BitOperations ?
            TC13::OperationFormat::S : TC13::OperationFormat::SYS } } ) );
  markInstruction( res[ 1 ]->get() );

  if ( isLDA( *ops[ 0 ] ) && isLDAW( *ops[ 1 ] ) ) {
    if ( ops[ 1 ]->getInstruction() != ops[ 0 ]->getInstruction() )
      res.push_back( p[ 1 ] );
    else {
      res.push_back( b.insertInstruction( p[ 1 ], {} ) );
      res[ 2 ]->get().moveOperation( *ops[ 1 ] );
    }
  } else
    res.push_back( p[ 1 ] );

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
  silicon bug CPU_TC.060.
*/
bool TC1796_CPU_TC_060::isLDA( const WIR_Operation &o ) const
{
  DSTART( "bool TC1796_CPU_TC_060::isLDA(const WIR_Operation&) const" );

  return(
    ( o.getOpCode() == TC13::OpCode::LD_A ) ||
    ( o.getOpCode() == TC13::OpCode::LD_DA ) );
};


/*
  isLDAW determines whether an operation is an LD_DA, LD_D or LD_W subject to
  silicon bug CPU_TC.060.
*/
bool TC1796_CPU_TC_060::isLDAW( const WIR_Operation &o ) const
{
  DSTART( "bool TC1796_CPU_TC_060::isLDAW(const WIR_Operation&) const" );

  return(
    ( o.getOpCode() == TC13::OpCode::LD_DA ) ||
    ( o.getOpCode() == TC13::OpCode::LD_D ) ||
    ( o.getOpCode() == TC13::OpCode::LD_W ) );
};

}       // namespace WIR
