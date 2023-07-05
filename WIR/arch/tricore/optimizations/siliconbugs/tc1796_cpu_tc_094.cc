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
  @file tc1796_cpu_tc_094.cc
  @brief This file implements a peephole optimizer for silicon bug TC1796
         CPU_TC.094 detection and correction.

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
#include "tc1796_cpu_tc_094.h"


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
TC1796_CPU_TC_094::TC1796_CPU_TC_094( WIR_Function &f ) :
  WIR_Optimization { f },
  WIR_SiliconBugs { f, "TC1796 CPU_TC.094", 2, true }
{
  DSTART( "TC1796_CPU_TC_094::TC1796_CPU_TC_094(WIR_Function&)" );
};


/*
  Destructor.
*/
TC1796_CPU_TC_094::~TC1796_CPU_TC_094( void )
{
  DSTART( "virtual TC1796_CPU_TC_094::~TC1796_CPU_TC_094()" );
};


//
// Protected class methods
//

/*
  matchSiliconBug determines whether the specified peephole matches with silicon
  bug CPU_TC.094.
*/
bool TC1796_CPU_TC_094::matchSiliconBug( const WIR_Peephole::peephole &p )
{
  DSTART( "virtual bool TC1796_CPU_TC_094::matchSiliconBug(const peephole&)" );

  vector<WIR_Operation *> ops;

  for ( auto it : p )
    for ( WIR_Operation &o : it->get() )
      if ( ops.size() < 3 )
        ops.push_back( &o );

  if ( ( ops[ 0 ]->getInstruction() == p[ 0 ]->get() ) &&
       isIPJmp( *ops[ 0 ] ) && isCSA( *ops[ 1 ] ) )
    return( true );

  if ( ( ops.size() == 3 ) && ( ops[ 1 ]->getInstruction() == p[ 0 ]->get() ) &&
       isIPJmp( *ops[ 1 ] ) && isCSA( *ops[ 2 ] ) )
    return( true );

  return( false );
};


/*
  fixSiliconBug fixes silicon bug CPU_TC.094.
*/
WIR_Peephole::peephole TC1796_CPU_TC_094::fixSiliconBug( const WIR_Peephole::peephole &p ) const
{
  DSTART(
    "virtual WIR_Peephole::peephole TC1796_CPU_TC_094::fixSiliconBug(const peephole&) const" );

  WIR_Peephole::peephole res;

  WIR_BasicBlock &b = p[ 1 ]->get().getBasicBlock();

  res.push_back( p[ 0 ] );

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

  return( res );
};


//
// Private class methods
//

/*
  isIPJmp determines whether an operation is a jump executed in the IP pipeline,
  which is subject to silicon bug CPU_TC.094.
*/
bool TC1796_CPU_TC_094::isIPJmp( const WIR_Operation &o ) const
{
  DSTART( "bool TC1796_CPU_TC_094::isIPJmp(const WIR_Operation&) const" );

  return(
    ( o.getOpCode() == TC13::OpCode::JEQ ) ||
    ( o.getOpCode() == TC13::OpCode::JGE ) ||
    ( o.getOpCode() == TC13::OpCode::JGE_U ) ||
    ( o.getOpCode() == TC13::OpCode::JGEZ ) ||
    ( o.getOpCode() == TC13::OpCode::JGTZ ) ||
    ( o.getOpCode() == TC13::OpCode::JLEZ ) ||
    ( o.getOpCode() == TC13::OpCode::JLT ) ||
    ( o.getOpCode() == TC13::OpCode::JLT_U ) ||
    ( o.getOpCode() == TC13::OpCode::JLTZ ) ||
    ( o.getOpCode() == TC13::OpCode::JNE ) ||
    ( o.getOpCode() == TC13::OpCode::JNED ) ||
    ( o.getOpCode() == TC13::OpCode::JNEI ) ||
    ( o.getOpCode() == TC13::OpCode::JNZ ) ||
    ( o.getOpCode() == TC13::OpCode::JNZ_T ) ||
    ( o.getOpCode() == TC13::OpCode::JZ ) ||
    ( o.getOpCode() == TC13::OpCode::JZ_T ) );
};


/*
  isCSA determines whether an operation is a CSA list operation subject to
  silicon bug CPU_TC.094.
*/
bool TC1796_CPU_TC_094::isCSA( const WIR_Operation &o ) const
{
  DSTART( "bool TC1796_CPU_TC_094::isCSA(const WIR_Operation&) const" );

  return(
    ( o.getOpCode() == TC13::OpCode::CALL ) ||
    ( o.getOpCode() == TC13::OpCode::CALLA ) ||
    ( o.getOpCode() == TC13::OpCode::CALLI ) ||
    ( o.getOpCode() == TC13::OpCode::SYSCALL) ||
    ( o.getOpCode() == TC13::OpCode::RET ) ||
    ( o.getOpCode() == TC13::OpCode::RFE ) );
};

}       // namespace WIR
