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
  @file tc1796_cpu_tc_065.cc
  @brief This file implements a peephole optimizer for silicon bug TC1796
         CPU_TC.065 detection and correction.

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
#include "tc1796_cpu_tc_065.h"


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
TC1796_CPU_TC_065::TC1796_CPU_TC_065( WIR_Function &f ) :
  WIR_Optimization { f },
  WIR_SiliconBugs { f, "TC1796 CPU_TC.065", 2, true }
{
  DSTART( "TC1796_CPU_TC_065::TC1796_CPU_TC_065(WIR_Function&)" );
};


/*
  Destructor.
*/
TC1796_CPU_TC_065::~TC1796_CPU_TC_065( void )
{
  DSTART( "virtual TC1796_CPU_TC_065::~TC1796_CPU_TC_065()" );
};


//
// Protected class methods
//

/*
  matchSiliconBug determines whether the specified peephole matches with silicon
  bug CPU_TC.065.
*/
bool TC1796_CPU_TC_065::matchSiliconBug( const WIR_Peephole::peephole &p )
{
  DSTART( "virtual bool TC1796_CPU_TC_065::matchSiliconBug(const peephole&)" );

  WIR_Operation &o0 = p[ 0 ]->get().rbegin()->get();
  WIR_Operation &o1 = p[ 1 ]->get().begin()->get();

  if ( ( o0.getOpCode() == TC13::OpCode::LOOPU ) &&
       ( ( o1.getOpCode() == TC13::OpCode::J ) ||
         ( o1.getOpCode() == TC13::OpCode::JA ) ||
         ( o1.getOpCode() == TC13::OpCode::JI ) ||
         ( o1.getOpCode() == TC13::OpCode::JL ) ||
         ( o1.getOpCode() == TC13::OpCode::JLA ) ||
         ( o1.getOpCode() == TC13::OpCode::JLI ) ) &&
       ( dynamic_cast<WIR_LabelParameter &>(
           o0.getExplicitParameters().rbegin()->get() ).getBasicBlock() ==
         o1.getInstruction().getBasicBlock() ) )
    return( true );

  return( false );
};


/*
  fixSiliconBug fixes silicon bug CPU_TC.065.
*/
WIR_Peephole::peephole TC1796_CPU_TC_065::fixSiliconBug( const WIR_Peephole::peephole &p ) const
{
  DSTART(
    "virtual WIR_Peephole::peephole TC1796_CPU_TC_065::fixSiliconBug(const peephole&) const" );

  WIR_BasicBlock &b = p[ 1 ]->get().getBasicBlock();

  WIR_Peephole::peephole res;
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

}       // namespace WIR
