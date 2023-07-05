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
  @file tc1796_cpu_tc_081.cc
  @brief This file implements a peephole optimizer for silicon bug TC1796
         CPU_TC.081 detection and correction.

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
#include <libuseful/exceptions.h>

// Include WIR headers
#include <wir/wir.h>
#include <arch/tricore/tc13.h>

// Include local headers
#include "tc1796_cpu_tc_081.h"


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
TC1796_CPU_TC_081::TC1796_CPU_TC_081( WIR_Function &f ) :
  WIR_Optimization { f },
  WIR_SiliconBugs { f, "TC1796 CPU_TC.081", 1, false }
{
  DSTART( "TC1796_CPU_TC_081::TC1796_CPU_TC_081(WIR_Function&)" );
};


/*
  Destructor.
*/
TC1796_CPU_TC_081::~TC1796_CPU_TC_081( void )
{
  DSTART( "virtual TC1796_CPU_TC_081::~TC1796_CPU_TC_081()" );
};


//
// Protected class methods
//

/*
  matchSiliconBug determines whether the specified peephole matches with silicon
  bug CPU_TC.081.
*/
bool TC1796_CPU_TC_081::matchSiliconBug( const WIR_Peephole::peephole &p )
{
  DSTART( "virtual bool TC1796_CPU_TC_081::matchSiliconBug(const peephole&)" );

  vector<WIR_Operation *> ops;

  for ( auto it : p )
    for ( WIR_Operation &o : it->get() )
      if ( ops.size() < 2 )
        ops.push_back( &o );

  if ( isLDA( *ops[ 0 ] ) && definesSP( *ops[ 0 ] ) &&
       !mMatchedPeepholes.count( ops[ 0 ]->getID() ) ) {
    mMatchedPeepholes.insert( ops[ 0 ]->getID() );
    return( true );
  }

  if ( ( ops.size() == 2 ) && isLDA( *ops[ 1 ] ) && definesSP( *ops[ 1 ] ) &&
       !mMatchedPeepholes.count( ops[ 1 ]->getID() ) ) {
    mMatchedPeepholes.insert( ops[ 1 ]->getID() );
    return( true );
  }

  return( false );
};


/*
  fixSiliconBug fixes silicon bug CPU_TC.081.
*/
WIR_Peephole::peephole TC1796_CPU_TC_081::fixSiliconBug( const WIR_Peephole::peephole &p ) const
{
  DSTART(
    "virtual WIR_Peephole::peephole TC1796_CPU_TC_081::fixSiliconBug(const peephole&) const" );

  WIR_Peephole::peephole res;

  WIR_BasicBlock &b = p[ 0 ]->get().getBasicBlock();
  WIR_Function &f = b.getFunction();
  WIR_System &sys = f.getCompilationUnit().getSystem();
  TC13 &tc =
    dynamic_cast<TC13 &>( sys.findSymbol( f ).getSection().getProcessor() );
  WIR_VirtualRegister *sp = nullptr;

  // A small lambda to replace the stack pointer in an operation.
  auto replaceSP = [&]( WIR_Operation &o ) -> WIR_VirtualRegister & {
    sp =
      &( dynamic_cast<WIR_VirtualRegister &>(
           dynamic_cast<WIR_RegisterParameter &>(
             o.getExplicitParameters().begin()->get() ).getRegister() ) );

    // Add new virtual register that must not be A10.
    auto &vreg =
      ( sp->getType() == TC13::RegisterType::aReg ) ?
        b.getFunction().pushBackVirtualRegister( TC_ARegV() ) :
        b.getFunction().pushBackVirtualRegister( TC_PRegV() );
    if ( sp->getType() == TC13::RegisterType::aReg )
      f.insertInterference( vreg, tc.A10() );
    else
      f.insertInterference( vreg, tc.P10() );

    // Replace A10 by new virtual register in LD.A/LD.DA.
    o.replaceParameter(
      o.begin(), WIR_RegisterParameter( vreg, WIR_Usage::def ) );

    return( vreg );
  };

  // A small lambda to add MOV.AA operations.
  auto addMOV = [&]( WIR_VirtualRegister &vreg ) {
    if ( vreg.getType() == TC13::RegisterType::aReg )
      res.push_back(
        b.insertInstruction(
          next( p[ 0 ] ),
          { { TC13::OpCode::MOV_AA,
              m16BitOperations ?
                TC13::OperationFormat::SAA_1 : TC13::OperationFormat::AA,
              WIR_RegisterParameter( *sp, WIR_Usage::def ),
              WIR_RegisterParameter( vreg, WIR_Usage::use ) } } ) );
    else
      res.push_back(
        b.insertInstruction(
          next( p[ 0 ] ),
          WIR_Instruction {
            WIR_Operation {
              TC13::OpCode::MOV_AA,
              m16BitOperations ?
                TC13::OperationFormat::SAA_1 : TC13::OperationFormat::AA,
              WIR_RegisterParameter( sp->begin()->get(), WIR_Usage::def ),
              WIR_RegisterParameter(
                vreg.begin()->get(), WIR_Usage::use ) },
            WIR_Operation {
              TC13::OpCode::MOV_AA,
              m16BitOperations ?
                TC13::OperationFormat::SAA_1 : TC13::OperationFormat::AA,
              WIR_RegisterParameter( sp->rbegin()->get(), WIR_Usage::def ),
              WIR_RegisterParameter(
                vreg.rbegin()->get(), WIR_Usage::use ) } } ) );

    markInstruction( res.back()->get() );

    b.insertContainer(
      WIR_SchedulingConstraint {
        WIR_SchedulingConstraintType::sequential,
        (*(next( res.rbegin() )))->get() , res.back()->get() } );
  };

  vector<WIR_Operation *> ops;

  for ( auto it : p )
    for ( WIR_Operation &o : it->get() )
      if ( ops.size() < 2 )
        ops.push_back( &o );

  res.push_back( p[ 0 ] );

  if ( ( ops.size() == 1 ) && isLDA( *ops[ 0 ] ) && definesSP( *ops[ 0 ] ) ) {
    auto &vreg = replaceSP( *ops[ 0 ] );
    addMOV( vreg );
  } else

  if ( ( ops.size() == 2 ) && isLDA( *ops[ 0 ] ) && definesSP( *ops[ 0 ] ) &&
       ( !isLDA( *ops[ 1 ] ) || !definesSP( *ops[ 1 ] ) ) ) {
    auto it = b.insertInstruction( next( p[ 0 ] ), {} );
    it->get().moveOperation( *ops[ 1 ] );

    auto &vreg = replaceSP( *ops[ 0 ] );
    addMOV( vreg );

    res.push_back( it );
  } else

  if ( ( ops.size() == 2 ) && isLDA( *ops[ 1 ] ) && definesSP( *ops[ 1 ] ) &&
       ( !isLDA( *ops[ 0 ] ) || !definesSP( *ops[ 0 ] ) ) ) {
    auto &vreg = replaceSP( *ops[ 1 ] );
    addMOV( vreg );
  } else {
    auto it = b.insertInstruction( next( p[ 0 ] ), {} );
    it->get().moveOperation( *ops[ 1 ] );

    auto &vreg1 = replaceSP( *ops[ 0 ] );
    addMOV( vreg1 );

    res.push_back( it );

    auto &vreg2 = replaceSP( *(it->get().begin()) );
    addMOV( vreg2 );
  }

  return( res );
};


//
// Private class methods
//

/*
  isLDA determines whether an operation is an address register load subject to
  silicon bug CPU_TC.013.
*/
bool TC1796_CPU_TC_081::isLDA( const WIR_Operation &o ) const
{
  DSTART( "bool TC1796_CPU_TC_081::isLDA(const WIR_Operation&) const" );

  return(
    ( o.getOpCode() == TC13::OpCode::LD_A ) ||
    ( o.getOpCode() == TC13::OpCode::LD_DA ) );
};


/*
  definesSP determines whether an operation defines the stack pointer A10.
*/
bool TC1796_CPU_TC_081::definesSP( const WIR_Operation &o ) const
{
  DSTART( "bool TC1796_CPU_TC_081::definesSP(const WIR_Operation&) const" );

  auto &r =
    dynamic_cast<WIR_RegisterParameter &>(
      o.getExplicitParameters().begin()->get() ).getRegister();

  bool isSP = false;
  if ( r.getType() == TC13::RegisterType::aReg )
    isSP = TC13::isSP( r );
  else {
    auto childs = r.getLeafs();
    isSP =
      TC13::isSP( childs.front().get() ) || TC13::isSP( childs.back().get() );
  }

  if ( isSP ) {
    if ( r.isVirtual() )
      return( true );
    else
      throw ufFatalError(
        o.getInstruction().getBasicBlock().getFunction().getName(), 0,
        "Detected code triggering silicon bug " + mName + "." );
  }

  return( false );
};

}       // namespace WIR
