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
  @file tc1796_cpu_tc_095.cc
  @brief This file implements a peephole optimizer for silicon bug TC1796
         CPU_TC.095 detection and correction.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include standard headers
#include <set>
#include <vector>

// Include libuseful headers
#include <libuseful/debugmacros.h>

// Include WIR headers
#include <wir/wir.h>
#include <arch/tricore/tc13.h>

// Include local headers
#include "tc1796_cpu_tc_095.h"


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
TC1796_CPU_TC_095::TC1796_CPU_TC_095( WIR_Function &f ) :
  WIR_Optimization { f },
  WIR_SiliconBugs { f, "TC1796 CPU_TC.095", 3, true }
{
  DSTART( "TC1796_CPU_TC_095::TC1796_CPU_TC_095(WIR_Function&)" );
};


/*
  Destructor.
*/
TC1796_CPU_TC_095::~TC1796_CPU_TC_095( void )
{
  DSTART( "virtual TC1796_CPU_TC_095::~TC1796_CPU_TC_095()" );
};


//
// Protected class methods
//

/*
  matchSiliconBug determines whether the specified peephole matches with silicon
  bug CPU_TC.095.
*/
bool TC1796_CPU_TC_095::matchSiliconBug( const WIR_Peephole::peephole &p )
{
  DSTART( "virtual bool TC1796_CPU_TC_095::matchSiliconBug(const peephole&)" );

  WIR_BasicBlock &b = p[ 0 ]->get().getBasicBlock();
  WIR_Function &f = b.getFunction();
  WIR_System &sys = f.getCompilationUnit().getSystem();
  TC13 &tc =
    dynamic_cast<TC13 &>( sys.findSymbol( f ).getSection().getProcessor() );

  vector<WIR_Operation *> ops;

  for ( auto it : p )
    for ( WIR_Operation &o : it->get() )
      if ( ops.size() < 4 )
        ops.push_back( &o );

  if ( ( ops[ 0 ]->getInstruction() == p[ 0 ]->get() ) &&
       isSequence1Start( *ops[ 0 ] ) && isSATH( *ops[ 1 ] ) &&
       isSequenceEnd( *ops[ 2 ] ) ) {
    auto &destReg =
      dynamic_cast<WIR_RegisterParameter &>(
        ops[ 0 ]->begin()->get() ).getRegister();

    auto it = next( ops[ 2 ]->begin() );
    if ( ( ops[ 2 ]->getOpCode() == TC13::OpCode::ADDSC_A ) ||
         ( ops[ 2 ]->getOpCode() == TC13::OpCode::ADDSC_AT ) )
      it = next( it );
    auto &srcReg =
      dynamic_cast<WIR_RegisterParameter &>( it->get() ).getRegister();

    if ( destReg == srcReg )
      return( true );
  }

  if ( ( ops.size() == 4 ) && ( ops[ 1 ]->getInstruction() == p[ 0 ]->get() ) &&
       isSequence1Start( *ops[ 1 ] ) && isSATH( *ops[ 2 ] ) &&
       isSequenceEnd( *ops[ 3 ] ) ) {
    auto &destReg =
      dynamic_cast<WIR_RegisterParameter &>(
        ops[ 1 ]->begin()->get() ).getRegister();

    auto it = next( ops[ 2 ]->begin() );
    if ( ( ops[ 3 ]->getOpCode() == TC13::OpCode::ADDSC_A ) ||
         ( ops[ 3 ]->getOpCode() == TC13::OpCode::ADDSC_AT ) )
      it = next( it );
    auto &srcReg =
      dynamic_cast<WIR_RegisterParameter &>( it->get() ).getRegister();

    if ( destReg == srcReg )
      return( true );
  }

  if ( ( ops[ 0 ]->getInstruction() == p[ 0 ]->get() ) &&
       isSequence2Start( *ops[ 0 ] ) && isSAT( *ops[ 1 ] ) &&
       isSequenceEnd( *ops[ 2 ] ) ) {
    set<WIR_id_t> destRegs;

    if ( ops[ 0 ]->getOpCode() == TC13::OpCode::LD_D )
      destRegs.insert(
        dynamic_cast<WIR_RegisterParameter &>(
          ops[ 0 ]->begin()->get() ).getRegister().getLeafs().back().get().getID() );
    else

    if ( ( ops[ 0 ]->getOpCode() == TC13::OpCode::LDLCX ) ||
         ( ops[ 0 ]->getOpCode() == TC13::OpCode::RSLCX ) ) {
      destRegs.insert( tc.D1().getID() );
      destRegs.insert( tc.D3().getID() );
      destRegs.insert( tc.D5().getID() );
      destRegs.insert( tc.D7().getID() );
    } else {
      destRegs.insert( tc.D9().getID() );
      destRegs.insert( tc.D11().getID() );
      destRegs.insert( tc.D13().getID() );
      destRegs.insert( tc.D15().getID() );
    }

    auto it = next( ops[ 2 ]->begin() );
    if ( ( ops[ 2 ]->getOpCode() == TC13::OpCode::ADDSC_A ) ||
         ( ops[ 2 ]->getOpCode() == TC13::OpCode::ADDSC_AT ) )
      it = next( it );
    auto &srcReg =
      dynamic_cast<WIR_RegisterParameter &>( it->get() ).getRegister();

    if ( destRegs.count( srcReg.getID() ) )
      return( true );
  }

  if ( ( ops.size() == 4 ) && ( ops[ 1 ]->getInstruction() == p[ 0 ]->get() ) &&
       isSequence2Start( *ops[ 1 ] ) && isSAT( *ops[ 2 ] ) &&
       isSequenceEnd( *ops[ 3 ] ) ) {
    set<WIR_id_t> destRegs;

    if ( ops[ 1 ]->getOpCode() == TC13::OpCode::LD_D )
      destRegs.insert(
        dynamic_cast<WIR_RegisterParameter &>(
          ops[ 1 ]->begin()->get() ).getRegister().getLeafs().back().get().getID() );
    else

    if ( ( ops[ 1 ]->getOpCode() == TC13::OpCode::LDLCX ) ||
         ( ops[ 1 ]->getOpCode() == TC13::OpCode::RSLCX ) ) {
      destRegs.insert( tc.D1().getID() );
      destRegs.insert( tc.D3().getID() );
      destRegs.insert( tc.D5().getID() );
      destRegs.insert( tc.D7().getID() );
    } else {
      destRegs.insert( tc.D9().getID() );
      destRegs.insert( tc.D11().getID() );
      destRegs.insert( tc.D13().getID() );
      destRegs.insert( tc.D15().getID() );
    }

    auto it = next( ops[ 3 ]->begin() );
    if ( ( ops[ 3 ]->getOpCode() == TC13::OpCode::ADDSC_A ) ||
         ( ops[ 3 ]->getOpCode() == TC13::OpCode::ADDSC_AT ) )
      it = next( it );
    auto &srcReg =
      dynamic_cast<WIR_RegisterParameter &>( it->get() ).getRegister();

    if ( destRegs.count( srcReg.getID() ) )
      return( true );
  }

  return( false );
};


/*
  fixSiliconBug fixes silicon bug CPU_TC.095.
*/
WIR_Peephole::peephole TC1796_CPU_TC_095::fixSiliconBug( const WIR_Peephole::peephole &p ) const
{
  DSTART(
    "virtual WIR_Peephole::peephole TC1796_CPU_TC_095::fixSiliconBug(const peephole&) const" );

  WIR_Peephole::peephole res;

  WIR_BasicBlock &b = p[ 1 ]->get().getBasicBlock();

  vector<WIR_Operation *> ops;

  for ( auto it : p )
    for ( WIR_Operation &o : it->get() )
      if ( ops.size() < 4 )
        ops.push_back( &o );

  res.push_back( p[ 0 ] );

  if ( isSequenceEnd( *ops[ 2 ] ) ) {
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
      res.push_back( p[ 1 ] );

      res.push_back(
        b.insertInstruction(
          p[ 2 ],
          { { TC13::OpCode::NOP,
              m16BitOperations ?
                TC13::OperationFormat::S : TC13::OperationFormat::SYS } } ) );
      markInstruction( res[ 2 ]->get() );

      res.push_back( b.insertInstruction( p[ 1 ], {} ) );
      res[ 3 ]->get().moveOperation( *ops[ 2 ] );

      b.insertContainer(
        WIR_SchedulingConstraint {
          WIR_SchedulingConstraintType::sequential,
          res[ 2 ]->get(), res[ 3 ]->get() } );
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
  } else {
    res.push_back( p[ 1 ] );

    res.push_back(
      b.insertInstruction(
        p[ 2 ],
        { { TC13::OpCode::NOP,
            m16BitOperations ?
              TC13::OperationFormat::S : TC13::OperationFormat::SYS } } ) );
    markInstruction( res[ 2 ]->get() );

    if ( *ops[ 3 ] == p[ 1 ]->get().getOperations().back().get() ) {
      res.push_back( b.insertInstruction( p[ 1 ], {} ) );
      res[ 3 ]->get().moveOperation( *ops[ 3 ] );
    } else
      res.push_back( p[ 2 ] );

    b.insertContainer(
      WIR_SchedulingConstraint {
        WIR_SchedulingConstraintType::sequential,
        res[ 2 ]->get(), res[ 3 ]->get() } );
  }

  return( res );
};


//
// Private class methods
//

/*
  isSequence1Start determines whether an operation is the first one from Problem
  Sequence 1) subject to silicon bug CPU_TC.095.
*/
bool TC1796_CPU_TC_095::isSequence1Start( const WIR_Operation &o ) const
{
  DSTART(
    "bool TC1796_CPU_TC_095::isSequence1Start(const WIR_Operation&) const" );

  return(
    ( o.getOpCode() == TC13::OpCode::MOV_D ) ||
    ( o.getOpCode() == TC13::OpCode::EQ_A ) ||
    ( o.getOpCode() == TC13::OpCode::NE_A ) ||
    ( o.getOpCode() == TC13::OpCode::LT_A ) ||
    ( o.getOpCode() == TC13::OpCode::GE_A ) ||
    ( o.getOpCode() == TC13::OpCode::EQZ_A ) ||
    ( o.getOpCode() == TC13::OpCode::NEZ_A ) ||
    ( o.getOpCode() == TC13::OpCode::MFCR ) );
};


/*
  isSequence2Start determines whether an operation is the first one from Problem
  Sequence 2) subject to silicon bug CPU_TC.095.
*/
bool TC1796_CPU_TC_095::isSequence2Start( const WIR_Operation &o ) const
{
  DSTART(
    "bool TC1796_CPU_TC_095::isSequence2Start(const WIR_Operation&) const" );

  return(
    ( o.getOpCode() == TC13::OpCode::LD_D ) ||
    ( o.getOpCode() == TC13::OpCode::LDLCX ) ||
    ( o.getOpCode() == TC13::OpCode::LDUCX ) ||
    ( o.getOpCode() == TC13::OpCode::RSLCX ) ||
    ( o.getOpCode() == TC13::OpCode::RET ) ||
    ( o.getOpCode() == TC13::OpCode::RFE ) ||
    ( o.getOpCode() == TC13::OpCode::RFM ) );
};


/*
  isSequenceEnd determines whether an operation is the last one from Problem
  Sequences subject to silicon bug CPU_TC.095.
*/
bool TC1796_CPU_TC_095::isSequenceEnd( const WIR_Operation &o ) const
{
  DSTART( "bool TC1796_CPU_TC_095::isSequenceEnd(const WIR_Operation&) const" );

  return(
    ( o.getOpCode() == TC13::OpCode::ADDSC_A ) ||
    ( o.getOpCode() == TC13::OpCode::ADDSC_AT ) ||
    ( o.getOpCode() == TC13::OpCode::MOV_A ) ||
    ( o.getOpCode() == TC13::OpCode::MTCR ) );
};


/*
  isSATH determines whether an operation is a SAT.H subject to silicon bug
  CPU_TC.095.
*/
bool TC1796_CPU_TC_095::isSATH( const WIR_Operation &o ) const
{
  DSTART( "bool TC1796_CPU_TC_095::isSATH(const WIR_Operation&) const" );

  return( o.getOpCode() == TC13::OpCode::SAT_H );
};


/*
  isSAT determines whether an operation is a SAT.B or SAT.H subject to silicon
  bug CPU_TC.095.
*/
bool TC1796_CPU_TC_095::isSAT( const WIR_Operation &o ) const
{
  DSTART( "bool TC1796_CPU_TC_095::isSAT(const WIR_Operation&) const" );

  return( ( o.getOpCode() == TC13::OpCode::SAT_B ) || isSATH( o ) );
};

}       // namespace WIR
