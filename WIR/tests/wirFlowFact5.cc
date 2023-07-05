/*

   This source file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Intermediate Representation Framework

   and is property of its respective copyright holder. It must neither be used
   nor published even in parts without explicit written permission.

   Copyright 2021 - 2022

   Hamburg University of Technology (TUHH)
   Institute of Embedded Systems
   21071 Hamburg
   Germany

   http://www.tuhh.de/es/esd/research/wcc

*/


//
// Include section
//

// Include libuseful headers
#include <libuseful/io.h>

// Include WIR headers
#include <wir/wir.h>
#include <arch/generic/mips.h>


using namespace std;
using namespace WIR;


/*
  Testing structural analysis done by WIR loop bounds.
*/

int main( void )
{
  WIR_Init();

  // Check acyclic switch-case regions with structural CFG analysis.

  WIR_Function f( "foo" );
  auto &r1 = f.pushBackVirtualRegister( MIPS_RegV() );
  auto &r2 = f.pushBackVirtualRegister( MIPS_RegV() );

  // For this test CFG, we need 7 basic blocks.
  WIR_BasicBlock &b1 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &b2 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &b3 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &b4 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &b5 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &b6 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &b7 = f.pushBackBasicBlock( {} );

  b1.pushBackInstruction(
    { { MIPS::OpCode::LW, MIPS::OperationFormat::RIR_1,
        WIR_RegisterParameter( r1, WIR_Usage::def ),
        MIPS_Immediate16_Signed( -138 ),
        WIR_RegisterParameter( r2, WIR_Usage::use ) } } );
  b1.pushBackInstruction(
    { { MIPS::OpCode::JR, MIPS::OperationFormat::R_2,
        WIR_RegisterParameter( r1, WIR_Usage::use ) } } );
  b1.rbegin()->get().begin()->get().addJumpTarget( b2 );
  b1.rbegin()->get().begin()->get().addJumpTarget( b3 );
  b1.rbegin()->get().begin()->get().addJumpTarget( b4 );
  b1.rbegin()->get().begin()->get().addJumpTarget( b5 );
  b1.rbegin()->get().begin()->get().addJumpTarget( b6 );

  b2.pushBackInstruction(
    { { MIPS::OpCode::J, MIPS::OperationFormat::L,
        WIR_LabelParameter( b7 ) } } );

  b3.pushBackInstruction(
    { { MIPS::OpCode::J, MIPS::OperationFormat::L,
        WIR_LabelParameter( b7 ) } } );

  b4.pushBackInstruction(
    { { MIPS::OpCode::J, MIPS::OperationFormat::L,
        WIR_LabelParameter( b7 ) } } );

  b5.pushBackInstruction(
    { { MIPS::OpCode::J, MIPS::OperationFormat::L,
        WIR_LabelParameter( b7 ) } } );

  b6.pushBackInstruction(
    { { MIPS::OpCode::J, MIPS::OperationFormat::L,
        WIR_LabelParameter( b7 ) } } );

  b7.pushBackInstruction(
    { { MIPS::OpCode::JR, MIPS::OperationFormat::R_2,
        WIR_RegisterParameter( r1, WIR_Usage::use ) } } );

  WIR_LoopBound( 1, 2, b1, WIR_LoopBound::LoopControlType::headcontrolled );
  WIR_LoopBound( 1, 2, b2, WIR_LoopBound::LoopControlType::headcontrolled );
  WIR_LoopBound( 1, 2, b3, WIR_LoopBound::LoopControlType::headcontrolled );
  WIR_LoopBound( 1, 2, b4, WIR_LoopBound::LoopControlType::headcontrolled );
  WIR_LoopBound( 1, 2, b5, WIR_LoopBound::LoopControlType::headcontrolled );
  WIR_LoopBound( 1, 2, b6, WIR_LoopBound::LoopControlType::headcontrolled );

  return( 0 );
};
