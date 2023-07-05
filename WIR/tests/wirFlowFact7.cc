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
  Testing different kind of loops for WIR_Loopbounds.
*/

int main( void )
{
  WIR_Init();

  WIR_Function f1( "foo" );
  auto &r1 = f1.pushBackVirtualRegister( MIPS_RegV() );
  auto &r2 = f1.pushBackVirtualRegister( MIPS_RegV() );

  // For this test CFG, we need 8 basic blocks.
  WIR_BasicBlock &b1 = f1.pushBackBasicBlock( {} );
  WIR_BasicBlock &b2 = f1.pushBackBasicBlock( {} );
  WIR_BasicBlock &b3 = f1.pushBackBasicBlock( {} );
  WIR_BasicBlock &b4 = f1.pushBackBasicBlock( {} );
  WIR_BasicBlock &b5 = f1.pushBackBasicBlock( {} );
  WIR_BasicBlock &b6 = f1.pushBackBasicBlock( {} );
  WIR_BasicBlock &b7 = f1.pushBackBasicBlock( {} );
  WIR_BasicBlock &b8 = f1.pushBackBasicBlock( {} );

  b1.pushBackInstruction(
    { { MIPS::OpCode::LW, MIPS::OperationFormat::RIR_1,
        WIR_RegisterParameter( r1, WIR_Usage::def ),
        MIPS_Immediate16_Signed( -138 ),
        WIR_RegisterParameter( r2, WIR_Usage::use ) } } );

  b2.pushBackInstruction(
    { { MIPS::OpCode::JR, MIPS::OperationFormat::R_2,
        WIR_RegisterParameter( r1, WIR_Usage::use ) } } );
  b2.rbegin()->get().begin()->get().addJumpTarget( b3 );
  b2.rbegin()->get().begin()->get().addJumpTarget( b6 );

  b3.pushBackInstruction(
    { { MIPS::OpCode::ADDI, MIPS::OperationFormat::RRI,
        WIR_RegisterParameter( r1, WIR_Usage::def ),
        WIR_RegisterParameter( r2, WIR_Usage::use ),
        MIPS_Immediate16_Signed( 0 ) } } );

  b4.pushBackInstruction(
    { { MIPS::OpCode::ADDI, MIPS::OperationFormat::RRI,
        WIR_RegisterParameter( r1, WIR_Usage::def ),
        WIR_RegisterParameter( r2, WIR_Usage::use ),
        MIPS_Immediate16_Signed( 0 ) } } );

  b5.pushBackInstruction(
    { { MIPS::OpCode::JR, MIPS::OperationFormat::R_2,
        WIR_RegisterParameter( r1, WIR_Usage::use ) } } );
  b5.rbegin()->get().begin()->get().addJumpTarget( b2 );

  b6.pushBackInstruction(
    { { MIPS::OpCode::ADDI, MIPS::OperationFormat::RRI,
        WIR_RegisterParameter( r1, WIR_Usage::def ),
        WIR_RegisterParameter( r2, WIR_Usage::use ),
        MIPS_Immediate16_Signed( 0 ) } } );

  b7.pushBackInstruction(
    { { MIPS::OpCode::ADDI, MIPS::OperationFormat::RRI,
        WIR_RegisterParameter( r1, WIR_Usage::def ),
        WIR_RegisterParameter( r2, WIR_Usage::use ),
        MIPS_Immediate16_Signed( 0 ) } } );

  b8.pushBackInstruction(
    { { MIPS::OpCode::ADDI, MIPS::OperationFormat::RRI,
        WIR_RegisterParameter( r1, WIR_Usage::def ),
        WIR_RegisterParameter( r2, WIR_Usage::use ),
        MIPS_Immediate16_Signed( 0 ) } } );

  // Do structural analysis of f2.
  WIR_StructuralAnalysis a { f1 };
  a.analyze();

  // Check resulting control tree.
  auto &c = b2.getContainers<WIR_ControlTree>().begin()->get();
  auto &leaf = c.getBasicBlockTreeNode();
  auto &whileloop = leaf.getParent();

  ufAssert( whileloop.getType() == WIR_CTNodeType::whileloop );
  ufAssert( whileloop.isCyclic() && !whileloop.isAcyclic() );
  ufAssert(
    dynamic_cast<const WIR_BasicBlockTreeNode &>(
      whileloop.getEntry() ).getBasicBlock() == b2 );

  // Assign loop to loop bound.
  WIR_LoopBound loop1
    { 0, 5, b2, WIR_LoopBound::LoopControlType::headcontrolled };
  ufAssert( loop1.getLoop() == b2 );


  // Create a CFG containing a self-loop. b12 is the self-looping basic block.

  WIR_Function f2( "bar" );
  auto &r11 = f2.pushBackVirtualRegister( MIPS_RegV() );
  auto &r12 = f2.pushBackVirtualRegister( MIPS_RegV() );

  auto &b11 = f2.pushBackBasicBlock( {} );
  auto &b12 = f2.pushBackBasicBlock( {} );
  auto &b13 = f2.pushBackBasicBlock( {} );

  b11.pushBackInstruction(
    { { MIPS::OpCode::LW, MIPS::OperationFormat::RIR_1,
        WIR_RegisterParameter( r11, WIR_Usage::def ),
        MIPS_Immediate16_Signed( -138 ),
        WIR_RegisterParameter( r12, WIR_Usage::use ) } } );

  b12.pushBackInstruction(
    { { MIPS::OpCode::JR, MIPS::OperationFormat::R_2,
        WIR_RegisterParameter( r11, WIR_Usage::use ) } } );
  b12.rbegin()->get().begin()->get().addJumpTarget( b12 );
  b12.rbegin()->get().begin()->get().addJumpTarget( b13 );

  b13.pushBackInstruction(
    { { MIPS::OpCode::ADDI, MIPS::OperationFormat::RRI,
        WIR_RegisterParameter( r11, WIR_Usage::def ),
        WIR_RegisterParameter( r12, WIR_Usage::use ),
        MIPS_Immediate16_Signed( 0 ) } } );

  // Assign a loop bound to this self-loop.
  WIR_LoopBound loop2;
  loop2.setMax( 10 );
  loop2.setLoop( b12, WIR_LoopBound::LoopControlType::headcontrolled );
  ufAssert( loop2.getLoop() == b12 );


  //
  // Create a tail-controlled (natural) loop.
  //

  WIR_Function f3( "baz" );
  auto &r21 = f3.pushBackVirtualRegister( MIPS_RegV() );
  auto &r22 = f3.pushBackVirtualRegister( MIPS_RegV() );

  auto &b21 = f3.pushBackBasicBlock( {} );
  auto &b22 = f3.pushBackBasicBlock( {} );
  auto &b23 = f3.pushBackBasicBlock( {} );
  auto &b24 = f3.pushBackBasicBlock( {} );

  b21.pushBackInstruction(
    { { MIPS::OpCode::LW, MIPS::OperationFormat::RIR_1,
        WIR_RegisterParameter( r21, WIR_Usage::def ),
        MIPS_Immediate16_Signed( -138 ),
        WIR_RegisterParameter( r22, WIR_Usage::use ) } } );

  b22.pushBackInstruction(
    { { MIPS::OpCode::ADDI, MIPS::OperationFormat::RRI,
        WIR_RegisterParameter( r21, WIR_Usage::def ),
        WIR_RegisterParameter( r22, WIR_Usage::use ),
        MIPS_Immediate16_Signed( 0 ) } } );

  b23.pushBackInstruction(
    { { MIPS::OpCode::JR, MIPS::OperationFormat::R_2,
        WIR_RegisterParameter( r21, WIR_Usage::use ) } } );
  b23.rbegin()->get().begin()->get().addJumpTarget( b22 );
  b23.rbegin()->get().begin()->get().addJumpTarget( b24 );

  b24.pushBackInstruction(
    { { MIPS::OpCode::ADDI, MIPS::OperationFormat::RRI,
        WIR_RegisterParameter( r21, WIR_Usage::def ),
        WIR_RegisterParameter( r22, WIR_Usage::use ),
        MIPS_Immediate16_Signed( 0 ) } } );

  // Create a loop bound referring to the natural loop.
  WIR_LoopBound loop3(
    0, 42, b22, WIR_LoopBound::LoopControlType::tailcontrolled );
  ufAssert( loop3.getLoop() == b22 );
};
