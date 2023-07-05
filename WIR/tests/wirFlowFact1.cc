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

// Include standard headers
#include <sstream>

// Include libuseful headers
#include <libuseful/io.h>

// Include WIR headers
#include <wir/wir.h>
#include <arch/generic/mips.h>


using namespace std;
using namespace WIR;


/*
  This file will test the class WIR_Loopbound.
  This test should exit normally, no assertion should fail.
*/

int main( void )
{
  WIR_Init();

  // Create a test CFG containing a while-loop, basic block b2 is the head/entry
  // of that loop.

  WIR_Function f( "foo" );
  auto &r1 = f.pushBackVirtualRegister( MIPS_RegV() );
  auto &r2 = f.pushBackVirtualRegister( MIPS_RegV() );

  // For this test CFG, we need 8 basic blocks.
  WIR_BasicBlock &b1 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &b2 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &b3 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &b4 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &b5 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &b6 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &b7 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &b8 = f.pushBackBasicBlock( {} );

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

  // Test default constructor.
  WIR_LoopBound loop1;

  ufAssert( loop1.getType() == WIR_FlowFactType::loopbound );
  ufAssert( loop1.getMin() == -1 );
  ufAssert( loop1.getMax() == -1 );
  ufAssert( loop1.isBoundSpecified() == false );

  loop1.setLoop( b2, WIR_LoopBound::LoopControlType::headcontrolled );
  ufAssert( loop1.getLoop().getID() == b2.getID() );
  ufAssert(
    loop1.getLoopControlType() ==
      WIR_LoopBound::LoopControlType::headcontrolled );

  // Setting a maximum iteration should also set the min to 0.
  loop1.setMax( 10 );
  ufAssert( loop1.getMin() == 0 );
  ufAssert( loop1.getMax() == 10 );
  ufAssert( loop1.isBoundSpecified() == true );

  loop1.setMin( 5 );
  ufAssert( loop1.getMin() == 5 );
  ufAssert( loop1.getMax() == 10 );

  // Setting min or max to impossible values changes nothing.
  loop1.setMax( 2 );
  ufAssert( loop1.getMin() == 5 );
  ufAssert( loop1.getMax() == 10 );

  loop1.setMin( 15 );
  ufAssert( loop1.getMin() == 5 );
  ufAssert( loop1.getMax() == 10 );

  // Resetting min to -1 should also reset max.
  loop1.setMin( -1 );
  ufAssert( loop1.getMin() == -1 );
  ufAssert( loop1.getMax() == -1 );

  // Test value constructor.
  WIR_LoopBound loop2(
    2, 4, b2, WIR_LoopBound::LoopControlType::tailcontrolled );

  ufAssert( loop2.getMin() == 2 );
  ufAssert( loop2.getMax() == 4 );
  ufAssert( loop2.getLoop().getID() == b2.getID() );
  ufAssert(
    loop2.getLoopControlType() ==
      WIR_LoopBound::LoopControlType::tailcontrolled );

  // Test copy constructor.
  WIR_LoopBound loop3( loop2 );

  ufAssert( loop3.getID() != loop2.getID() );
  ufAssert( loop3.getMin() == 2 );
  ufAssert( loop3.getMax() == 4 );
  ufAssert( loop3.getLoop().getID() == b2.getID() );
  ufAssert(
    loop3.getLoopControlType() ==
      WIR_LoopBound::LoopControlType::tailcontrolled );

  // Test stream I/O.
  stringstream ss;
  ss << loop3;

  string verify = "Loop bound min: 2 max: 4 for: " + b2.getName() + "\n";
  ufAssert( ss.str() == verify );
};
