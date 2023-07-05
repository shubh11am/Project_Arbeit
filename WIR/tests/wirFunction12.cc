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


int main( void )
{
  WIR_Init();

  // Check the successor/predecessor relations of basic blocks.

  WIR_Function f( "foo" );
  auto &r1 = f.pushBackVirtualRegister( MIPS_RegV() );
  auto &r2 = f.pushBackVirtualRegister( MIPS_RegV() );
  auto &r3 = f.pushBackVirtualRegister( MIPS_RegV() );

  // For this test CFG, we need 7 basic blocks.
  WIR_BasicBlock &b1 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &b2 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &b3 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &b4 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &b5 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &b6 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &b7 = f.pushBackBasicBlock( {} );

  b1.pushBackInstruction(
    { { MIPS::OpCode::BEQ, MIPS::OperationFormat::RRL,
        WIR_RegisterParameter( r1, WIR_Usage::use ),
        WIR_RegisterParameter( r2, WIR_Usage::use ),
        WIR_LabelParameter( b7 ) } } );

  b2.pushBackInstruction(
    { { MIPS::OpCode::BNE, MIPS::OperationFormat::RRL,
        WIR_RegisterParameter( r3, WIR_Usage::use ),
        WIR_RegisterParameter( r1, WIR_Usage::use ),
        WIR_LabelParameter( b5 ) } } );

  b3.pushBackInstruction(
    { { MIPS::OpCode::LW, MIPS::OperationFormat::RIR_1,
        WIR_RegisterParameter( r1, WIR_Usage::def ),
        MIPS_Immediate16_Signed( -138 ),
        WIR_RegisterParameter( r2, WIR_Usage::use ) } } );

  b4.pushBackInstruction(
    { { MIPS::OpCode::ADDI, MIPS::OperationFormat::RRI,
        WIR_RegisterParameter( r1, WIR_Usage::def ),
        WIR_RegisterParameter( r2, WIR_Usage::use ),
        MIPS_Immediate16_Signed( 1 ) } } );
  b4.pushBackInstruction(
    { { MIPS::OpCode::J, MIPS::OperationFormat::L,
        WIR_LabelParameter( b2 ) } } );

  b5.pushBackInstruction(
    { { MIPS::OpCode::JAL, MIPS::OperationFormat::L,
        WIR_LabelParameter( f ) } } );

  b6.pushBackInstruction(
    { { MIPS::OpCode::ADDI, MIPS::OperationFormat::RRI,
        WIR_RegisterParameter( r1, WIR_Usage::def ),
        WIR_RegisterParameter( r2, WIR_Usage::use ),
        MIPS_Immediate16_Signed( -4 ) } } );
  b6.pushBackInstruction(
    { { MIPS::OpCode::J, MIPS::OperationFormat::L,
        WIR_LabelParameter( b1 ) } } );

  b7.pushBackInstruction(
    { { MIPS::OpCode::JR, MIPS::OperationFormat::R_2,
        WIR_RegisterParameter( r1, WIR_Usage::use ) } } );

  // Now, let's add some explicit successors.
  WIR_Operation &o1 =
    b7.getInstructions().back().get().getOperations().back().get();
  WIR_Operation &o3 =
    b1.getInstructions().back().get().getOperations().back().get();
  WIR_Operation &o4 =
    b4.getInstructions().front().get().getOperations().front().get();
  o1.addJumpTarget( b3 );
  o1.addJumpTarget( b1 );
  o3.addJumpTarget( b4 );
  o4.addJumpTarget( b7 );

  // Copy function and verify that all explicit successors were correctly
  // adjusted.
  WIR_Function f1( f );
  f1.setName( "bar" );

  auto it = f1.getBasicBlocks().begin();
  WIR_BasicBlock &b1new = *(it++);
  WIR_BasicBlock &b2new = *(it++);
  WIR_BasicBlock &b3new = *(it++);
  WIR_BasicBlock &b4new = *(it++);
  WIR_BasicBlock &b5new = *(it++);
  WIR_BasicBlock &b6new = *(it++);
  WIR_BasicBlock &b7new = *(it++);

  WIR_BasicBlockSet succ1 = b1new.getSuccessors();
  WIR_BasicBlockSet succ2 = b2new.getSuccessors();
  WIR_BasicBlockSet succ3 = b3new.getSuccessors();
  WIR_BasicBlockSet succ4 = b4new.getSuccessors();
  WIR_BasicBlockSet succ5 = b5new.getSuccessors();
  WIR_BasicBlockSet succ6 = b6new.getSuccessors();
  WIR_BasicBlockSet succ7 = b7new.getSuccessors();

  ufAssert( ( succ1.size() == 1 ) && succ1.count( b4new ) );
  ufAssert(
    ( succ2.size() == 2 ) && succ2.count( b3new ) && succ2.count( b5new ) );
  ufAssert( ( succ3.size() == 1 ) && succ3.count( b4new ) );
  ufAssert( ( succ4.size() == 1 ) && succ4.count( b2new ) );
  ufAssert( ( succ5.size() == 1 ) && succ5.count( b6new ) );
  ufAssert( ( succ6.size() == 1 ) && succ6.count( b1new ) );
  ufAssert(
    ( succ7.size() == 2 ) && succ7.count( b1new ) && succ7.count( b3new ) );

  WIR_BasicBlockSet pred1 = b1new.getPredecessors();
  WIR_BasicBlockSet pred2 = b2new.getPredecessors();
  WIR_BasicBlockSet pred3 = b3new.getPredecessors();
  WIR_BasicBlockSet pred4 = b4new.getPredecessors();
  WIR_BasicBlockSet pred5 = b5new.getPredecessors();
  WIR_BasicBlockSet pred6 = b6new.getPredecessors();
  WIR_BasicBlockSet pred7 = b7new.getPredecessors();

  ufAssert(
    ( pred1.size() == 2 ) && pred1.count( b6new ) && pred1.count( b7new ) );
  ufAssert( ( pred2.size() == 1 ) && ( pred2.count( b4new ) ) );
  ufAssert(
    ( pred3.size() == 2 ) && pred3.count( b2new ) && pred3.count( b7new ) );
  ufAssert(
    ( pred4.size() == 2 ) && pred4.count( b1new ) && pred4.count( b3new ) );
  ufAssert( ( pred5.size() == 1 ) && pred5.count( b2new ) );
  ufAssert( ( pred6.size() == 1 ) && pred6.count( b5new ) );
  ufAssert( pred7.size() == 0 );

  return( 0 );
}
