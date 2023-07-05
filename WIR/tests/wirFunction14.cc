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

  MIPS_RegV r1, r2, r3;
  WIR_Function f( "foo" );

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

  // Now, let's add some explicit successors and check what happens.
  WIR_Operation &o1 =
    b7.getInstructions().back().get().getOperations().back().get();
  o1.addJumpTarget( b3 );

  WIR_Operation &o3 =
    b1.getInstructions().back().get().getOperations().back().get();
  o3.addJumpTarget( b4 );

  WIR_BasicBlockSet succ1 = b1.getSuccessors();
  WIR_BasicBlockSet succ2 = b2.getSuccessors();
  WIR_BasicBlockSet succ3 = b3.getSuccessors();
  WIR_BasicBlockSet succ4 = b4.getSuccessors();
  WIR_BasicBlockSet succ5 = b5.getSuccessors();
  WIR_BasicBlockSet succ6 = b6.getSuccessors();
  WIR_BasicBlockSet succ7 = b7.getSuccessors();

  ufAssert( ( succ1.size() == 1 ) && succ1.count( b4 ) );
  ufAssert( ( succ2.size() == 2 ) && succ2.count( b3 ) && succ2.count( b5 ) );
  ufAssert( ( succ3.size() == 1 ) && succ3.count( b4 ) );
  ufAssert( ( succ4.size() == 1 ) && succ4.count( b2 ) );
  ufAssert( ( succ5.size() == 1 ) && succ5.count( b6 ) );
  ufAssert( ( succ6.size() == 1 ) && succ6.count( b1 ) );
  ufAssert( ( succ7.size() == 1 ) && succ7.count( b3 ) );

  WIR_BasicBlockSet pred1 = b1.getPredecessors();
  WIR_BasicBlockSet pred2 = b2.getPredecessors();
  WIR_BasicBlockSet pred3 = b3.getPredecessors();
  WIR_BasicBlockSet pred4 = b4.getPredecessors();
  WIR_BasicBlockSet pred5 = b5.getPredecessors();
  WIR_BasicBlockSet pred6 = b6.getPredecessors();
  WIR_BasicBlockSet pred7 = b7.getPredecessors();

  ufAssert( ( pred1.size() == 1 ) && pred1.count( b6 ) );
  ufAssert( ( pred2.size() == 1 ) && pred2.count( b4 ) );
  ufAssert( ( pred3.size() == 2 ) && pred3.count( b2 ) && pred3.count( b7 ) );
  ufAssert( ( pred4.size() == 2 ) && pred4.count( b1 ) && pred4.count( b3 ) );
  ufAssert( ( pred5.size() == 1 ) && pred5.count( b2 ) );
  ufAssert( ( pred6.size() == 1 ) && pred6.count( b5 ) );
  ufAssert( pred7.size() == 0 );

  // Erase basic block b4 that is explicit jump target and check what happens.
  auto it = f.getBasicBlocks().begin();
  (it++);
  (it++);
  (it++);
  f.eraseBasicBlock( it );

  succ1 = b1.getSuccessors();
  succ2 = b2.getSuccessors();
  succ3 = b3.getSuccessors();
  succ5 = b5.getSuccessors();
  succ6 = b6.getSuccessors();
  succ7 = b7.getSuccessors();

  ufAssert( ( succ1.size() == 2 ) && succ1.count( b2 ) && succ1.count( b7 ) );
  ufAssert( ( succ2.size() == 2 ) && succ2.count( b3 ) && succ2.count( b5 ) );
  ufAssert( ( succ3.size() == 1 ) && succ3.count( b5 ) );
  ufAssert( ( succ5.size() == 1 ) && succ5.count( b6 ) );
  ufAssert( ( succ6.size() == 1 ) && succ6.count( b1 ) );
  ufAssert( ( succ7.size() == 1 ) && succ7.count( b3 ) );

  pred1 = b1.getPredecessors();
  pred2 = b2.getPredecessors();
  pred3 = b3.getPredecessors();
  pred5 = b5.getPredecessors();
  pred6 = b6.getPredecessors();
  pred7 = b7.getPredecessors();

  ufAssert( ( pred1.size() == 1 ) && pred1.count( b6 ) );
  ufAssert( ( pred2.size() == 1 ) && pred2.count( b1 ) );
  ufAssert( ( pred3.size() == 2 ) && pred3.count( b2 ) && pred3.count( b7 ) );
  ufAssert( ( pred5.size() == 2 ) && pred5.count( b2 ) && pred5.count( b3 ) );
  ufAssert( ( pred6.size() == 1 ) && pred6.count( b5 ) );
  ufAssert( ( pred7.size() == 1 ) && pred7.count( b1 ) );

  return( 0 );
}
