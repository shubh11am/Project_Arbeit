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

// Include standard headers
#include <set>
#include <string>

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

  MIPS_RegV r1, r2, r3;
  WIR_Function f( "foo" );

  // For this test CFG, we need 14 basic blocks.
  WIR_BasicBlock &b0 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &b1 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &b2 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &b3 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &b3e1 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &b3e2 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &b3e3 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &b4 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &b5 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &b6 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &b7 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &bexit1 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &bexit2 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &bexit3 = f.pushBackBasicBlock( {} );

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
        WIR_LabelParameter( b3e1 ) } } );

  b7.pushBackInstruction(
    { { MIPS::OpCode::JR, MIPS::OperationFormat::R_2,
        WIR_RegisterParameter( r1, WIR_Usage::use ) } } );

  // Now, add an explicit successor.
  WIR_Operation &o1 =
    b7.getInstructions().back().get().getOperations().back().get();
  o1.addJumpTarget( b3 );

  // Attach some comments to b3e1, b3e2 and b3e3 that must be moved afterwards.
  b3e1.insertContainer( WIR_Comment( "b3e1" ) );
  b3e2.insertContainer( WIR_Comment( "b3e2" ) );
  b3e3.insertContainer( WIR_Comment( "b3e3" ) );

  WIR_BasicBlockSet succ0 = b0.getSuccessors();
  WIR_BasicBlockSet succ1 = b1.getSuccessors();
  WIR_BasicBlockSet succ2 = b2.getSuccessors();
  WIR_BasicBlockSet succ3 = b3.getSuccessors();
  WIR_BasicBlockSet succ3e1 = b3e1.getSuccessors();
  WIR_BasicBlockSet succ3e2 = b3e2.getSuccessors();
  WIR_BasicBlockSet succ3e3 = b3e3.getSuccessors();
  WIR_BasicBlockSet succ4 = b4.getSuccessors();
  WIR_BasicBlockSet succ5 = b5.getSuccessors();
  WIR_BasicBlockSet succ6 = b6.getSuccessors();
  WIR_BasicBlockSet succ7 = b7.getSuccessors();
  WIR_BasicBlockSet succexit1 = bexit1.getSuccessors();
  WIR_BasicBlockSet succexit2 = bexit2.getSuccessors();
  WIR_BasicBlockSet succexit3 = bexit3.getSuccessors();

  ufAssert( ( succ0.size() == 1 ) && succ0.count( b1 ) );
  ufAssert( ( succ1.size() == 2 ) && succ1.count( b2 ) && succ1.count( b7 ) );
  ufAssert( ( succ2.size() == 2 ) && succ2.count( b3 ) && succ2.count( b5 ) );
  ufAssert( ( succ3.size() == 1 ) && succ3.count( b3e1 ) );
  ufAssert( ( succ3e1.size() == 1 ) && succ3e1.count( b3e2 ) );
  ufAssert( ( succ3e2.size() == 1 ) && succ3e2.count( b3e3 ) );
  ufAssert( ( succ3e3.size() == 1 ) && succ3e3.count( b4 ) );
  ufAssert( ( succ4.size() == 1 ) && succ4.count( b2 ) );
  ufAssert( ( succ5.size() == 1 ) && succ5.count( b6 ) );
  ufAssert( ( succ6.size() == 1 ) && succ6.count( b3e1 ) );
  ufAssert( ( succ7.size() == 1 ) && succ7.count( b3 ) );
  ufAssert( ( succexit1.size() == 1 ) && succexit1.count( bexit2 ) );
  ufAssert( ( succexit2.size() == 1 ) && succexit2.count( bexit3 ) );
  ufAssert( succexit3.size() == 0 );

  WIR_BasicBlockSet pred0 = b0.getPredecessors();
  WIR_BasicBlockSet pred1 = b1.getPredecessors();
  WIR_BasicBlockSet pred2 = b2.getPredecessors();
  WIR_BasicBlockSet pred3 = b3.getPredecessors();
  WIR_BasicBlockSet pred3e1 = b3e1.getPredecessors();
  WIR_BasicBlockSet pred3e2 = b3e2.getPredecessors();
  WIR_BasicBlockSet pred3e3 = b3e3.getPredecessors();
  WIR_BasicBlockSet pred4 = b4.getPredecessors();
  WIR_BasicBlockSet pred5 = b5.getPredecessors();
  WIR_BasicBlockSet pred6 = b6.getPredecessors();
  WIR_BasicBlockSet pred7 = b7.getPredecessors();
  WIR_BasicBlockSet predexit1 = bexit1.getPredecessors();
  WIR_BasicBlockSet predexit2 = bexit2.getPredecessors();
  WIR_BasicBlockSet predexit3 = bexit3.getPredecessors();

  ufAssert( pred0.size() == 0 );
  ufAssert( ( pred1.size() == 1 ) && pred1.count( b0 ) );
  ufAssert( ( pred2.size() == 2 ) && pred2.count( b1 ) && pred2.count( b4 ) );
  ufAssert( ( pred3.size() == 2 ) && pred3.count( b2 ) && pred3.count( b7 ) );
  ufAssert(
    ( pred3e1.size() == 2 ) && pred3e1.count( b3 ) && pred3e1.count( b6 ) );
  ufAssert( ( pred3e2.size() == 1 ) && pred3e2.count( b3e1 ) );
  ufAssert( ( pred3e3.size() == 1 ) && pred3e3.count( b3e2 ) );
  ufAssert( ( pred4.size() == 1 ) && pred4.count( b3e3 ) );
  ufAssert( ( pred5.size() == 1 ) && pred5.count( b2 ) );
  ufAssert( ( pred6.size() == 1 ) && pred6.count( b5 ) );
  ufAssert( ( pred7.size() == 1 ) && pred7.count( b1 ) );
  ufAssert( predexit1.size() == 0 );
  ufAssert( ( predexit2.size() == 1 ) && predexit2.count( bexit1 ) );
  ufAssert( ( predexit3.size() == 1 ) && predexit3.count( bexit2 ) );

  ufAssert( b3e1.getContainers<WIR_Comment>().size() == 1 );
  ufAssert( b3e2.getContainers<WIR_Comment>().size() == 1 );
  ufAssert( b3e3.getContainers<WIR_Comment>().size() == 1 );

  // Now, remove all empty basic blocks from f.
  WIR_EmptyBlocks a( f );
  a.optimize();

  // Check the resulting CFG.
  succ1 = b1.getSuccessors();
  succ2 = b2.getSuccessors();
  succ3 = b3.getSuccessors();
  succ4 = b4.getSuccessors();
  succ5 = b5.getSuccessors();
  succ6 = b6.getSuccessors();
  succ7 = b7.getSuccessors();

  ufAssert( ( succ1.size() == 2 ) && succ1.count( b2 ) && succ1.count( b7 ) );
  ufAssert( ( succ2.size() == 2 ) && succ2.count( b3 ) && succ2.count( b5 ) );
  ufAssert( ( succ3.size() == 1 ) && succ3.count( b4 ) );
  ufAssert( ( succ4.size() == 1 ) && succ4.count( b2 ) );
  ufAssert( ( succ5.size() == 1 ) && succ5.count( b6 ) );
  ufAssert( ( succ6.size() == 1 ) && succ6.count( b4 ) );
  ufAssert( ( succ7.size() == 1 ) && succ7.count( b3 ) );

  pred1 = b1.getPredecessors();
  pred2 = b2.getPredecessors();
  pred3 = b3.getPredecessors();
  pred4 = b4.getPredecessors();
  pred5 = b5.getPredecessors();
  pred6 = b6.getPredecessors();
  pred7 = b7.getPredecessors();

  ufAssert( pred1.size() == 0 );
  ufAssert( ( pred2.size() == 2 ) && pred2.count( b1 ) && pred2.count( b4 ) );
  ufAssert( ( pred3.size() == 2 ) && pred3.count( b2 ) && pred3.count( b7 ) );
  ufAssert( ( pred4.size() == 2 ) && pred4.count( b3 ) && pred4.count( b6 ) );
  ufAssert( ( pred5.size() == 1 ) && pred5.count( b2 ) );
  ufAssert( ( pred6.size() == 1 ) && pred6.count( b5 ) );
  ufAssert( ( pred7.size() == 1 ) && pred7.count( b1 ) );

  ufAssert( b4.getContainers<WIR_Comment>().size() == 3 );
  set<string> commentStrings;
  for ( WIR_Comment &c : b4.getContainers<WIR_Comment>() )
    commentStrings.insert( c.getText() );
  ufAssert( commentStrings.count( "b3e1" ) );
  ufAssert( commentStrings.count( "b3e2" ) );
  ufAssert( commentStrings.count( "b3e3" ) );

  ufAssert(
    dynamic_cast<WIR_LabelParameter &>(
      b6.getInstructions().rbegin()->get().getOperations().begin()->
      get().getParameters().begin()->get() ).getName() == b4.getName() );

  return( 0 );
}
