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

  // Check nested loops with structural CFG analysis.

  WIR_Function f( "foo" );
  auto &r1 = f.pushBackVirtualRegister( MIPS_RegV() );
  auto &r2 = f.pushBackVirtualRegister( MIPS_RegV() );

  // For this test CFG, we need 11 basic blocks.
  WIR_BasicBlock &b1 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &b2 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &b3 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &b4 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &b5 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &b6 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &b7 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &b8 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &b9 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &b10 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &b11 = f.pushBackBasicBlock( {} );

  b1.pushBackInstruction(
    { { MIPS::OpCode::LW, MIPS::OperationFormat::RIR_1,
        WIR_RegisterParameter( r1, WIR_Usage::def ),
        MIPS_Immediate16_Signed( -138 ),
        WIR_RegisterParameter( r2, WIR_Usage::use ) } } );

  b2.pushBackInstruction(
    { { MIPS::OpCode::BNE, MIPS::OperationFormat::RRL,
        WIR_RegisterParameter( r1, WIR_Usage::use ),
        WIR_RegisterParameter( r2, WIR_Usage::use ),
        WIR_LabelParameter( b9 ) } } );
  b2.rbegin()->get().begin()->get().insertContainer( WIR_LoopExit( true ) );

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
    { { MIPS::OpCode::ADDI, MIPS::OperationFormat::RRI,
        WIR_RegisterParameter( r1, WIR_Usage::def ),
        WIR_RegisterParameter( r2, WIR_Usage::use ),
        MIPS_Immediate16_Signed( 0 ) } } );

  b6.pushBackInstruction(
    { { MIPS::OpCode::JR, MIPS::OperationFormat::R_2,
        WIR_RegisterParameter( r1, WIR_Usage::use ) } } );
  b6.rbegin()->get().begin()->get().addJumpTarget( b4 );
  b6.rbegin()->get().begin()->get().addJumpTarget( b7 );

  b7.pushBackInstruction(
    { { MIPS::OpCode::ADDI, MIPS::OperationFormat::RRI,
        WIR_RegisterParameter( r1, WIR_Usage::def ),
        WIR_RegisterParameter( r2, WIR_Usage::use ),
        MIPS_Immediate16_Signed( 0 ) } } );

  b8.pushBackInstruction(
    { { MIPS::OpCode::JR, MIPS::OperationFormat::R_2,
        WIR_RegisterParameter( r1, WIR_Usage::use ) } } );
  b8.rbegin()->get().begin()->get().addJumpTarget( b2 );

  b9.pushBackInstruction(
    { { MIPS::OpCode::ADDI, MIPS::OperationFormat::RRI,
        WIR_RegisterParameter( r1, WIR_Usage::def ),
        WIR_RegisterParameter( r2, WIR_Usage::use ),
        MIPS_Immediate16_Signed( 0 ) } } );

  b10.pushBackInstruction(
    { { MIPS::OpCode::ADDI, MIPS::OperationFormat::RRI,
        WIR_RegisterParameter( r1, WIR_Usage::def ),
        WIR_RegisterParameter( r2, WIR_Usage::use ),
        MIPS_Immediate16_Signed( 0 ) } } );

  b11.pushBackInstruction(
    { { MIPS::OpCode::ADDI, MIPS::OperationFormat::RRI,
        WIR_RegisterParameter( r1, WIR_Usage::def ),
        WIR_RegisterParameter( r2, WIR_Usage::use ),
        MIPS_Immediate16_Signed( 0 ) } } );

  // Do structural analysis of f.
  WIR_StructuralAnalysis a { f };
  a.analyze();

  // Check resulting control tree.
  auto &c = b2.getContainers<WIR_ControlTree>().begin()->get();
  auto &leaf = c.getBasicBlockTreeNode();
  auto &outerloop = leaf.getParent();

  ufAssert( outerloop.getType() == WIR_CTNodeType::whileloop );
  ufAssert( outerloop.isCyclic() && !outerloop.isAcyclic() );
  auto &ol = dynamic_cast<WIR_WhileLoopTreeNode &>( outerloop );
  ufAssert(
    dynamic_cast<const WIR_BasicBlockTreeNode &>(
      outerloop.getEntry() ).getBasicBlock() == b2 );

  auto &c1 = b4.getContainers<WIR_ControlTree>().begin()->get();
  auto &leaf1 = c1.getBasicBlockTreeNode();
  auto &innerloop = leaf1.getParent();

  ufAssert( innerloop.getType() == WIR_CTNodeType::naturalloop );
  ufAssert( innerloop.isCyclic() && !innerloop.isAcyclic() );
  auto &il = dynamic_cast<WIR_NaturalLoopTreeNode &>( innerloop );
  ufAssert(
    dynamic_cast<const WIR_BasicBlockTreeNode &>(
      innerloop.getEntry() ).getBasicBlock() == b4 );

  set<WIR_id_t> bbIDs;
  bbIDs.insert( b1.getID() );
  bbIDs.insert( b2.getID() );
  bbIDs.insert( b3.getID() );
  bbIDs.insert( b4.getID() );
  bbIDs.insert( b5.getID() );
  bbIDs.insert( b6.getID() );
  bbIDs.insert( b7.getID() );
  bbIDs.insert( b8.getID() );
  bbIDs.insert( b9.getID() );
  bbIDs.insert( b10.getID() );
  bbIDs.insert( b11.getID() );

  ufAssert( ol.getNodes().size() == 2 );
  for ( WIR_BasicBlock &b : ol.getBasicBlocks() )
    bbIDs.erase( b.getID() );

  ufAssert( bbIDs.size() == 4 );
  ufAssert( bbIDs.count( b1.getID() ) );
  ufAssert( bbIDs.count( b9.getID() ) );
  ufAssert( bbIDs.count( b10.getID() ) );
  ufAssert( bbIDs.count( b11.getID() ) );

  bbIDs.clear();
  bbIDs.insert( b1.getID() );
  bbIDs.insert( b2.getID() );
  bbIDs.insert( b3.getID() );
  bbIDs.insert( b4.getID() );
  bbIDs.insert( b5.getID() );
  bbIDs.insert( b6.getID() );
  bbIDs.insert( b7.getID() );
  bbIDs.insert( b8.getID() );
  bbIDs.insert( b9.getID() );
  bbIDs.insert( b10.getID() );
  bbIDs.insert( b11.getID() );

  ufAssert( il.getNodes().size() == 2 );
  for ( WIR_BasicBlock &b : il.getBasicBlocks() )
    bbIDs.erase( b.getID() );

  ufAssert( bbIDs.size() == 8 );
  ufAssert( bbIDs.count( b1.getID() ) );
  ufAssert( bbIDs.count( b2.getID() ) );
  ufAssert( bbIDs.count( b3.getID() ) );
  ufAssert( bbIDs.count( b7.getID() ) );
  ufAssert( bbIDs.count( b8.getID() ) );
  ufAssert( bbIDs.count( b9.getID() ) );
  ufAssert( bbIDs.count( b10.getID() ) );
  ufAssert( bbIDs.count( b11.getID() ) );

  return( 0 );
}
