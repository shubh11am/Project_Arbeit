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

  // Check acyclic proper regions with structural CFG analysis.

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

  b2.pushBackInstruction(
    { { MIPS::OpCode::J, MIPS::OperationFormat::L,
        WIR_LabelParameter( b4 ) } } );

  b3.pushBackInstruction(
    { { MIPS::OpCode::JR, MIPS::OperationFormat::R_2,
        WIR_RegisterParameter( r1, WIR_Usage::use ) } } );
  b3.rbegin()->get().begin()->get().addJumpTarget( b4 );
  b3.rbegin()->get().begin()->get().addJumpTarget( b5 );

  b4.pushBackInstruction(
    { { MIPS::OpCode::J, MIPS::OperationFormat::L,
        WIR_LabelParameter( b6 ) } } );

  b5.pushBackInstruction(
    { { MIPS::OpCode::J, MIPS::OperationFormat::L,
        WIR_LabelParameter( b6 ) } } );

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

  // Do structural analysis of f.
  WIR_StructuralAnalysis a { f };
  a.analyze();

  // Check resulting control tree.
  auto &c = b1.getContainers<WIR_ControlTree>().begin()->get();
  auto &leaf = c.getBasicBlockTreeNode();
  auto &proper = leaf.getParent();

  ufAssert( proper.getType() == WIR_CTNodeType::proper );
  ufAssert( proper.isAcyclic() && !proper.isCyclic() );
  auto &pr = dynamic_cast<WIR_ProperTreeNode &>( proper );
  ufAssert(
    dynamic_cast<const WIR_BasicBlockTreeNode &>(
      proper.getEntry() ).getBasicBlock() == b1 );

  set<WIR_id_t> bbIDs;
  bbIDs.insert( b1.getID() );
  bbIDs.insert( b2.getID() );
  bbIDs.insert( b3.getID() );
  bbIDs.insert( b4.getID() );
  bbIDs.insert( b5.getID() );
  bbIDs.insert( b6.getID() );
  bbIDs.insert( b7.getID() );

  for ( WIR_ControlTreeNode &c : pr.getNodes() )
    if ( c.getType() == WIR_CTNodeType::bb )
      bbIDs.erase(
        dynamic_cast<WIR_BasicBlockTreeNode &>( c ).getBasicBlock().getID() );

  ufAssert( bbIDs.size() == 3 );
  ufAssert( bbIDs.count( b4.getID() ) );
  ufAssert( bbIDs.count( b6.getID() ) );
  ufAssert( bbIDs.count( b7.getID() ) );

  auto &ifthen = proper.getParent();

  ufAssert( ifthen.getType() == WIR_CTNodeType::ifthen );
  ufAssert( ifthen.isAcyclic() && !ifthen.isCyclic() );
  auto &it = dynamic_cast<WIR_IfThenTreeNode &>( ifthen );
  ufAssert( it.getEntry() == proper );
  ufAssert(
    dynamic_cast<const WIR_BasicBlockTreeNode &>(
      it.getBranch() ).getBasicBlock() == b4 );

  auto &c1 = b7.getContainers<WIR_ControlTree>().begin()->get();
  auto &reg1 = c1.getBasicBlockTreeNode().getParent();
  ufAssert( reg1.getType() == WIR_CTNodeType::block );

  auto &c2 = b6.getContainers<WIR_ControlTree>().begin()->get();
  auto &reg2 = c2.getBasicBlockTreeNode().getParent();
  ufAssert( reg2.getType() == WIR_CTNodeType::block );

  ufAssert( reg1 == reg2 );

  return( 0 );
}
