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

  // For this test CFG, we need 7 basic blocks, plus two unreachable ones.
  WIR_BasicBlock &b1 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &b2 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &b3 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &b4 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &ur1 = f.pushBackBasicBlock( {} );        // Unreachable
  WIR_BasicBlock &b5 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &b6 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &b7 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &ur2 = f.pushBackBasicBlock( {} );        // Unreachable

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

  // Do DFS traversal of f.
  WIR_CFG cfg { f };

  // Check DFS visiting order. The unreachable blocks must be the last ones.
  auto it = cfg.getDFSOrder().begin();
  ufAssert( (it++)->get() == b1 );
  ufAssert( (it++)->get() == b2 );
  ufAssert( (it++)->get() == b3 );
  ufAssert( (it++)->get() == b4 );
  ufAssert( (it++)->get() == b5 );
  ufAssert( (it++)->get() == b6 );
  ufAssert( (it++)->get() == b7 );
  ufAssert( (it++)->get() == ur1 );
  ufAssert( (it++)->get() == ur2 );

  WIR_id_t id1 = ur1.getID();
  WIR_id_t id2 = ur2.getID();

  // Remove unreachable basic blocks.
  WIR_UnreachableBlocks urblocks( f );
  urblocks.optimize();

  // Check that unreachable blocks are removed.
  ufAssert( f.getBasicBlocks().size() == 7 );
  for ( WIR_BasicBlock &b : f )
    ufAssert( ( b.getID() != id1 ) && ( b.getID() != id2 ) );

  return( 0 );
}
