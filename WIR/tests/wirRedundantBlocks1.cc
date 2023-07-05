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

  // For this test CFG, we need 5 basic blocks.
  WIR_BasicBlock &b1 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &b2 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &b3 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &b4 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &b5 = f.pushBackBasicBlock( {} );

  b1.pushBackInstruction(
    { { MIPS::OpCode::ADDI, MIPS::OperationFormat::RRI,
        WIR_RegisterParameter( r1, WIR_Usage::def ),
        WIR_RegisterParameter( r2, WIR_Usage::use ),
        MIPS_Immediate16_Signed( 1 ) } }  );

  b2.pushBackInstruction(
    { { MIPS::OpCode::ADDI, MIPS::OperationFormat::RRI,
        WIR_RegisterParameter( r1, WIR_Usage::def ),
        WIR_RegisterParameter( r2, WIR_Usage::use ),
        MIPS_Immediate16_Signed( -4 ) } } );

  b2.pushBackInstruction(
    { { MIPS::OpCode::BEQ, MIPS::OperationFormat::RRL,
        WIR_RegisterParameter( r1, WIR_Usage::use ),
        WIR_RegisterParameter( r2, WIR_Usage::use ),
        WIR_LabelParameter( b5 ) } } );

  b3.pushBackInstruction(
    { { MIPS::OpCode::LW, MIPS::OperationFormat::RIR_1,
        WIR_RegisterParameter( r1, WIR_Usage::def ),
        MIPS_Immediate16_Signed( -138 ),
        WIR_RegisterParameter( r2, WIR_Usage::use ) } } );

  b3.pushBackInstruction(
    { { MIPS::OpCode::JR, MIPS::OperationFormat::R_2,
        WIR_RegisterParameter( r1, WIR_Usage::use ) } } );
  b3.rbegin()->get().begin()->get().addJumpTarget( b4 );

  b4.pushBackInstruction(
    { { MIPS::OpCode::SW, MIPS::OperationFormat::RIR_2,
        WIR_RegisterParameter( r1, WIR_Usage::use ),
        MIPS_Immediate16_Signed( 112 ),
        WIR_RegisterParameter( r2, WIR_Usage::use ) } } );

  b5.pushBackInstruction(
    { { MIPS::OpCode::AND, MIPS::OperationFormat::RRR,
        WIR_RegisterParameter( r1, WIR_Usage::def ),
        WIR_RegisterParameter( r2, WIR_Usage::use ),
        WIR_RegisterParameter( r3, WIR_Usage::use ) } } );

  // Store IDs of redundant basic blocks b2 and b4.
  WIR_id_t id2 = b2.getID();
  WIR_id_t id4 = b4.getID();

  ufAssert( f.getBasicBlocks().size() == 5 );

  // Remove redundant basic blocks.
  WIR_RedundantBlocks rblocks( f );
  rblocks.optimize();

  // Check that redundant blocks are removed.
  ufAssert( f.getBasicBlocks().size() == 3 );
  for ( WIR_BasicBlock &b : f )
    ufAssert( ( b.getID() != id2 ) && ( b.getID() != id4 ) );

  // Check that all instructions are properly moved.
  ufAssert( b1.getInstructions().size() == 3 );
  ufAssert( b3.getInstructions().size() == 2 );
  ufAssert( b5.getInstructions().size() == 1 );

  return( 0 );
}
